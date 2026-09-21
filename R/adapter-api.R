#' Read a paginated HTTP API using httr2
#'
#' Supply a prepared httr2 request, including authentication, retry and throttle
#' policies, or a zero-argument factory that builds one at execution time. When
#' no retry limit was configured, requests retry up to three attempts using
#' httr2's transient-status rules. Request details, headers, URLs and response
#' bodies are excluded from inspection and adapter error messages.
#'
#' `parse` receives each response and must return a data frame. The default reads
#' a JSON array of records, or a JSON object with a `data` array. `next_request`
#' receives each response and returns another prepared request or `NULL`.
#' Pagination must terminate before `max_pages` is exceeded; truncation fails
#' rather than silently dropping rows. Repeated identical requests are rejected.
#' Each page must have the same column names; incompatible types fail binding.
#' Authentication and rate limiting remain httr2's responsibility.
#' @param request An httr2 request or zero-argument request factory.
#' @param parse Function converting one response into a data frame.
#' @param next_request Optional function returning the next request or `NULL`.
#' @param max_pages Maximum number of pages, default 1000.
#' @returns A source specification for [dataraft.core::dr_add_source()].
#' @export
#' @examplesIf requireNamespace("httr2", quietly = TRUE)
#' # Construction does not send a request.
#' api <- dr_source_api(httr2::request("https://example.org/orders"))
#' dataraft.core::dr_inspect(api)
dr_source_api <- function(
  request,
  parse = api_parse_json,
  next_request = NULL,
  max_pages = 1000L
) {
  if (!inherits(request, "httr2_request") && !is.function(request)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      "request must be an httr2 request or a zero-argument request factory."
    )
  }
  if (!is.function(parse)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      "parse must be a response-to-data-frame function."
    )
  }
  if (!is.null(next_request) && !is.function(next_request)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      "next_request must be a function returning a request or NULL."
    )
  }
  if (
    !is.numeric(max_pages) ||
      length(max_pages) != 1L ||
      !is.finite(max_pages) ||
      max_pages < 1 ||
      max_pages != floor(max_pages)
  ) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      "max_pages must be a positive whole number."
    )
  }
  structure(
    list(
      request = request,
      parse = parse,
      next_request = next_request,
      max_pages = max_pages
    ),
    class = "dr_api_source"
  )
}


#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_api_source <- function(x, ...) {
  dataraft.core::dr_internal_need("httr2")
  invisible(x)
}


#' @export
#' @importFrom dataraft.core dr_read_source
dr_read_source.dr_api_source <- function(source, ...) {
  dataraft.core::dr_check_component(source)
  request <- if (is.function(source$request)) {
    api_safely(source$request(), "The API request factory failed.")
  } else {
    source$request
  }
  pages <- list()
  seen <- character()
  while (!is.null(request)) {
    if (!inherits(request, "httr2_request")) {
      dataraft.core::dr_internal_abort(
        subclass = "dataraft_error_adapters",
        "The API request factory or pagination callback must return an httr2 request."
      )
    }
    key <- digest::digest(
      request[c("url", "method", "headers", "body", "fields")],
      algo = "sha256"
    )
    if (key %in% seen) {
      dataraft.core::dr_internal_abort(
        subclass = "dataraft_error_adapters",
        "API pagination repeated a request. Check next_request."
      )
    }
    if (length(pages) >= source$max_pages) {
      dataraft.core::dr_internal_abort(
        subclass = "dataraft_error_adapters",
        "API pagination exceeded max_pages. Increase the limit or fix next_request."
      )
    }
    seen <- c(seen, key)
    if (
      is.null(request$policies$retry_max_tries) &&
        is.null(request$policies$retry_max_wait)
    ) {
      request <- httr2::req_retry(request, max_tries = 3L)
    }
    response <- api_safely(
      httr2::req_perform(request, verbosity = 0),
      "The API request failed. Check connectivity, credentials and HTTP policies."
    )
    data <- api_safely(
      source$parse(response),
      "The API response could not be parsed. Check the parse function."
    )
    if (!is.data.frame(data)) {
      dataraft.core::dr_internal_abort(
        subclass = "dataraft_error_adapters",
        "The API parse function must return a data frame or tibble."
      )
    }
    dataraft.core::dr_check_component(data)
    if (length(pages) && !identical(names(data), names(pages[[1L]]))) {
      dataraft.core::dr_internal_abort(
        subclass = "dataraft_error_adapters",
        "API pages have different columns. Normalize them in parse."
      )
    }
    pages[[length(pages) + 1L]] <- tibble::as_tibble(data)
    request <- if (is.null(source$next_request)) {
      NULL
    } else {
      api_safely(
        source$next_request(response),
        "The API pagination callback failed. Check next_request."
      )
    }
  }
  if (!length(pages)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      "The API request factory must return an httr2 request."
    )
  }
  api_safely(
    dplyr::bind_rows(pages),
    "API pages have incompatible column types. Normalize them in parse."
  )
}


#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_api_source <- function(x, ...) {
  list(
    type = "HTTP API",
    pagination = !is.null(x$next_request),
    max_pages = x$max_pages,
    request = if (is.function(x$request)) "factory" else "configured request"
  )
}


#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_api_source <- function(x, ...) {
  dataraft.core::dr_component_capabilities(
    read = TRUE,
    write = FALSE,
    lazy = FALSE,
    transactions = FALSE,
    partition = FALSE,
    immutable = FALSE
  )
}


api_parse_json <- function(response) {
  rlang::local_error_call(rlang::caller_env())
  value <- httr2::resp_body_json(response, simplifyVector = TRUE)
  if (!is.data.frame(value) && is.list(value) && !is.null(value$data)) {
    value <- value$data
  }
  if (!is.data.frame(value)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      "Expected a JSON array of records."
    )
  }
  tibble::as_tibble(value)
}


api_safely <- function(code, message) {
  rlang::local_error_call(rlang::caller_env())
  tryCatch(code, error = function(e) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      message,
      "dr_api_failed"
    )
  })
}
