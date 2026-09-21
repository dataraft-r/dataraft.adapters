api_fixture <- function(env = parent.frame()) {
  skip_if_not_installed("httr2")
  skip_if_not_installed("webfakes")
  directory <- withr::local_tempdir(.local_envir = env)
  app <- webfakes::new_app()
  app$get("/orders", function(req, res) {
    page <- if (is.null(req$query$page)) 1L else as.integer(req$query$page)
    res$set_header("X-Next", if (page < 2L) "2" else "")
    res$send_json(data.frame(id = page, amount = page * 10))
  })
  app$get(
    "/retry",
    local({
      marker <- file.path(directory, "retried")
      function(req, res) {
        if (!file.exists(marker)) {
          writeLines("attempt", marker)
          res$set_status(429L)$set_header("Retry-After", "0")$send("retry")
        } else {
          res$send_json(data.frame(id = 1L))
        }
      }
    })
  )
  app$get("/invalid", function(req, res) {
    res$send_json(list(error = "response-secret"), auto_unbox = TRUE)
  })
  app$get("/failure", function(req, res) {
    res$set_status(403L)$send("response-secret")
  })
  process <- webfakes::new_app_process(app)
  withr::defer(process$stop(), envir = env)
  list(url = sub("/$", "", process$url()), directory = directory)
}

test_that("API sources read actual paginated responses and respect retry policies", {
  server <- api_fixture()
  request <- httr2::request(paste0(server$url, "/orders"))
  next_request <- function(response) {
    next_page <- httr2::resp_header(response, "X-Next")
    if (is.null(next_page) || !nzchar(next_page)) {
      return(NULL)
    }
    httr2::req_url_query(request, page = next_page)
  }
  source <- dr_source_api(request, next_request = next_request)
  expect_equal(dr_read_source(source)$id, 1:2)
  result <- dr_product("api_orders") |>
    dr_add_source(source) |>
    dr_add_quality(~ amount > 0) |>
    dr_run()
  expect_equal(dr_collect(result)$amount, c(10, 20))
  expect_true(dr_inspect(source)$pagination)
  retry <- httr2::request(paste0(server$url, "/retry")) |>
    httr2::req_retry(max_tries = 2L, backoff = function(tries) 0)
  expect_equal(dr_read_source(dr_source_api(retry))$id, 1L)
  expect_true(file.exists(file.path(server$directory, "retried")))
  called <- FALSE
  factory <- dr_source_api(function() {
    called <<- TRUE
    request
  })
  dr_check_component(factory)
  expect_false(called)
  expect_equal(dr_read_source(factory)$id, 1L)
})

test_that("API pagination fails closed at cycles and page limits", {
  server <- api_fixture()
  request <- httr2::request(paste0(server$url, "/orders"))
  expect_error(
    dr_read_source(dr_source_api(request, next_request = function(response) {
      request
    })),
    "repeated a request"
  )
  expect_error(
    dr_read_source(dr_source_api(
      request,
      max_pages = 1L,
      next_request = function(response) {
        httr2::req_url_query(request, page = 2L)
      }
    )),
    "exceeded max_pages"
  )
  expect_error(dr_source_api(request, max_pages = 0), "positive whole")
  expect_error(
    dr_read_source(dr_source_api(function() NULL)),
    "must return an httr2 request"
  )
})

test_that("API failures and inspection do not disclose request or response secrets", {
  server <- api_fixture()
  request <- httr2::request(paste0(server$url, "/failure")) |>
    httr2::req_url_query(token = "query-secret") |>
    httr2::req_auth_bearer_token("header-secret")
  source <- dr_source_api(request)
  text <- jsonlite::toJSON(dr_inspect(source), auto_unbox = TRUE)
  expect_false(grepl("secret|127.0.0.1", text))
  failure <- tryCatch(dr_read_source(source), error = identity)
  expect_s3_class(failure, "dr_api_failed")
  expect_false(grepl("secret", conditionMessage(failure)))
  expect_null(failure$parent)
  expect_error(
    dr_read_source(dr_source_api(httr2::request(paste0(
      server$url,
      "/invalid"
    )))),
    "could not be parsed"
  )
  expect_error(
    dr_read_source(dr_source_api(
      httr2::request(paste0(server$url, "/orders")),
      parse = function(response) list(id = 1)
    )),
    "must return a data frame"
  )
})

test_that("API ingestion rejects inconsistent page schemas", {
  server <- api_fixture()
  request <- httr2::request(paste0(server$url, "/orders"))
  parse <- function(response) {
    data <- api_parse_json(response)
    if (data$id == 2L) {
      names(data)[2L] <- "changed"
    }
    data
  }
  next_request <- function(response) {
    if (!nzchar(httr2::resp_header(response, "X-Next"))) {
      return(NULL)
    }
    httr2::req_url_query(request, page = 2L)
  }
  expect_error(
    dr_read_source(dr_source_api(request, parse, next_request)),
    "different columns"
  )
})
