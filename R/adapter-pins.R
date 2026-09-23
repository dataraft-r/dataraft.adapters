#' Read a table from a pins board
#'
#' Pass a configured pins board; authentication belongs to pins. A version fixes
#' the requested revision when the board supports versioning. Without a version,
#' the board's current pin is read at execution time. Some boards resolve versions
#' with identical timestamps by their content hash. For these ties, dataraft
#' uses its publication metadata to recover write order. A tie containing external
#' writes without this metadata requires an explicit `version`; no order is
#' guessed. Board credentials are excluded from inspection. Pin contents must be
#' a data frame or tibble.
#' @param board Configured pins board.
#' @param name Pin name.
#' @param version Optional pin version.
#' @returns A source specification for [dataraft.core::dr_add_source()].
#' @export
#' @examplesIf requireNamespace("pins", quietly = TRUE)
#' board <- pins::board_temp(versioned = TRUE)
#' pins::pin_write(board, data.frame(id = 1:2), "orders", type = "rds")
#' dataraft.core::dr_read_source(dr_source_pins(board, "orders"))
dr_source_pins <- function(board, name, version = NULL) {
  dataraft.core::dr_internal_scalar(name, "name")
  if (!is.null(version)) {
    dataraft.core::dr_internal_scalar(version, "version")
  }
  structure(
    list(board = board, name = name, version = version),
    class = "dr_pins_source"
  )
}


#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_pins_source <- function(x, ...) {
  dataraft.core::dr_internal_need("pins")
  if (utils::packageVersion("pins") < "1.2.0") {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      "Install optional package pins version 1.2.0 or newer."
    )
  }
  if (!inherits(x$board, "pins_board")) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      "board must be a configured pins board."
    )
  }
  invisible(x)
}


#' @export
#' @importFrom dataraft.core dr_read_source
dr_read_source.dr_pins_source <- function(source, ...) {
  dataraft.core::dr_check_component(source)
  version <- source$version
  if (is.null(version)) {
    version <- pins_metadata_version(pins_current_metadata(
      source$board,
      source$name
    ))
  }
  dataraft.core::dr_internal_frame_result(
    pins::pin_read(source$board, source$name, version = version),
    "The pin"
  )
}


#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_pins_source <- function(x, ...) {
  list(type = "pins", name = x$name, version = x$version)
}


#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_pins_source <- function(x, ...) {
  dataraft.core::dr_component_capabilities(
    read = TRUE,
    write = FALSE,
    lazy = FALSE,
    transactions = FALSE,

    immutable = FALSE
  )
}


#' Publish a checked table to a pins board
#'
#' Storage, authentication and version retention belong to pins and the board.
#' The output descriptor identifies this write using supported pins user metadata,
#' rather than assuming that a hash-sorted version is newest. Identical content
#' may reuse the current version. Coordinate writers externally; this adapter
#' does not promise atomic or immutable publication. A return to older tied
#' content can trigger pins' unchanged-content shortcut and is rejected if it
#' cannot be confirmed. After the board timestamp advances, use
#' `dr_target_pins(board, name, force_identical_write = TRUE)` to publish that content,
#' or choose a new pin name. Ordinary retries can keep hitting the same shortcut.
#' Forcing an identical version identifier within the same timestamp can fail
#' after pins updates its metadata; do not use it to bypass timestamp collisions.
#' The publication ID is verified after writing. Storage remains pins' responsibility.
#' Extra arguments go to [pins::pin_write()].
#' @param board Configured pins board.
#' @param name Pin name.
#' @param type Storage format accepted by pins. Defaults to `"rds"`.
#' @param ... Named arguments to [pins::pin_write()], such as `versioned = TRUE`.
#' @returns A target specification for [dataraft.core::dr_set_target()].
#' @export
#' @examplesIf requireNamespace("pins", quietly = TRUE)
#' board <- pins::board_temp(versioned = TRUE)
#' dataraft.core::dr_product("orders") |>
#'   dataraft.core::dr_add_source(data.frame(id = 1:2)) |>
#'   dataraft.core::dr_set_target(dr_target_pins(board, "orders")) |>
#'   dataraft.core::dr_run()
dr_target_pins <- function(board, name, type = "rds", ...) {
  dataraft.core::dr_internal_scalar(name, "name")
  dataraft.core::dr_internal_scalar(type, "type")
  options <- list(...)
  adapter_named_options(options, c("board", "x", "name", "type", "metadata"))
  structure(
    list(board = board, name = name, type = type, options = options),
    class = "dr_pins_target"
  )
}


#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_pins_target <- dr_check_component.dr_pins_source


#' @export
#' @importFrom dataraft.core dr_write_target
dr_write_target.dr_pins_target <- function(target, data, context, ...) {
  dataraft.core::dr_check_component(target)
  data <- adapter_frame(data)
  native <- previous <- NULL
  if (pins::pin_exists(target$board, target$name)) {
    native <- pins::pin_meta(target$board, target$name)
    previous <- pins_current_metadata(target$board, target$name, native)
  }
  native_is_current <- is.null(previous) ||
    identical(
      pins_metadata_version(native),
      pins_metadata_version(previous)
    )
  if (!native_is_current && !isTRUE(target$options$force_identical_write)) {
    current <- pins::pin_read(
      target$board,
      target$name,
      version = pins_metadata_version(previous)
    )
    if (identical(as.data.frame(current), as.data.frame(data))) {
      return(pins_output_descriptor(target, data, previous))
    }
  }
  publication_id <- dataraft.core::dr_internal_uid()
  publication_order <- (previous$user$dataraft$publication_order %||% 0) + 1
  do.call(
    pins::pin_write,
    c(
      list(
        board = target$board,
        x = data,
        name = target$name,
        type = target$type,
        metadata = list(
          dataraft = list(
            product = context$product,
            run_id = context$run_id,
            publication_id = publication_id,
            publication_order = publication_order,
            published_at = now()
          )
        )
      ),
      target$options
    )
  )
  meta <- pins_current_metadata(target$board, target$name)
  written <- identical(meta$user$dataraft$publication_id, publication_id)
  unchanged <- native_is_current &&
    !is.null(previous) &&
    identical(meta$pin_hash, previous$pin_hash) &&
    identical(pins_metadata_version(meta), pins_metadata_version(previous))
  if (!written && !unchanged) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      paste0(
        "pins did not confirm this publication as current. ",
        "A same-timestamp version may have triggered its unchanged-content ",
        "shortcut. After the board's timestamp advances, use ",
        "dr_target_pins(..., force_identical_write = TRUE), or use a different pin name."
      ),
      "dr_pin_unconfirmed"
    )
  }
  pins_output_descriptor(target, data, meta)
}


pins_output_descriptor <- function(target, data, meta) {
  rlang::local_error_call(rlang::caller_env())
  list(
    type = "pin",
    name = target$name,
    rows = nrow(data),
    version = pins_metadata_version(meta),
    hash = meta$pin_hash
  )
}


pins_metadata_version <- function(meta) meta$local$version %||% meta$version


pins_current_metadata <- function(
  board,
  name,
  current = pins::pin_meta(board, name)
) {
  rlang::local_error_call(rlang::caller_env())
  # A version index is not supported by every board. Without a demonstrated tie,
  # preserve the board's own definition of current.
  versions <- tryCatch(pins::pin_versions(board, name), error = function(e) {
    NULL
  })
  if (
    !is.data.frame(versions) ||
      !all(c("version", "created") %in% names(versions))
  ) {
    return(current)
  }
  position <- match(pins_metadata_version(current), versions$version)
  if (
    length(position) != 1L ||
      is.na(position) ||
      is.na(versions$created[position])
  ) {
    return(current)
  }
  tied <- which(
    !is.na(versions$created) &
      versions$created == versions$created[position]
  )
  if (length(tied) < 2L) {
    return(current)
  }
  candidates <- lapply(versions$version[tied], function(version) {
    if (identical(version, pins_metadata_version(current))) {
      current
    } else {
      pins::pin_meta(board, name, version = version)
    }
  })
  order <- vapply(
    candidates,
    function(meta) {
      value <- meta$user$dataraft$publication_order
      if (
        is.numeric(value) &&
          length(value) == 1L &&
          is.finite(value) &&
          value >= 1
      ) {
        value
      } else {
        NA_real_
      }
    },
    numeric(1)
  )
  if (anyNA(order) || anyDuplicated(order)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      paste0(
        "Several pin versions share a timestamp without an unambiguous ",
        "dataraft publication order. Supply version explicitly in ",
        "dr_source_pins(); external writes are not assumed to be older."
      ),
      "dr_pin_ambiguous"
    )
  }
  candidates[[which.max(order)]]
}


#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_pins_target <- function(x, ...) {
  list(type = "pins target", name = x$name, format = x$type)
}


#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_pins_target <- function(x, ...) {
  dataraft.core::dr_component_capabilities(
    read = FALSE,
    write = TRUE,
    lazy = FALSE,
    transactions = FALSE,

    immutable = FALSE
  )
}
