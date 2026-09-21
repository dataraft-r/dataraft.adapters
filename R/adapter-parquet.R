#' Read Parquet files or an Arrow dataset
#'
#' Arrow handles local files, directories and supported object-storage URIs.
#' Configure remote credentials through Arrow. A lazy source returns an Arrow
#' Dataset; use dplyr verbs and [dataraft.core::dr_collect()] to materialize it. Remote availability
#' is checked when reading, not during structural validation.
#' @param path File, dataset directory or Arrow-supported URI.
#' @param lazy Return a lazy Dataset instead of an ordinary tibble.
#' @param ... Named arguments to [arrow::open_dataset()].
#' @returns A source specification for [dataraft.core::dr_add_source()].
#' @export
#' @examplesIf requireNamespace("arrow", quietly = TRUE)
#' path <- tempfile(fileext = ".parquet")
#' arrow::write_parquet(data.frame(id = 1:2), path)
#' dataraft.core::dr_read_source(dr_source_parquet(path)) |> dataraft.core::dr_collect()
#' unlink(path)
dr_source_parquet <- function(path, lazy = TRUE, ...) {
  dataraft.core::scalar(path, "path")
  dataraft.core::flag(lazy, "lazy")
  options <- list(...)
  adapter_named_options(options, c("sources", "format"))
  structure(
    list(path = path, lazy = lazy, options = options),
    class = "dr_parquet_source"
  )
}


#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_parquet_source <- function(x, ...) {
  dataraft.core::need("arrow")
  if (!dataraft.core::adapter_remote_path(x$path) && !file.exists(x$path)) {
    dataraft.core::abort(
      subclass = "dataraft_error_adapters",
      "The Parquet source is missing. Check its path."
    )
  }
  invisible(x)
}


#' @export
#' @importFrom dataraft.core dr_read_source
dr_read_source.dr_parquet_source <- function(source, ...) {
  dataraft.core::dr_check_component(source)
  data <- do.call(
    arrow::open_dataset,
    c(list(sources = source$path, format = "parquet"), source$options)
  )
  if (source$lazy) data else dplyr::collect(data)
}


#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_parquet_source <- function(x, ...) {
  list(type = "Parquet", path = adapter_path_descriptor(x$path), lazy = x$lazy)
}


#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_parquet_source <- function(x, ...) {
  dataraft.core::dr_component_capabilities(
    read = TRUE,
    write = FALSE,
    lazy = x$lazy,
    transactions = FALSE,
    partition = TRUE,
    immutable = FALSE
  )
}


#' Publish a single local Parquet file
#'
#' Writes a temporary file beside its destination, then publishes with one
#' filesystem rename. Writing or rename failure preserves the existing file.
#' Atomic replacement depends on the filesystem: platforms that cannot replace
#' an existing file this way report an error. Coordinate concurrent writers
#' externally. Remote URIs and dataset-directory writes are unsupported here.
#' @param path Local destination file. Its parent directory must already exist.
#' @param overwrite Allow replacing an existing file; defaults to `FALSE`.
#' @param ... Named arguments to [arrow::write_parquet()].
#' @returns A target specification for [dataraft.core::dr_set_target()].
#' @export
#' @examplesIf requireNamespace("arrow", quietly = TRUE)
#' path <- tempfile(fileext = ".parquet")
#' dataraft.core::dr_product("orders") |>
#'   dataraft.core::dr_add_source(data.frame(id = 1:2)) |>
#'   dataraft.core::dr_set_target(dr_target_parquet(path)) |>
#'   dataraft.core::dr_run()
#' unlink(path)
dr_target_parquet <- function(path, overwrite = FALSE, ...) {
  dataraft.core::scalar(path, "path")
  dataraft.core::flag(overwrite, "overwrite")
  if (dataraft.core::adapter_remote_path(path)) {
    dataraft.core::abort(
      subclass = "dataraft_error_adapters",
      "Use a local path for dr_target_parquet()."
    )
  }
  options <- list(...)
  adapter_named_options(options, c("x", "sink"))
  structure(
    list(
      path = dataraft.core::absolute_path(path),
      overwrite = overwrite,
      options = options
    ),
    class = "dr_parquet_target"
  )
}


#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_parquet_target <- function(x, ...) {
  dataraft.core::need("arrow")
  if (!dir.exists(dirname(x$path))) {
    dataraft.core::abort(
      subclass = "dataraft_error_adapters",
      "The Parquet target directory is missing. Create it first."
    )
  }
  if (dir.exists(x$path)) {
    dataraft.core::abort(
      subclass = "dataraft_error_adapters",
      "The Parquet target must be a file, not a directory."
    )
  }
  if (file.exists(x$path) && !x$overwrite) {
    dataraft.core::abort(
      subclass = "dataraft_error_adapters",
      "The Parquet target already exists. Set overwrite = TRUE to replace it."
    )
  }
  invisible(x)
}


#' @export
#' @importFrom dataraft.core dr_write_target
dr_write_target.dr_parquet_target <- function(target, data, context, ...) {
  dataraft.core::dr_check_component(target)
  data <- adapter_frame(data)
  candidate <- tempfile(
    ".dataraft-",
    tmpdir = dirname(target$path),
    fileext = ".parquet"
  )
  on.exit(unlink(candidate), add = TRUE)
  do.call(
    arrow::write_parquet,
    c(list(x = data, sink = candidate), target$options)
  )
  if (file.exists(target$path) && !target$overwrite) {
    dataraft.core::abort(
      subclass = "dataraft_error_adapters",
      "The Parquet target appeared during writing. Nothing was replaced."
    )
  }
  if (!suppressWarnings(file.rename(candidate, target$path))) {
    dataraft.core::abort(
      subclass = "dataraft_error_adapters",
      "Could not publish the Parquet file. The previous file was preserved."
    )
  }
  list(type = "parquet", path = target$path, rows = nrow(data))
}


#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_parquet_target <- function(x, ...) {
  list(type = "Parquet target", path = x$path, overwrite = x$overwrite)
}


#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_parquet_target <- function(x, ...) {
  dataraft.core::dr_component_capabilities(
    read = FALSE,
    write = TRUE,
    lazy = FALSE,
    transactions = FALSE,
    partition = FALSE,
    immutable = FALSE
  )
}


#' @export
#' @importFrom dataraft.core dr_read_source
dr_read_source.Dataset <- function(source, ...) source

#' @export
#' @importFrom dataraft.core dr_read_source
dr_read_source.Table <- function(source, ...) source

#' @export
#' @importFrom dataraft.core dr_read_source
dr_read_source.RecordBatch <- function(source, ...) source

#' @export
#' @importFrom dataraft.core dr_read_source
dr_read_source.arrow_dplyr_query <- function(source, ...) source

#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.Dataset <- function(x, ...) {
  dataraft.core::need("arrow")
  columns <- names(x)
  if (anyNA(columns) || any(!nzchar(columns)) || anyDuplicated(columns)) {
    dataraft.core::abort(
      subclass = "dataraft_error_adapters",
      "Arrow source column names must be non-empty and unique."
    )
  }
  invisible(x)
}

#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.Table <- dr_check_component.Dataset

#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.RecordBatch <- dr_check_component.Dataset

#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.arrow_dplyr_query <- dr_check_component.Dataset

#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.Dataset <- function(x, ...) list(type = "Arrow", columns = names(x))

#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.Table <- dr_inspect.Dataset

#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.RecordBatch <- dr_inspect.Dataset

#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.arrow_dplyr_query <- dr_inspect.Dataset

#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.Dataset <- function(x, ...) {
  dataraft.core::dr_component_capabilities(
    read = TRUE,
    write = FALSE,
    lazy = TRUE,
    transactions = FALSE,
    immutable = FALSE
  )
}

#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.Table <- dr_capabilities.Dataset

#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.RecordBatch <- dr_capabilities.Dataset

#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.arrow_dplyr_query <- dr_capabilities.Dataset


adapter_path_descriptor <- function(path) {
  rlang::local_error_call(rlang::caller_env())
  if (dataraft.core::adapter_remote_path(path)) {
    "remote object storage"
  } else {
    path
  }
}
