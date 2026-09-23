#' Store checked deliveries as local RDS releases
#'
#' Publish a table using only R and the hard DataRaft dependencies. Each successful
#' write creates a new version directory containing data and its checksum. Existing
#' versions are never overwritten. A directory lock rejects concurrent writers;
#' staging is renamed into place only after both files are complete. This is local
#' filesystem storage, not a database transaction or a distributed lock service.
#' After a crashed writer, inspect the directory before manually removing `.lock`
#' and abandoned `.staging-*` directories. Network filesystems must provide the
#' same directory creation and rename semantics to use this adapter safely.
#' @param path Local directory containing versioned releases, not a single file.
#' @returns A target for [dataraft.core::dr_set_target()].
#' @export
#' @examples
#' path <- tempfile("orders-")
#' result <- dataraft.core::dr_product("orders", data.frame(id = 1:2)) |>
#'   dataraft.core::dr_set_target(dr_target_rds(path)) |>
#'   dataraft.core::dr_publish()
#' dataraft.core::dr_read_source(dr_source_rds(path, result$outputs$version))
#' unlink(path, recursive = TRUE)
dr_target_rds <- function(path) {
  dataraft.core::dr_internal_scalar(path, "path")
  if (adapter_remote_path(path)) {
    dataraft.core::dr_internal_abort(
      "Use a local directory for RDS releases.",
      subclass = "dataraft_error_definition"
    )
  }
  structure(
    list(path = dataraft.core::dr_internal_absolute_path(path)),
    class = "dr_rds_target"
  )
}

#' Read a versioned local RDS release
#'
#' Use the version returned by the publication result to reproduce a delivery.
#' With `version = NULL`, the latest committed version is resolved when reading.
#' A checksum detects changed data files before deserialization. Read only trusted
#' RDS directories: checksums detect accidental changes, not malicious replacement.
#' @param path Directory created by [dr_target_rds()].
#' @param version Version identifier from `result$outputs$version`, or `NULL`.
#' @returns A source for [dataraft.core::dr_add_source()].
#' @export
#' @examples
#' dr_source_rds(tempfile("releases-"), version = "v000000000001")
dr_source_rds <- function(path, version = NULL) {
  target <- dr_target_rds(path)
  if (
    !is.null(version) &&
      (!is.character(version) ||
        length(version) != 1L ||
        is.na(version) ||
        !grepl("^v[0-9]{12}$", version))
  ) {
    dataraft.core::dr_internal_abort(
      "Use the version identifier returned by the RDS publication.",
      subclass = "dataraft_error_definition"
    )
  }
  structure(
    list(path = target$path, version = version),
    class = "dr_rds_source"
  )
}

#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_rds_target <- function(x, ...) {
  dataraft.core::dr_internal_scalar(x$path, "path")
  invisible(x)
}

#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_rds_source <- dr_check_component.dr_rds_target

rds_versions <- function(path) {
  entries <- list.files(path, pattern = "^v[0-9]{12}$", full.names = TRUE)
  sort(basename(entries[dir.exists(entries)]))
}

rds_backend <- function(message, ...) {
  dataraft.core::dr_internal_abort(
    message,
    class = c("dataraft_error_backend", "dataraft_error_adapters"),
    ...
  )
}

#' @export
#' @importFrom dataraft.core dr_write_target
dr_write_target.dr_rds_target <- function(target, data, context, ...) {
  rlang::check_dots_empty()
  dataraft.core::dr_check_component(target)
  data <- adapter_frame(data)
  root <- target$path
  if (
    !dir.exists(root) &&
      !dir.create(root, recursive = TRUE, showWarnings = FALSE)
  ) {
    rds_backend(
      "Cannot create the RDS release directory. Check the path and permissions."
    )
  }
  lock <- file.path(root, ".lock")
  if (!dir.create(lock, showWarnings = FALSE)) {
    rds_backend(
      "The RDS directory is locked. Wait for the writer; inspect stale locks after a crash."
    )
  }
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  versions <- rds_versions(root)
  number <- if (length(versions)) {
    as.numeric(sub("^v", "", utils::tail(versions, 1L))) + 1
  } else {
    1
  }
  if (number > 999999999999) {
    rds_backend("The RDS version counter is exhausted. Use a new directory.")
  }
  version <- sprintf("v%012.0f", number)
  stage <- tempfile(".staging-", tmpdir = root)
  if (!dir.create(stage)) {
    rds_backend("Cannot stage the RDS release. Check permissions.")
  }
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  data_path <- file.path(stage, "data.rds")
  tryCatch(
    {
      saveRDS(data, data_path, version = 3)
      hash <- unname(tools::md5sum(data_path))
      metadata <- list(
        format = 1L,
        version = version,
        product = context$product,
        run_id = context$run_id,
        rows = nrow(data),
        hash = hash
      )
      saveRDS(metadata, file.path(stage, "metadata.rds"), version = 3)
      if (!file.rename(stage, file.path(root, version))) {
        rds_backend(
          "Cannot commit the RDS release. The previous releases remain available."
        )
      }
    },
    error = function(e) {
      rds_backend(
        "RDS publication failed. Check disk space and directory permissions.",
        parent = e
      )
    }
  )
  list(
    type = "rds",
    path = root,
    version = version,
    rows = nrow(data),
    hash = hash
  )
}

#' @export
#' @importFrom dataraft.core dr_read_source
dr_read_source.dr_rds_source <- function(source, ...) {
  rlang::check_dots_empty()
  versions <- rds_versions(source$path)
  version <- source$version %||%
    if (length(versions)) utils::tail(versions, 1L) else NULL
  if (is.null(version) || !version %in% versions) {
    rds_backend(
      "No matching RDS release exists. Publish a delivery or supply an existing version."
    )
  }
  path <- file.path(source$path, version)
  tryCatch(
    {
      metadata <- readRDS(file.path(path, "metadata.rds"))
      data_path <- file.path(path, "data.rds")
      if (
        !identical(metadata$format, 1L) ||
          !identical(metadata$version, version) ||
          !identical(metadata$hash, unname(tools::md5sum(data_path)))
      ) {
        rds_backend(
          "The RDS release failed its integrity check. Restore the original release files."
        )
      }
      adapter_frame(readRDS(data_path))
    },
    error = function(e) {
      rds_backend(
        "Cannot read the RDS release. Check its files and integrity.",
        parent = e
      )
    }
  )
}

#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_rds_target <- function(x, ...) list(type = "rds", path = x$path)
#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_rds_source <- function(x, ...) {
  list(type = "rds", path = x$path, version = x$version)
}
#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_rds_target <- function(x, ...) {
  dataraft.core::dr_component_capabilities(
    write = TRUE,
    transactions = FALSE,
    immutable = TRUE,
    lazy = FALSE
  )
}
#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_rds_source <- function(x, ...) {
  dataraft.core::dr_component_capabilities(
    read = TRUE,
    immutable = !is.null(x$version),
    lazy = FALSE,
    transactions = FALSE
  )
}
