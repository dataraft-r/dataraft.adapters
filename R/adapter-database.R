#' Publish a checked table through DBI
#'
#' Writing and candidate validation require a DBI transaction. Before committing,
#' the adapter reads the complete stored table and checks it against the contract.
#' This catches constraint violations caused by driver casts as well as append
#' conflicts with existing rows. It materializes the stored table in R. Append
#' requires an explicit contract; direct replacement without one infers structure
#' from the incoming table. [dataraft.core::dr_run()] always supplies its resolved contract.
#'
#' The driver and table storage must support transactions, including rollback of
#' replacement DDL. No silent fallback is provided. Coordinate concurrent writers
#' externally; isolation and transactional DDL remain the driver's responsibility.
#' Transactional SQLite and DuckDB are covered by the package tests. An arbitrary
#' DBI driver or nontransactional table is not certified by a successful preflight.
#' DuckDB connections reading or writing `BIGINT`/`integer64` columns must use
#' `bigint = "integer64"` to prevent lossy conversion. This is checked before
#' appending to an existing BIGINT table and before validating a stored table.
#' For append, `dataraft.core::dr_collect(result)` returns the transformed incoming batch;
#' `result$outputs$rows` counts the complete destination and `written_rows`
#' counts appended rows. Run quality evidence describes the complete candidate.
#'
#' Caller connections stay open; factory connections close after success and
#' failure. Extra arguments go to [DBI::dbWriteTable()]. Reserved arguments
#' `conn`, `name`, `value`, `append`, and `overwrite` cannot be overridden.
#' @param connection DBI connection or zero-argument connection factory.
#' @param table Table name or [DBI::Id()].
#' @param mode Replace the table or append, with a gate before commit.
#' @param transaction Must be `TRUE`. Nontransactional publication requires a
#'   custom [dataraft.core::dr_write_target()] adapter with an explicit consistency policy.
#' @param ... Named additional arguments to [DBI::dbWriteTable()].
#' @returns A target specification for [dataraft.core::dr_set_target()].
#' @export
#' @examplesIf requireNamespace("RSQLite", quietly = TRUE)
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' result <- dataraft.core::dr_product("orders") |>
#'   dataraft.core::dr_add_source(data.frame(id = 1:2)) |>
#'   dataraft.core::dr_set_target(dr_target_database(con, "orders")) |>
#'   dataraft.core::dr_run()
#' DBI::dbReadTable(con, "orders")
#' DBI::dbDisconnect(con)
dr_target_database <- function(
  connection,
  table,
  mode = c("replace", "append"),
  transaction = TRUE,
  ...
) {
  mode <- match.arg(mode)
  dataraft.core::dr_internal_flag(transaction, "transaction")
  if (!is.function(connection) && !inherits(connection, "DBIConnection")) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      "connection must be a DBI connection or a function opening one."
    )
  }
  if (!inherits(table, "Id")) {
    dataraft.core::dr_internal_scalar(table, "table")
  }
  if (!transaction) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      paste0(
        "dr_target_database() requires transaction = TRUE. ",
        "Use a custom dr_write_target() adapter for nontransactional publication."
      )
    )
  }
  options <- list(...)
  adapter_named_options(
    options,
    c("conn", "name", "value", "append", "overwrite")
  )
  structure(
    list(
      connection = connection,
      table = table,
      mode = mode,
      transaction = transaction,
      options = options
    ),
    class = "dr_database_target"
  )
}


#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_database_target <- function(x, ...) {
  dataraft.core::dr_internal_need("DBI")
  if (!isTRUE(x$transaction)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      "dr_target_database() requires transaction = TRUE to protect its stored-data gate."
    )
  }
  if (!is.function(x$connection) && !DBI::dbIsValid(x$connection)) {
    dataraft.core::dr_internal_abort(
      subclass = c("dataraft_error_backend", "dataraft_error_adapters"),
      "The target connection is closed. Open it or use a connection factory."
    )
  }
  invisible(x)
}


#' @export
#' @importFrom dataraft.core dr_write_target
dr_write_target.dr_database_target <- function(target, data, context, ...) {
  dataraft.core::dr_check_component(target)
  data <- adapter_frame(data)
  con <- target$connection
  if (is.function(con)) {
    con <- con()
    if (!inherits(con, "DBIConnection")) {
      dataraft.core::dr_internal_abort(
        subclass = "dataraft_error_adapters",
        "The connection factory must return a DBI connection."
      )
    }
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  if (!DBI::dbIsValid(con)) {
    dataraft.core::dr_internal_abort(
      subclass = c("dataraft_error_backend", "dataraft_error_adapters"),
      "The target connection is closed."
    )
  }
  database_integer64_guard(con, data)
  candidate_quality <- NULL
  candidate_schema <- NULL
  schema <- context$contract
  if (is.null(schema)) {
    if (target$mode == "append") {
      dataraft.core::dr_internal_abort(
        subclass = "dataraft_error_adapters",
        "Append requires a contract to check the full publication candidate."
      )
    }
    schema <- dataraft.core::dr_internal_automatic_schema(
      "target",
      dataraft.core::dr_internal_automatic_types(dataraft.core::dr_internal_infer_column_types(
        data
      ))
    )
  }
  write <- function() {
    rlang::local_error_call(rlang::caller_env())
    if (target$mode == "append" && DBI::dbExistsTable(con, target$table)) {
      database_integer64_guard(con, data, target$table)
    }
    do.call(
      DBI::dbWriteTable,
      c(
        list(
          conn = con,
          name = target$table,
          value = as.data.frame(data),
          overwrite = target$mode == "replace",
          append = target$mode == "append"
        ),
        target$options
      )
    )
    database_integer64_guard(con, data, target$table)
    candidate <- DBI::dbReadTable(con, target$table)
    checks <- dataraft.core::dr_validate(candidate, schema, keep_errors = TRUE)
    candidate_quality <<- checks
    if (!dataraft.core::dr_internal_quality_ok(checks)) {
      dataraft.core::dr_internal_abort(
        subclass = "dataraft_error_adapters",
        "The stored publication candidate failed its contract. Rolling back the transaction.",
        "dr_target_quality_failed",
        quality = checks
      )
    }
    candidate_schema <<- dataraft.core::dr_internal_infer_column_types(
      candidate
    )
    nrow(candidate)
  }
  rows <- DBI::dbWithTransaction(con, write())
  list(
    type = "database",
    table = database_table_descriptor(target$table),
    mode = target$mode,
    rows = rows,
    written_rows = nrow(data),
    transaction = target$transaction,
    schema = candidate_schema,
    candidate_quality = candidate_quality
  )
}


#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_database_target <- function(x, ...) {
  list(
    type = "DBI target",
    table = database_table_descriptor(x$table),
    mode = x$mode,
    transaction = x$transaction,
    connection = if (is.function(x$connection)) "factory" else "caller-owned"
  )
}


#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_database_target <- function(x, ...) {
  dataraft.core::dr_component_capabilities(
    write = TRUE,
    lazy = FALSE,
    transactions = NA,

    immutable = FALSE
  )
}


database_table_descriptor <- function(table) {
  rlang::local_error_call(rlang::caller_env())
  if (inherits(table, "Id")) as.list(table@name) else table
}


database_integer64_guard <- function(con, data, table = NULL) {
  rlang::local_error_call(rlang::caller_env())
  if (!inherits(con, "duckdb_connection")) {
    return(invisible(NULL))
  }
  needs_exact <- any(vapply(data, inherits, logical(1), "integer64"))
  if (!is.null(table)) {
    # dbColumnInfo() reports an R type after conversion, so inspect DuckDB's
    # declared SQL types. Identifiers are quoted by the driver, never interpolated.
    description <- DBI::dbGetQuery(
      con,
      paste(
        "DESCRIBE SELECT * FROM",
        DBI::dbQuoteIdentifier(con, table)
      )
    )
    needs_exact <- needs_exact ||
      any(grepl("BIGINT", description$column_type, fixed = TRUE))
  }
  if (!needs_exact) {
    return(invisible(NULL))
  }
  # DuckDB exposes no public accessor for its R conversion mode. Read only this
  # driver-local option and fail closed if its representation changes.
  options <- tryCatch(methods::slot(con, "convert_opts"), error = function(e) {
    NULL
  })
  if (!identical(options$bigint, "integer64")) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      paste0(
        "DuckDB targets with BIGINT or integer64 columns require a connection ",
        "opened with bigint = 'integer64'. The transaction cannot be committed."
      )
    )
  }
  invisible(NULL)
}


adapter_frame <- function(data) {
  rlang::local_error_call(rlang::caller_env())
  if (is.data.frame(data)) {
    return(tibble::as_tibble(data))
  }
  dataraft.core::dr_collect(data)
}


adapter_named_options <- function(options, reserved = character()) {
  rlang::local_error_call(rlang::caller_env())
  if (
    length(options) &&
      (is.null(names(options)) ||
        anyNA(names(options)) ||
        any(!nzchar(names(options))) ||
        anyDuplicated(names(options)))
  ) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      "Adapter options must have unique, non-empty names."
    )
  }
  if (any(names(options) %in% reserved)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      paste(
        "These adapter options are reserved:",
        paste(intersect(names(options), reserved), collapse = ", ")
      )
    )
  }
  invisible(options)
}
