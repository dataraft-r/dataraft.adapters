#' Experimental Iceberg reads and ungoverned REST catalog writes
#'
#' Uses a caller-owned DuckDB connection with the Iceberg extension already
#' loaded. Configure REST catalogs and credentials outside product definitions.
#' Sources use iceberg_scan() on a metadata file or table location. Pin a
#' snapshot ID to reproduce a read; retention remains the catalog's policy.
#'
#' Targets require DBI::Id(catalog=, schema=, table=) in an attached Iceberg
#' REST catalog. Create fails if the table exists; append requires an existing
#' table. The stored candidate is checked lazily inside a transaction
#' before commit. Catalogs must support staged writes and transactions; uncertain
#' failures are propagated, with no nontransactional fallback. No replace, branch/merge,
#' snapshot-retention or cross-table atomicity guarantee is made.
#' Each write stores a `dataraft.write-id` Iceberg table property. If DuckDB
#' reports an error after commit, the adapter checks this marker and the row
#' count before accepting the already published write; uncertain outcomes fail.
#'
#' This experimental adapter delegates to DuckDB's Iceberg extension. Writes
#' are not DataRaft lake releases: no registry release, lineage edge or managed
#' rollback is created. Use a managed lake target when release governance is
#' required. Lazy validation avoids collecting the entire table by default;
#' custom quality rules may still collect data.
#' @param connection Caller-owned DuckDB connection.
#' @param path Iceberg metadata file or table location.
#' @param snapshot Optional snapshot ID as a decimal string, never a double.
#' @param table Fully qualified DBI::Id in an attached Iceberg catalog.
#' @param mode Create a new table or append to an existing one.
#' @returns A source or target specification.
#' @export
dr_source_iceberg <- function(connection, path, snapshot = NULL) {
  dataraft.core::dr_internal_scalar(path, "path")
  if (
    !is.null(snapshot) &&
      (!is.character(snapshot) ||
        length(snapshot) != 1L ||
        is.na(snapshot) ||
        !grepl("^[0-9]+$", snapshot))
  ) {
    stop("snapshot must be a decimal string.")
  }
  structure(
    list(connection = connection, path = path, snapshot = snapshot),
    class = "dr_iceberg_source"
  )
}

#' @rdname dr_source_iceberg
#' @export
dr_target_iceberg <- function(connection, table, mode = c("create", "append")) {
  if (
    !inherits(table, "Id") ||
      !setequal(names(table@name), c("catalog", "schema", "table"))
  ) {
    stop(
      "Use DBI::Id(catalog = ..., schema = ..., table = ...) for an attached Iceberg REST catalog."
    )
  }
  structure(
    list(connection = connection, table = table, mode = match.arg(mode)),
    class = "dr_iceberg_target"
  )
}

iceberg_connection <- function(connection) {
  dataraft.core::dr_internal_need("duckdb")
  if (
    !inherits(connection, "duckdb_connection") || !DBI::dbIsValid(connection)
  ) {
    stop("Supply an open caller-owned DuckDB connection.")
  }
  loaded <- DBI::dbGetQuery(
    connection,
    "SELECT loaded FROM duckdb_extensions() WHERE extension_name = 'iceberg'"
  )
  if (nrow(loaded) != 1L || !isTRUE(loaded$loaded[[1]])) {
    stop(
      "Load the DuckDB Iceberg extension before execution: INSTALL iceberg; LOAD iceberg;"
    )
  }
  connection
}

iceberg_transaction <- function(connection, code, verify_commit = NULL,
                                commit = DBI::dbCommit) {
  DBI::dbBegin(connection)
  committing <- FALSE
  tryCatch(
    {
      result <- force(code)
      committing <- TRUE
      commit(connection)
      result
    },
    error = function(e) {
      # DuckDB may have already aborted a remote catalog transaction. Preserve
      # the original error when a second rollback reports no active transaction.
      try(DBI::dbRollback(connection), silent = TRUE)
      # DuckDB can report an error after the REST catalog has committed. Never
      # repeat an uncertain write. Only acknowledge the exact operation whose
      # marker and stored rows are visible in the catalog.
      if (committing && !is.null(verify_commit) &&
          isTRUE(tryCatch(verify_commit(result), error = function(...) FALSE))) {
        message("Iceberg write verified in the REST catalog after a DuckDB commit error.")
        return(result)
      }
      stop(e)
    }
  )
}

iceberg_commit_visible <- function(connection, table, write_id, rows) {
  table_sql <- as.character(DBI::dbQuoteIdentifier(connection, table))
  property_sql <- as.character(DBI::dbQuoteString(connection, "dataraft.write-id"))
  actual <- DBI::dbGetQuery(connection, paste0(
    "SELECT value FROM iceberg_table_properties(", table_sql,
    ") WHERE key = ", property_sql
  ))
  nrow(actual) == 1L && identical(actual$value[[1L]], write_id) &&
    count_rows(dplyr::tbl(connection, table)) == rows
}

#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_iceberg_source <- function(x, ...) {
  iceberg_connection(x$connection)
  invisible(x)
}

#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_iceberg_target <- function(x, ...) {
  con <- iceberg_connection(x$connection)
  catalogs <- DBI::dbGetQuery(
    con,
    "SELECT database_name, type FROM duckdb_databases()"
  )
  if (
    !any(
      catalogs$database_name == x$table@name[["catalog"]] &
        tolower(catalogs$type) == "iceberg"
    )
  ) {
    stop("The target catalog must be attached with TYPE ICEBERG.")
  }
  invisible(x)
}

#' @export
#' @importFrom dataraft.core dr_read_source
dr_read_source.dr_iceberg_source <- function(source, ...) {
  con <- iceberg_connection(source$connection)
  sql <- paste0(
    "SELECT * FROM iceberg_scan(",
    DBI::dbQuoteString(con, source$path),
    if (!is.null(source$snapshot)) {
      paste0(", snapshot_from_id = ", source$snapshot)
    } else {
      ""
    },
    ")"
  )
  DBI::dbGetQuery(con, sql)
}

#' @export
#' @importFrom dataraft.core dr_write_target
dr_write_target.dr_iceberg_target <- function(target, data, context, ...) {
  dataraft.core::dr_check_component(target)
  con <- target$connection
  data <- adapter_frame(data)
  contract <- context$contract
  if (!inherits(contract, "dr_contract")) {
    stop("Iceberg writes require an explicit contract.")
  }
  database_integer64_guard(con, data)
  name <- paste0(
    "dr_iceberg_",
    gsub("[^a-zA-Z0-9]", "", dataraft.core::dr_internal_uid())
  )
  duckdb::duckdb_register(con, name, as.data.frame(data))
  on.exit(duckdb::duckdb_unregister(con, name), add = TRUE)
  table <- as.character(DBI::dbQuoteIdentifier(con, target$table))
  source <- as.character(DBI::dbQuoteIdentifier(con, name))
  write_id <- dataraft.core::dr_internal_uid()
  marker <- as.character(DBI::dbQuoteString(con, "dataraft.write-id"))
  marker_value <- as.character(DBI::dbQuoteString(con, write_id))
  iceberg_transaction(con, {
    sql <- if (target$mode == "create") {
      paste("CREATE TABLE", table, "WITH (", marker, "=", marker_value,
            ") AS SELECT * FROM", source)
    } else {
      paste("INSERT INTO", table, "BY NAME SELECT * FROM", source)
    }
    DBI::dbExecute(con, sql)
    if (target$mode == "append") {
      DBI::dbExecute(con, paste0(
        "CALL set_iceberg_table_properties(", table, ", {",
        marker, ": ", marker_value, "})"
      ))
    }
    database_integer64_guard(con, data, target$table)
    candidate <- dplyr::tbl(con, target$table)
    quality <- dataraft.core::dr_validate(
      candidate,
      contract,
      keep_errors = TRUE
    )
    if (!dataraft.core::dr_internal_quality_ok(quality)) {
      dataraft.core::dr_internal_abort(
        "Stored Iceberg candidate failed its contract; rolling back.",
        class = "dr_target_quality_failed",
        quality = quality
      )
    }
    list(
      type = "iceberg",
      table = database_table_descriptor(target$table),
      mode = target$mode,
      rows = count_rows(candidate),
      written_rows = nrow(data),
      candidate_quality = quality,
      schema = dataraft.core::dr_internal_infer_column_types(candidate)
    )
  }, verify_commit = function(result) {
    iceberg_commit_visible(con, target$table, write_id, result$rows)
  })
}

#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_iceberg_source <- function(x, ...) {
  list(
    type = "Iceberg",
    path = adapter_path_descriptor(x$path),
    snapshot = x$snapshot
  )
}
#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_iceberg_target <- function(x, ...) {
  list(
    type = "Iceberg REST target",
    lifecycle = "experimental",
    governed = FALSE,
    table = database_table_descriptor(x$table),
    mode = x$mode
  )
}
#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_iceberg_source <- function(x, ...) {
  dataraft.core::dr_component_capabilities(
    read = TRUE,
    write = FALSE,
    lazy = FALSE,
    immutable = !is.null(x$snapshot)
  )
}
#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_iceberg_target <- function(x, ...) {
  capabilities <- dataraft.core::dr_component_capabilities(
    read = FALSE,
    write = TRUE,
    lazy = FALSE,
    transactions = NA,
    immutable = FALSE
  )
  c(capabilities, list(governed = FALSE, lifecycle = "experimental"))
}
