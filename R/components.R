#' Describe a DBI table or SQL query as a source
#'
#' Supply exactly one of `table` and `query`. The connection may be an open DBI
#' connection or a zero-argument factory. Only factory-created connections are
#' closed by dataraft. SQL is passed to DBI unchanged; use `params` for values.
#' Nothing is queried during construction or structural preflight.
#' @param connection DBI connection or function opening one.
#' @param table Table name or [DBI::Id()].
#' @param query A single SQL query string.
#' @param params Parameters passed to [DBI::dbGetQuery()]. Parameterized
#'   queries are materialized, because DBI binding is not a lazy query.
#' @param lazy Keep a caller-owned DBI table lazy. Defaults to `TRUE` for open
#'   connections without parameters and `FALSE` for factories. Factories are
#'   closed before returning and cannot provide lazy output.
#' @returns A source specification accepted by [dataraft.core::dr_add_source()].
#' @export
#' @examplesIf requireNamespace("duckdb", quietly = TRUE)
#' con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
#' DBI::dbWriteTable(con, "orders", data.frame(id = 1:2))
#' dataraft.core::dr_read_source(dr_source_database(con, table = "orders"))
#' DBI::dbDisconnect(con, shutdown = TRUE)
dr_source_database <- function(
  connection,
  table = NULL,
  query = NULL,
  params = NULL,
  lazy = !is.function(connection) && is.null(params)
) {
  dataraft.core::flag(lazy, "lazy")
  if (lazy && (is.function(connection) || !is.null(params))) {
    dataraft.core::abort(
      subclass = "dataraft_error_adapters",
      "Use lazy = FALSE for connection factories or parameterized queries."
    )
  }
  if (is.null(table) == is.null(query)) {
    dataraft.core::abort(
      subclass = "dataraft_error_adapters",
      "Supply exactly one of table or query."
    )
  }
  if (!is.function(connection) && !inherits(connection, "DBIConnection")) {
    dataraft.core::abort(
      subclass = "dataraft_error_adapters",
      "connection must be a DBI connection or a function opening one."
    )
  }
  if (!is.null(table) && !inherits(table, "Id")) {
    dataraft.core::scalar(table, "table")
  }
  if (!is.null(query)) {
    dataraft.core::scalar(query, "query")
  }
  if (!is.null(params) && is.null(query)) {
    dataraft.core::abort(
      subclass = "dataraft_error_adapters",
      "params requires a SQL query."
    )
  }
  structure(
    list(
      connection = connection,
      table = table,
      query = query,
      params = params,
      lazy = lazy
    ),
    class = "dr_database_source"
  )
}

#' @export
#' @importFrom dataraft.core dr_read_source
dr_read_source.dr_database_source <- function(source, ...) {
  con <- source$connection
  if (is.function(con)) {
    con <- con()
    if (!inherits(con, "DBIConnection")) {
      dataraft.core::abort(
        subclass = "dataraft_error_adapters",
        "The connection factory must return a DBI connection."
      )
    }
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  if (!DBI::dbIsValid(con)) {
    dataraft.core::abort(
      subclass = "dataraft_error_adapters",
      "The source connection is closed. Open it or supply a connection factory."
    )
  }
  if (isTRUE(source$lazy)) {
    relation <- if (!is.null(source$table)) {
      source$table
    } else {
      dbplyr::sql(source$query)
    }
    return(dplyr::tbl(con, relation))
  }
  if (!is.null(source$table)) {
    DBI::dbReadTable(con, source$table)
  } else if (is.null(source$params)) {
    DBI::dbGetQuery(con, source$query)
  } else {
    DBI::dbGetQuery(con, source$query, params = source$params)
  }
}


#' Apply a DuckDB SQL query to a data frame
#'
#' The input is available as `data` in a private, temporary DuckDB connection.
#' SQL is executed unchanged by DuckDB and must return a table. This adapter is
#' intended for trusted analytical queries; it does not sandbox SQL.
#' @param query SQL query referencing the input table `data`.
#' @returns A transformation accepted by [dataraft.core::dr_step_transform()].
#' @export
#' @examplesIf requireNamespace("duckdb", quietly = TRUE)
#' dataraft.core::dr_product("totals") |>
#'   dataraft.core::dr_add_source(data.frame(amount = c(10, 20))) |>
#'   dataraft.core::dr_add_recipe(dataraft.core::dr_recipe() |> dataraft.core::dr_step_transform(dr_sql_transform("SELECT sum(amount) AS total FROM data"))) |>
#'   dataraft.core::dr_run() |>
#'   dataraft.core::dr_collect()
dr_sql_transform <- function(query) {
  structure(
    list(query = dataraft.core::scalar(query, "query")),
    class = "dr_sql_transform"
  )
}

#' @export
#' @importFrom dataraft.core dr_execute_transform
dr_execute_transform.dr_sql_transform <- function(transform, data, ...) {
  dataraft.core::need("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "data", as.data.frame(data))
  DBI::dbGetQuery(con, transform$query)
}

#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_database_source <- function(x, ...) {
  if (!is.function(x$connection) && !DBI::dbIsValid(x$connection)) {
    dataraft.core::abort(
      subclass = "dataraft_error_adapters",
      "The source connection is closed. Open it or supply a connection factory."
    )
  }
  invisible(x)
}

#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_sql_transform <- function(x, ...) {
  dataraft.core::scalar(x$query, "query")
  dataraft.core::need("duckdb")
  invisible(x)
}
