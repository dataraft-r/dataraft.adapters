#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_database_source <- function(x, ...) {
  list(
    type = "DBI",
    table = if (inherits(x$table, "Id")) as.list(x$table@name) else x$table,
    query = x$query,
    parameter_names = names(x$params),
    connection = if (is.function(x$connection)) "factory" else "caller-owned"
  )
}

#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_sql_transform <- function(x, ...) {
  list(type = "DuckDB SQL", query = x$query)
}
