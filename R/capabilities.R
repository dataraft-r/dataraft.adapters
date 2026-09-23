#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_database_source <- function(x, ...) {
  dataraft.core::dr_component_capabilities(
    read = TRUE,
    write = FALSE,
    lazy = x$lazy,

    immutable = FALSE
  )
}

#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_sql_transform <- function(x, ...) {
  dataraft.core::dr_component_capabilities(
    read = FALSE,
    write = FALSE,
    lazy = FALSE,
    transactions = FALSE,

    immutable = FALSE
  )
}
