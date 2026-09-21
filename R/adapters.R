#' Export the portable contract subset
#' @param contract Contract definition.
#' @param path YAML output file.
#' @return Output path, invisibly. Executable R rules are described, never
#'   serialized as runnable YAML.
#' @export
#' @examplesIf requireNamespace("yaml", quietly = TRUE)
#' contract <- dataraft.core::dr_contract(
#'   "orders", "1.0.0", "Analytics", "Order amounts", "One order",
#'   c(order_id = "integer", amount = "numeric"), key = "order_id"
#' )
#' path <- tempfile(fileext = ".yml")
#' dr_contract_yaml(contract, path)
#' cat(readLines(path), sep = "\n")
#' unlink(path)
dr_contract_yaml <- function(contract, path) {
  dataraft.core::dr_internal_need("yaml")
  x <- list(
    format = "dataraft-contract",
    format_version = "1.0",
    id = contract$id,
    version = contract$version,
    owner = contract$owner,
    producer = contract$producer,
    operator = contract$operator %||% contract$owner,
    column_metadata = contract$column_metadata %||% list(),
    description = contract$description,
    grain = contract$grain,
    columns = contract$columns,
    required = contract$required,
    key = contract$key,
    max_age_hours = contract$max_age_hours,
    allow_empty = contract$allow_empty,
    allow_extra = contract$allow_extra,
    rules = lapply(contract$rules, function(r) {
      list(
        name = r$name,
        engine = r$engine,
        severity = r$severity,
        max_failure = r$max_failure,
        policy = r$policy %||% "rule",
        description = r$description %||% "",
        implementation = "R code in versioned project"
      )
    })
  )
  yaml::write_yaml(x, path)
  invisible(path)
}
