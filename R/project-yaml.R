#' Read a file-first checked delivery project
#'
#' A YAML project declares id, contract (an ODCS file), source (a CSV file),
#' and target (an RDS release directory). Paths are resolved relative to the
#' project file and must stay inside its directory, including symbolic links.
#' YAML is parsed with expression evaluation disabled. Custom R transformations
#' can be added explicitly to the returned product in trusted project code.
#' @param path Project YAML path.
#' @returns A product definition; no source data are read or targets written.
#' @export
dr_project_yaml <- function(path) {
  dataraft.core::dr_internal_need("yaml")
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  root <- dirname(path)
  project <- yaml::read_yaml(path, eval.expr = FALSE)
  if (
    !is.list(project) ||
      !setequal(names(project), c("id", "contract", "source", "target"))
  ) {
    stop("Project YAML needs exactly id, contract, source and target.")
  }
  resolve <- function(value, exists = TRUE) {
    dataraft.core::dr_internal_scalar(value, "project path")
    if (grepl("^(/|[A-Za-z]:|\\\\)|(^|[/\\\\])[.][.]([/\\\\]|$)", value)) {
      stop("Project paths must stay within the project directory.")
    }
    candidate <- file.path(root, value)
    parent <- candidate
    while (!file.exists(parent) && dirname(parent) != parent) {
      parent <- dirname(parent)
    }
    actual <- normalizePath(parent, winslash = "/", mustWork = TRUE)
    if (actual != root && !startsWith(actual, paste0(root, "/"))) {
      stop("Project paths must not escape through symbolic links.")
    }
    if (exists && !file.exists(candidate)) {
      stop("Project input does not exist: ", value)
    }
    candidate
  }
  source <- resolve(project$source)
  if (!grepl("[.]csv$", source, ignore.case = TRUE)) {
    stop("The file-first source must be CSV.")
  }
  dataraft.core::dr_product(project$id) |>
    dataraft.core::dr_add_contract(dr_contract_from_odcs(resolve(
      project$contract
    ))) |>
    dataraft.core::dr_add_source(source) |>
    dataraft.core::dr_set_target(dr_target_rds(resolve(project$target, FALSE)))
}
