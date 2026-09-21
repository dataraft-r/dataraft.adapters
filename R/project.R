#' Start a runnable data-product project
#'
#' Creates a small project with synthetic CSV data, one product definition and
#' a runner. Optional files integrate with targets, renv or Posit Connect.
#' No packages are installed, services contacted or workflows executed.
#' Existing non-empty directories are never overwritten.
#'
#' Run `source("run.R")` in the new project to create `output.rds` and durable
#' run evidence. Set `DATARAFT_EVIDENCE` to an appropriate persistent directory
#' in deployments. The renv option writes a bootstrap script; a lockfile is
#' created only when you explicitly run that script.
#' @param path New or empty project directory.
#' @param name Product identity.
#' @param renv Write `init-renv.R` for an optional isolated R environment.
#' @param targets Write `_targets.R` for dependency-aware execution.
#' @param connect Write `job.qmd` and deployment instructions for Posit Connect.
#' @returns The normalized project path, invisibly.
#' @seealso [dataraft.core::dr_product()], [dr_as_targets()]
#' @export
#' @examples
#' path <- tempfile("orders-project-")
#' dr_init_project(path)
#' list.files(path)
#' unlink(path, recursive = TRUE)
dr_init_project <- function(
  path,
  name = "orders",
  renv = FALSE,
  targets = FALSE,
  connect = FALSE
) {
  dataraft.core::dr_internal_asset_id(name)
  dataraft.core::dr_internal_flag(renv, "renv")
  dataraft.core::dr_internal_flag(targets, "targets")
  dataraft.core::dr_internal_flag(connect, "connect")
  path <- dataraft.core::dr_internal_absolute_path(path)
  if (file.exists(path) && !dir.exists(path)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      "path is a file. Choose a new or empty project directory."
    )
  }
  if (
    dir.exists(path) && length(list.files(path, all.files = TRUE, no.. = TRUE))
  ) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      "Choose a new or empty project directory; existing files are never overwritten."
    )
  }
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_adapters",
      "Could not create the project directory."
    )
  }
  dir.create(file.path(path, "data"), showWarnings = FALSE)
  templates <- c(
    "definitions.R",
    "run.R",
    "project-README.md",
    "project-gitignore"
  )
  if (renv) {
    templates <- c(templates, "init-renv.R")
  }
  if (targets) {
    templates <- c(templates, "_targets.R")
  }
  if (connect) {
    templates <- c(templates, "job.qmd")
  }
  for (template in templates) {
    input <- system.file("templates", template, package = "dataraft.adapters")
    if (!nzchar(input)) {
      dataraft.core::dr_internal_abort(
        subclass = "dataraft_error_adapters",
        paste("Missing project template:", template)
      )
    }
    text <- readLines(input, warn = FALSE)
    text <- gsub("PROJECT_PRODUCT_ID", deparse(name), text, fixed = TRUE)
    output <- switch(
      template,
      "project-README.md" = "README.md",
      "project-gitignore" = ".gitignore",
      template
    )
    writeLines(text, file.path(path, output))
  }
  utils::write.csv(
    data.frame(id = 1:3, amount = c(25, 75, 50)),
    file.path(path, "data", "input.csv"),
    row.names = FALSE
  )
  invisible(path)
}
