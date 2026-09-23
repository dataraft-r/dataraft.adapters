test_that("contract and commons exports preserve their declared scope", {
  skip_if_not_installed("dataraft.metrics")
  skip_if_not_installed("yaml")
  f <- fixture()
  on.exit(fixture_cleanup(f))
  path <- file.path(f$root, "contract.yaml")
  dr_contract_yaml(f$contract, path)
  x <- yaml::read_yaml(path)
  expect_equal(x$format, "dataraft-contract")
  expect_equal(x$grain, f$contract$grain)
  expect_equal(x$key, c("id", "date"))
  expect_identical(x$allow_extra, FALSE)
  dr_commons_yaml(reserve_metric(), "published_reserves", "SUM(reserve)", path)
  x <- yaml::read_yaml(path)
  expect_equal(x$tables[[1]]$definitions[[1]]$expr, "SUM(reserve)")
  expect_error(
    dr_commons_yaml(reserve_metric(), "x", "SUM(x); DROP TABLE y", path),
    "one expression"
  )
})

test_that("dm foreign keys reject orphaned references", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("dm")
  f <- fixture()
  on.exit(fixture_cleanup(f))
  dr_run(f$pipeline, f$lake)
  companies <- dr_contract(
    "risk.company_contract",
    version = "1.0.0",
    owner = "Risk",
    description = "Companies",
    grain = "One company",
    columns = c(company = "character"),
    key = "company"
  )
  p <- dr_product(
    "risk.companies",
    contract = companies,
    code_version = "v1"
  ) |>
    dr_add_source(dr_source_release(f$lake, "risk.validated")) |>
    dr_add_transform(function(data) {
      dplyr::distinct(dplyr::select(data, company))
    }) |>
    dr_set_target(f$lake)
  dr_run(p)
  tables <- c(reserves = "risk.validated", companies = "risk.companies")
  keys <- list(reserves = c("id", "date"), companies = "company")
  foreign <- list(list(
    table = "reserves",
    columns = "company",
    ref_table = "companies",
    ref_columns = "company"
  ))
  expect_s3_class(dr_model(f$lake, tables, keys, foreign), "dm")
  p$transforms[[1]] <- function(data) {
    dplyr::filter(
      dplyr::distinct(dplyr::select(data, company)),
      company == "Alpha"
    )
  }
  p$version <- "2.0.0"
  p$code_version <- "v2"
  dr_run(p)
  expect_error(
    dr_model(f$lake, tables, keys, foreign),
    class = "dr_model_invalid"
  )
})
