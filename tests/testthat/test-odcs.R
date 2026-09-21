test_that("ODCS round trips executable rules and preserves governance", {
  contract <- dataraft.core::dr_contract(
    "orders",
    columns = c(id = "integer", amount = "numeric"),
    key = "id",
    owner = "Risk",
    rules = list(dataraft.core::dr_quality_rule(
      ~ amount >= 0,
      action = "warn",
      threshold = .02
    )),
    governance = list(
      steward = "Reporting",
      retention = "P7Y",
      tags = c("finance", "report")
    )
  )
  document <- dr_contract_odcs(contract)
  expect_identical(document$apiVersion, "v3.2.0")
  restored <- dr_contract_from_odcs(document)
  expect_equal(restored$columns, contract$columns)
  expect_equal(restored$key, contract$key)
  expect_equal(restored$governance$retention, "P7Y")
  expect_equal(
    dataraft.core::dr_validate(
      data.frame(id = 1:2, amount = c(1, -1)),
      restored
    )$status,
    dataraft.core::dr_validate(
      data.frame(id = 1:2, amount = c(1, -1)),
      contract
    )$status
  )
  path <- tempfile(fileext = ".json")
  dr_contract_odcs(contract, path)
  expect_equal(dr_contract_from_odcs(path)$columns, contract$columns)
})

test_that("ODCS import never evaluates code or drops unsupported checks", {
  document <- dr_contract_odcs(dataraft.core::dr_contract(
    "orders",
    columns = c(id = "integer")
  ))
  document$schema[[1]]$quality <- list(list(type = "sql", query = "select 1"))
  expect_error(dr_contract_from_odcs(document), "Unsupported ODCS quality")
  expect_error(
    safe_odcs_formula("system('false')", "id"),
    "Unsupported predicate"
  )
  expect_error(safe_odcs_formula("id > 0; stop('x')", "id"), "one expression")
  expect_error(
    safe_odcs_formula("get('id') > 0", "id"),
    "Unsupported predicate"
  )
})

test_that("standard library checks execute without custom R code", {
  doc <- dr_contract_odcs(dataraft.core::dr_contract(
    "orders",
    columns = c(id = "integer"),
    required = character()
  ))
  doc$schema[[1]]$properties[[1]]$quality <- list(list(
    name = "complete",
    type = "library",
    metric = "nullValues",
    unit = "percent",
    mustBeLessOrEqualTo = 2
  ))
  contract <- dr_contract_from_odcs(doc)
  expect_equal(
    dataraft.core::dr_run_quality(
      contract$rules[[1]],
      data.frame(id = c(1L, NA))
    )$status,
    "failed"
  )
  again <- dr_contract_from_odcs(dr_contract_odcs(contract))
  expect_length(again$rules, 1L)
})

test_that("YAML project creates a compiler-free checked release", {
  skip_if_not_installed("yaml")
  root <- tempfile("yaml-project-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  contract <- dataraft.core::dr_contract("orders", columns = c(id = "integer"))
  dr_contract_odcs(contract, file.path(root, "contract.json"))
  utils::write.csv(
    data.frame(id = 1:2),
    file.path(root, "input.csv"),
    row.names = FALSE
  )
  path <- file.path(root, "project.yaml")
  yaml::write_yaml(
    list(
      id = "orders",
      contract = "contract.json",
      source = "input.csv",
      target = "releases"
    ),
    path
  )
  result <- dataraft.core::dr_run(dr_project_yaml(path))
  expect_equal(result$status, "published")
  expect_equal(dataraft.core::dr_collect(result)$id, 1:2)
  yaml::write_yaml(
    list(
      id = "orders",
      contract = "../contract.json",
      source = "input.csv",
      target = "releases"
    ),
    path
  )
  expect_error(dr_project_yaml(path), "within")
})

test_that("ODCS UUID identities and schema names survive import and re-export", {
  document <- dr_contract_odcs(dataraft.core::dr_contract(
    "orders",
    columns = c(id = "integer")
  ))
  document$id <- "123e4567-e89b-12d3-a456-426614174000"
  document$name <- "Reviewed order contract"
  restored <- dr_contract_from_odcs(document)
  expect_match(restored$id, "^odcs[.]")
  exported <- dr_contract_odcs(restored)
  expect_equal(exported$id, document$id)
  expect_equal(exported$name, document$name)
  expect_equal(exported$schema[[1]]$name, "orders")
})
