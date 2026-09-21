test_that("Iceberg definitions validate identifiers without opening a connection", {
  expect_error(
    dr_source_iceberg(NULL, "table", snapshot = 123),
    "decimal string"
  )
  expect_error(dr_target_iceberg(NULL, "orders"), "DBI::Id")
  x <- dr_source_iceberg(
    NULL,
    "table/metadata/v1.json",
    snapshot = "3776207205136740581"
  )
  expect_identical(x$snapshot, "3776207205136740581")
  expect_s3_class(
    dr_target_iceberg(
      NULL,
      DBI::Id(catalog = "lake", schema = "default", table = "orders")
    ),
    "dr_iceberg_target"
  )
})

test_that("Iceberg writes reject an ordinary DuckDB catalog", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  target <- dr_target_iceberg(
    con,
    DBI::Id(catalog = "memory", schema = "main", table = "orders")
  )
  expect_error(dataraft.core::dr_check_component(target), "Iceberg|ICEBERG")
})


test_that("Iceberg target scope declares missing managed release governance", {
  target <- dr_target_iceberg(
    NULL,
    DBI::Id(catalog = "lake", schema = "default", table = "orders")
  )
  expect_identical(dataraft.core::dr_capabilities(target)$governed, FALSE)
  expect_identical(dataraft.core::dr_inspect(target)$lifecycle, "experimental")
})

test_that("Iceberg write protocol validates a lazy candidate before commit", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  # Exercise the SQL/validation transaction on DuckDB; a real REST catalog is
  # still required to establish the extension's remote atomicity guarantees.
  local_mocked_bindings(
    dr_check_component.dr_iceberg_target = function(
      x,
      ...
    ) {
      invisible(x)
    },
    .package = "dataraft.adapters"
  )
  target <- dr_target_iceberg(
    con,
    DBI::Id(catalog = "memory", schema = "main", table = "orders")
  )
  context <- list(
    contract = dataraft.core::dr_contract(
      columns = c(id = "integer"),
      key = "id"
    )
  )
  output <- dataraft.core::dr_write_target(
    target,
    data.frame(id = 1:3),
    context
  )
  expect_equal(output$rows, 3)
  expect_identical(output$schema, c(id = "integer"))
  target$mode <- "append"
  error <- tryCatch(
    dataraft.core::dr_write_target(target, data.frame(id = 1L), context),
    error = identity
  )
  expect_s3_class(error, "dr_target_quality_failed")
  expect_equal(
    as.numeric(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM orders")$n),
    3
  )
})
