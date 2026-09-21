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
