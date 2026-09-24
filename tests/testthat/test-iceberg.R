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

test_that("Iceberg transaction verifies uncertain commits without replaying writes", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE writes (id INTEGER)")
  attempts <- 0L
  after_commit_error <- function(connection) {
    DBI::dbCommit(connection)
    stop("simulated post-commit error")
  }
  committed <- iceberg_transaction(con, {
    attempts <- attempts + 1L
    DBI::dbExecute(con, "INSERT INTO writes VALUES (1)")
    "published"
  }, verify_commit = function(result) {
    identical(result, "published") &&
      identical(DBI::dbGetQuery(con, "SELECT id FROM writes")$id, 1L)
  }, commit = after_commit_error)
  expect_identical(committed, "published")
  expect_identical(attempts, 1L)

  expect_error(iceberg_transaction(con, {
    DBI::dbExecute(con, "INSERT INTO writes VALUES (2)")
    "not published"
  }, verify_commit = function(...) FALSE, commit = function(connection) {
    stop("commit rejected")
  }), "commit rejected")
  expect_identical(DBI::dbGetQuery(con, "SELECT id FROM writes")$id, 1L)
})
