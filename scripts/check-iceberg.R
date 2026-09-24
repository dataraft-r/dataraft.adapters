# Exercise the actual REST catalog and object store provisioned by CI.
library(dataraft.adapters)

stopifnot(nzchar(Sys.getenv("DATARAFT_TEST_ICEBERG_ENDPOINT")))
open_catalog <- function() {
  connection <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
  for (extension in c("httpfs", "iceberg")) {
    DBI::dbExecute(connection, paste("INSTALL", extension))
    DBI::dbExecute(connection, paste("LOAD", extension))
  }
  DBI::dbExecute(connection, sprintf(
    "CREATE SECRET iceberg_s3 (TYPE S3, KEY_ID '%s', SECRET '%s', REGION 'us-east-1', ENDPOINT '127.0.0.1:9000', URL_STYLE 'path', USE_SSL false)",
    Sys.getenv("AWS_ACCESS_KEY_ID"), Sys.getenv("AWS_SECRET_ACCESS_KEY")
  ))
  DBI::dbExecute(connection, sprintf(
    "ATTACH 'warehouse' AS iceberg_ci (TYPE ICEBERG, ENDPOINT '%s', AUTHORIZATION_TYPE 'none', ACCESS_DELEGATION_MODE 'none', DISABLE_MULTI_TABLE_COMMIT true)",
    Sys.getenv("DATARAFT_TEST_ICEBERG_ENDPOINT")
  ))
  connection
}

check_iceberg <- function(iteration) {
  con <- open_catalog()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  schema <- paste0("test", iteration)
  DBI::dbExecute(con, paste("CREATE SCHEMA iceberg_ci.", schema, sep = ""))
  table <- DBI::Id(catalog = "iceberg_ci", schema = schema, table = "orders")
  contract <- dataraft.core::dr_contract(
    columns = c(id = "integer", amount = "numeric"), key = "id"
  )
  context <- list(contract = contract)
  write_id <- function(connection) DBI::dbGetQuery(connection, paste0(
    "SELECT value FROM iceberg_table_properties(",
    DBI::dbQuoteIdentifier(connection, table),
    ") WHERE key = 'dataraft.write-id'"
  ))$value
  message("Iceberg phase: create")
  created <- dataraft.core::dr_write_target(
    dr_target_iceberg(con, table),
    data.frame(id = 1:2, amount = c(10, 20)), context
  )
  stopifnot(created$rows == 2L, created$written_rows == 2L)
  created_id <- write_id(con)
  stopifnot(length(created_id) == 1L)
  DBI::dbDisconnect(con, shutdown = TRUE)
  con <- open_catalog()
  message("Iceberg phase: append")
  appended <- dataraft.core::dr_write_target(
    dr_target_iceberg(con, table, mode = "append"),
    data.frame(id = 3L, amount = 30), context
  )
  stopifnot(appended$rows == 3L)
  appended_id <- write_id(con)
  stopifnot(length(appended_id) == 1L, appended_id != created_id)
  message("Iceberg phase: reject duplicate")
  bad <- tryCatch(
    dataraft.core::dr_write_target(
      dr_target_iceberg(con, table, mode = "append"),
      data.frame(id = 3L, amount = 99), context
    ),
    error = identity
  )
  stopifnot(inherits(bad, "dr_target_quality_failed"))
  stopifnot(identical(write_id(con), appended_id))
  stopifnot(identical(
    DBI::dbGetQuery(con, paste("SELECT id, amount FROM", DBI::dbQuoteIdentifier(con, table), "ORDER BY id"))$id,
    1:3
  ))
  DBI::dbDisconnect(con, shutdown = TRUE)
  con <- open_catalog()
  stopifnot(nrow(DBI::dbGetQuery(con, paste("SELECT * FROM", DBI::dbQuoteIdentifier(con, table)))) == 3L)
  cat("Iceberg REST create, append, rejected candidate and catalog reopen passed.\n")
}
withCallingHandlers(
  for (iteration in seq_len(5L)) {
    message("Iceberg iteration: ", iteration)
    check_iceberg(iteration)
  },
  error = function(e) message("Iceberg primary error: ", conditionMessage(e))
)

# Exercise both sides of the uncertain-commit decision against the real catalog.
check_commit_reconciliation <- function() {
  con <- open_catalog()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE))
  DBI::dbExecute(con, "CREATE SCHEMA iceberg_ci.recovery")
  write <- function(table_name, write_id, commit) {
    table <- DBI::Id(catalog = "iceberg_ci", schema = "recovery", table = table_name)
    dataraft.adapters:::iceberg_transaction(con, {
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", DBI::dbQuoteIdentifier(con, table),
        " WITH ('dataraft.write-id' = ", DBI::dbQuoteString(con, write_id),
        ") AS SELECT 1::INTEGER AS id"
      ))
      list(rows = 1L)
    }, verify_commit = function(result) {
      dataraft.adapters:::iceberg_commit_visible(con, table, write_id, result$rows)
    }, commit = commit)
  }
  published <- write("published", "ci-published", function(connection) {
    DBI::dbCommit(connection)
    stop("simulated error after commit")
  })
  stopifnot(identical(published$rows, 1L))
  rejected <- tryCatch(write("rejected", "ci-rejected", function(connection) {
    stop("simulated error before commit")
  }), error = identity)
  stopifnot(inherits(rejected, "error"),
            grepl("simulated error before commit", conditionMessage(rejected), fixed = TRUE))
  DBI::dbDisconnect(con, shutdown = TRUE)
  con <- open_catalog()
  stopifnot(!DBI::dbExistsTable(con, DBI::Id(
    catalog = "iceberg_ci", schema = "recovery", table = "rejected"
  )))
  message("Iceberg verified published commit and rejected uncommitted write.")
}
check_commit_reconciliation()
