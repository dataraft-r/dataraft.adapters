# Exercise the actual REST catalog and object store provisioned by CI.
library(dataraft.adapters)

check_iceberg <- function() {
stopifnot(nzchar(Sys.getenv("DATARAFT_TEST_ICEBERG_ENDPOINT")))
con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
for (extension in c("httpfs", "iceberg")) {
  DBI::dbExecute(con, paste("INSTALL", extension))
  DBI::dbExecute(con, paste("LOAD", extension))
}
DBI::dbExecute(con, sprintf(
  "CREATE SECRET iceberg_s3 (TYPE S3, KEY_ID '%s', SECRET '%s', REGION 'us-east-1', ENDPOINT '127.0.0.1:9000', URL_STYLE 'path', USE_SSL false)",
  Sys.getenv("AWS_ACCESS_KEY_ID"), Sys.getenv("AWS_SECRET_ACCESS_KEY")
))
DBI::dbExecute(con, sprintf(
  "ATTACH '%s' AS iceberg_ci (TYPE ICEBERG, ENDPOINT '%s', AUTHORIZATION_TYPE 'none', ACCESS_DELEGATION_MODE 'none')",
  "warehouse", Sys.getenv("DATARAFT_TEST_ICEBERG_ENDPOINT")
))
DBI::dbExecute(con, "CREATE SCHEMA iceberg_ci.test")
table <- DBI::Id(catalog = "iceberg_ci", schema = "test", table = "orders")
contract <- dataraft.core::dr_contract(
  columns = c(id = "integer", amount = "numeric"), key = "id"
)
context <- list(contract = contract)
message("Iceberg phase: create")
created <- dataraft.core::dr_write_target(
  dr_target_iceberg(con, table),
  data.frame(id = 1:2, amount = c(10, 20)), context
)
stopifnot(created$rows == 2L, created$written_rows == 2L)
message("Iceberg phase: append")
appended <- dataraft.core::dr_write_target(
  dr_target_iceberg(con, table, mode = "append"),
  data.frame(id = 3L, amount = 30), context
)
stopifnot(appended$rows == 3L)
message("Iceberg phase: reject duplicate")
bad <- tryCatch(
  dataraft.core::dr_write_target(
    dr_target_iceberg(con, table, mode = "append"),
    data.frame(id = 3L, amount = 99), context
  ),
  error = identity
)
stopifnot(inherits(bad, "dr_target_quality_failed"))
stopifnot(identical(
  DBI::dbGetQuery(con, "SELECT id, amount FROM iceberg_ci.test.orders ORDER BY id")$id,
  1:3
))
DBI::dbExecute(con, "DETACH iceberg_ci")
DBI::dbExecute(con, sprintf(
  "ATTACH 'warehouse' AS iceberg_ci (TYPE ICEBERG, ENDPOINT '%s', AUTHORIZATION_TYPE 'none', ACCESS_DELEGATION_MODE 'none')",
  Sys.getenv("DATARAFT_TEST_ICEBERG_ENDPOINT")
))
stopifnot(nrow(DBI::dbGetQuery(con, "SELECT * FROM iceberg_ci.test.orders")) == 3L)
cat("Iceberg REST create, append, rejected candidate and catalog reopen passed.\n")

}
withCallingHandlers(
  check_iceberg(),
  error = function(e) message("Iceberg primary error: ", conditionMessage(e))
)
