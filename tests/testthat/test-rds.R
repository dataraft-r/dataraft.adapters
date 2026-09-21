test_that("RDS publication preserves prior versions and gates bad deliveries", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  product <- dataraft.core::dr_product("orders", data.frame(amount = 1)) |>
    dataraft.core::dr_add_quality(~ amount >= 0) |>
    dataraft.core::dr_set_target(dr_target_rds(path))
  first <- dataraft.core::dr_publish(product)
  second <- dataraft.core::dr_publish(product, data = data.frame(amount = 2))
  expect_false(identical(first$outputs$version, second$outputs$version))
  expect_equal(
    dataraft.core::dr_read_source(dr_source_rds(
      path,
      first$outputs$version
    ))$amount,
    1
  )
  expect_equal(dataraft.core::dr_read_source(dr_source_rds(path))$amount, 2)
  versions <- rds_versions(path)
  expect_error(dataraft.core::dr_publish(
    product,
    data = data.frame(amount = -1)
  ))
  expect_identical(rds_versions(path), versions)
  expect_error(
    dr_source_rds(path, "../outside"),
    class = "dataraft_error_definition"
  )
})

test_that("RDS locks and damaged releases fail explicitly", {
  path <- tempfile()
  on.exit(unlink(path, recursive = TRUE))
  target <- dr_target_rds(path)
  result <- dataraft.core::dr_write_target(
    target,
    data.frame(id = 1),
    list(product = "orders")
  )
  dir.create(file.path(path, ".lock"))
  expect_error(
    dataraft.core::dr_write_target(target, data.frame(id = 2), list()),
    class = "dataraft_error_backend"
  )
  expect_length(rds_versions(path), 1L)
  unlink(file.path(path, ".lock"), recursive = TRUE)
  saveRDS(data.frame(id = 999), file.path(path, result$version, "data.rds"))
  expect_error(
    dataraft.core::dr_read_source(dr_source_rds(path)),
    class = "dataraft_error_backend"
  )
})
