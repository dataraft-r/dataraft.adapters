test_that("pins targets expose readable version references without board credentials", {
  skip_if_not_installed("pins")
  board <- pins::board_temp(versioned = TRUE)
  old <- data.frame(id = 1L)
  target <- dr_target_pins(board, "orders")
  output <- dataraft.core::dr_write_target(
    target,
    old,
    list(product = "orders", run_id = "r1")
  )
  expect_true(nzchar(output$version))
  dataraft.core::dr_write_target(
    target,
    data.frame(id = 2L),
    list(product = "orders", run_id = "r2")
  )
  expect_equal(
    dataraft.core::dr_read_source(dr_source_pins(
      board,
      "orders",
      output$version
    ))$id,
    1L
  )
  expect_equal(
    dataraft.core::dr_read_source(dr_source_pins(board, "orders"))$id,
    2L
  )
  expect_null(dataraft.core::dr_inspect(target)$board)
  expect_false(dataraft.core::dr_capabilities(target)$immutable)
  expect_false(dataraft.core::dr_capabilities(target)$transactions)
  expect_error(
    dataraft.core::dr_check_component(dr_source_pins(
      list(token = "secret"),
      "orders"
    )),
    "configured pins board"
  )
  expect_error(dr_target_pins(board, "orders", metadata = list()), "reserved")
})

test_that("a local RDS board completes a checked lifecycle without a lake", {
  skip_if_not_installed("pins")
  board <- pins::board_temp(versioned = TRUE)
  product <- dataraft.core::dr_product(
    "orders",
    data.frame(amount = c(25, 75))
  ) |>
    dataraft.core::dr_add_quality(~ amount >= 0)
  target <- dr_target_pins(board, "orders", type = "rds")
  result <- dataraft.core::dr_publish(
    product,
    to = target,
    data = data.frame(amount = c(25, 75))
  )
  reference <- result$outputs$version
  expect_equal(
    dataraft.core::dr_read_source(dr_source_pins(
      board,
      "orders",
      reference
    ))$amount,
    c(25, 75)
  )
  blocked <- dataraft.core::dr_publish(
    product,
    to = target,
    data = data.frame(amount = -1),
    stop_on_failure = FALSE
  )
  expect_identical(blocked$status, "blocked")
  expect_equal(
    dataraft.core::dr_read_source(dr_source_pins(board, "orders"))$amount,
    c(25, 75)
  )
})
