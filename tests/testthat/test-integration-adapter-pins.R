test_that("same-second pins publications identify their exact version and latest data", {
  skip_if_not_installed("pins")
  # Freeze only pins' timestamp allocation. Serialization, boards, storage and
  # reads are real. Choose descending hashes to reproduce the native ordering bug
  # independently of platform-specific RDS serialization.
  standard_meta <- get("standard_meta", asNamespace("pins"))
  pin_created <- "20000101T000000Z"
  local_family_bindings(
    standard_meta = function(...) {
      meta <- standard_meta(...)
      meta$created <- pin_created
      meta
    },
    .package = "pins"
  )
  board <- pins::board_temp(versioned = TRUE)
  data <- list(tibble::tibble(id = 1L), tibble::tibble(id = 2L))
  for (i in seq_along(data)) {
    pins::pin_write(board, data[[i]], paste0("probe", i), type = "rds")
  }
  hashes <- vapply(
    seq_along(data),
    function(i) {
      pins::pin_meta(board, paste0("probe", i))$pin_hash
    },
    character(1)
  )
  older <- data[[order(hashes)[2L]]]
  newer <- data[[order(hashes)[1L]]]
  target <- dr_target_pins(board, "orders")
  first <- dr_write_target(
    target,
    older,
    list(product = "orders", run_id = "r1")
  )
  second <- dr_write_target(
    target,
    newer,
    list(product = "orders", run_id = "r2")
  )
  expect_identical(
    substr(first$version, 1L, 16L),
    substr(second$version, 1L, 16L)
  )
  expect_false(identical(first$version, second$version))
  expect_equal(dr_read_source(dr_source_pins(board, "orders")), newer)
  expect_equal(
    dr_read_source(dr_source_pins(board, "orders", first$version)),
    older
  )
  expect_equal(
    dr_read_source(dr_source_pins(board, "orders", second$version)),
    newer
  )
  expect_identical(
    pins::pin_meta(board, "orders", second$version)$user$dataraft$run_id,
    "r2"
  )
  same <- dr_write_target(
    target,
    newer,
    list(product = "orders", run_id = "r3")
  )
  expect_identical(same$version, second$version)
  expect_equal(nrow(pins::pin_versions(board, "orders")), 2L)
  expect_error(
    dr_write_target(target, older, list(product = "orders", run_id = "r4")),
    class = "dr_pin_unconfirmed"
  )
  expect_equal(dr_read_source(dr_source_pins(board, "orders")), newer)
  expect_equal(nrow(pins::pin_versions(board, "orders")), 2L)
  # Forcing the native shortcut is safe once the pin's timestamp can advance.
  # Real pins serialization and storage allocate a distinct content version.
  pin_created <- "20000101T000001Z"
  reverted <- dr_write_target(
    dr_target_pins(board, "orders", force_identical_write = TRUE),
    older,
    list(product = "orders", run_id = "r5")
  )
  expect_false(identical(reverted$version, first$version))
  expect_equal(dr_read_source(dr_source_pins(board, "orders")), older)
  expect_equal(
    dr_read_source(dr_source_pins(board, "orders", reverted$version)),
    older
  )
  expect_equal(
    dr_read_source(dr_source_pins(board, "orders", second$version)),
    newer
  )
  reverted_meta <- pins::pin_meta(board, "orders", reverted$version)
  expect_identical(reverted_meta$user$dataraft$run_id, "r5")
  expect_equal(reverted_meta$user$dataraft$publication_order, 3)
  expect_equal(nrow(pins::pin_versions(board, "orders")), 3L)

  # An external write at the same timestamp has no framework ordering metadata.
  # Refuse to guess its position; an explicit reference still reads it normally.
  versions <- pins::pin_versions(board, "orders")$version
  pins::pin_write(board, tibble::tibble(id = 3L), "orders", type = "rds")
  external <- setdiff(pins::pin_versions(board, "orders")$version, versions)
  expect_error(
    dr_read_source(dr_source_pins(board, "orders")),
    class = "dr_pin_ambiguous"
  )
  expect_equal(dr_read_source(dr_source_pins(board, "orders", external))$id, 3L)
})

test_that("external pins and unchanged content keep the board's native reference", {
  skip_if_not_installed("pins")
  board <- pins::board_temp(versioned = TRUE)
  data <- tibble::tibble(id = 7L)
  pins::pin_write(board, data, "external", type = "rds")
  native <- pins::pin_meta(board, "external")$local$version
  expect_equal(dr_read_source(dr_source_pins(board, "external")), data)
  output <- dr_write_target(
    dr_target_pins(board, "external"),
    data,
    list(product = "external", run_id = "r1")
  )
  expect_identical(output$version, native)
  expect_equal(nrow(pins::pin_versions(board, "external")), 1L)
  expect_equal(dr_read_source(dr_source_pins(board, "external")), data)
})
