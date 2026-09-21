test_that("Parquet sources preserve lazy Arrow execution and table types", {
  skip_if_not_installed("arrow")
  path <- withr::local_tempfile(fileext = ".parquet")
  data <- data.frame(id = 1:3, amount = c(2, 4, 6))
  output <- dr_write_target(dr_target_parquet(path), data, list())
  expect_equal(output$rows, 3L)
  lazy <- dr_read_source(dr_source_parquet(path))
  expect_s3_class(lazy, "Dataset")
  expect_true(dr_capabilities(dr_source_parquet(path))$lazy)
  expect_equal(
    dr_collect(dplyr::filter(lazy, id > 1L)),
    tibble::as_tibble(data[2:3, ])
  )
  expect_equal(
    dr_read_source(dr_source_parquet(path, lazy = FALSE)),
    tibble::as_tibble(data)
  )
  expect_identical(dr_read_source(lazy), lazy)
  expect_silent(dr_check_component(lazy))
  expect_false(grepl(
    "token",
    paste(
      dr_inspect(dr_source_parquet(
        "s3://bucket/data?token=secret"
      )),
      collapse = " "
    )
  ))
})

test_that("Parquet publication preserves the old file on write failure", {
  skip_if_not_installed("arrow")
  path <- withr::local_tempfile(fileext = ".parquet")
  original <- data.frame(id = 1L)
  dr_write_target(dr_target_parquet(path), original, list())
  before <- readBin(path, "raw", n = file.size(path))
  expect_error(dr_check_component(dr_target_parquet(path)), "already exists")
  expect_error(dr_write_target(
    dr_target_parquet(
      path,
      overwrite = TRUE,
      compression = "invalid-compressor"
    ),
    data.frame(id = 2L),
    list()
  ))
  expect_identical(readBin(path, "raw", n = file.size(path)), before)
  expect_false(dr_capabilities(dr_target_parquet(path))$transactions)
  expect_error(dr_target_parquet("s3://bucket/path.parquet"), "local path")
  expect_error(
    dr_check_component(dr_source_parquet(paste0(path, "-missing"))),
    "missing"
  )
})
