test_that("adapter conformance checks source values and types", {
  data <- data.frame(id = 1:3)
  result <- dr_test_adapter(data, expected = data)
  expect_identical(result$read, data)
  error <- tryCatch(
    dr_test_adapter(data, expected = data.frame(id = as.double(1:3))),
    error = identity
  )
  expect_s3_class(error, "dr_adapter_nonconformant")
  error <- tryCatch(
    dr_test_adapter(data, expected = data.frame(id = 3:1)),
    error = identity
  )
  expect_s3_class(error, "dr_adapter_nonconformant")
})

test_that("adapter conformance verifies a committed target roundtrip", {
  path <- withr::local_tempdir()
  data <- data.frame(id = 1:3)
  result <- dr_test_adapter(
    dr_target_rds(path),
    data = data,
    context = list(
      contract = dataraft.core::dr_contract(columns = c(id = "integer"))
    ),
    read_back = function(output) {
      dataraft.core::dr_read_source(dr_source_rds(path, output$version))
    }
  )
  expect_equal(as.data.frame(result$read), data)
  expect_type(result$write$version, "character")
})

test_that("conformance refuses target writes without explicit readback and contract", {
  path <- withr::local_tempdir()
  error <- tryCatch(
    dr_test_adapter(dr_target_rds(path), data = data.frame(id = 1L)),
    error = identity
  )
  expect_s3_class(error, "dr_adapter_nonconformant")
  expect_length(list.files(path, all.files = FALSE), 0L)
})

test_that("failure probes detect partial writes to committed state", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(id = 1L), path)
  read_committed <- function() readRDS(path)
  harmless <- function(adapter, data, context) stop("injected before commit")
  result <- dr_test_adapter(
    data.frame(id = 1L),
    failure_probe = harmless,
    read_committed = read_committed
  )
  expect_true(result$failure_verified)
  partial <- function(adapter, data, context) {
    saveRDS(data.frame(id = 2L), path)
    stop("injected after partial write")
  }
  expect_error(
    dr_test_adapter(
      data.frame(id = 1L),
      failure_probe = partial,
      read_committed = read_committed
    ),
    class = "dr_adapter_nonconformant"
  )
})
