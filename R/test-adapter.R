#' Check an adapter implementation against the extension protocol
#'
#' Checks preflight, descriptive metadata and the six standard capabilities.
#' `NA` capability values are allowed as undeclared; they do not trigger I/O.
#' Source adapters are read and compared with `expected` when provided. Target
#' adapters are written only when `data` and an explicit `context$contract` are
#' supplied. Use a disposable target: this helper performs real I/O. A
#' `read_back` callback reads the committed result for a roundtrip comparison.
#' This smoke test cannot certify transactionality, concurrency, secret hygiene
#' or remote failure recovery. Test those guarantees separately in your adapter.
#' @param adapter Source or target adapter.
#' @param expected Optional expected source or roundtrip data frame.
#' @param data Optional data frame to write to a target.
#' @param context Target context including an explicit contract.
#' @param read_back Optional function accepting the write result and returning
#'   the committed table. Required when testing a target write.
#' @returns Invisibly, a list with capabilities, descriptor, read and write results.
#'   Protocol violations raise `dr_adapter_nonconformant`; adapter execution
#'   errors retain their original classes.
#' @export
#' @examples
#' dr_test_adapter(data.frame(id = 1:2), expected = data.frame(id = 1:2))
dr_test_adapter <- function(
  adapter,
  expected = NULL,
  data = NULL,
  context = list(),
  read_back = NULL
) {
  fail <- function(message) {
    dataraft.core::dr_internal_abort(
      message,
      class = "dr_adapter_nonconformant",
      subclass = "dataraft_error_adapters"
    )
  }
  checked <- dataraft.core::dr_check_component(adapter)
  if (!identical(checked, adapter)) {
    fail("Preflight must return the unchanged adapter.")
  }
  capabilities <- dataraft.core::dr_capabilities(adapter)
  fields <- c("read", "write", "lazy", "transactions", "partition", "immutable")
  if (
    !is.list(capabilities) ||
      !all(fields %in% names(capabilities)) ||
      !all(vapply(
        capabilities[fields],
        function(x) is.logical(x) && length(x) == 1L,
        logical(1)
      ))
  ) {
    fail("Declare all six standard capabilities as logical scalars.")
  }
  descriptor <- dataraft.core::dr_inspect(adapter)
  if (!is.list(descriptor)) {
    fail("Inspection must return a descriptive list.")
  }
  result <- list(capabilities = capabilities, descriptor = descriptor)
  compare <- function(actual, expected) {
    if (!is.data.frame(actual) && !inherits(actual, "tbl_lazy")) {
      actual <- tryCatch(dplyr::collect(actual), error = function(e) NULL)
    }
    if (inherits(actual, "tbl_lazy")) {
      actual <- dplyr::collect(actual)
    }
    if (!is.data.frame(actual)) {
      fail("Reading must return a supported table.")
    }
    if (
      !is.null(expected) &&
        (!identical(lapply(actual, class), lapply(expected, class)) ||
          !isTRUE(all.equal(
            as.data.frame(actual),
            as.data.frame(expected),
            check.attributes = TRUE
          )))
    ) {
      fail(
        "Read data do not match the expected values, types and column order."
      )
    }
    actual
  }
  if (isTRUE(capabilities$read)) {
    result$read <- compare(dataraft.core::dr_read_source(adapter), expected)
  }
  if (!is.null(data)) {
    if (!isTRUE(capabilities$write)) {
      fail("Write testing requires write = TRUE.")
    }
    if (!inherits(context$contract, "dr_contract") || !is.function(read_back)) {
      fail("Write testing requires context$contract and a read_back function.")
    }
    result$write <- dataraft.core::dr_write_target(adapter, data, context)
    if (!is.list(result$write)) {
      fail("Writing must return descriptive result metadata.")
    }
    result$read <- compare(read_back(result$write), expected %||% data)
  }
  invisible(result)
}
