#' @export
#' @importFrom dataraft.core dr_as_source
dr_as_source.character <- function(x, ...) dr_source_parquet(x, ...)
