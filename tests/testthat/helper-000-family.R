# Test bindings for this package; unavailable optional packages are not loaded.
family_owners <- c(
  "dr_source_api" = "dataraft.adapters",
  "api_parse_json" = "dataraft.adapters",
  "dr_target_database" = "dataraft.adapters",
  "dr_source_parquet" = "dataraft.adapters",
  "dr_target_parquet" = "dataraft.adapters",
  "dr_source_pins" = "dataraft.adapters",
  "dr_target_pins" = "dataraft.adapters",
  "dr_contract_yaml" = "dataraft.adapters",
  "dr_commons_yaml" = "dataraft.metrics",
  "dr_capabilities" = "dataraft.core",
  "dr_read_source" = "dataraft.core",
  "dr_source_database" = "dataraft.adapters",
  "dr_check_component" = "dataraft.core",
  "dr_source_release" = "dataraft.lake",
  "dr_add_source" = "dataraft.core",
  "dr_add_transform" = "dataraft.core",
  "dr_add_contract" = "dataraft.core",
  "dr_add_quality" = "dataraft.core",
  "dr_set_target" = "dataraft.core",
  "dr_inspect" = "dataraft.core",
  "dr_contract" = "dataraft.core",
  "dr_quality_rule" = "dataraft.core",
  "dr_validate" = "dataraft.core",
  "quality_ok" = "dataraft.core",
  "dr_write_target" = "dataraft.core",
  "dr_publish" = "dataraft.core",
  "dr_collect" = "dataraft.core",
  "dr_metric" = "dataraft.metrics",
  "dr_run" = "dataraft.core",
  "dr_product" = "dataraft.core",
  "dr_model" = "dataraft.core",
  "dr_disconnect_lake" = "dataraft.lake",
  "query" = "dataraft.lake",
  "meta" = "dataraft.lake"
)
for (name in names(family_owners)) {
  owner <- family_owners[[name]]
  if (requireNamespace(owner, quietly = TRUE)) {
    assign(name, get(name, asNamespace(owner), inherits = FALSE))
  }
}
local_family_bindings <- function(..., .package = NULL, .env = parent.frame()) {
  bindings <- list(...)
  if (
    !is.null(.package) && !.package %in% c("dataraft", unique(family_owners))
  ) {
    return(do.call(
      testthat::local_mocked_bindings,
      c(bindings, list(.package = .package, .env = .env))
    ))
  }
  owners <- unname(family_owners[names(bindings)])
  if (anyNA(owners)) {
    stop("Unknown mocked family binding")
  }
  for (owner in unique(owners)) {
    do.call(
      testthat::local_mocked_bindings,
      c(bindings[owners == owner], list(.package = owner, .env = .env))
    )
  }
}
