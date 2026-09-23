library(dataraft)

# Definitions do not read data. Run the product to read and validate its source.
definition <- dr_product(PROJECT_PRODUCT_ID) |>
  dr_set_sources(input = "data/input.csv") |>
  dplyr::mutate(amount = amount * 2) |>
  dr_add_quality(~ amount >= 0)
