library(dataraft)

# Definitions do not read data. Execution binds the source to the workflow.
specification <- dr_product(PROJECT_PRODUCT_ID) |>
  dr_add_quality(~ amount >= 0)
preparation <- dr_recipe() |>
  dr_step_mutate(amount = amount * 2)
definition <- dr_workflow() |>
  dr_add_product(specification) |>
  dr_add_recipe(preparation) |>
  dr_add_source("data/input.csv")
