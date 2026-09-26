# dataraft.adapters

**Connect checked DataRaft deliveries to files, databases and other tools.**

Use an adapter when a product needs an external source or destination. This package provides RDS, database, Parquet, pins and API connections, plus catalog and metadata integrations. Some adapters need additional packages or services; the RDS example below needs no database.

[`dataraft` overview](https://github.com/dataraft-r/dataraft) · [Adapters reference](https://dataraft-r.github.io/dataraft/packages/dataraft.adapters/)

## Try it

```r
library(dataraft.adapters)

path <- tempfile("orders-")
orders <- dataraft.core::dr_product(
  "orders", data.frame(id = 1L, amount = 25)
) |>
  dataraft.core::dr_add_quality(~ amount >= 0) |>
  dataraft.core::dr_set_target(dr_target_rds(path))

published <- dataraft.core::dr_publish(orders)
dataraft.core::dr_read_source(dr_source_rds(path, published$outputs$version))
```

The RDS target keeps versioned releases; a blocked delivery does not replace the accepted one. You own the temporary directory in this example and can remove it afterward. Database, Parquet, pins, API and catalog features have separate dependencies and configuration.

Install the development package with `pak::pak("dataraft-r/dataraft.adapters")`. See the [integration guide](https://dataraft-r.github.io/dataraft/articles/integrations.html) and [core workflow](https://github.com/dataraft-r/dataraft.core).
