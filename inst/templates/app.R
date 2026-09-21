library(dataraft)
# Deploy a metadata snapshot to this read-only app, or use a separately managed
# connection to a server-backed catalog. Do not open the same local catalog file
# from another process while a writer holds it.
dr_catalog_app(snapshot = "catalog.json", launch = FALSE)
