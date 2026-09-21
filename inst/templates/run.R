source("definitions.R")

result <- dr_run(
  definition,
  evidence = Sys.getenv("DATARAFT_EVIDENCE", ".dataraft/evidence")
)
saveRDS(dr_collect(result), "output.rds")
print(result)
