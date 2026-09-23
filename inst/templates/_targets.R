library(targets)
library(dataraft)
source("definitions.R")

# File inputs and product dependencies are tracked automatically.
# Use targets::tar_cue(mode = "always") for externally changing API/DB inputs.
dataraft.adapters::dr_as_targets(
  definition,
  evidence = Sys.getenv("DATARAFT_EVIDENCE", ".dataraft/evidence")
)
