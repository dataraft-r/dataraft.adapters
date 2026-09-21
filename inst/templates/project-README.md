# Data product project

This project reads synthetic orders, doubles their amounts and checks that every
amount is nonnegative. It needs only dataraft and its core dependencies.

From the project directory:

```r
source("run.R")
readRDS("output.rds")
```

Edit `definitions.R` to replace the input, transformations or checks. Add
`dr_set_target()` for a database, Parquet file, pins board or governed lake.
The runner records execution evidence in `.dataraft/evidence`. Set the
`DATARAFT_EVIDENCE` environment variable to use another persistent directory.

## Optional reproducibility

If `init-renv.R` was generated, install renv and run that script explicitly.
Commit the resulting `renv.lock`; use `renv::restore()` on a new machine.
Project creation itself does not install packages or create a lockfile.

## Optional dependency-aware execution

If `_targets.R` was generated, install targets and run `targets::tar_make()`.
Unchanged file inputs and upstream results are cached by targets. Changing an
input file rebuilds its dependent products. External API or database changes
need an explicit cue, such as `targets::tar_cue(mode = "always")`.
Do not persist live connections in a targets pipeline. Use connection factories.

## Optional Posit Connect deployment

If `job.qmd` was generated, render it locally with Quarto, then publish the
project as a Quarto document through the RStudio Publish button or rsconnect.
Include `definitions.R`, `run.R` and the input files in the deployment bundle.
Install required optional packages on the deployment environment as needed.
Set the schedule and credentials in Connect. Set `DATARAFT_EVIDENCE` to storage
that persists across redeployments if evidence must outlive the content bundle.
A failed render signals a failed job; the visible HTML may remain the last
successful render. Coordinate writers when publishing to a local lake.

Scheduling, secrets and runtime infrastructure remain responsibilities of the
host platform. No scheduler is embedded in dataraft.
