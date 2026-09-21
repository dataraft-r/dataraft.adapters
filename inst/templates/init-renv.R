# Run this script explicitly after installing dataraft in your R environment.
if (!requireNamespace("renv", quietly = TRUE)) {
  stop("Install renv first with install.packages('renv').")
}
renv::init()
renv::snapshot(prompt = FALSE)
# Commit renv.lock. Collaborators restore it with renv::restore().
