# =============================================================================
# Complete reproduction.
#   Rscript run_all.R            all stages (01 to 11) and the tests
#   Rscript run_all.R 05 06      only the listed stages (each stage is listed; "05 09" runs 05 and 09)
# On macOS, keep the computer awake during long runs:  caffeinate -i Rscript run_all.R
# =============================================================================
stages <- c("01" = "R/01_download.R", "02" = "R/02_build_census2010.R", "03" = "R/03_build_census2022.R",
            "04" = "R/04_af_profiles.R", "05" = "R/05_scores.R", "06" = "R/06_audit.R",
            "07" = "R/07_robustness.R", "08" = "R/08_numerical_example.R", "09" = "R/09_series_1991_2022.R", "10" = "R/10_figure.R", "11" = "R/11_manifest.R")
needed <- c("data.table", "lpSolve", "rDEA", "Benchmarking")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) && !(identical(missing, c("rDEA", "Benchmarking")) && nzchar(Sys.getenv("BOD_SKIP_EXTERNAL_SOLVERS"))))
  stop("Install the packages: ", paste(missing, collapse = ", "))
if (!nzchar(Sys.which("unzip")) || !nzchar(Sys.which("awk"))) stop("The commands unzip and awk must be on the PATH.")
args <- commandArgs(trailingOnly = TRUE)
run <- if (length(args)) stages[args] else stages
if (anyNA(run)) stop("Unknown stage. Valid stages: ", paste(names(stages), collapse = ", "))
for (k in names(run)) {
  t0 <- Sys.time(); cat("\n==== stage", k, "|", run[[k]], "|", format(t0), "\n")
  source(run[[k]], local = new.env()); invisible(gc())
  cat("==== stage", k, "finished in", format(round(difftime(Sys.time(), t0, units = "mins"), 1)), "\n")
}
if (!length(args)) source("tests/check_results.R", local = new.env())
