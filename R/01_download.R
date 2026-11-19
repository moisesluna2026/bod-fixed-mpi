# =============================================================================
# Stage 01. Census files. Downloads the IBGE microdata when they are not present
# and records the SHA-256 of every input file (output/inputs_sha256.csv).
# 2010: one zip per UF (Sao Paulo in two parts) plus Documentacao.zip.
# 2022: Todas_as_UFs.zip (public sample).
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
options(timeout = 3600)
files2010 <- c(paste0(setdiff(UF_ABBR, "SP"), ".zip"), "SP1.zip", "SP2_RM.zip", "Documentacao.zip")
dir.create(PATHS$census2010_dir, recursive = TRUE, showWarnings = FALSE)
for (f in files2010) {
  dest <- file.path(PATHS$census2010_dir, f)
  if (!file.exists(dest)) { log_msg("downloading", f); download.file(paste0(URLS$census2010, f), dest, mode = "wb") }
}
if (!file.exists(PATHS$census2022_zip)) {
  dir.create(dirname(PATHS$census2022_zip), recursive = TRUE, showWarnings = FALSE)
  log_msg("downloading Todas_as_UFs.zip (2022)"); download.file(URLS$census2022, PATHS$census2022_zip, mode = "wb")
}
inputs <- c(file.path(PATHS$census2010_dir, files2010), PATHS$census2022_zip)
fwrite(data.table(file = basename(inputs), bytes = file.size(inputs), sha256 = file_sha256(inputs)),
       file.path(PATHS$output, "inputs_sha256.csv"))
log_msg("stage 01 complete")
