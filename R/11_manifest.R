# =============================================================================
# Stage 11. Manifest: parameters, session information and SHA-256 of every output.
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
fwrite(data.table(parameter = names(PARAM), value = vapply(PARAM, function(x) paste(x, collapse = " "), "")),
       file.path(PATHS$output, "parameters.csv"))
writeLines(capture.output(sessionInfo()), file.path(PATHS$output, "session_info.txt"))
outs <- setdiff(list.files(PATHS$output, recursive = TRUE, full.names = TRUE),
                file.path(PATHS$output, c("outputs_sha256.csv", "run_log.txt")))
fwrite(data.table(file = sub(paste0("^", PATHS$output, "/"), "", outs), sha256 = file_sha256(outs)),
       file.path(PATHS$output, "outputs_sha256.csv"))
log_msg("stage 11 complete:", length(outs), "output files listed")
