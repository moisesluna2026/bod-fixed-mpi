# Reconstruct only the main profile cache from archived aggregate data.
source('R/config.R'); library(data.table)
P <- fread(file.path(PATHS$output, 'af/profiles.csv'))
stopifnot(nrow(P) == 54L, all(c('uf','ano',INDICATORS) %in% names(P)))
saveRDS(list(main=P), file.path(PATHS$work, 'profiles.rds'))
cat('Main aggregate profile cache prepared. It contains no household records.\n')
