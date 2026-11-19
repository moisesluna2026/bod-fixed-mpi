# =============================================================================
# Stage 04. Alkire-Foster identification (12 indicators, equal weights 1/12,
# k = 1/3) and censored headcounts by UF, 2010 and 2022. These 54 profiles are
# the data of the index. Also: profiles with the minimum-wage income line, with
# the four-dimension aggregation, and the share of missing answers.
# Outputs: output/af/profiles.csv (main), profiles_income_real.csv, profiles_delay_harmonized.csv,
#          profiles_dimensions.csv, missing_income.csv; work/profiles.rds
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
O <- file.path(PATHS$output, "af"); dir.create(O, showWarnings = FALSE)
cols <- c("uf", "pop", INDICATORS, "atraso_h", "renda_real", "renda_missing")
b10 <- readRDS(file.path(PATHS$work, "households_2010.rds"))[, cols, with = FALSE]
b22 <- readRDS(file.path(PATHS$work, "households_2022.rds"))[, c(cols, "age_suppressed"), with = FALSE]

prof <- rbind(af_profiles(copy(b10))[, ano := 2010L], af_profiles(copy(b22))[, ano := 2022L])
setcolorder(prof, c("ano", "uf")); setorder(prof, ano, uf)
stopifnot(nrow(prof) == 54, max(abs(prof$M0 - prof$H * prof$A)) < 1e-12)   # M0 = H x A
fwrite(prof, file.path(O, "profiles.csv"))

# Brazil and regions (persons as the unit, same identification)
grp <- function(d, g) { d <- copy(d); d[, grupo := g]; d }
agg <- rbind(
  af_profiles(grp(b10, "Brasil"), by = "grupo")[, ano := 2010L], af_profiles(grp(b22, "Brasil"), by = "grupo")[, ano := 2022L],
  af_profiles(grp(b10, region(b10$uf)), by = "grupo")[, ano := 2010L], af_profiles(grp(b22, region(b22$uf)), by = "grupo")[, ano := 2022L])
fwrite(agg, file.path(O, "profiles_brazil_regions.csv"))

# Robustness: income line constant in real terms (one quarter of the July 2010 minimum
# wage, updated by the IPCA), instead of one quarter of each year's minimum wage
swap <- function(d, from, to) { d <- copy(d); d[, (to) := get(from)]; d }
prof_mw <- rbind(af_profiles(swap(b10, "renda_real", "renda"))[, ano := 2010L],
                 af_profiles(swap(b22, "renda_real", "renda"))[, ano := 2022L]); setorder(prof_mw, ano, uf)
fwrite(prof_mw, file.path(O, "profiles_income_real.csv"))

# Robustness: school delay with the same rule in both Censuses (age group and course attended)
prof_dl <- rbind(af_profiles(swap(b10, "atraso_h", "atraso"))[, ano := 2010L],
                 af_profiles(swap(b22, "atraso_h", "atraso"))[, ano := 2022L]); setorder(prof_dl, ano, uf)
fwrite(prof_dl, file.path(O, "profiles_delay_harmonized.csv"))

# Robustness: 2022 without households with a suppressed age
prof_as <- af_profiles(copy(b22)[age_suppressed == FALSE])[, ano := 2022L]
fwrite(prof_as, file.path(O, "profiles_2022_without_suppressed_age.csv"))

# Robustness: eleven indicators, without child labour, whose 2022 occupation concept
# differs from 2010 (identification redone with weights 1/11 and k = 1/3)
ind11 <- setdiff(INDICATORS, "trab_infantil")
prof11 <- rbind(af_profiles(copy(b10), ind = ind11)[, ano := 2010L], af_profiles(copy(b22), ind = ind11)[, ano := 2022L])
setorder(prof11, ano, uf); fwrite(prof11, file.path(O, "profiles_without_child_labour.csv"))

# Dimension scores: mean of the three censored headcounts of each dimension
dims <- unique(DIMENSIONS)
dimprof <- copy(prof)[, (dims) := lapply(dims, function(dd) rowMeans(.SD[, names(DIMENSIONS)[DIMENSIONS == dd], with = FALSE]))]
fwrite(dimprof[, c("ano", "uf", "N", "M0", dims), with = FALSE], file.path(O, "profiles_dimensions.csv"))

fwrite(rbind(b10[, .(ano = 2010L, income_missing_share = sum(pop * renda_missing) / sum(pop)), by = uf],
             b22[, .(ano = 2022L, income_missing_share = sum(pop * renda_missing) / sum(pop)), by = uf]),
       file.path(O, "missing_income.csv"))
saveRDS(list(main = prof, income_real = prof_mw, delay_harmonized = prof_dl, no_suppressed = prof_as, without_child_labour = prof11,
             dimensions = dimprof, aggregates = agg),
        file.path(PATHS$work, "profiles.rds"))
log_msg("stage 04 complete; Brazil M0 2010 =", signif(agg[grupo == "Brasil" & ano == 2010, M0], 4),
        "| 2022 =", signif(agg[grupo == "Brasil" & ano == 2022, M0], 4))
