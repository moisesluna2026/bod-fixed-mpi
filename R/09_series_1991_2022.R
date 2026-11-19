# =============================================================================
# Stage 09 (optional). Series 1991-2022 against the fixed 2010 reference.
# Builds the households of the 1991 and 2000 Census samples with the same twelve
# indicators (harmonized rules; see docs/indicators.md), computes their censored
# headcount profiles by UF, scores them against B (the 27 UF profiles of 2010) and
# against each year's own profiles, and aggregates by region and Brazil. The 2010 and
# 2022 profiles come from stage 04. The stage runs only when the two sample files
# are available (environment variables BOD_CENSUS1991_SAV and BOD_CENSUS2000_SAV,
# SPSS format); otherwise it is skipped.
# Outputs: output/series/*.csv
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
f91 <- Sys.getenv("BOD_CENSUS1991_SAV"); f00 <- Sys.getenv("BOD_CENSUS2000_SAV")
if (!nzchar(f91) || !nzchar(f00) || !file.exists(f91) || !file.exists(f00) || !requireNamespace("haven", quietly = TRUE)) {
  log_msg("stage 09 skipped: set BOD_CENSUS1991_SAV and BOD_CENSUS2000_SAV and install haven")
} else {
O <- file.path(PATHS$output, "series"); dir.create(O, showWarnings = FALSE)
MW <- c(`1991` = 36161.60, `2000` = 151)   # minimum wage of reference in each Census (current currency)
group5 <- function(age) as.integer(pmin(floor(age / 5) + 1, 17))      # 1 = 0-4, ..., 17 = 80+
read_sav_cols <- function(f, cols) {
  d <- haven::read_sav(f, col_select = tidyselect::any_of(cols)); setDT(d)
  miss <- setdiff(cols, names(d)); if (length(miss)) stop("Missing variables: ", paste(miss, collapse = ", "))
  for (v in names(d)) set(d, j = v, value = as.numeric(haven::zap_labels(d[[v]])))
  d
}
to_households <- function(d) {
  d[, `:=`(frequencia = any(freq_p), atraso = any(atraso_p), escolaridade = any(esc_p),
           trabalho = any(trab_p), trab_infantil = any(ti_p)), by = hh]
  h <- d[, c(list(uf = uf[1], pop = sum(w)), lapply(.SD, function(x) as.integer(any(x %in1% TRUE)))),
         by = hh, .SDcols = INDICATORS]
  h[]
}

# 1991 (25% sample): permanent private households (V0201 = 1)
d <- read_sav_cols(f91, c("IDQues", "UF", "V0201", "V7301", "V7300", "V3072", "V0302", "V0205", "V0206", "V0213",
                          "V0214", "V2121", "V0226", "V0221", "V0325", "V0326", "V3241", "V0345", "V0349", "V0353",
                          "V0358", "V2012"))[V0201 %in% 1]
d[, `:=`(hh = IDQues, w = fifelse(is.na(V7301), V7300, V7301), uf = UF, g = group5(V3072))]
age <- d$V3072
d[, `:=`(agua = V0205 %in1% c(5, 6), esgoto = V0206 %in1% c(0, 4, 5, 6, 7), lixo = V0214 %in1% 3:7,
         densidade = !is.na(V2121) & V2121 > 200, banheiro = V0213 %in1% 0,
         consumo = V0226 %in1% 0 | V0221 %in1% 3:4)]              # no washing machine, or no electric lighting
attend <- (d$V0325 %in1% 1:5) | (d$V0326 %in1% 1:6)
level <- fcase(d$V0325 %in1% 3 | d$V0326 %in1% 6, 4L, d$V0325 %in1% c(2, 5) | d$V0326 %in1% c(4, 5), 3L,
               d$V0325 %in1% c(1, 4) | d$V0326 %in1% 3, 2L, d$V0326 %in1% c(1, 2), 1L, default = NA_integer_)
d[, freq_p := g %in1% 2:3 & !attend]
d[, atraso_p := attend & !is.na(age) & age >= 6 & age <= 19 & !is.na(level) &
     ((age <= 14 & level < 2) | (age >= 15 & age <= 17 & level < 3) | (age >= 18 & level < 4))]
d[, esc_p := g %in1% 5:17 & !is.na(V3241) & V3241 < 8]
d[, trab_p := g %in1% 5:17 & (V0358 %in1% 1:2 | (!is.na(V0349) & !V0349 %in1% 10 & !V0353 %in1% 1 & !is.na(V0353)))]
d[, ti_p := g %in1% 3 & V0345 %in1% 1:2]
d[, nres := sum(V0302 < 14, na.rm = TRUE), by = hh]                 # without boarders and domestic workers
d[, renda := !is.na(V2012) & nres > 0 & V2012 / nres / MW[["1991"]] < PARAM$income_share]
h91 <- to_households(d); rm(d); invisible(gc())
log_msg("stage 09: 1991,", format(nrow(h91), big.mark = ","), "households")

# 2000 (10% sample): permanent private households (V0201 = 1)
d <- read_sav_cols(f00, c("AREAP", "V0300", "V0102", "V0201", "P001", "V4752", "V0402", "V0207", "V0208", "V0209",
                          "V0210", "V0211", "V0212", "V0217", "V7204", "V0429", "V0430", "V4300", "V0439", "V0440",
                          "V0441", "V0442", "V0443", "V0447", "V0450", "V0455", "V7616"))[V0201 %in% 1]
d[, `:=`(hh = paste(AREAP, V0300), w = P001, uf = V0102, g = group5(V4752))]
age <- d$V4752
d[, `:=`(agua = !is.na(V0207) & V0207 != 1 & V0208 %in1% 3, esgoto = V0210 %in1% 2 | V0211 %in1% 3:6,
         lixo = V0212 %in1% 3:7, densidade = !is.na(V7204) & V7204 > 2, banheiro = V0209 %in1% 0,
         consumo = V0217 %in1% 2)]
attend <- d$V0429 %in1% 1:2
level <- fcase(d$V0430 %in1% 12:13, 4L, d$V0430 %in1% 8:11, 3L, d$V0430 %in1% 5:7, 2L, d$V0430 %in1% 1:4, 1L,
               default = NA_integer_)
d[, freq_p := g %in1% 2:3 & V0429 %in1% 3:4]
d[, atraso_p := attend & !is.na(age) & age >= 6 & age <= 19 & !is.na(level) &
     ((age <= 14 & level < 2) | (age >= 15 & age <= 17 & level < 3) | (age >= 18 & level < 4))]
d[, esc_p := g %in1% 5:17 & !is.na(V4300) & V4300 < 8]
d[, trab_p := g %in1% 5:17 & (V0455 %in1% 1 | (!is.na(V0447) & !V0447 %in1% 5 & V0450 %in1% 2))]
d[, ti_p := g %in1% 3 & (V0439 %in1% 1 | V0440 %in1% 1 | V0441 %in1% 1 | V0442 %in1% 1 | V0443 %in1% 1)]
d[, nres := sum(V0402 < 9, na.rm = TRUE), by = hh]
d[, renda := !is.na(V7616) & nres > 0 & V7616 / nres / MW[["2000"]] < PARAM$income_share]
h00 <- to_households(d); rm(d); invisible(gc())
log_msg("stage 09: 2000,", format(nrow(h00), big.mark = ","), "households")

# profiles 1991 and 2000, plus 2010 and 2022 from stage 04
pr <- readRDS(file.path(PATHS$work, "profiles.rds"))
P <- rbind(af_profiles(copy(h91))[, ano := 1991L], af_profiles(copy(h00))[, ano := 2000L], fill = TRUE)
setcolorder(P, c("ano", "uf")); stopifnot(nrow(P) == 54, max(abs(P$M0 - P$H * P$A)) < 1e-12)
P <- rbind(P, pr$main[, names(P), with = FALSE]); setorder(P, ano, uf)
fwrite(P, file.path(O, "profiles_1991_2022.csv"))
grp <- function(d, g) { d <- copy(d); d[, grupo := g]; d }
agg_af <- rbind(af_profiles(grp(h91, "Brasil"), by = "grupo")[, ano := 1991L], af_profiles(grp(h00, "Brasil"), by = "grupo")[, ano := 2000L],
                af_profiles(grp(h91, region(h91$uf)), by = "grupo")[, ano := 1991L],
                af_profiles(grp(h00, region(h00$uf)), by = "grupo")[, ano := 2000L])
agg_af <- rbind(agg_af, pr$aggregates[, names(agg_af), with = FALSE]); setorder(agg_af, ano, grupo)
fwrite(agg_af, file.path(O, "profiles_brazil_regions_1991_2022.csv"))

# scores against the fixed 2010 reference and against each year's own profiles
B <- profile_matrix(P[ano == 2010]); years <- c(1991L, 2000L, 2010L, 2022L)
s <- rbindlist(lapply(PARAM$tau_grid, function(tau) rbindlist(lapply(years, function(y) {
  H <- profile_matrix(P[ano == y])
  data.table(ano = y, uf = P[ano == y, uf], tau = tau, score = bod_scores(H, B, tau), score_moving = bod_scores(H, H, tau))
}))))
s <- merge(s, P[, .(ano, uf, N, H, A, M0)], by = c("ano", "uf"))
s[, `:=`(sigla = UF_ABBR[as.character(uf)], regiao = region(uf))]; setorder(s, tau, ano, uf)
z <- readRDS(file.path(PATHS$work, "scores.rds"))$scores           # consistency with stage 05
chk <- merge(s[ano %in% c(2010L, 2022L)], z, by = c("tau", "ano", "uf"))
stopifnot(nrow(chk) == nrow(z), max(abs(chk$score.x - chk$score.y)) < 1e-9, max(abs(chk$score_moving.x - chk$score_moving.y)) < 1e-9)
fwrite(s, file.path(O, "scores_uf_1991_2022.csv"))
agg <- rbind(s[, .(grupo = "Brasil", J = sum(N * score) / sum(N), J_moving = sum(N * score_moving) / sum(N),
                   M0 = sum(N * M0) / sum(N), above_one = sum(score > 1 + 1e-8)), by = .(tau, ano)],
             s[, .(J = sum(N * score) / sum(N), J_moving = sum(N * score_moving) / sum(N),
                   M0 = sum(N * M0) / sum(N), above_one = sum(score > 1 + 1e-8)), by = .(tau, ano, grupo = regiao)])
fwrite(agg, file.path(O, "aggregates_1991_2022.csv"))
# dominance between consecutive Censuses: share of the 324 UF-indicator cells that fall
dm <- rbindlist(lapply(1:3, function(k) {
  a <- profile_matrix(P[ano == years[k]]); b <- profile_matrix(P[ano == years[k + 1]])
  data.table(from = years[k], to = years[k + 1], cells_lower = sum(b < a), cells = length(a),
             ufs_all_lower = sum(apply(b < a, 1, all)),
             moving_rises = sum(s[tau == PARAM$tau_main & ano == years[k + 1], score_moving] >
                                s[tau == PARAM$tau_main & ano == years[k], score_moving] + 1e-9),
             fixed_rises = sum(s[tau == PARAM$tau_main & ano == years[k + 1], score] >
                               s[tau == PARAM$tau_main & ano == years[k], score] + 1e-9))
}))
fwrite(dm, file.path(O, "dominance_consecutive.csv"))
log_msg("stage 09 complete; Brazil M0 1991 =", signif(agg_af[grupo == "Brasil" & ano == 1991, M0], 4),
        "| 2000 =", signif(agg_af[grupo == "Brasil" & ano == 2000, M0], 4))
}
