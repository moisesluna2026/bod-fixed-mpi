# =============================================================================
# Stage 05. Index with the fixed 2010 reference.
# Reference B: the 27 UF profiles of 2010, frozen. Every profile of 2010 and 2022
# is scored against B, for each weight restriction tau in PARAM$tau_grid.
# For comparison, each year is also scored against a reference re-estimated with
# that year's own profiles ("moving reference"), the usual practice.
# Aggregation: population-weighted mean of the UF scores (regions and Brazil).
# Outputs: output/scores/*.csv, work/scores.rds
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
O <- file.path(PATHS$output, "scores"); dir.create(O, showWarnings = FALSE)
P <- readRDS(file.path(PATHS$work, "profiles.rds"))$main
B <- profile_matrix(P[ano == 2010])
stopifnot(all(B > 0))                                   # every indicator observed in every reference UF

s <- rbindlist(lapply(PARAM$tau_grid, function(tau) rbindlist(lapply(c(2010L, 2022L), function(y) {
  H <- profile_matrix(P[ano == y])
  data.table(ano = y, uf = P[ano == y, uf], tau = tau, score = bod_scores(H, B, tau),
             score_moving = bod_scores(H, H, tau))
}))))
s <- merge(s, P[, .(ano, uf, N, H, A, M0)], by = c("ano", "uf"))
s[, `:=`(sigla = UF_ABBR[as.character(uf)], regiao = region(uf))]
setorder(s, tau, ano, uf)
# identity at tau = 1: equal coefficients, S = M0 / max M0 of the reference
stopifnot(max(abs(s[tau == 1, score] - s[tau == 1, M0] / max(P[ano == 2010, M0]))) < 1e-9)
fwrite(s, file.path(O, "scores_uf.csv"))

agg <- rbind(s[, .(grupo = "Brasil", J = sum(N * score) / sum(N), J_moving = sum(N * score_moving) / sum(N),
                   M0 = sum(N * M0) / sum(N)), by = .(tau, ano)],
             s[, .(J = sum(N * score) / sum(N), J_moving = sum(N * score_moving) / sum(N),
                   M0 = sum(N * M0) / sum(N)), by = .(tau, ano, grupo = regiao)])
fwrite(agg, file.path(O, "aggregates.csv"))
chg <- dcast(s, tau + uf + sigla + regiao ~ ano, value.var = c("score", "score_moving", "M0"))
chg[, `:=`(change_pct = 100 * (score_2022 / score_2010 - 1), change_M0_pct = 100 * (M0_2022 / M0_2010 - 1))]
fwrite(chg, file.path(O, "change_uf.csv"))

summ <- s[, .(min_score = min(score), max_score = max(score), on_frontier = sum(abs(score - 1) < 1e-8),
              above_one = sum(score > 1 + 1e-8),
              spearman_M0 = rank_cor(score, M0), spearman_H = rank_cor(score, H)),
          by = .(tau, ano)]
fwrite(summ, file.path(O, "summary_by_tau.csv"))

# Weights and virtual contributions (share of the score) in the main specification
wt <- rbindlist(lapply(c(2010L, 2022L), function(y) {
  H <- profile_matrix(P[ano == y])
  rbindlist(lapply(seq_len(nrow(H)), function(i) {
    f <- bod_score(H[i, ], B, PARAM$tau_main)
    data.table(ano = y, uf = P[ano == y, uf][i], indicador = INDICATORS, w = f$w,
               contribution = f$w * H[i, ] / f$score)
  }))
}))
fwrite(wt, file.path(O, "weights_main.csv"))
saveRDS(list(scores = s, aggregates = agg, B = B), file.path(PATHS$work, "scores.rds"))
log_msg("stage 05 complete; main tau =", PARAM$tau_main, "| Brazil J 2010 =",
        signif(agg[tau == PARAM$tau_main & grupo == "Brasil" & ano == 2010, J], 4), "| 2022 =",
        signif(agg[tau == PARAM$tau_main & grupo == "Brasil" & ano == 2022, J], 4))
