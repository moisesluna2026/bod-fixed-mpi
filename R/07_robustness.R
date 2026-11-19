# =============================================================================
# Stage 07. Robustness of the main specification (fixed 2010 reference).
#  - weight restriction: tau grid (stage 05 already scores every tau);
#  - income line constant in real terms instead of one quarter of each year's
#    minimum wage;
#  - school delay with the same rule in both Censuses;
#  - aggregation of the 12 indicators into the four dimensions before scoring;
#  - 2022 without households with a suppressed age;
#  - eleven indicators, without child labour (occupation concept differs in 2022);
#  - reference: each UF removed in turn from B (leave one out); pooled 2010 and 2022
#    profiles as reference (intertemporal frontier);
#  - sampling: Poisson bootstrap of households within UF (both years, so that the
#    reference is resampled too), 200 replicates; exploratory percentile intervals.
# Outputs: output/robustness/*.csv
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
O <- file.path(PATHS$output, "robustness"); dir.create(O, showWarnings = FALSE)
pr <- readRDS(file.path(PATHS$work, "profiles.rds")); P <- pr$main
z <- readRDS(file.path(PATHS$work, "scores.rds")); s <- z$scores; B <- z$B
tm <- PARAM$tau_main
national <- function(sc, N) sum(N * sc) / sum(N)

score_version <- function(P10, P22, cols = INDICATORS, label, tau = tm, Bref = NULL) {
  B0 <- if (is.null(Bref)) profile_matrix(P10, cols) else Bref
  rbind(data.table(version = label, ano = 2010L, uf = P10$uf, N = P10$N, score = bod_scores(profile_matrix(P10, cols), B0, tau)),
        data.table(version = label, ano = 2022L, uf = P22$uf, N = P22$N, score = bod_scores(profile_matrix(P22, cols), B0, tau)))
}
main <- s[abs(tau - tm) < 1e-12, .(version = "main", ano, uf, N, score)]
dims <- unique(DIMENSIONS)
alt <- rbind(main,
  score_version(pr$income_real[ano == 2010], pr$income_real[ano == 2022], label = "income_line_constant_real"),
  score_version(pr$delay_harmonized[ano == 2010], pr$delay_harmonized[ano == 2022], label = "school_delay_harmonized"),
  score_version(pr$dimensions[ano == 2010], pr$dimensions[ano == 2022], cols = dims, label = "four_dimensions"),
  score_version(P[ano == 2010], pr$no_suppressed, label = "2022_without_suppressed_age"),
  score_version(pr$without_child_labour[ano == 2010], pr$without_child_labour[ano == 2022],
                cols = setdiff(INDICATORS, "trab_infantil"), label = "eleven_indicators_without_child_labour"),
  score_version(P[ano == 2010], P[ano == 2022], label = "pooled_reference_2010_2022",
                Bref = rbind(profile_matrix(P[ano == 2010]), profile_matrix(P[ano == 2022]))),
  score_version(P[ano == 2010], P[ano == 2022], label = "normative_reference_total_deprivation",
                Bref = matrix(1, 1, length(INDICATORS))))
# normative reference: a single fictitious profile with every headcount equal to 1 (all people poor and
# deprived in every indicator). Closed form: S = tau * mean(h) + (1 - tau) * max(h).
nr <- alt[version == "normative_reference_total_deprivation"]
cf <- c(apply(profile_matrix(P[ano == 2010]), 1, function(h) tm * mean(h) + (1 - tm) * max(h)),
        apply(profile_matrix(P[ano == 2022]), 1, function(h) tm * mean(h) + (1 - tm) * max(h)))
stopifnot(max(abs(nr$score - cf)) < 1e-9)
fwrite(alt, file.path(O, "alternative_versions_uf.csv"))
cmp <- merge(alt, main[, .(ano, uf, score_main = score)], by = c("ano", "uf"))
summ <- cmp[, .(spearman_with_main = rank_cor(score, score_main), J = national(score, N)), by = .(version, ano)]
summ <- merge(summ, dcast(summ, version ~ ano, value.var = "J")[, .(version, change_pct = 100 * (`2022` / `2010` - 1))], by = "version")
fwrite(summ, file.path(O, "alternative_versions_summary.csv"))
ch <- dcast(alt, version + uf ~ ano, value.var = "score")
fwrite(ch[, .(all_ufs_fall = all(`2022` < `2010`), min_fall_pct = 100 * min(1 - `2022` / `2010`),
              max_fall_pct = 100 * max(1 - `2022` / `2010`)), by = version], file.path(O, "alternative_versions_change.csv"))

# rank of each UF across the tau grid (how much the position depends on the weight restriction)
rk <- s[, .(uf, rank = frank(-round(score, 9), ties.method = "min")), by = .(ano, tau)]
fwrite(rk[, .(rank_tau1 = rank[abs(tau - 1) < 1e-12], rank_main = rank[abs(tau - tm) < 1e-12],
              rank_min = min(rank), rank_max = max(rank)), by = .(ano, uf)], file.path(O, "rank_range_tau.csv"))
log_msg("stage 07: alternative versions, normative reference and rank ranges written")

# dominant indicator: indicator with the largest contribution w_j h_j to the score. The optimum of (2)
# may not be unique, so the range of each contribution share over the set of optimal coefficients is
# computed; the dominant indicator is unique when its minimum share exceeds the maximum share of
# every other indicator.
share_range <- function(h, Bm, tau) {
  p <- length(h); S <- bod_score(h, Bm, tau, FALSE)$score
  R <- diag(p) - matrix(tau / p, p, p)
  A <- rbind(Bm, R, h); dir <- c(rep("<=", nrow(Bm)), rep(">=", p), ">=")
  rhs <- c(rep(1, nrow(Bm)), rep(0, p), S * (1 - 1e-9))
  ext <- function(j, sense) { o <- numeric(p); o[j] <- h[j]; lpSolve::lp(sense, o, A, dir, rhs)$objval / S }
  data.table(indicador = INDICATORS, share_min = vapply(seq_len(p), ext, 0, sense = "min"),
             share_max = vapply(seq_len(p), ext, 0, sense = "max"))
}
dom <- rbindlist(lapply(c(2010L, 2022L), function(y) {
  Hy <- profile_matrix(P[ano == y]); ufs <- P[ano == y, uf]
  rbindlist(lapply(seq_len(nrow(Hy)), function(i) {
    r <- share_range(Hy[i, ], B, tm); k <- which.max(r$share_min)
    data.table(ano = y, uf = ufs[i], dominant = r$indicador[k], share_min = r$share_min[k], share_max = r$share_max[k],
               unique = r$share_min[k] > max(r$share_max[-k]) + 1e-9)
  }))
}))
fwrite(dom, file.path(O, "dominant_indicator.csv"))

# leave one out of the reference
H10 <- profile_matrix(P[ano == 2010]); H22 <- profile_matrix(P[ano == 2022])
loo <- rbindlist(lapply(seq_len(nrow(B)), function(k) {
  s10 <- bod_scores(H10, B[-k, , drop = FALSE], tm); s22 <- bod_scores(H22, B[-k, , drop = FALSE], tm)
  data.table(uf_removed = P[ano == 2010, uf][k], spearman_2010 = rank_cor(s10, main[ano == 2010, score]),
             spearman_2022 = rank_cor(s22, main[ano == 2022, score]),
             J_change_pct = 100 * (national(s22, P[ano == 2022, N]) / national(s10, P[ano == 2010, N]) - 1),
             all_ufs_fall = all(s22 < s10))
}))
fwrite(loo, file.path(O, "leave_one_out_reference.csv"))

# Poisson bootstrap of households within UF
cols <- c("uf", "pop", INDICATORS)
prep <- function(f) {
  d <- readRDS(file.path(PATHS$work, f))[, cols, with = FALSE]
  poor <- (Reduce(`+`, d[, INDICATORS, with = FALSE]) / length(INDICATORS)) >= PARAM$k - 1e-12
  m <- cbind(pop = d$pop, as.matrix(d[, INDICATORS, with = FALSE]) * (d$pop * poor))
  list(m = m, uf = d$uf)
}
d10 <- prep("households_2010.rds"); d22 <- prep("households_2022.rds")
boot_prof <- function(d) {
  x <- rowsum(d$m * rpois(nrow(d$m), 1), d$uf)          # Poisson weights; rows ordered by UF code
  list(H = x[, INDICATORS] / x[, "pop"], N = x[, "pop"])
}
set.seed(SEEDS$bootstrap)
bt <- rbindlist(lapply(seq_len(200), function(b) {
  x10 <- boot_prof(d10); x22 <- boot_prof(d22)
  s10 <- bod_scores(x10$H, x10$H, tm); s22 <- bod_scores(x22$H, x10$H, tm)
  data.table(rep = b, uf = as.integer(rownames(x10$H)), score_2010 = s10, score_2022 = s22,
             J_2010 = national(s10, x10$N), J_2022 = national(s22, x22$N))
}))
fwrite(bt, file.path(O, "bootstrap_replicates.csv"))
ci <- bt[, .(change_low = quantile(100 * (score_2022 / score_2010 - 1), 0.025),
             change_high = quantile(100 * (score_2022 / score_2010 - 1), 0.975)), by = uf]
ciJ <- unique(bt[, .(rep, J_2010, J_2022)])[, .(J_change_low = quantile(100 * (J_2022 / J_2010 - 1), 0.025),
                                                  J_change_high = quantile(100 * (J_2022 / J_2010 - 1), 0.975))]
fwrite(ci, file.path(O, "bootstrap_change_uf.csv")); fwrite(ciJ, file.path(O, "bootstrap_change_brazil.csv"))
log_msg("stage 07 complete")
