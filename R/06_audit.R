# =============================================================================
# Stage 06. Audit of the method (main restriction and tau = 0).
#  (a) Solver check: every score is solved in primal and dual form (bod_score);
#      here the 54 x |tau grid| scores are also checked against an independent
#      solver: GLPK (rDEA) for every tau and the Benchmarking package for tau = 0
#      (output-oriented CRS radial expansion with unit input; score = 1/expansion).
#  (b) Interference: each 2010 UF in turn has all its censored headcounts raised by
#      20% (worsening) or reduced by 20% (improvement). Scores of the other UFs and
#      the national mean are recomputed with the moving reference (re-estimated
#      with the shocked profiles) and with the fixed reference.
#  (c) Monotonicity in the profile: each headcount of each profile raised by 0.01.
#  (d) Monotonicity at the person level (2022 microdata): 2,000 households drawn at
#      random; one indicator in which the household is not deprived is switched to
#      deprived; the UF profile is recomputed exactly and the UF is rescored.
#  (e) Subgroup consistency: 10% of the households of one UF (Bahia, 2022) gain one
#      deprivation; the national mean must rise and no other UF may change.
# Outputs: output/audit/*.csv
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
O <- file.path(PATHS$output, "audit"); dir.create(O, showWarnings = FALSE)
P <- readRDS(file.path(PATHS$work, "profiles.rds"))$main
z <- readRDS(file.path(PATHS$work, "scores.rds")); s <- z$scores; B <- z$B
TAUS <- c(0, PARAM$tau_main)

# (a) independent solvers --------------------------------------------------------
glpk_score <- function(H, B, tau) {
  # Batch GLPK (rDEA), same interface as the DEA-MP/H package: variables (t, w);
  # maximize t subject to t - h'w <= 0, Bw <= 1, w_j - (tau/p) sum w >= 0. Only the
  # first row changes between profiles, so all profiles are solved in one call.
  p <- ncol(H); R <- diag(p) - matrix(tau / p, p, p)
  A <- rbind(c(1, rep(0, p)), cbind(0, B), cbind(0, R))
  dir <- c("<=", rep("<=", nrow(B)), rep(">=", p)); rhs <- c(0, rep(1, nrow(B)), rep(0, p))
  sol <- rDEA::multi_glpk_solve_LP(obj = c(1, rep(0, p)), mat = A, dir = dir, rhs = rhs, max = TRUE,
                                   mmat_i = cbind(1L, 1L + seq_len(p)), mmat_val = -t(H))
  if (any(sol$status != 0)) stop("GLPK did not reach an optimal solution.")
  sol$optimum
}
if (nzchar(Sys.getenv("BOD_SKIP_EXTERNAL_SOLVERS"))) {
  log_msg("independent solver check skipped (BOD_SKIP_EXTERNAL_SOLVERS is set)")
} else {
  for (pk in c("rDEA", "Benchmarking")) if (!requireNamespace(pk, quietly = TRUE)) stop("Install the package ", pk)
  H10 <- profile_matrix(P[ano == 2010]); H22 <- profile_matrix(P[ano == 2022])
  bm <- function(H) 1 / Benchmarking::dea(matrix(1, nrow(H), 1), H, RTS = "crs", ORIENTATION = "out",
                                          XREF = matrix(1, nrow(B), 1), YREF = B)$eff
  chk <- rbindlist(lapply(PARAM$tau_grid, function(tt) rbindlist(lapply(c(2010L, 2022L), function(y) {
    H <- profile_matrix(P[ano == y]); ref <- s[abs(tau - tt) < 1e-12 & ano == y]
    out <- data.table(tau = tt, ano = y, uf = P[ano == y, uf], lpsolve = ref[match(P[ano == y, uf], uf), score],
                      glpk = glpk_score(H, B, tt))
    if (tt == 0) out[, benchmarking := bm(H)]
    out
  }))), fill = TRUE)
  fwrite(chk, file.path(O, "solver_check.csv"))
  log_msg("solver check: max |lpSolve - GLPK| =", signif(max(abs(chk$lpsolve - chk$glpk)), 3),
          "| max |lpSolve - Benchmarking| (tau = 0) =", signif(max(abs(chk[tau == 0, lpsolve - benchmarking])), 3))
}

# (b) interference ---------------------------------------------------------------
H10 <- profile_matrix(P[ano == 2010]); H22 <- profile_matrix(P[ano == 2022])
N10 <- P[ano == 2010, N]; ufs <- P[ano == 2010, uf]
inter <- rbindlist(lapply(TAUS, function(tau) {
  base_fixed <- bod_scores(H10, B, tau)
  rbindlist(lapply(seq_along(ufs), function(k) rbindlist(lapply(c(1.2, 0.8), function(f) {
    D <- H10; D[k, ] <- pmin(1, D[k, ] * f)
    fixed <- bod_scores(D, B, tau); moving <- bod_scores(D, D, tau)
    others <- setdiff(seq_along(ufs), k)
    data.table(tau = tau, uf_shocked = ufs[k], shock = if (f > 1) "worsening" else "improvement",
               on_frontier = abs(base_fixed[k] - 1) < 1e-8,
               own_fixed_change = fixed[k] - base_fixed[k], own_moving_change = moving[k] - base_fixed[k],
               others_changed_moving = sum(abs(moving[others] - base_fixed[others]) > 1e-9),
               others_changed_fixed = sum(abs(fixed[others] - base_fixed[others]) > 1e-9),
               max_other_change_moving = max(abs(moving[others] - base_fixed[others])),
               J_change_fixed = sum(N10 * fixed) / sum(N10) - sum(N10 * base_fixed) / sum(N10),
               J_change_moving = sum(N10 * moving) / sum(N10) - sum(N10 * base_fixed) / sum(N10))
  }))))
}))
fwrite(inter, file.path(O, "interference.csv"))

# (c) profile monotonicity ---------------------------------------------------------
mono <- rbindlist(lapply(TAUS, function(tau) rbindlist(lapply(c(2010L, 2022L), function(y) {
  H <- profile_matrix(P[ano == y]); base <- bod_scores(H, B, tau)
  rbindlist(lapply(seq_len(nrow(H)), function(i) rbindlist(lapply(seq_along(INDICATORS), function(j) {
    h <- H[i, ]; d <- min(0.01, 1 - h[j]); h[j] <- h[j] + d
    data.table(tau = tau, ano = y, uf = P[ano == y, uf][i], indicador = INDICATORS[j], delta_h = d,
               change = bod_score(h, B, tau, FALSE)$score - base[i])
  }))))
}))))
fwrite(mono, file.path(O, "monotonicity_profile.csv"))

# (d) person-level monotonicity -----------------------------------------------------
b22 <- readRDS(file.path(PATHS$work, "households_2022.rds"))[, c("uf", "pop", INDICATORS), with = FALSE]
a <- 1 / length(INDICATORS)
b22[, c_n := Reduce(`+`, .SD) * a, .SDcols = INDICATORS][, poor := c_n >= PARAM$k - 1e-12]
tot <- b22[, .(N = sum(pop)), by = uf]
set.seed(SEEDS$person_monotonicity)
draw <- b22[sample.int(.N, 2000)][c_n < 1]
# Every tau evaluates the identical household shock.
draw[, changed_indicator := vapply(seq_len(.N), function(r) {
  free <- INDICATORS[unlist(draw[r, INDICATORS, with = FALSE]) == 0]
  free[sample.int(length(free), 1)]
}, "")]
pm <- rbindlist(lapply(TAUS, function(tau) {
  base22 <- setNames(bod_scores(H22, B, tau), P[ano == 2022, uf])
  rbindlist(lapply(seq_len(nrow(draw)), function(r) {
    x <- draw[r]; free <- INDICATORS[unlist(x[, INDICATORS, with = FALSE]) == 0]
    j <- x$changed_indicator
    g0 <- unlist(x[, INDICATORS, with = FALSE]); g1 <- g0; g1[j] <- 1L
    poor1 <- sum(g1) * a >= PARAM$k - 1e-12
    # exact change of the censored headcounts of the UF
    h <- H22[as.character(x$uf), ]; Nuf <- tot[uf == x$uf, N]
    h <- h + x$pop / Nuf * (g1 * poor1 - g0 * x$poor)
    data.table(tau = tau, uf = x$uf, indicador = j, was_poor = x$poor, poor_after = poor1,
               change = bod_score(h, B, tau, FALSE)$score - base22[as.character(x$uf)])
  }))
}))
fwrite(pm, file.path(O, "monotonicity_person.csv"))

# (e) subgroup consistency -----------------------------------------------------------
set.seed(SEEDS$subgroup)
d_subgroup <- copy(b22[uf == 29]); idx <- sample.int(nrow(d_subgroup), round(0.1 * nrow(d_subgroup)))
for (i in idx) { free <- which(unlist(d_subgroup[i, INDICATORS, with = FALSE]) == 0)
  if (length(free)) set(d_subgroup, i, INDICATORS[free[sample.int(length(free), 1)]], 1L) }
hb_subgroup <- af_profiles(d_subgroup[, c("uf", "pop", INDICATORS), with = FALSE])
sg <- rbindlist(lapply(TAUS, function(tau) {
  hb <- copy(hb_subgroup)
  H <- H22; H["29", ] <- unlist(hb[, INDICATORS, with = FALSE])
  N22 <- P[ano == 2022, N]
  before <- bod_scores(H22, B, tau); after <- bod_scores(H, B, tau)
  data.table(tau = tau, households_worsened = length(idx), score_BA_before = before[rownames(H22) == "29"],
             score_BA_after = after[rownames(H) == "29"], other_ufs_changed = sum(abs(after - before)[rownames(H) != "29"] > 1e-12),
             J_before = sum(N22 * before) / sum(N22), J_after = sum(N22 * after) / sum(N22))
}))
fwrite(sg, file.path(O, "subgroup_consistency.csv"))
log_msg("stage 06 complete")
