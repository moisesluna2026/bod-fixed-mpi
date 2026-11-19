# =============================================================================
# Checks run after the pipeline. They test properties that must hold for any data.
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
O <- PATHS$output; ok <- TRUE
check <- function(label, cond) { cat(if (isTRUE(cond)) "PASS" else "FAIL", "|", label, "\n"); if (!isTRUE(cond)) ok <<- FALSE }

pr <- fread(file.path(O, "af", "profiles.csv"))
check("AF identity: M0 = H x A in the 54 profiles", max(abs(pr$M0 - pr$H * pr$A)) < 1e-12)
sc <- fread(file.path(O, "scores", "scores_uf.csv"))
check("tau = 1 recovers M0 divided by the largest M0 of the reference",
      max(abs(sc[tau == 1, score] - sc[tau == 1, M0] / max(pr[ano == 2010, M0]))) < 1e-9)
f <- file.path(O, "audit", "solver_check.csv")
if (file.exists(f)) {
  sv <- fread(f)
  check("lpSolve equals GLPK in every score (max abs diff < 1e-8)", max(abs(sv$lpsolve - sv$glpk)) < 1e-8)
  check("tau = 0: lpSolve equals Benchmarking (max abs diff < 1e-8)", max(abs(sv[tau == 0, lpsolve - benchmarking])) < 1e-8)
}
it <- fread(file.path(O, "audit", "interference.csv"))
check("fixed reference: no other UF changes after any shock", all(it$others_changed_fixed == 0))
check("fixed reference: own score does not fall after a worsening nor rise after an improvement",
      all(it[shock == "worsening", own_fixed_change] >= -1e-9) && all(it[shock == "improvement", own_fixed_change] <= 1e-9))
check("fixed reference: national mean does not fall after a worsening", all(it[shock == "worsening", J_change_fixed] >= -1e-12))
mo <- fread(file.path(O, "audit", "monotonicity_profile.csv"))
check("weak monotonicity in the profile: no score falls", all(mo$change >= -1e-9))
check("strict monotonicity with tau > 0: every score rises", all(mo[tau > 0 & delta_h > 0, change] > 0))
pm <- fread(file.path(O, "audit", "monotonicity_person.csv"))
check("person level: no score falls when a household gains a deprivation", all(pm$change >= -1e-9))
check("person level, tau > 0: score rises whenever the household is poor after the change",
      all(pm[tau > 0 & poor_after == TRUE, change] > 0))
check("person level: score unchanged when the household stays non-poor (focus)", all(abs(pm[poor_after == FALSE, change]) < 1e-12))
sg <- fread(file.path(O, "audit", "subgroup_consistency.csv"))
check("within-UF componentwise worsening: national mean rises and no other UF changes", all(sg$J_after > sg$J_before) && all(sg$other_ufs_changed == 0))
w10 <- pr[ano == 2010][order(uf), ..INDICATORS]; w22 <- pr[ano == 2022][order(uf), ..INDICATORS]
check("every censored headcount is lower in 2022 than in 2010 in every UF (dominance)", all(as.matrix(w22) < as.matrix(w10)))
ch <- fread(file.path(O, "scores", "change_uf.csv"))
check("fixed reference, tau > 0: every UF score falls between 2010 and 2022", all(ch[tau > 0, score_2022 < score_2010]))
f <- file.path(O, "robustness", "dominant_indicator.csv")
if (file.exists(f)) check("dominant indicator unique in every UF in 2022", all(fread(f)[ano == 2022, unique]))
ex <- fread(file.path(O, "numerical_example", "example.csv"))
check("numerical example: fixed-reference mean = 16/15", abs(ex[territory == "mean", score_after_fixed] - 16 / 15) < 1e-9)
if (!ok) stop("At least one check failed.") else cat("All checks passed.\n")
