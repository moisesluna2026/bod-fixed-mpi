# =============================================================================
# Core functions
#
# Notation follows the article:
#   g_nj   deprivation of household n in indicator j (0/1), j = 1, ..., 12
#   c_n    weighted deprivation count, c_n = sum_j a_j g_nj with a_j = 1/12
#   rho_n  AF identification, rho_n = 1{c_n >= k}
#   h_ij   censored headcount of UF i in indicator j (share of persons who are poor
#          and deprived in j), the profile evaluated by the index
#   B      matrix of the 27 profiles of 2010 (fixed reference)
#   S(h)   score of profile h, max w'h s.t. Bw <= 1, w >= 0, w in W(tau)
# =============================================================================
log_msg <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ..., "\n")

zip_members <- function(zip) system2("unzip", c("-Z1", shQuote(zip)), stdout = TRUE)

read_fixed_width_zip <- function(zip, member, spec) {
  cut <- paste0("substr($0,", spec$start, ",", spec$length, ")", collapse = "\";\"")
  cmd <- sprintf("unzip -p %s %s | LC_ALL=C awk '{print %s}'", shQuote(zip), shQuote(member), cut)
  fread(cmd = cmd, sep = ";", header = FALSE, col.names = spec$var, colClasses = "character",
        na.strings = "", strip.white = TRUE, showProgress = FALSE)
}

file_sha256 <- function(path) {
  tool <- if (nzchar(Sys.which("sha256sum"))) "sha256sum" else "shasum -a 256"
  vapply(path, function(f) strsplit(system(paste(tool, shQuote(f)), intern = TRUE), " ")[[1]][1], "")
}

`%in1%` <- function(x, v) !is.na(x) & x %in% v      # missing answer = not deprived

# -----------------------------------------------------------------------------
# Alkire-Foster identification and censored headcounts by UF.
# hh: one row per household with the 12 indicators (0/1) and pop = sum of the
# person weights of its residents. Every resident shares the household profile.
# -----------------------------------------------------------------------------
af_profiles <- function(hh, k = PARAM$k, by = "uf", ind = INDICATORS) {
  a <- 1 / length(ind)
  hh[, c_n := Reduce(`+`, .SD) * a, .SDcols = ind]
  hh[, poor := c_n >= k - 1e-12]
  prof <- hh[, c(list(N = sum(pop), H = sum(pop * poor) / sum(pop),
                      A = if (any(poor)) sum(pop * poor * c_n) / sum(pop * poor) else 0),
                 lapply(.SD, function(g) sum(pop * g * poor) / sum(pop))),
             by = by, .SDcols = ind]
  prof[, M0 := Reduce(`+`, .SD) * a, .SDcols = ind]
  prof[]
}

# -----------------------------------------------------------------------------
# Benefit-of-the-Doubt score against a fixed reference.
#   S(h) = max w'h  s.t.  b'w <= 1 (every reference profile b),  w >= 0,
#                         w_j >= (tau / p) * sum_l w_l  (j = 1, ..., p).
# The restriction keeps every coefficient at least a fraction tau of the equal
# share 1/p of the coefficient sum. It does not depend on the evaluated profile,
# so the admissible set W(B, tau) is the same for every evaluation. tau = 0 is the
# unrestricted model; tau = 1 forces equal coefficients and S(h) = M0(h)/max_b M0(b).
# Solved with lpSolve; the dual is solved too and must give the same value.
# -----------------------------------------------------------------------------
bod_score <- function(h, B, tau = 0, dual_check = TRUE) {
  p <- length(h)
  R <- diag(p) - matrix(tau / p, p, p)                  # rows: w_j - (tau/p) sum w >= 0
  A <- rbind(B, R)
  fit <- lpSolve::lp("max", h, A, c(rep("<=", nrow(B)), rep(">=", p)), c(rep(1, nrow(B)), rep(0, p)))
  if (fit$status != 0) stop("Primal problem without optimal solution (status ", fit$status, ").")
  out <- list(score = fit$objval, w = fit$solution)
  if (dual_check) {
    # dual: min 1'lambda  s.t.  B'lambda - R'mu >= h,  lambda, mu >= 0
    du <- lpSolve::lp("min", c(rep(1, nrow(B)), rep(0, p)), cbind(t(B), -t(R)), rep(">=", p), h)
    if (du$status != 0 || abs(du$objval - fit$objval) > 1e-7 * max(1, fit$objval))
      stop("Primal and dual values differ.")
    out$lambda <- du$solution[seq_len(nrow(B))]
  }
  out
}
bod_scores <- function(H, B, tau = 0) vapply(seq_len(nrow(H)), function(i) bod_score(H[i, ], B, tau, FALSE)$score, 0)

# Rank correlation with scores rounded to 1e-9, so that UFs on the frontier (score 1 up to
# solver precision) are treated as ties on every platform.
rank_cor <- function(x, y) cor(round(x, 9), round(y, 9), method = "spearman")

profile_matrix <- function(prof, cols = INDICATORS) {
  m <- as.matrix(prof[, cols, with = FALSE]); rownames(m) <- prof$uf; m
}
region <- function(uf) fcase(uf %in% 11:17, "Norte", uf %in% 21:29, "Nordeste", uf %in% 31:35, "Sudeste",
                             uf %in% 41:43, "Sul", uf %in% 50:53, "Centro-Oeste")
