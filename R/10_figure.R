# =============================================================================
# Stage 10. Figure 1: UF scores in 2010 and 2022 against the fixed 2010 reference and, for 2022,
# against the frontier of 2022 profiles (tau = main). Output: output/figure/figure_1.png
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", if (.Platform$OS.type == "windows") "" else "C.UTF-8")))
O <- file.path(PATHS$output, "figure"); dir.create(O, showWarnings = FALSE)
s <- fread(file.path(PATHS$output, "scores", "scores_uf.csv"))[abs(tau - PARAM$tau_main) < 1e-12]
w <- dcast(s, sigla ~ ano, value.var = c("score", "score_moving"))[order(score_2010)]
png(file.path(O, "figure_1.png"), width = 6.3, height = 6.2, units = "in", res = 300, family = "serif", pointsize = 9, type = if (capabilities("cairo")) "cairo" else getOption("bitmapType"))
par(mar = c(4, 3.2, 0.5, 0.5), las = 1)
y <- seq_len(nrow(w))
plot(NA, xlim = c(0, 1.05), ylim = c(0.5, nrow(w) + 0.5), yaxt = "n", xaxt = "n", xlab = "Score S (τ = 0.5)", ylab = "", bty = "l")
abline(v = seq(0, 1, 0.2), col = "grey90", lwd = 0.6)
axis(1, at = seq(0, 1, 0.2), labels = format(seq(0, 1, 0.2), nsmall = 1))
axis(2, at = y, labels = w$sigla, tick = FALSE, line = -0.5)
segments(w$score_2022, y, w$score_2010, y, col = "grey60", lwd = 1.2)
points(w$score_moving_2022, y, pch = 4, col = "grey35", cex = 0.9)
points(w$score_2010, y, pch = 19, col = "grey12", cex = 0.9)
points(w$score_2022, y, pch = 21, bg = "white", col = "grey12", cex = 0.9)
legend("bottomright", bty = "n", pch = c(19, 21, 4), pt.bg = "white", col = c("grey12", "grey12", "grey35"),
       legend = c("2010", "2022, fixed reference (2010)", "2022, re-estimated reference (2022)"))
invisible(dev.off())
log_msg("stage 10 complete: figure_1.png")
