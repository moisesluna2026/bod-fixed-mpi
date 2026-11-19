# =============================================================================
# Stage 08. Numerical example of the article (section 3): three territories with
# equal populations and two censored headcounts. Territory A gains deprivation in
# the second indicator (0.10 -> 0.30). With the reference re-estimated after the
# change, the score of C falls and the mean falls although nobody improved; with the
# fixed reference, only A changes and the mean rises.
# Output: output/numerical_example/example.csv
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)
O <- file.path(PATHS$output, "numerical_example"); dir.create(O, showWarnings = FALSE)
H0 <- rbind(A = c(0.40, 0.10), B = c(0.10, 0.40), C = c(0.20, 0.20))
H1 <- H0; H1["A", 2] <- 0.30
before <- bod_scores(H0, H0, 0)
moving <- bod_scores(H1, H1, 0)
fixed  <- bod_scores(H1, H0, 0)
ex <- data.table(territory = rownames(H0), h1_before = H0[, 1], h2_before = H0[, 2], h2_after = H1[, 2],
                 score_before = before, score_after_moving = moving, score_after_fixed = fixed)
ex <- rbind(ex, data.table(territory = "mean", score_before = mean(before), score_after_moving = mean(moving),
                           score_after_fixed = mean(fixed)), fill = TRUE)
stopifnot(abs(before - c(1, 1, 0.8)) < 1e-9, abs(moving - c(1, 1, 8 / 13)) < 1e-9, abs(fixed - c(1.4, 1, 0.8)) < 1e-9)
fwrite(ex, file.path(O, "example.csv")); print(ex)
log_msg("stage 08 complete")
