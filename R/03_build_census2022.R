# =============================================================================
# Stage 03. Households of the 2022 Census public sample (27 UFs) with the same 12
# indicators. Definitions follow the 2022 variable dictionary (Layout Microdados
# CD2022, acesso publico). Limits of the public 2022 files: no municipality code
# and age in five-year groups (P0180). Age bands used for the indicators (5-14,
# 10-14, 15-19, 20+) coincide with group limits, so they are exact in both years.
# Persons with suppressed age (P0180 = 99) cannot be assigned to an age band;
# they are counted as not deprived in the age-specific indicators and their
# households are flagged (age_suppressed) for the sensitivity check in stage 07.
# Universe: residents of permanent private occupied households (D0130 = 1).
# Output: work/households_2022.rds
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)

COLS_D <- c("D0020", "D0100", "D0110", "D0130", "D0230", "D0240", "D0250", "D0260", "D0270",
            "D0300", "D0310", "D0320", "D0360")
COLS_P <- c("P0100", "P0110", "P0130", "P0180", "P0650", "P0660", "P0770", "P0780", "P0930", "P0940", "P0950", "P0960", "P0990")
members <- zip_members(PATHS$census2022_zip)
read_member <- function(type, uf, cols) {
  m <- grep(sprintf("(^|/)%s_%d_.*\\.csv$", type, uf), members, value = TRUE)
  if (length(m) != 1) stop("File ", type, " of UF ", uf, " not found (or not unique)")
  fread(cmd = sprintf("unzip -p %s %s", shQuote(PATHS$census2022_zip), shQuote(m)),
        sep = ";", select = cols, encoding = "UTF-8", showProgress = FALSE)
}
line_real <- PARAM$income_share * PARAM$min_wage2010 * PARAM$ipca_jul2022 / PARAM$ipca_jul2010   # R$ of July 2022
line_sm   <- PARAM$income_share * PARAM$min_wage2022

build_uf <- function(uf) {
  dom <- read_member("Domicilios", uf, COLS_D)[D0130 == 1]
  p <- read_member("Pessoas", uf, COLS_P)[P0100 %in% dom$D0100 & P0130 == 1]
  g <- p$P0180
  p[, frequencia_p := g %in1% 2:3 & P0650 %in1% 2:3]                          # 5-14, not attending
  p[, atraso_p := g %in1% 2:4 & P0650 %in1% 1 & P0780 %in1% 2]               # main (IPM-Brasil): level not adequate to the age (IBGE)
  p[, atraso_h_p := P0650 %in1% 1 & ((g %in1% 3 & P0660 %in1% 1:3) |          # robustness: 10-14 below fundamental,
                                      (g %in1% 4 & P0660 %in1% 1:5))]          # 15-19 fundamental or below
  p[, escolaridade_p := g %in1% 5:17 & P0770 %in1% 1]                         # 20+, no schooling or incomplete fundamental
  p[, trabalho_p := g %in1% 5:17 & ((P0930 %in1% 1 & P0950 %in1% 0) |         # unemployed, or
                                    (P0950 %in1% 1 & !P0990 %in1% 7 & P0940 %in1% 0))]  # employed, not employer, no contribution in main job
  p[, trab_infantil_p := g %in1% 3 & P0960 %in1% 1]                           # 10-14 employed
  a <- p[, .(n_persons = .N, pop = sum(P0110),
             frequencia = any(frequencia_p), atraso = any(atraso_p), atraso_h = any(atraso_h_p), escolaridade = any(escolaridade_p),
             trabalho = any(trabalho_p), trab_infantil = any(trab_infantil_p),
             age_suppressed = any(P0180 %in% 99)), by = .(D0100 = P0100)]
  d <- merge(dom, a, by = "D0100")
  d[, .(uf = D0020, id_domicilio = D0100, w_dom = D0110, n_persons, pop, age_suppressed,
        agua = D0260 %in1% 2:8 & D0300 %in1% 3,                     # not from the network and not piped
        esgoto = D0250 %in1% 4:9,                                   # rudimentary pit, ditch, river, other, no toilet
        lixo = D0310 %in1% 3:6,                                     # not collected
        densidade = !is.na(D0240) & D0240 > 2,                      # more than 2 residents per bedroom (IBGE derived variable)
        banheiro = D0270 %in1% 5:7,                                 # no bathroom for exclusive use
        consumo = D0320 %in1% 2,                                    # no washing machine
        frequencia, atraso, atraso_h, escolaridade, trabalho, trab_infantil,
        renda_pc = D0360,                                           # household income per capita, R$ of July 2022
        renda = !is.na(D0360) & D0360 < line_sm,                    # below one quarter of the 2022 minimum wage
        renda_real = !is.na(D0360) & D0360 < line_real,             # robustness: 2010 line in July 2022 reais
        renda_missing = is.na(D0360),
        density_ibge = D0240)]
}

d <- rbindlist(lapply(UFS, function(u) { r <- build_uf(u); log_msg("UF", u, format(nrow(r), big.mark = ","), "households"); r }))
stopifnot(!anyDuplicated(d$id_domicilio), setequal(unique(d$uf), UFS))
for (v in c(INDICATORS, "atraso_h", "renda_real")) set(d, j = v, value = as.integer(d[[v]]))
d[, ano := 2022L]
saveRDS(d, file.path(PATHS$work, "households_2022.rds"))
fwrite(d[, .(households = .N, persons = sum(n_persons), population = sum(pop), households_age_suppressed = sum(age_suppressed)),
         by = uf][order(uf)], file.path(PATHS$output, "base_2022_counts.csv"))
log_msg("stage 03 complete:", format(nrow(d), big.mark = ","), "households; income line (R$ of July 2022):", round(line_real, 2))
