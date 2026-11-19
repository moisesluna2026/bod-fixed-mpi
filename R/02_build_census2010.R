# =============================================================================
# Stage 02. Households of the 2010 Census sample (27 UFs) with the 12 deprivation
# indicators, read from the official IBGE fixed-width files. Positions follow
# Layout_microdados_Amostra.xls (Documentacao.zip).
# Universe: residents of permanent private occupied households (V4001 = 01).
# Output: work/households_2010.rds, one row per household.
# =============================================================================
source("R/config.R"); source("R/functions.R"); library(data.table)

SPEC_DOM <- data.table(
  var    = c("V0001", "V0002", "V0300", "V0010", "V4001", "V0204", "V6204", "V0205", "V0206", "V0207",
             "V0208", "V0209", "V0210", "V0215", "V0401", "V6532"),
  start  = c(1, 3, 21, 29, 54, 80, 82, 85, 86, 87, 88, 90, 91, 96, 105, 134),
  length = c(2, 5, 8, 16, 2, 2, 3, 1, 1, 1, 2, 1, 1, 1, 2, 9))
SPEC_PES <- data.table(
  var    = c("V0002", "V0300", "V0010", "V6036", "V0628", "V0629", "V6400", "V0641", "V0642", "V0643",
             "V0644", "V0648", "V0650", "V0654", "V6910"),
  start  = c(3, 21, 29, 62, 147, 148, 158, 195, 196, 197, 198, 209, 211, 316, 392),
  length = c(5, 8, 16, 3, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1))

build_zip <- function(zip) {
  members <- zip_members(zip)
  f_dom <- grep("domicilios.*\\.txt$", members, ignore.case = TRUE, value = TRUE)
  f_pes <- grep("pessoas.*\\.txt$", members, ignore.case = TRUE, value = TRUE)
  stopifnot(length(f_dom) == 1, length(f_pes) == 1)

  dom <- read_fixed_width_zip(zip, f_dom, SPEC_DOM)
  dom[, names(dom) := lapply(.SD, as.numeric)]
  dom[, `:=`(V0010 = V0010 / 1e13, V6204 = V6204 / 10, V6532 = V6532 / 1e5)]   # implied decimals
  dom <- dom[V4001 == 1]

  p <- read_fixed_width_zip(zip, f_pes, SPEC_PES)
  p[, names(p) := lapply(.SD, as.numeric)]
  p[, V0010 := V0010 / 1e13]
  p <- p[dom[, .(V0002, V0300)], on = .(V0002, V0300), nomatch = NULL]
  age <- p$V6036
  attending <- p$V0628 %in1% 1:2
  level <- fcase(p$V0629 %in1% 9:12, 4L, p$V0629 %in1% 7:8, 3L, p$V0629 %in1% 5:6, 2L, p$V0629 %in1% 1:4, 1L,
                 default = NA_integer_)                                   # 1 pre-school/literacy, 2 fundamental, 3 secondary, 4 tertiary
  p[, frequencia_p := age >= 5 & age <= 14 & V0628 %in1% 3:4]                # 5-14, not attending
  # delay (main, as in the IPM-Brasil): attending, age 6-19, below the level adequate to the age
  p[, atraso_p := attending & age >= 6 & age <= 19 & !is.na(level) &
                  ((age <= 14 & level < 2) | (age >= 15 & age <= 17 & level < 3) | (age >= 18 & level < 4))]
  # delay (robustness): same rule in both Censuses, by age group and course attended
  p[, atraso_h_p := attending & ((age >= 10 & age <= 14 & V0629 %in1% 1:4) | (age >= 15 & age <= 19 & V0629 %in1% 1:6))]
  p[, escolaridade_p := age >= 20 & V6400 %in1% 1]                           # 20+, no schooling or incomplete fundamental
  p[, trabalho_p := age >= 20 & (V0654 %in1% 1 |                             # looked for work, or
                                 (!is.na(V0648) & !V0648 %in1% 6 & V0650 %in1% 3))]  # employed, not employer, no social security contribution
  p[, trab_infantil_p := age >= 10 & age <= 14 &                             # 10-14 working
                         (V0641 %in1% 1 | V0642 %in1% 1 | V0643 %in1% 1 | V0644 %in1% 1)]
  a <- p[, .(n_persons = .N, pop = sum(V0010),
             frequencia = any(frequencia_p), atraso = any(atraso_p), atraso_h = any(atraso_h_p), escolaridade = any(escolaridade_p),
             trabalho = any(trabalho_p), trab_infantil = any(trab_infantil_p)), by = .(V0002, V0300)]
  d <- merge(dom, a, by = c("V0002", "V0300"))
  if (any(d$n_persons != d$V0401)) stop("Person records do not match V0401 in ", basename(zip))
  line2010 <- PARAM$income_share * PARAM$min_wage2010
  d[, .(uf = V0001, municipio = V0002, id_domicilio = V0300, w_dom = V0010, n_persons, pop,
        agua = V0208 %in1% 2:10 & V0209 %in1% 3,                   # not from the network and not piped
        esgoto = V0206 %in1% 2 | V0207 %in1% 3:6,                  # no toilet, rudimentary pit, ditch, river, other
        lixo = V0210 %in1% 3:7,                                    # not collected
        densidade = !is.na(V6204) & V6204 > 2,                     # more than 2 residents per bedroom (IBGE derived variable)
        banheiro = V0205 %in1% 0,                                  # no bathroom for exclusive use
        consumo = V0215 %in1% 2,                                   # no washing machine
        frequencia, atraso, atraso_h, escolaridade, trabalho, trab_infantil,
        renda_pc = V6532 * PARAM$min_wage2010,                     # household income per capita, R$ of July 2010
        renda = !is.na(V6532) & V6532 < PARAM$income_share,        # below one quarter of the minimum wage
        renda_real = !is.na(V6532) & V6532 * PARAM$min_wage2010 < line2010,
        renda_missing = is.na(V6532), density_ibge = V6204)]
}

zips <- file.path(PATHS$census2010_dir, setdiff(list.files(PATHS$census2010_dir, "\\.zip$"), "Documentacao.zip"))
stopifnot(length(zips) == 28)
parts <- lapply(zips, function(z) { r <- build_zip(z); log_msg(basename(z), format(nrow(r), big.mark = ","), "households"); r })
d <- rbindlist(parts); rm(parts); invisible(gc())
stopifnot(!anyDuplicated(d[, .(uf, municipio, id_domicilio)]), setequal(unique(d$uf), UFS))
for (v in c(INDICATORS, "atraso_h", "renda_real")) set(d, j = v, value = as.integer(d[[v]]))
d[, ano := 2010L]
saveRDS(d, file.path(PATHS$work, "households_2010.rds"))
fwrite(d[, .(households = .N, persons = sum(n_persons), population = sum(pop)), by = uf][order(uf)],
       file.path(PATHS$output, "base_2010_counts.csv"))
log_msg("stage 02 complete:", format(nrow(d), big.mark = ","), "households,", format(sum(d$n_persons), big.mark = ","), "persons")
