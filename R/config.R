# =============================================================================
# Fixed-reference Benefit-of-the-Doubt index on Alkire-Foster censored headcounts
# Configuration: paths, parameters and seeds. Paths can be set with environment
# variables, so the package runs from any folder.
# =============================================================================
PATHS <- list(
  census2010_dir = Sys.getenv("BOD_CENSUS2010_DIR", "raw/census2010"),
  census2022_zip = Sys.getenv("BOD_CENSUS2022_ZIP", "raw/census2022/Todas_as_UFs.zip"),
  work   = Sys.getenv("BOD_WORK_DIR", "work"),
  output = Sys.getenv("BOD_OUTPUT_DIR", "output"))

URLS <- list(
  census2010 = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_Gerais_da_Amostra/Microdados/",
  census2022 = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Microdados_e_Areas_de_Ponderacao/Microdados_de_acesso_Publico/csv/Todas_as_UFs.zip")

PARAM <- list(
  k            = 1/3,          # AF poverty cut-off (share of weighted deprivations)
  min_wage2010 = 510,          # minimum wage, July 2010 (R$)
  min_wage2022 = 1212,         # minimum wage, July 2022 (R$)
  ipca_jul2010 = 3111.05,      # IPCA number index, July 2010 (IBGE, SIDRA table 1737)
  ipca_jul2022 = 6411.95,      # IPCA number index, July 2022
  income_share = 0.25,         # income line: one quarter of the minimum wage (robustness: 2010 line in real terms)
  tau_main     = 0.5,          # main weight restriction (see functions.R)
  tau_grid     = c(0, 0.1, 0.25, 0.5, 0.75, 1),
  tol          = 1e-9)

SEEDS <- list(person_monotonicity = 41, subgroup = 42, bootstrap = 43)
INDICATORS <- c("agua", "esgoto", "lixo", "densidade", "banheiro", "consumo",
                "frequencia", "atraso", "escolaridade", "trabalho", "trab_infantil", "renda")
DIMENSIONS <- setNames(rep(c("saneamento", "moradia_bens", "educacao", "trabalho_renda"), each = 3), INDICATORS)

UFS <- c(11:17, 21:29, 31:33, 35, 41:43, 50:53)
UF_ABBR <- setNames(c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE","AL","SE","BA",
                      "MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF"), UFS)
CORES <- as.integer(Sys.getenv("BOD_CORES", max(1L, min(4L, parallel::detectCores() - 1L))))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
for (p in c(PATHS$work, PATHS$output)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
