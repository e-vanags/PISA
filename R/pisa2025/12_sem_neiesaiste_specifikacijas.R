# ============================================================================
# 12. Neiesaistes lomas pārbaude: divas konkurējošas SEM specifikācijas
# ============================================================================
#
# Pētnieciskais jautājums (sk. docs/11_lasitprasmes_kritums_hipotezes.md, 3.a):
#   Vai uzvedības neiesaiste ir lasītprasmes krituma CĒLONIS vai SIMPTOMS?
#
#   M-a  ESCS ---------> Neiesaiste --> Lasītprasme   (eksogēna motivācija)
#   M-b  Dekodēšana ---> Neiesaiste --> Lasītprasme   (atduršanās pie teksta)
#
# Abām specifikācijām ir vienāds brīvības pakāpju skaits, tāpēc tās NAV
# ligzdotas. Salīdzināšanai lieto AIC/BIC un ceļu koeficientu ticamību,
# nevis chi-kvadrāta starpības testu.
#
# STATUSS: sagatave. Neizpildās, kamēr nav PISA 2025 mikrodatu.
# ============================================================================

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
output_file <- file.path("output", "pisa2025", "logs",
                         "12_sem_neiesaiste_specifikacijas_output.txt")
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

# --- Pakotnes ----------------------------------------------------------------
required_packages <- c("lavaan", "dplyr", "haven")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Instalēju pakotni:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}
cat("Pakotnes ielādētas.\n\n")


# === 1. KONFIGURĀCIJA ========================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("1. Konfigurācija\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

DATU_FAILS <- "data/pisa2025/processed/pisa_lva_2025_clean.rds"

# Mainīgo nosaukumi PISA 2025 ciklā VĒL NAV APSTIPRINĀTI.
# Pirms palaišanas salīdzināt ar 2025. codebook — starp cikliem tie mainās.
mv <- list(
  pv_read   = paste0("PV", 1:10, "READ"),
  pv_math   = paste0("PV", 1:10, "MATH"),
  escs      = "ESCS",
  valoda    = "LANGN",      # runātā valoda mājās
  imig      = "IMMIG",
  # Dekodēšanas indikatori — no fluences uzdevumiem
  fluence   = c("RFTIME", "RFACC"),
  # Neiesaistes indikatori — PRASA ITEM-LĪMEŅA ATBILŽU LAIKUS
  neiesaiste = c("DISENGPCT", "SPEEDEDFL", "EFFORT")
)

cat("Datu fails:", DATU_FAILS, "\n\n")

if (!file.exists(DATU_FAILS)) {
  cat("!! Mikrodati vēl nav pieejami. Skripts apstājas.\n\n")
  cat("Pirms palaišanas jāizdara:\n")
  cat(" 1. Lejupielādēt PISA 2025 mikrodatus no OECD datubāzes.\n")
  cat(" 2. Adaptēt 01_datu_sagatavosana.R un ģenerēt", DATU_FAILS, "\n")
  cat(" 3. PĀRBAUDĪT, vai item-līmeņa atbilžu laiki ir publiskajā failā.\n")
  cat("    Ja nav, latento 'Neiesaiste' konstruēt nevar un abas\n")
  cat("    specifikācijas atkrīt. Sk. Technical Report un COG failu.\n")
  cat(" 4. Atjaunināt mainīgo nosaukumus 'mv' sarakstā pēc 2025. codebook.\n")
  while (sink.number() > 0) sink()
  quit(save = "no", status = 0)
}


# === 2. SPECIFIKĀCIJAS =======================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("2. Modeļu specifikācijas\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Kopīgā mērījumu daļa. Lasītprasme ievadās pa vienai ticamajai vērtībai,
# tāpēc modelī tā ir novērots mainīgais 'read_pv', nevis latents.
mer_dala <- '
  dekod =~ RFTIME + RFACC
  nedar =~ DISENGPCT + SPEEDEDFL + EFFORT
'

# --- M-a: neiesaiste kā cēlonis ---------------------------------------------
# Neiesaisti prognozē ESCS; dekodēšana uz to neietekmē.
model_a <- paste(mer_dala, '
  dekod   ~ a1*ESCS + a2*valoda_d
  nedar   ~ b1*ESCS
  read_pv ~ c1*dekod + c2*nedar + c3*ESCS
')

# --- M-b: neiesaiste kā pazīme ----------------------------------------------
# Neiesaisti prognozē dekodēšana; ESCS uz to iedarbojas tikai netieši.
model_b <- paste(mer_dala, '
  dekod   ~ a1*ESCS + a2*valoda_d
  nedar   ~ d1*dekod
  read_pv ~ c1*dekod + c2*nedar + c3*ESCS

  # Netiešais ceļš, kas ir visa jautājuma būtība
  netiesais := d1*c2
  kopejais  := c1 + d1*c2
')

cat("M-a (neiesaiste kā cēlonis):\n", model_a, "\n\n")
cat("M-b (neiesaiste kā pazīme):\n", model_b, "\n\n")


# === 3. NOVĒRTĒŠANA PA TICAMAJĀM VĒRTĪBĀM ====================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("3. Novērtēšana\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

dati <- readRDS(DATU_FAILS)

# Katrs modelis jāfitē 10 reizes, pa vienai ticamajai vērtībai, un rezultāti
# jāapvieno pēc Rubina likumiem. Svari: W_FSTUWT.
fit_pa_pv <- function(modelis, dati, pv_mainigie, svars = "W_FSTUWT") {
  lapply(pv_mainigie, function(pv) {
    d <- dati
    d$read_pv <- d[[pv]]
    lavaan::sem(modelis, data = d, sampling.weights = svars,
                estimator = "MLR", missing = "fiml")
  })
}

apvienot_rubins <- function(fiti) {
  est <- sapply(fiti, function(f) lavaan::parameterEstimates(f)$est)
  se  <- sapply(fiti, function(f) lavaan::parameterEstimates(f)$se)
  m <- ncol(est)
  q_bar <- rowMeans(est)                       # apvienotā aplēse
  u_bar <- rowMeans(se^2)                      # vidējā izlases dispersija
  b     <- apply(est, 1, var)                  # dispersija starp PV
  total <- u_bar + (1 + 1 / m) * b             # Rubina kopējā dispersija
  data.frame(
    parametrs = lavaan::parameterEstimates(fiti[[1]])$label,
    lhs = lavaan::parameterEstimates(fiti[[1]])$lhs,
    op  = lavaan::parameterEstimates(fiti[[1]])$op,
    rhs = lavaan::parameterEstimates(fiti[[1]])$rhs,
    est = q_bar, se = sqrt(total)
  )
}

fiti_a <- fit_pa_pv(model_a, dati, mv$pv_read)
fiti_b <- fit_pa_pv(model_b, dati, mv$pv_read)

cat("--- M-a apvienotās aplēses ---\n"); print(apvienot_rubins(fiti_a))
cat("\n--- M-b apvienotās aplēses ---\n"); print(apvienot_rubins(fiti_b))


# === 4. SALĪDZINĀJUMS ========================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("4. Modeļu salīdzinājums\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

fitmeras <- c("cfi", "tli", "rmsea", "srmr", "aic", "bic")
sal <- rbind(
  `M-a` = colMeans(t(sapply(fiti_a, function(f) lavaan::fitMeasures(f, fitmeras)))),
  `M-b` = colMeans(t(sapply(fiti_b, function(f) lavaan::fitMeasures(f, fitmeras))))
)
print(round(sal, 4))

cat("\nInterpretācija:\n")
cat(" M-b labāk  -> neiesaiste ir pazīme. Pārvērtēšana bez neiesaistītajām\n")
cat("              atbildēm kritumu NEATGŪS. Problēma ir dekodēšanas prasme.\n")
cat(" M-a labāk  -> neiesaiste ir patstāvīgs cēlonis. Daļa krituma ir testa\n")
cat("              motivācijas, nevis prasmes jautājums.\n")
cat(" Neatšķiras -> dati nešķir; vajadzīgs eksogēns instruments vai\n")
cat("              2022/2025 daudzgrupu modelis ar invariances testēšanu.\n")


# === 5. NEPADARĪTAIS =========================================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("5. Kas šim skriptam vēl trūkst\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
cat(" - BRR standartkļūdas ar 80 atkārtoto izlašu svariem (Fay 0.5).\n")
cat("   Pašlaik SE nāk no MLR un ir sistemātiski par mazu.\n")
cat(" - Daudzgrupu variants 2022 pret 2025 ar mērījumu invariances testēšanu.\n")
cat(" - Stratifikācija pēc eksogēniem mainīgajiem, NEVIS pēc izmērītā\n")
cat("   lasītprasmes līmeņa (cirkularitāte, sk. dokumenta 3.b sadaļu).\n")

cat("\nPabeigts:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
while (sink.number() > 0) sink()
