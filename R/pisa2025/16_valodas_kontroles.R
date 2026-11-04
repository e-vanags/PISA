# ============================================================================
# 16. Vai valodu grupu starpība saglabājas, kontrolējot SES un skolu
# ============================================================================
#
# Jautājums. 2025. gadā krieviski runājošo skolēnu lasīšanas rezultāts ir
# 83 punktus zemāks nekā latviski runājošo. Cik no šīs starpības paliek, ja
# kontrolē sociālekonomisko statusu, demogrāfiju un skolu.
#
# Kāpēc skolas fiksētie efekti ir svarīgi. Ja starpība pazūd, kontrolējot
# skolu, tad tā ir starp skolām, proti, bijušo mazākumtautību skolu efekts.
# Ja tā saglabājas skolas iekšienē, tad runa ir par pašu skolēnu, nevis par
# viņa skolu. Šie divi gadījumi nozīmē atšķirīgu politiku.
#
# Modeļi, katrs uz 10 PV ar BRR standartkļūdām:
#   M0  tikai grupa
#   M1  + ESCS
#   M2  + ESCS, dzimums, klase, imigrācijas statuss
#   M3  + skolas fiksētie efekti, proti, salīdzinājums skolas iekšienē
#
# SVARĪGI. M3 identificē starpību tikai tajās skolās, kurās ir abas grupas.
# Skripts atsevišķi parāda, cik liela daļa skolēnu mācās jauktās skolās.
#
# Ievade:  data/pisa2025/processed/pisa_lva_2025_stu.rds
#          data/pisa2022/processed/pisa_lva_2022_stu.rds  (skripts 15)
# Izvade:  output/pisa2025/logs/16_valodas_kontroles_output.txt
#          output/pisa2025/valodas_kontroles_modeli.csv
# ============================================================================

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/16_valodas_kontroles_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

# --- Pakotnes ----------------------------------------------------------------
required_packages <- c("haven")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Instalēju pakotni:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}
cat("Pakotnes ielādētas.\n\n")


# === 1. DATI =================================================================
cat(strrep("=", 70), "\n")
cat("1. Dati\n")
cat(strrep("=", 70), "\n\n")

d25 <- readRDS("data/pisa2025/processed/pisa_lva_2025_stu.rds")
d22 <- readRDS("data/pisa2022/processed/pisa_lva_2022_stu.rds")

sagatavo <- function(d) {
  lab <- as.character(as_factor(d$LANGN))
  d$krievu <- as.numeric(grepl("^Russian", lab))
  d$cita   <- as.numeric(!grepl("^Russian|^Latvian", lab))
  d$meitene <- as.numeric(as.numeric(d$ST004D01T) == 1)
  d$klase   <- as.numeric(d$GRADE)
  im <- as.character(as_factor(d$IMMIG))
  d$imigrants <- as.numeric(grepl("^First|^Second", im))
  d$skola <- as.character(d$CNTSCHID)
  d
}
d25 <- sagatavo(d25)
d22 <- sagatavo(d22)

cat("2025:", nrow(d25), "skolēni,", length(unique(d25$skola)), "skolas\n")
cat("2022:", nrow(d22), "skolēni,", length(unique(d22$skola)), "skolas\n\n")


# === 2. SKOLU SASTĀVS ========================================================
cat(strrep("=", 70), "\n")
cat("2. Vai skolās ir abas grupas\n")
cat(strrep("=", 70), "\n\n")

skolu_sastavs <- function(d, gads) {
  dala <- tapply(d$krievu, d$skola, mean)
  n_sk <- tapply(d$krievu, d$skola, length)
  tips <- cut(dala, breaks = c(-0.01, 0.05, 0.25, 0.75, 0.95, 1.01),
              labels = c("gandrīz tikai latviski", "pārsvarā latviski",
                         "jaukta", "pārsvarā krieviski",
                         "gandrīz tikai krieviski"))
  skolenu_dala <- tapply(n_sk, tips, sum, na.rm = TRUE)
  skolenu_dala[is.na(skolenu_dala)] <- 0
  cat(gads, "\n")
  cat(sprintf("  %-25s %4s skolas  %5.1f %% skolēnu\n",
              levels(tips), table(tips),
              100 * skolenu_dala / sum(skolenu_dala)), sep = "")
  cat(sprintf("  Jauktās skolās (5 līdz 95 %% krievu) mācās %.1f %% skolēnu\n\n",
              100 * sum(n_sk[dala > 0.05 & dala < 0.95]) / nrow(d)))
}
skolu_sastavs(d25, "2025")
skolu_sastavs(d22, "2022")


# === 3. NOVĒRTĒŠANAS FUNKCIJA ================================================
# Svērtā mazāko kvadrātu metode ar 10 PV un 80 BRR replikātsvariem.
# Skolas fiksētos efektus absorbē ar svērtu centrēšanu skolas iekšienē,
# kas ir ekvivalenti 268 fiktīvo mainīgo iekļaušanai, bet daudz ātrāk.

FAY_DALITAJS <- 20

svertais_vidējais_pa_grupam <- function(x, g, w) {
  sw  <- tapply(w, g, sum)
  swx <- tapply(w * x, g, sum)
  (swx / sw)[as.character(g)]
}

wls_koef <- function(X, y, w, mainigais, grupa = NULL) {
  if (!is.null(grupa)) {
    y <- y - svertais_vidējais_pa_grupam(y, grupa, w)
    X <- apply(X, 2, function(col) col - svertais_vidējais_pa_grupam(col, grupa, w))
    X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  }
  sw <- sqrt(w)
  fit <- .lm.fit(X * sw, y * sw)
  cf <- fit$coefficients
  names(cf) <- colnames(X)
  cf[[mainigais]]
}

novertē <- function(d, kontroles, absorb_skola = FALSE, domens = "READ",
                    mainigais = "krievu") {
  pvs <- paste0("PV", 1:10, domens)
  rw  <- grep("^W_FSTURWT", names(d), value = TRUE)
  mainigie <- c(mainigais, "cita", kontroles)

  pilni <- stats::complete.cases(d[, c(mainigie, pvs, "W_FSTUWT", "skola")])
  dd <- d[pilni, ]
  X <- stats::model.matrix(
    stats::as.formula(paste("~", paste(mainigie, collapse = " + "))), data = dd)
  grupa <- if (absorb_skola) dd$skola else NULL

  est_pv <- vapply(pvs, function(p)
    wls_koef(X, dd[[p]], dd$W_FSTUWT, mainigais, grupa), numeric(1))
  est <- mean(est_pv)
  rep_est <- vapply(rw, function(r)
    wls_koef(X, dd[[pvs[1]]], dd[[r]], mainigais, grupa), numeric(1))
  var_izl <- sum((rep_est - est_pv[1])^2) / FAY_DALITAJS
  var_imp <- (1 + 1 / length(pvs)) * stats::var(est_pv)

  list(est = est, se = sqrt(var_izl + var_imp), n = nrow(dd),
       izmesti = nrow(d) - nrow(dd))
}


# === 4. MODEĻI ===============================================================
cat(strrep("=", 70), "\n")
cat("4. Krieviski runājošo starpība lasīšanā pēc kontrolēm\n")
cat(strrep("=", 70), "\n\n")

modeli <- list(
  M0 = list(kontroles = character(0), skola = FALSE,
            apraksts = "tikai grupa"),
  M1 = list(kontroles = "ESCS", skola = FALSE,
            apraksts = "+ ESCS"),
  M2 = list(kontroles = c("ESCS", "meitene", "klase", "imigrants"),
            skola = FALSE, apraksts = "+ demogrāfija"),
  M3 = list(kontroles = c("ESCS", "meitene", "klase", "imigrants"),
            skola = TRUE, apraksts = "+ skolas fiksētie efekti")
)

rez <- list()
for (gads in c("2022", "2025")) {
  d <- if (gads == "2022") d22 else d25
  cat("--- ", gads, " ---\n", sep = "")
  for (m in names(modeli)) {
    sp <- modeli[[m]]
    r <- novertē(d, sp$kontroles, sp$skola)
    cat(sprintf("  %-3s %-28s beta = %7.1f  (SE %4.1f)  n = %d\n",
                m, sp$apraksts, r$est, r$se, r$n))
    rez[[length(rez) + 1]] <- data.frame(
      gads = gads, modelis = m, apraksts = sp$apraksts,
      beta = round(r$est, 1), se = round(r$se, 1), n = r$n,
      izmesti = r$izmesti)
  }
  cat("\n")
}

cat("Tas pats matemātikā, 2025\n\n")
for (m in names(modeli)) {
  sp <- modeli[[m]]
  r <- novertē(d25, sp$kontroles, sp$skola, domens = "MATH")
  cat(sprintf("  %-3s %-28s beta = %7.1f  (SE %4.1f)\n", m, sp$apraksts,
              r$est, r$se))
  rez[[length(rez) + 1]] <- data.frame(
    gads = "2025 MATH", modelis = m, apraksts = sp$apraksts,
    beta = round(r$est, 1), se = round(r$se, 1), n = r$n, izmesti = r$izmesti)
}
cat("\n")

tab <- do.call(rbind, rez)
write.csv(tab, "output/pisa2025/valodas_kontroles_modeli.csv",
          row.names = FALSE, fileEncoding = "UTF-8")


# === 5. INTERPRETĀCIJA =======================================================
cat(strrep("=", 70), "\n")
cat("5. Kā to lasīt\n")
cat(strrep("=", 70), "\n\n")

g25 <- tab$beta[tab$gads == "2025" & tab$modelis == "M0"]
g25_esc <- tab$beta[tab$gads == "2025" & tab$modelis == "M1"]
g25_fe <- tab$beta[tab$gads == "2025" & tab$modelis == "M3"]
g22 <- tab$beta[tab$gads == "2022" & tab$modelis == "M0"]
g22_fe <- tab$beta[tab$gads == "2022" & tab$modelis == "M3"]

cat(sprintf("2025. gadā neapstrādātā starpība ir %.1f punkti.\n", g25))
cat(sprintf("Pēc ESCS kontroles paliek %.1f, proti, %.0f %% no sākotnējās.\n",
            g25_esc, 100 * g25_esc / g25))
cat(sprintf("Skolas iekšienē paliek %.1f, proti, %.0f %%.\n\n",
            g25_fe, 100 * g25_fe / g25))
cat(sprintf("Salīdzinājumam 2022. gadā tie paši skaitļi bija %.1f un %.1f.\n\n",
            g22, g22_fe))

cat("Ja skolas fiksētie efekti starpību lielā mērā noņem, tad atšķirība ir\n")
cat("starp skolām, nevis starp skolēniem vienā klasē. Ja tie to nenoņem,\n")
cat("tad tā ir arī vienas skolas iekšienē.\n\n")
cat("Neviens no šiem modeļiem nešķir mērījuma efektu no mācīšanās efekta.\n")
cat("Tie visi mēra to pašu latviski kārtoto testu.\n\n")


# === 6. DABISKAIS SALĪDZINĀJUMS 2022. GADĀ ==================================
cat(strrep("=", 70), "\n")
cat("6. Krieviski runājošie, kas jau 2022. gadā kārtoja testu latviski\n")
cat(strrep("=", 70), "\n\n")

# 2022. gadā daļa krieviski runājošo skolēnu testu kārtoja latviešu valodā.
# Tas ir tuvākais pieejamais salīdzinājums 2025. gada situācijai. Tomēr tā ir
# pašatlasīta grupa, proti, ģimenes, kas jau bija izvēlējušās latviešu valodas plūsmu,
# tāpēc tas nav cēloņsakarības pierādījums.

d22$testa_lat <- grepl("Latvian", as.character(as_factor(d22$LANGTEST_COG)))
w <- d22$W_FSTUWT
pv22 <- rowMeans(sapply(paste0("PV", 1:10, "READ"), function(p) d22[[p]]))
pm22 <- rowMeans(sapply(paste0("PV", 1:10, "MATH"), function(p) d22[[p]]))

apak <- list(
  "Latviski mājās, tests latviski" = d22$krievu == 0 & d22$testa_lat,
  "Krieviski mājās, tests latviski" = d22$krievu == 1 & d22$testa_lat,
  "Krieviski mājās, tests krieviski" = d22$krievu == 1 & !d22$testa_lat
)
for (nos in names(apak)) {
  i <- apak[[nos]]
  cat(sprintf("  %-34s n = %4d  dala = %.3f  READ = %5.1f  MATH = %5.1f  ESCS = %5.2f\n",
              nos, sum(i), sum(w[i]) / sum(w),
              sum(pv22[i] * w[i]) / sum(w[i]),
              sum(pm22[i] * w[i]) / sum(w[i]),
              sum(d22$ESCS[i] * w[i], na.rm = TRUE) / sum(w[i & !is.na(d22$ESCS)])))
}
cat("\n")

i_lat <- apak[["Krieviski mājās, tests latviski"]]
i_kr  <- apak[["Krieviski mājās, tests krieviski"]]
starp <- sum(pv22[i_lat] * w[i_lat]) / sum(w[i_lat]) -
         sum(pv22[i_kr] * w[i_kr]) / sum(w[i_kr])
cat(sprintf("Starpība lasīšanā šo divu krieviski runājošo apakšgrupu starpā: %.1f punkti\n",
            starp))
cat("Šo starpību veido gan testa valoda, gan atšķirīga ģimeņu izvēle un\n")
cat("skolas plūsma. Šie dati tos nešķir.\n\n")


# === 7. KOPSAVILKUMS =========================================================
cat(strrep("=", 70), "\n")
cat("7. Kopsavilkums\n")
cat(strrep("=", 70), "\n\n")
print(tab, row.names = FALSE)
cat("\nIzveidotie faili:\n")
cat("  output/pisa2025/valodas_kontroles_modeli.csv\n")
cat("  ", output_file, "\n", sep = "")
cat("\nPabeigts:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")


# --- Noslēgums ---------------------------------------------------------------
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
