# ============================================================================
# 17. Igaunija un Lietuva kā salīdzinājums Latvijas lasītprasmes kritumam
# ============================================================================
#
# Jautājums. Vai krieviski runājošo skolēnu lasītprasmes kritums 2022 līdz
# 2025 ir Latvijai specifisks, vai tas pats notika arī kaimiņvalstīs.
#
# Kāpēc tieši Igaunija. Tai ir līdzīgs krieviski runājošo skolēnu īpatsvars,
# līdzīga padomju laika skolu tīkla vēsture un tāda pati PISA administrēšana,
# bet atšķirīgs valodas pārejas grafiks. 2025. gadā Igaunijā 21.8 % skolēnu
# kognitīvo testu joprojām kārtoja krievu valodā, Latvijā 0 %. Tas ļauj
# atdalīt Latvijai specifisko notikumu no reģiona kopīgā fona.
#
# Lietuva ir otrs atskaites punkts, bet tur krieviski runājošo grupa ir maza,
# tāpēc tās aplēses ir nedrošas un dotas tikai pilnīguma pēc.
#
# SVARĪGI. Šī ir starpību salīdzināšana, nevis eksperiments. Valstis atšķiras
# ne tikai valodas politikā. Sk. 6. sadaļu.
#
# Standartkļūdas. 80 BRR replikātsvari ar Fay koeficientu 0.5 plus PV
# imputācijas dispersija, tāpat kā skriptā 15.
#
# Ievade:  atarhivētie PUF .sav faili, ceļš SAV_DIR
#          data/pisa2022/raw/CY08MSP_STU_QQQ.SAV
# Izvade:  output/pisa2025/logs/17_igaunija_salidzinajums_output.txt
#          output/pisa2025/baltija_valodas_grupas.csv
#          output/pisa2025/plots/17_baltija_kritums.png
# ============================================================================

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs",  showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/17_igaunija_salidzinajums_output.txt"
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


# === 1. KONFIGURĀCIJA ========================================================
cat(strrep("=", 70), "\n")
cat("1. Konfigurācija\n")
cat(strrep("=", 70), "\n\n")

SAV_DIR <- Sys.getenv("PISA2025_SAV_DIR", unset = "")
if (!nzchar(SAV_DIR)) {
  # Kandidātu vietas, kur meklēt atarhivētos .sav failus, ja vides mainīgais
  # nav uzstādīts. Failus tur ārpus Google Drive, jo tie aizņem vairākus GB.
  kandidati <- c(
    file.path(path.expand("~"), "pisa2025_sav"),
    file.path(path.expand("~"), "Documents", "pisa2025_sav"),
    "/Volumes/PISA/pisa2025_sav",
    file.path(tempdir(), "pisa2025"))
  atrasts <- kandidati[file.exists(file.path(kandidati, "CY09_MS_STU_PUF.sav"))]
  SAV_DIR <- if (length(atrasts)) atrasts[1] else kandidati[1]
}

FAILS_2025 <- file.path(SAV_DIR, "CY09_MS_STU_PUF.sav")
FAILS_2022 <- "data/pisa2022/raw/CY08MSP_STU_QQQ.SAV"

VALSTIS <- c(LVA = "Latvija", EST = "Igaunija", LTU = "Lietuva")

# Katras valsts titulārā valoda. Krievu grupa visur ir "Russian".
TITULARA <- c(LVA = "Latvian", EST = "Estonian", LTU = "Lithuanian")

cat("Valstis:", paste(names(VALSTIS), collapse = ", "), "\n")
cat("2025 fails:", FAILS_2025, "\n")
cat("2022 fails:", FAILS_2022, "\n\n")

if (!file.exists(FAILS_2025)) {
  stop("Nav atrasts 2025. gada fails. Norādiet PISA2025_SAV_DIR.")
}


# === 2. DATU IZGŪŠANA ========================================================
cat(strrep("=", 70), "\n")
cat("2. Datu izgūšana\n")
cat(strrep("=", 70), "\n\n")

# Nolasa vienas valsts rindas. PISA failos rindas ir kārtotas pēc valsts.
izgut_valsti <- function(fails, valsts) {
  cnt <- read_sav(fails, col_select = "CNT")
  idx <- which(as.character(cnt$CNT) == valsts)
  if (length(idx) == 0) return(NULL)
  if (identical(idx, seq(min(idx), max(idx)))) {
    read_sav(fails, skip = min(idx) - 1L, n_max = length(idx))
  } else {
    d <- read_sav(fails)
    d[as.character(d$CNT) == valsts, ]
  }
}

# Kešo, lai atkārtota palaišana neprasa lasīt vairāku GB failus no jauna.
ielade <- function(valsts, gads) {
  mape <- if (gads == 2025) "data/pisa2025/processed" else "data/pisa2022/processed"
  ceļš <- file.path(mape, sprintf("pisa_%s_%d_stu.rds", tolower(valsts), gads))
  if (file.exists(ceļš)) return(readRDS(ceļš))
  cat("  Izgūstu", valsts, gads, "\n")
  d <- izgut_valsti(if (gads == 2025) FAILS_2025 else FAILS_2022, valsts)
  if (!is.null(d)) saveRDS(d, ceļš)
  d
}

dati <- list()
for (v in names(VALSTIS)) {
  for (g in c(2022, 2025)) {
    d <- ielade(v, g)
    if (is.null(d)) {
      cat("  ", v, g, "nav pieejams\n")
      next
    }
    dati[[paste(v, g)]] <- d
    cat(sprintf("  %s %d: %s skolēni, %s skolas\n", v, g,
                format(nrow(d), big.mark = " "), length(unique(d$CNTSCHID))))
  }
}
cat("\n")


# === 3. TESTA VALODA =========================================================
cat(strrep("=", 70), "\n")
cat("3. Kognitīvā testa valoda, svērtie īpatsvari\n")
cat(strrep("=", 70), "\n\n")

for (v in names(VALSTIS)) {
  cat(VALSTIS[[v]], "\n")
  for (g in c(2022, 2025)) {
    d <- dati[[paste(v, g)]]
    if (is.null(d)) next
    lab <- as.character(as_factor(d$LANGTEST_COG))
    # Igaunijā daļai skolēnu ir jaukti varianti, piemēram, dabaszinātnes
    # krieviski un LDW igauniski. Šeit skaita, vai vispār kaut kas bija krieviski.
    krievu <- grepl("Russian", lab)
    cat(sprintf("  %d: krievu valodā %.1f %%\n", g,
                100 * sum(d$W_FSTUWT[krievu]) / sum(d$W_FSTUWT)))
  }
  cat("\n")
}


# === 4. APLĒSES AR BRR UN PV DISPERSIJU ======================================
cat(strrep("=", 70), "\n")
cat("4. Lasīšana un matemātika pa mājās runātās valodas grupām\n")
cat(strrep("=", 70), "\n\n")

FAY_DALITAJS <- 20

pisa_videjais <- function(d, domens, i) {
  pvs <- paste0("PV", 1:10, domens)
  rw  <- grep("^W_FSTURWT", names(d), value = TRUE)
  w   <- d$W_FSTUWT[i]
  sv <- function(x, wt) sum(x * wt, na.rm = TRUE) / sum(wt[!is.na(x)])
  est_pv <- vapply(pvs, function(p) sv(d[[p]][i], w), numeric(1))
  est <- mean(est_pv)
  rep_est <- vapply(rw, function(r) sv(d[[pvs[1]]][i], d[[r]][i]), numeric(1))
  var_izl <- sum((rep_est - est_pv[1])^2) / FAY_DALITAJS
  var_imp <- (1 + 1 / length(pvs)) * stats::var(est_pv)
  c(est = est, se = sqrt(var_izl + var_imp), n = sum(i),
    dala = sum(w) / sum(d$W_FSTUWT))
}

grupa_no_langn <- function(d, titulara) {
  lab <- as.character(as_factor(d$LANGN))
  g <- rep("Cita vai nezināma", length(lab))
  g[grepl(paste0("^", titulara), lab)] <- "Titulārā"
  g[grepl("^Russian", lab)] <- "Krievu"
  factor(g, levels = c("Titulārā", "Krievu", "Cita vai nezināma"))
}

rez <- list()
for (v in names(VALSTIS)) {
  for (g in c(2022, 2025)) {
    d <- dati[[paste(v, g)]]
    if (is.null(d)) next
    d$grupa <- grupa_no_langn(d, TITULARA[[v]])
    for (gr in c(levels(d$grupa), "KOPĀ")) {
      i <- if (gr == "KOPĀ") rep(TRUE, nrow(d)) else d$grupa == gr
      if (sum(i) < 30) next
      for (dom in c("READ", "MATH")) {
        r <- pisa_videjais(d, dom, i)
        rez[[length(rez) + 1]] <- data.frame(
          valsts = VALSTIS[[v]], gads = g, grupa = gr, domens = dom,
          n = r[["n"]], dala = round(r[["dala"]], 3),
          videjais = round(r[["est"]], 1), se = round(r[["se"]], 1))
      }
    }
  }
}
tab <- do.call(rbind, rez)
print(tab[tab$domens == "READ", ], row.names = FALSE)
cat("\n")
write.csv(tab, "output/pisa2025/baltija_valodas_grupas.csv",
          row.names = FALSE, fileEncoding = "UTF-8")


# === 5. PĀRMAIŅAS UN STARPĪBU STARPĪBA =======================================
cat(strrep("=", 70), "\n")
cat("5. Pārmaiņas 2022 līdz 2025\n")
cat(strrep("=", 70), "\n\n")

parmaina <- function(t, valsts, grupa, domens) {
  a <- t[t$valsts == valsts & t$grupa == grupa & t$gads == 2022 & t$domens == domens, ]
  b <- t[t$valsts == valsts & t$grupa == grupa & t$gads == 2025 & t$domens == domens, ]
  if (nrow(a) == 0 || nrow(b) == 0) return(NULL)
  c(est = b$videjais - a$videjais, se = sqrt(a$se^2 + b$se^2))
}

for (dom in c("READ", "MATH")) {
  cat("--- ", dom, " ---\n", sep = "")
  for (v in VALSTIS) {
    cat(sprintf("  %-10s", v))
    for (gr in c("Titulārā", "Krievu", "KOPĀ")) {
      p <- parmaina(tab, v, gr, dom)
      if (is.null(p)) { cat(sprintf("  %-10s      ", gr)); next }
      cat(sprintf("  %s %6.1f (%.1f)", gr, p[["est"]], p[["se"]]))
    }
    cat("\n")
  }
  cat("\n")
}

cat("Starpību starpība lasīšanā, krieviski runājošie\n\n")
lva <- parmaina(tab, "Latvija", "Krievu", "READ")
for (v in c("Igaunija", "Lietuva")) {
  o <- parmaina(tab, v, "Krievu", "READ")
  if (is.null(o)) next
  d_est <- lva[["est"]] - o[["est"]]
  d_se  <- sqrt(lva[["se"]]^2 + o[["se"]]^2)
  cat(sprintf("  Latvija pret %s: %6.1f punkti (SE %.1f)%s\n", v, d_est, d_se,
              if (abs(d_est) > 1.96 * d_se) "  *" else ""))
}
cat("\n")

cat("Sniegumu starpība starp valodu grupām katrā valstī, lasīšana\n\n")
for (v in VALSTIS) {
  cat(sprintf("  %-10s", v))
  for (g in c(2022, 2025)) {
    a <- tab[tab$valsts == v & tab$grupa == "Titulārā" & tab$gads == g & tab$domens == "READ", ]
    b <- tab[tab$valsts == v & tab$grupa == "Krievu"   & tab$gads == g & tab$domens == "READ", ]
    if (nrow(a) == 0 || nrow(b) == 0) next
    cat(sprintf("  %d: %6.1f (SE %.1f)", g, b$videjais - a$videjais,
                sqrt(a$se^2 + b$se^2)))
  }
  cat("\n")
}
cat("\n")


# === 6. KO ŠIS SALĪDZINĀJUMS NEPASAKA ========================================
cat(strrep("=", 70), "\n")
cat("6. Ierobežojumi\n")
cat(strrep("=", 70), "\n\n")
cat("1. Valstis atšķiras ne tikai valodas politikā. Šis nav eksperiments.\n")
cat("2. Igaunijā daļai skolēnu testa valoda mainījās pa domēniem, tāpēc\n")
cat("   robeža starp grupām tur nav tik asa kā Latvijā.\n")
cat("3. Lietuvā krieviski runājošo grupa ir maza, tāpēc aplēse nedroša.\n")
cat("4. Grupas definē mājās runātā valoda, nevis skolas mācību valoda.\n")
cat("   Skolas valoda būtu precīzāka, bet PUF failā tās nav.\n\n")


# === 7. GRAFIKS ==============================================================
png("output/pisa2025/plots/17_baltija_kritums.png", width = 1100, height = 650,
    res = 120)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
for (gr in c("Titulārā", "Krievu")) {
  plot(NA, xlim = c(0.9, 2.4), ylim = c(360, 540), xaxt = "n",
       xlab = "", ylab = "Lasīšana, PISA punkti",
       main = paste("Mājās runā:", gr))
  axis(1, at = 1:2, labels = c("2022", "2025"))
  krasas <- c("Latvija" = "#d95f0e", "Igaunija" = "#2c7fb8",
              "Lietuva" = "#31a354")
  for (v in VALSTIS) {
    y <- c(tab$videjais[tab$valsts == v & tab$grupa == gr & tab$gads == 2022 & tab$domens == "READ"],
           tab$videjais[tab$valsts == v & tab$grupa == gr & tab$gads == 2025 & tab$domens == "READ"])
    if (length(y) < 2) next
    lines(1:2, y, col = krasas[[v]], lwd = 3)
    points(1:2, y, pch = 19, col = krasas[[v]])
    text(2.05, y[2], sprintf("%s %.0f", v, y[2] - y[1]), col = krasas[[v]],
         pos = 4, cex = 0.75, xpd = NA)
  }
}
dev.off()
cat("Saglabāts: output/pisa2025/plots/17_baltija_kritums.png\n\n")


# === 8. KOPSAVILKUMS =========================================================
cat(strrep("=", 70), "\n")
cat("8. Kopsavilkums\n")
cat(strrep("=", 70), "\n\n")
cat("Izveidotie faili:\n")
cat("  output/pisa2025/baltija_valodas_grupas.csv\n")
cat("  output/pisa2025/plots/17_baltija_kritums.png\n")
cat("  ", output_file, "\n", sep = "")
cat("\nPabeigts:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")


# --- Noslēgums ---------------------------------------------------------------
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
