# ============================================================================
# 15. Lasītprasmes krituma sadalījums pēc mājās runātās valodas, 2022 pret 2025
# ============================================================================
#
# Jautājums. Cik liela daļa no Latvijas lasītprasmes krituma 2022 līdz 2025
# (kopumā 44.9 punkti) nāk no skolēniem, kuri mājās runā krieviski, un cik
# no pārējiem.
#
# Iemesls šim jautājumam. Inventarizācija parādīja, ka 2022. gadā 27.7 % no
# svērtās Latvijas izlases kognitīvo testu pildīja krievu valodā, bet
# 2025. gadā to darīja 0 %. Visi 2025. gadā testēti latviešu valodā. Tas
# sakrīt ar pāreju uz mācībām valsts valodā. Tātad daļai skolēnu mainījās
# ne tikai mācību valoda, bet arī testa valoda.
#
# SVARĪGI. Šis skripts ir aprakstošs. Tas sadala kritumu pa grupām, nevis
# pierāda cēloni. Testa valodas maiņa un mācību valodas maiņa notika
# vienlaikus, tāpēc šie dati vieni paši tos nešķir. Sk. 6. sadaļu.
#
# Standartkļūdas. PISA metode, proti, 80 BRR replikātsvari ar Fay koeficientu
# 0.5 izlases dispersijai un 10 plausible values imputācijas dispersijai.
#
# Ievade:  data/pisa2025/processed/pisa_lva_2025_stu.rds  (skripts 14)
#          data/pisa2022/raw/CY08MSP_STU_QQQ.SAV
# Izvade:  output/pisa2025/logs/15_valodas_dekompozicija_output.txt
#          output/pisa2025/valodas_grupas_2022_2025.csv
#          output/pisa2025/plots/15_valodas_kritums.png
# ============================================================================

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs",  showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/15_valodas_dekompozicija_output.txt"
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
cat("2025:", nrow(d25), "skolēni,", length(unique(d25$CNTSCHID)), "skolas\n")

# 2022. gada Latvijas dati. Ja kešots fails nav, izgūst no pilnā SPSS faila.
kess_2022 <- "data/pisa2022/processed/pisa_lva_2022_stu.rds"
if (file.exists(kess_2022)) {
  d22 <- readRDS(kess_2022)
} else {
  f22 <- "data/pisa2022/raw/CY08MSP_STU_QQQ.SAV"
  cnt <- read_sav(f22, col_select = "CNT")
  idx <- which(as.character(cnt$CNT) == "LVA")
  d22 <- read_sav(f22, skip = min(idx) - 1L, n_max = length(idx))
  saveRDS(d22, kess_2022)
  cat("Izveidots kešs:", kess_2022, "\n")
}
cat("2022:", nrow(d22), "skolēni,", length(unique(d22$CNTSCHID)), "skolas\n\n")


# === 2. TESTA VALODA =========================================================
cat(strrep("=", 70), "\n")
cat("2. Kognitīvā testa valoda\n")
cat(strrep("=", 70), "\n\n")

testa_valoda <- function(d, gads) {
  t <- xtabs(d$W_FSTUWT ~ droplevels(as_factor(d$LANGTEST_COG)))
  cat(gads, "\n"); print(round(prop.table(t), 3)); cat("\n")
}
testa_valoda(d22, "2022")
testa_valoda(d25, "2025")


# === 3. GRUPAS PĒC MĀJĀS RUNĀTĀS VALODAS =====================================
cat(strrep("=", 70), "\n")
cat("3. Grupas pēc mājās runātās valodas (LANGN)\n")
cat(strrep("=", 70), "\n\n")

# LANGN etiķetes starp cikliem var atšķirties, tāpēc grupē pēc etiķetes sākuma,
# nevis pēc skaitliskā koda.
grupa_no_langn <- function(d) {
  lab <- as.character(as_factor(d$LANGN))
  g <- rep("Cita vai nezināma", length(lab))
  g[grepl("^Latvian", lab)] <- "Latviešu"
  g[grepl("^Russian", lab)] <- "Krievu"
  factor(g, levels = c("Latviešu", "Krievu", "Cita vai nezināma"))
}
d22$grupa <- grupa_no_langn(d22)
d25$grupa <- grupa_no_langn(d25)

for (gads in c("2022", "2025")) {
  d <- if (gads == "2022") d22 else d25
  cat(gads, "LANGN etiķetes:\n")
  print(sort(table(as.character(as_factor(d$LANGN))), decreasing = TRUE))
  cat("\n")
}


# === 4. PISA APLĒSES AR BRR UN PV DISPERSIJU =================================
cat(strrep("=", 70), "\n")
cat("4. Vidējie ar korektām standartkļūdām\n")
cat(strrep("=", 70), "\n\n")

# Fay koeficients PISA replikātsvariem ir 0.5, tāpēc izlases dispersija ir
# summa no kvadrātiem, dalīta ar 80 * (1 - 0.5)^2 = 20.
FAY_DALITAJS <- 20

pisa_videjais <- function(d, domens = "READ", i = rep(TRUE, nrow(d))) {
  pvs <- paste0("PV", 1:10, domens)
  rw  <- grep("^W_FSTURWT", names(d), value = TRUE)
  w   <- d$W_FSTUWT[i]

  sv <- function(x, wt) sum(x * wt, na.rm = TRUE) / sum(wt[!is.na(x)])

  # Punkta aplēse, proti, 10 PV vidējais.
  est_pv <- vapply(pvs, function(p) sv(d[[p]][i], w), numeric(1))
  est <- mean(est_pv)

  # Izlases dispersija, aprēķināta uz PV1, kā PISA rokasgrāmatā.
  rep_est <- vapply(rw, function(r) sv(d[[pvs[1]]][i], d[[r]][i]), numeric(1))
  var_izl <- sum((rep_est - est_pv[1])^2) / FAY_DALITAJS

  # Imputācijas dispersija starp 10 PV.
  var_imp <- (1 + 1 / length(pvs)) * stats::var(est_pv)

  c(est = est, se = sqrt(var_izl + var_imp),
    n = sum(i), dala = sum(w) / sum(d$W_FSTUWT))
}

rezultati <- list()
for (gads in c("2022", "2025")) {
  d <- if (gads == "2022") d22 else d25
  for (g in levels(d$grupa)) {
    i <- d$grupa == g
    if (sum(i) < 30) next
    for (dom in c("READ", "MATH")) {
      r <- pisa_videjais(d, dom, i)
      rezultati[[length(rezultati) + 1]] <- data.frame(
        gads = gads, grupa = g, domens = dom,
        n = r[["n"]], dala = round(r[["dala"]], 3),
        videjais = round(r[["est"]], 1), se = round(r[["se"]], 1))
    }
  }
  for (dom in c("READ", "MATH")) {
    r <- pisa_videjais(d, dom)
    rezultati[[length(rezultati) + 1]] <- data.frame(
      gads = gads, grupa = "KOPĀ", domens = dom,
      n = r[["n"]], dala = 1, videjais = round(r[["est"]], 1),
      se = round(r[["se"]], 1))
  }
}
tab <- do.call(rbind, rezultati)
print(tab[order(tab$domens, tab$grupa, tab$gads), ], row.names = FALSE)
cat("\n")

write.csv(tab, "output/pisa2025/valodas_grupas_2022_2025.csv",
          row.names = FALSE, fileEncoding = "UTF-8")


# === 5. PĀRMAIŅAS UN TO SADALĪJUMS ===========================================
cat(strrep("=", 70), "\n")
cat("5. Pārmaiņas 2022 līdz 2025\n")
cat(strrep("=", 70), "\n\n")

for (dom in c("READ", "MATH")) {
  cat("--- ", dom, " ---\n", sep = "")
  t <- tab[tab$domens == dom, ]
  for (g in unique(t$grupa)) {
    a <- t[t$grupa == g & t$gads == "2022", ]
    b <- t[t$grupa == g & t$gads == "2025", ]
    if (nrow(a) == 0 || nrow(b) == 0) next
    d_est <- b$videjais - a$videjais
    d_se  <- sqrt(a$se^2 + b$se^2)
    cat(sprintf("  %-18s %6.1f -> %6.1f   starpība %6.1f (SE %.1f)%s\n",
                g, a$videjais, b$videjais, d_est, d_se,
                if (abs(d_est) > 1.96 * d_se) "  *" else ""))
  }
  cat("\n")
}

# Shift share sadalījums, proti, cik no kopējās pārmaiņas rada pārmaiņas
# grupu iekšienē un cik grupu īpatsvaru maiņa.
cat("Shift share sadalījums lasīšanai\n\n")
t <- tab[tab$domens == "READ" & tab$grupa != "KOPĀ", ]
grupas <- unique(t$grupa)
iekseja <- 0; sastava <- 0
for (g in grupas) {
  a <- t[t$grupa == g & t$gads == "2022", ]
  b <- t[t$grupa == g & t$gads == "2025", ]
  if (nrow(a) == 0 || nrow(b) == 0) next
  vid_dala  <- (a$dala + b$dala) / 2
  vid_limen <- (a$videjais + b$videjais) / 2
  ie <- vid_dala * (b$videjais - a$videjais)
  sa <- (b$dala - a$dala) * vid_limen
  iekseja <- iekseja + ie
  sastava <- sastava + sa
  cat(sprintf("  %-18s iekšējā %6.1f   sastāva %6.1f\n", g, ie, sa))
}
kopa_22 <- tab$videjais[tab$domens == "READ" & tab$grupa == "KOPĀ" & tab$gads == "2022"]
kopa_25 <- tab$videjais[tab$domens == "READ" & tab$grupa == "KOPĀ" & tab$gads == "2025"]
cat(sprintf("\n  Kopā iekšējā %.1f, sastāva %.1f, summa %.1f\n",
            iekseja, sastava, iekseja + sastava))
cat(sprintf("  Faktiskā pārmaiņa %.1f\n\n", kopa_25 - kopa_22))

# Cik no kopējā krituma nāk no katras grupas.
cat("Ieguldījums kopējā lasīšanas kritumā\n\n")
for (g in grupas) {
  a <- t[t$grupa == g & t$gads == "2022", ]
  b <- t[t$grupa == g & t$gads == "2025", ]
  if (nrow(a) == 0 || nrow(b) == 0) next
  iegul <- b$dala * b$videjais - a$dala * a$videjais
  cat(sprintf("  %-18s %6.1f punkti no %.1f, proti, %.0f %%\n",
              g, iegul, kopa_25 - kopa_22,
              100 * iegul / (kopa_25 - kopa_22)))
}
cat("\n")


# === 6. KONKURĒJOŠIE SKAIDROJUMI, KO ŠIE DATI NEŠĶIR =========================
cat(strrep("=", 70), "\n")
cat("6. Ko šie skaitļi nepasaka\n")
cat(strrep("=", 70), "\n\n")
cat("1. Testa valodas maiņa un mācību valodas maiņa notika vienlaikus.\n")
cat("   Šie dati nešķir mērījuma efektu no mācīšanās efekta.\n")
cat("2. Grupas nav identiskas kohortas. Mainījies gan skolu tīkls, gan\n")
cat("   iespējams izlases pārklājums. Sk. 7. sadaļu.\n")
cat("3. Iespējams, ka daļa krituma šajā grupā ir neiesaiste, nevis prasme.\n")
cat("   To daļēji pārbauda 7. sadaļa un pilnībā tikai uzdevumu līmeņa laiki.\n\n")


# === 7. PIEPŪLE UN IESAISTE PA GRUPĀM ========================================
cat(strrep("=", 70), "\n")
cat("7. Pašvērtētā piepūle testā pa grupām, 2025\n")
cat(strrep("=", 70), "\n\n")

piepule <- intersect(c("EFFORT1", "EFFORT2", "ST331Q01JA", "ST331Q03JA"),
                     names(d25))
if (length(piepule) > 0) {
  for (m in piepule) {
    cat(sprintf("  %-11s ", m))
    for (g in levels(d25$grupa)) {
      i <- d25$grupa == g
      x <- as.numeric(d25[[m]][i]); w <- d25$W_FSTUWT[i]
      cat(sprintf("%s M=%.2f  ", g, sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)])))
    }
    cat("\n")
  }
} else {
  cat("  Piepūles mainīgie šajā failā nav atrasti.\n")
}
cat("\n")

cat("Sadalījuma apakšgals, lasīšanas P10 un P90 pa grupām\n\n")
kvantiles <- function(d, g) {
  i <- d$grupa == g
  pv <- rowMeans(sapply(paste0("PV", 1:10, "READ"), function(p) d[[p]][i]))
  w <- d$W_FSTUWT[i]
  o <- order(pv); pv <- pv[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  c(P10 = pv[which.max(cw >= 0.10)], P50 = pv[which.max(cw >= 0.50)],
    P90 = pv[which.max(cw >= 0.90)])
}
for (g in levels(d25$grupa)) {
  a <- kvantiles(d22, g); b <- kvantiles(d25, g)
  cat(sprintf("  %-18s 2022 P10=%5.0f P50=%5.0f P90=%5.0f | 2025 P10=%5.0f P50=%5.0f P90=%5.0f\n",
              g, a[1], a[2], a[3], b[1], b[2], b[3]))
}
cat("\n")


# === 8. GRAFIKS ==============================================================
png("output/pisa2025/plots/15_valodas_kritums.png", width = 1000, height = 700,
    res = 120)
t <- tab[tab$domens == "READ" & tab$grupa != "Cita vai nezināma", ]
grupas_g <- unique(t$grupa)
krasas <- c("Latviešu" = "#2c7fb8", "Krievu" = "#d95f0e", "KOPĀ" = "#636363")
plot(NA, xlim = c(0.8, 2.2), ylim = c(350, 500), xaxt = "n",
     xlab = "", ylab = "Lasīšana, PISA punkti",
     main = "Lasītprasme Latvijā pēc mājās runātās valodas")
axis(1, at = 1:2, labels = c("2022", "2025"))
for (g in grupas_g) {
  y <- c(t$videjais[t$grupa == g & t$gads == "2022"],
         t$videjais[t$grupa == g & t$gads == "2025"])
  se <- c(t$se[t$grupa == g & t$gads == "2022"],
          t$se[t$grupa == g & t$gads == "2025"])
  lines(1:2, y, col = krasas[[g]], lwd = 3)
  arrows(1:2, y - 1.96 * se, 1:2, y + 1.96 * se, angle = 90, code = 3,
         length = 0.05, col = krasas[[g]])
  points(1:2, y, pch = 19, col = krasas[[g]])
  text(2.05, y[2], sprintf("%s (%.0f)", g, y[2] - y[1]), col = krasas[[g]],
       pos = 4, cex = 0.8, xpd = NA)
}
dev.off()
cat("Saglabāts: output/pisa2025/plots/15_valodas_kritums.png\n\n")


# === 9. KOPSAVILKUMS =========================================================
cat(strrep("=", 70), "\n")
cat("9. Kopsavilkums\n")
cat(strrep("=", 70), "\n\n")
cat("Izveidotie faili:\n")
cat("  output/pisa2025/valodas_grupas_2022_2025.csv\n")
cat("  output/pisa2025/plots/15_valodas_kritums.png\n")
cat("  ", output_file, "\n", sep = "")
cat("\nPabeigts:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")


# --- Noslēgums ---------------------------------------------------------------
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
