# ============================================================================
# 63. Ex ante prognoze: vai PISA 2025 rezultātu varēja paredzēt (docs/60)
# ============================================================================
#
# Specifikācija fiksēta docs/60 1. sadaļā pirms aprēķina.
#
# Prognozējamie: Y1 A plūsmas vidējais, Y2 A plūsmas 9. klase, Y3 A zem 2. līmeņa.
# Brīži: O1 (2020-12, pēdējais PISA 2018), O2 (2023-12, pēdējais PISA 2022).
# Rindas: oficiālā; informētā analītiķa (2018+ + 9,6, docs/55).
# Modeļi: M1 naivais, M2 trends no 2006, M3 trends pēdējos 3 ciklos, M4 pēdējās
# pārmaiņas turpinājums, vidējais. Jutīgums: trendi no 2003.
# Intervāli: starptautiska kalibrācija uz visu valstu (bez LVA) lasīšanas vidējiem,
# tie paši modeļi un horizonts; jutīgums bez 2022. gada mērķiem.
#
# Izvade:  output/pisa2025/ex_ante_prognozes.csv, ex_ante_kalibracija.csv,
#          ex_ante_2025_valstis.csv
#          output/pisa2025/logs/63_ex_ante_prognoze_output.txt
# ============================================================================

source("R/pisa2025/00_ab_dati.R")

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/63_ex_ante_prognoze_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

NOBIDE <- 9.6
L2 <- 407.47
CIKLI <- c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025)
hdr <- function(t) cat("\n", strrep("=", 100), "\n", t, "\n", strrep("=", 100), "\n\n", sep = "")

# ---------------------------------------------------------------------------
# Rindas
# ---------------------------------------------------------------------------
VER <- utils::read.csv("output/pisa2025/verifikacija.csv", stringsAsFactors = FALSE)
v <- unique(VER[VER$grupa == "A", c("gads", "aplese")])
Y1 <- stats::setNames(v$aplese, v$gads)
D <- ab_dati()
pvvid <- function(d, i, f) mean(vapply(d$pv, function(p) f(p[i], d$sk$w[i]), 1))
Y2 <- vapply(names(D), function(g) { d <- D[[g]]; i <- d$sk$grupa %in% "A" & d$sk$klase %in% 9
  pvvid(d, i, function(x, w) sum(x * w) / sum(w)) }, 1)
Y3 <- vapply(names(D), function(g) { d <- D[[g]]; i <- d$sk$grupa %in% "A"
  pvvid(d, i, function(x, w) 100 * sum(w * (x < L2)) / sum(w)) }, 1)
RINDAS <- list(Y1 = Y1, Y2 = Y2, Y3 = Y3)
hdr("1. Rindas (latviešu valodas plūsma)")
print(round(do.call(rbind, RINDAS), 1))
cat("\n  Y1 vidējais, Y2 9. klase, Y3 % zem 2. līmeņa. Y2 salīdzinājumam ar docs/52: 505,1 (2012), 484,3 (2022), 465,2 (2025).\n")

# ---------------------------------------------------------------------------
# Modeļi: katrs saņem gadus t un vērtības y (tikai līdz prognozes brīdim) un mērķa gadu
# ---------------------------------------------------------------------------
MOD <- list(
  M1 = function(t, y, T) y[length(y)],
  M2 = function(t, y, T) { k <- t >= 2006; if (sum(k) < 3) return(NA_real_)
         co <- stats::coef(stats::lm(y[k] ~ t[k])); co[1] + co[2] * T },
  M3 = function(t, y, T) { n <- length(y); if (n < 3) return(NA_real_); k <- (n - 2):n
         co <- stats::coef(stats::lm(y[k] ~ t[k])); co[1] + co[2] * T },
  M4 = function(t, y, T) { n <- length(y); if (n < 2) return(NA_real_); y[n] + (y[n] - y[n - 1]) / (t[n] - t[n - 1]) * (T - t[n]) })
MOD_2003 <- list(M2_no2003 = function(t, y, T) { k <- t >= 2003; if (sum(k) < 3) return(NA_real_)
  co <- stats::coef(stats::lm(y[k] ~ t[k])); co[1] + co[2] * T })
prognoze <- function(ser, pedejais, T = 2025, mods = MOD) {
  t <- as.numeric(names(ser)); k <- t <= pedejais & !is.na(ser)
  vapply(mods, function(f) unname(f(t[k], unname(ser[k]), T)), 1)
}

# ---------------------------------------------------------------------------
# Starptautiskā kalibrācija
# ---------------------------------------------------------------------------
hdr("2. Starptautiskā kalibrācija: prognozes kļūdas visām valstīm (bez Latvijas)")
PV <- utils::read.csv("output/pisa2025/pisa_valstu_videjie.csv", stringsAsFactors = FALSE)
PV <- PV[PV$cnt != "LVA" & !is.na(PV$lasisana), ]
valstis <- split(PV, PV$cnt)
kal <- list()
for (x in valstis) {
  ser <- stats::setNames(x$lasisana, x$gads); t <- sort(as.numeric(names(ser))); ser <- ser[as.character(t)]
  for (mer in c(2015, 2018, 2022, 2025)) {
    ic <- which(CIKLI == mer)
    for (sol in 1:2) {
      orig <- CIKLI[ic - sol]; if (!(mer %in% t) || !(orig %in% t)) next
      pr <- c(prognoze(ser, orig, mer), prognoze(ser, orig, mer, MOD_2003))
      kal[[length(kal) + 1]] <- data.frame(cnt = x$cnt[1], merkis = mer, soli = sol, modelis = names(pr),
        prognoze = pr, faktiskais = ser[[as.character(mer)]], stringsAsFactors = FALSE)
    }
  }
}
kal <- do.call(rbind, kal); kal$kluda <- kal$faktiskais - kal$prognoze
kal <- kal[!is.na(kal$kluda), ]
vid <- stats::aggregate(prognoze ~ cnt + merkis + soli, kal[kal$modelis %in% names(MOD), ], mean)
vid$modelis <- "Vid."; vid$faktiskais <- kal$faktiskais[match(paste(vid$cnt, vid$merkis), paste(kal$cnt, kal$merkis))]
vid$kluda <- vid$faktiskais - vid$prognoze
kal <- rbind(kal, vid[, names(kal)])
utils::write.csv(kal, "output/pisa2025/ex_ante_kalibracija.csv", row.names = FALSE)
KAL <- kal[kal$merkis <= 2022, ]                                 # ex ante zināmais
ks <- do.call(rbind, lapply(split(KAL, list(KAL$modelis, KAL$soli), drop = TRUE), function(z) data.frame(
  modelis = z$modelis[1], soli = z$soli[1], n = nrow(z), vid_kluda = mean(z$kluda), sd = stats::sd(z$kluda),
  q025 = stats::quantile(z$kluda, .025), q05 = stats::quantile(z$kluda, .05), q95 = stats::quantile(z$kluda, .95),
  q975 = stats::quantile(z$kluda, .975),
  sd_bez2022 = stats::sd(z$kluda[z$merkis < 2022]), q05_bez2022 = stats::quantile(z$kluda[z$merkis < 2022], .05))))
print(format(ks, digits = 3), row.names = FALSE)
cat("\n  soli 1: viena cikla prognoze (mērķi 2015, 2018, 2022); soli 2: divu ciklu (2018, 2022).\n",
    " Kļūda = faktiskais − prognoze; negatīvs = rezultāts zemāks par prognozi.\n")

# ---------------------------------------------------------------------------
# Latvijas prognozes
# ---------------------------------------------------------------------------
hdr("3. Latvijas A plūsmas prognozes 2025. gadam")
BRIZI <- list(O1 = list(ped = 2018, soli = 2, nos = "2020-12 (pēdējais PISA 2018)"),
              O2 = list(ped = 2022, soli = 1, nos = "2023-12 (pēdējais PISA 2022)"))
rez <- list()
for (Y in names(RINDAS)) for (b in names(BRIZI)) for (var in c("oficiālā", "informētā")) {
  ser <- RINDAS[[Y]]
  if (var == "informētā") { if (Y == "Y3") next; t <- as.numeric(names(ser)); ser[t >= 2018] <- ser[t >= 2018] + NOBIDE }
  B <- BRIZI[[b]]
  pr <- c(prognoze(ser, B$ped), prognoze(ser, B$ped, mods = MOD_2003))
  pr <- c(pr[names(MOD)], Vid. = mean(pr[names(MOD)]), pr["M2_no2003"])
  if (var == "informētā") pr <- pr - NOBIDE
  fakt <- RINDAS[[Y]][["2025"]]
  for (m in names(pr)) {
    z <- KAL$kluda[KAL$modelis == m & KAL$soli == B$soli]
    e <- fakt - pr[[m]]
    rez[[length(rez) + 1]] <- data.frame(lielums = Y, brizis = b, rinda = var, modelis = m, prognoze = pr[[m]],
      pi90_no = if (Y != "Y3") pr[[m]] + stats::quantile(z, .05) else NA, pi90_lidz = if (Y != "Y3") pr[[m]] + stats::quantile(z, .95) else NA,
      pi95_no = if (Y != "Y3") pr[[m]] + stats::quantile(z, .025) else NA, faktiskais = fakt, kluda = e,
      procentile = if (Y != "Y3") 100 * mean(z <= e) else NA, stringsAsFactors = FALSE)
  }
}
rez <- do.call(rbind, rez)
utils::write.csv(rez, "output/pisa2025/ex_ante_prognozes.csv", row.names = FALSE)
for (Y in names(RINDAS)) {
  cat(sprintf("\n  %s (faktiskais 2025: %.1f)\n", c(Y1 = "Y1 A vidējais", Y2 = "Y2 A 9. klase", Y3 = "Y3 A % zem 2. līmeņa")[[Y]], RINDAS[[Y]][["2025"]]))
  x <- rez[rez$lielums == Y, ]
  for (i in seq_len(nrow(x))) cat(sprintf("    %-3s %-10s %-10s prognoze %6.1f | 90%% PI %6.1f … %6.1f | 95%% no %6.1f | kļūda %+6.1f | procentile %5.1f\n",
    x$brizis[i], x$rinda[i], x$modelis[i], x$prognoze[i], x$pi90_no[i], x$pi90_lidz[i], x$pi95_no[i], x$kluda[i], x$procentile[i]))
}
cat("\n  procentile: kurā starptautiskā kļūdu sadalījuma procentilē ir Latvijas faktiskā kļūda (zema = retāk sastopami\n",
    " sliktāks rezultāts par prognozi). PI no starptautiskās kalibrācijas (mērķi ≤ 2022).\n")

# ---------------------------------------------------------------------------
hdr("4. Ex post konteksts: visu valstu 2025. gada kļūdas no 2022. gada prognozes (visa valsts)")
# ---------------------------------------------------------------------------
PVa <- utils::read.csv("output/pisa2025/pisa_valstu_videjie.csv", stringsAsFactors = FALSE)
lv <- PVa[PVa$cnt == "LVA", ]; serL <- stats::setNames(lv$lasisana, lv$gads)
prL <- prognoze(serL, 2022); prL <- c(prL, Vid. = mean(prL))
e25 <- kal[kal$merkis == 2025 & kal$soli == 1, ]
for (m in c(names(MOD), "Vid.")) {
  z <- e25$kluda[e25$modelis == m]; eL <- serL[["2025"]] - prL[[m]]
  cat(sprintf("  %-4s valstis %3d | 2025 kļūdas mediāna %+6.1f, 10.–90. procentile %+6.1f … %+6.1f | Latvija (visa) %+6.1f, zemāk par to %d valstis (%.0f %%)\n",
      m, length(z), stats::median(z), stats::quantile(z, .1), stats::quantile(z, .9), eL, sum(z < eL), 100 * mean(z < eL)))
}
x <- e25[e25$modelis == "M1", c("cnt", "prognoze", "faktiskais", "kluda")]; x <- x[order(x$kluda), ]
cat("\n  Lielākie kritumi pret 2022. gadu (M1), 15 valstis:\n"); print(head(format(x, digits = 4), 15), row.names = FALSE)
cat(sprintf("\n  Latvija (visa): 2022 %.1f → 2025 %.1f (%+.1f); latviešu valodas plūsma %+.1f\n",
            serL[["2022"]], serL[["2025"]], serL[["2025"]] - serL[["2022"]], Y1[["2025"]] - Y1[["2022"]]))
utils::write.csv(e25, "output/pisa2025/ex_ante_2025_valstis.csv", row.names = FALSE)

cat("\nIzveidotie faili:\n  output/pisa2025/ex_ante_prognozes.csv\n  output/pisa2025/ex_ante_kalibracija.csv\n",
    " output/pisa2025/ex_ante_2025_valstis.csv\n ", output_file, "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
