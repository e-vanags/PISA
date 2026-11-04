# ============================================================================
# 58b. Citi mērījumi: Latvijas relatīvais stāvoklis IEA pētījumos un kohortu
#      ekstrapolācija uz PISA (docs/57, F grupa)
# ============================================================================
#
# DATI. Visu valstu skolēnu faili: PIRLS 2001, 2006, 2016, 2021 (lasīšana, 4. kl.),
# TIMSS 2003, 2007, 2019, 2023 (matemātika, 4. kl.), TIMSS 1999, 2003 (matemātika,
# 8. kl.), ICCS 2009, 2016, 2022 (pilsoniskās zināšanas, 8. kl.; tests lielā mērā
# balstīts lasīšanā). Valsts vidējais: 5 PV, svars TOTWGT (ICCS: TOTWGTS).
# PISA valstu vidējie: output/pisa2025/pisa_valstu_videjie.csv (58a).
#
# LATVIEŠU VALODAS PLŪSMA IEA DATOS: testa valoda latviešu (ITLANG kods 1, kā 38.
# skriptā). TIMSS 1999 un 2007 Latvija testēja tikai latviešu mācībvalodas skolas.
# ICCS 2009 failos testa valodas nav, tur ir tikai visa Latvija. Atšķirība no PISA:
# PISA plūsma ir pēc mājās lietotās valodas.
#
# ANALĪZES
#   (i)  Latvijas relatīvais stāvoklis katrā IEA rindā pret valstu kopu, kas
#        piedalījās visos rindas ciklos: starpība no kopas vidējā punktos un z.
#   (ii) Kohortu pāri: tā pati paaudze IEA pētījumā un vēlāk PISA. Latvijas z pret
#        abos piedalījušos valstu kopu; Δz = z(PISA) − z(IEA) ir relatīvā izaugsme.
#        Negatīvs Δz nozīmē, ka paaudze starp agrāko mērījumu un 15 gadiem atpalika
#        no citām valstīm.
#
# Punktu aplēses bez SE; valstu kopas vidējais un izkliede ir atskaites līmenis.
#
# Izvade:  output/pisa2025/iea_valstu_videjie.csv
#          output/pisa2025/iea_rindas.csv, iea_kohortas.csv
#          output/pisa2025/logs/58b_iea_kohortas_output.txt
# ============================================================================

suppressPackageStartupMessages(library(haven))

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/58b_iea_kohortas_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

pvs <- function(p) paste0(p, 1:5)
PET <- list(
  list(r = "PIRLS 4", g = "2001", dir = "data/iea/pirls2001", pre = "asg", suf = "r1", pv = pvs("ASRREA0"), w = "TOTWGT"),
  list(r = "PIRLS 4", g = "2006", dir = "data/iea/pirls2006", pre = "asg", suf = "r2", pv = pvs("ASRREA0"), w = "TOTWGT"),
  list(r = "PIRLS 4", g = "2016", dir = "data/iea/pirls2016", pre = "asg", suf = "r4", pv = pvs("ASRREA0"), w = "TOTWGT"),
  list(r = "PIRLS 4", g = "2021", dir = "data/iea/pirls2021", pre = "asg", suf = "r5", pv = pvs("ASRREA0"), w = "TOTWGT"),
  list(r = "TIMSS 4", g = "2003", dir = "data/iea/timss2003/g4", pre = "asg", suf = "m3", pv = pvs("ASMMAT0"), w = "TOTWGT"),
  list(r = "TIMSS 4", g = "2007", dir = "data/iea/timss2007/g4", pre = "asg", suf = "m4", pv = pvs("ASMMAT0"), w = "TOTWGT"),
  list(r = "TIMSS 4", g = "2019", dir = "data/iea/timss2019/g4", pre = "asg", suf = "m7", pv = pvs("ASMMAT0"), w = "TOTWGT"),
  list(r = "TIMSS 4", g = "2023", dir = "data/iea/timss2023/g4", pre = "asg", suf = "m8", pv = pvs("ASMMAT0"), w = "TOTWGT"),
  list(r = "TIMSS 8", g = "1999", dir = "data/iea/timss1999/g8", pre = "bsg", suf = "m2", pv = pvs("BSMMAT0"), w = "TOTWGT"),
  list(r = "TIMSS 8", g = "2003", dir = "data/iea/timss2003/g8", pre = "bsg", suf = "m3", pv = pvs("BSMMAT0"), w = "TOTWGT"),
  list(r = "ICCS 8", g = "2009", dir = "data/iea/iccs2009", pre = "isg", suf = "c2", pv = paste0("PV", 1:5, "CIV"), w = "TOTWGTS"),
  list(r = "ICCS 8", g = "2016", dir = "data/iea/iccs2016", pre = "isg", suf = "c3", pv = paste0("PV", 1:5, "CIV"), w = "TOTWGTS"),
  list(r = "ICCS 8", g = "2022", dir = "data/iea/iccs2022", pre = "isg", suf = "c4", pv = paste0("PV", 1:5, "CIV"), w = "TOTWGTS"))
KODI <- c(ROM = "ROU", TWN = "TAP")   # IEA kods -> PISA kods, kur tie atšķiras

wm <- function(x, w) { i <- !is.na(x) & !is.na(w); sum(x[i] * w[i]) / sum(w[i]) }
iea <- list()
for (P in PET) {
  f <- list.files(P$dir, recursive = TRUE, full.names = TRUE)
  f <- f[grepl(paste0("^", P$pre, "[a-z0-9]{3}", P$suf, "\\.sav$"), basename(f), ignore.case = TRUE)]
  cat(sprintf("%s %s: %d valstu faili\n", P$r, P$g, length(f)))
  for (ff in f) {
    kods <- toupper(substr(basename(ff), 4, 6)); kods <- if (kods %in% names(KODI)) KODI[[kods]] else kods
    n <- names(read_sav(ff, n_max = 0))
    v <- intersect(c(P$pv, P$w, "ITLANG_SA", "ITLANG", "ITLANG_SQ"), n)
    if (!all(c(P$pv, P$w) %in% v)) next
    d <- read_sav(ff, col_select = tidyselect::all_of(v))
    w <- as.numeric(d[[P$w]])
    vid <- mean(vapply(P$pv, function(p) wm(as.numeric(d[[p]]), w), 1))
    vA <- NA_real_; piez <- ""
    if (kods == "LVA") {
      # Testa valoda: ITLANG_SA (no 2019./2021. gada), citādi ITLANG. Latviešu kods
      # pēc etiķetes ("Latvian", no 2016. gada kods 21); vecajos failos etiķete ir
      # "<TEST LANGUAGE>" un latviešu ir kods 1.
      lf <- intersect(c("ITLANG_SA", "ITLANG", "ITLANG_SQ"), v)[1]
      if (!is.na(lf)) {
        il <- as.numeric(d[[lf]]); lab <- attr(d[[lf]], "labels")
        lk <- if (length(lab) && any(grepl("latvi", names(lab), ignore.case = TRUE)))
          unname(lab[grepl("latvi", names(lab), ignore.case = TRUE)][1]) else 1
        a <- il %in% lk
        piez <- sprintf("%s: %s | latviešu kods %s", lf,
                        paste(names(table(il)), table(il), sep = ":", collapse = " "), lk)
        vA <- mean(vapply(P$pv, function(p) wm(as.numeric(d[[p]])[a], w[a]), 1))
      } else piez <- "testa valodas nav"
      cat(sprintf("   Latvija: visi %.1f, latviešu testa valoda %.1f; %s\n", vid, vA, piez))
    }
    iea[[length(iea) + 1]] <- data.frame(rinda = P$r, gads = P$g, cnt = kods, vid = vid, vid_A = vA, n = nrow(d))
  }
}
iea <- do.call(rbind, iea)
utils::write.csv(iea, "output/pisa2025/iea_valstu_videjie.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 90), "\n(i) Latvijas relatīvais stāvoklis katrā IEA rindā\n", strrep("=", 90), "\n\n")
cat("  Kopa: valstis, kas piedalījās visos rindas ciklos, kuros piedalījās Latvija (bez Latvijas).\n",
    " Starpība = Latvija − kopas vidējais, punktos; z = starpība / kopas SD.\n\n")
rin <- list()
for (r in unique(iea$rinda)) {
  x <- iea[iea$rinda == r, ]; gadi <- sort(unique(x$gads[x$cnt == "LVA"]))
  kopa <- Reduce(intersect, lapply(gadi, function(g) x$cnt[x$gads == g])); kopa <- setdiff(kopa, "LVA")
  for (g in gadi) {
    y <- x[x$gads == g, ]; k <- y$vid[y$cnt %in% kopa]; lv <- y[y$cnt == "LVA", ]
    rin[[length(rin) + 1]] <- data.frame(rinda = r, gads = g, kopa_n = length(kopa), kopa_vid = mean(k), kopa_sd = stats::sd(k),
      lv = lv$vid, lv_A = lv$vid_A, starp = lv$vid - mean(k), starp_A = lv$vid_A - mean(k),
      z = (lv$vid - mean(k)) / stats::sd(k), z_A = (lv$vid_A - mean(k)) / stats::sd(k))
  }
  cat(sprintf("  %s, kopa %d valstis: %s\n", r, length(kopa), paste(sort(kopa), collapse = " ")))
}
rin <- do.call(rbind, rin)
print(format(rin, digits = 3), row.names = FALSE)
utils::write.csv(rin, "output/pisa2025/iea_rindas.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 90), "\n(ii) Kohortu pāri: tā pati paaudze IEA pētījumā un PISA\n", strrep("=", 90), "\n\n")
PV <- utils::read.csv("output/pisa2025/pisa_valstu_videjie.csv", stringsAsFactors = FALSE)
VER <- utils::read.csv("output/pisa2025/verifikacija.csv", stringsAsFactors = FALSE)
CJ <- utils::read.csv("output/pisa2025/citas_jomas_limeni.csv", stringsAsFactors = FALSE)
pisa_A <- function(g, joma) {
  if (joma == "lasisana") { x <- VER[VER$grupa == "A" & VER$gads == as.numeric(g), "aplese"] }
  else { rk <- grep("^rad", names(CJ), value = TRUE)[1]   # kolonnas nosaukums ar diakritiku
         x <- CJ[CJ$joma == "MATH" & CJ[[rk]] == "vid" & CJ$gads == as.numeric(g), "vertiba"] }
  if (length(x)) x[1] else NA_real_
}
PARI <- list(
  c("PIRLS 4", "2001", "2006", "lasisana", "tā pati paaudze (dz. ~1990)"),
  c("PIRLS 4", "2006", "2012", "lasisana", "tā pati paaudze (dz. ~1996)"),
  c("PIRLS 4", "2016", "2022", "lasisana", "tā pati paaudze (dz. ~2006)"),
  c("TIMSS 4", "2003", "2009", "matematika", "tā pati paaudze (dz. ~1993)"),
  c("TIMSS 4", "2007", "2012", "matematika", "tā pati paaudze (dz. ~1997)"),
  c("TIMSS 4", "2019", "2025", "matematika", "tā pati paaudze (dz. ~2009)"),
  c("TIMSS 8", "1999", "2000", "matematika", "tuvu (dz. ~1985 / 1984)"),
  c("TIMSS 8", "2003", "2003", "matematika", "tuvu (dz. ~1989 / 1987)"),
  c("ICCS 8", "2016", "2018", "lasisana", "tā pati paaudze (dz. ~2002)"),
  c("ICCS 8", "2022", "2025", "lasisana", "gadu vecāki (dz. ~2008 / 2009)"),
  c("ICCS 8", "2009", "2009", "lasisana", "vienlaicīgi, ne kohorta"))
koh <- list()
for (p in PARI) {
  a <- iea[iea$rinda == p[1] & iea$gads == p[2] & !is.na(iea$vid), ]
  b <- PV[PV$gads == p[3] & !is.na(PV[[p[4]]]), ]
  kopa <- setdiff(intersect(a$cnt, b$cnt), "LVA")
  ka <- a$vid[a$cnt %in% kopa]; kb <- b[[p[4]]][match(kopa, b$cnt)]
  lva <- a[a$cnt == "LVA", ]; lvb <- b[b$cnt == "LVA", p[4]]; lvbA <- pisa_A(p[3], p[4])
  z1 <- (lva$vid - mean(ka)) / stats::sd(ka); z2 <- (lvb - mean(kb)) / stats::sd(kb)
  z1A <- (lva$vid_A - mean(ka)) / stats::sd(ka); z2A <- (lvbA - mean(kb)) / stats::sd(kb)
  koh[[length(koh) + 1]] <- data.frame(iea = paste(p[1], p[2]), pisa = p[3], joma = p[4], paaudze = p[5],
    kopa_n = length(kopa), z_iea = z1, z_pisa = z2, dz = z2 - z1, z_iea_A = z1A, z_pisa_A = z2A, dz_A = z2A - z1A,
    lv_iea = lva$vid, lv_iea_A = lva$vid_A, lv_pisa = lvb, lv_pisa_A = lvbA)
  cat(sprintf("  %-13s -> PISA %s %-10s %-32s kopa %2d | visa Latvija z %+.2f -> %+.2f (Δ %+.2f) | latviešu plūsma z %s -> %s (Δ %s)\n",
              paste(p[1], p[2]), p[3], p[4], p[5], length(kopa), z1, z2, z2 - z1,
              sprintf("%+.2f", z1A), sprintf("%+.2f", z2A), sprintf("%+.2f", z2A - z1A)))
}
koh <- do.call(rbind, koh)
utils::write.csv(koh, "output/pisa2025/iea_kohortas.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# (iii) Paaudzes savā starpā var salīdzināt tikai ar vienu un to pašu valstu kopu:
# valstis, kas ir visos trīs IEA ciklos un visos trīs atbilstošajos PISA ciklos.
# Tad Δz pārmaiņa starp paaudzēm nav valstu kopas maiņas sekas.
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 90), "\n(iii) Paaudžu salīdzinājums ar nemainīgu valstu kopu\n", strrep("=", 90), "\n\n")
FIKS <- list(lasisana = 1:3, matematika = 4:6)
fik <- list()
for (nm in names(FIKS)) {
  ids <- FIKS[[nm]]
  sets <- lapply(PARI[ids], function(p) intersect(
    iea$cnt[iea$rinda == p[1] & iea$gads == p[2] & !is.na(iea$vid)],
    PV$cnt[PV$gads == p[3] & !is.na(PV[[p[4]]])]))
  kopa <- setdiff(Reduce(intersect, sets), "LVA")
  cat(sprintf("  %s: nemainīga kopa %d valstis: %s\n", nm, length(kopa), paste(sort(kopa), collapse = " ")))
  for (i in ids) {
    p <- PARI[[i]]
    a <- iea[iea$rinda == p[1] & iea$gads == p[2], ]; b <- PV[PV$gads == p[3], ]
    ka <- a$vid[match(kopa, a$cnt)]; kb <- b[[p[4]]][match(kopa, b$cnt)]
    lva <- a[a$cnt == "LVA", ]; lvb <- b[b$cnt == "LVA", p[4]]; lvbA <- pisa_A(p[3], p[4])
    z <- c(iea = (lva$vid - mean(ka)) / stats::sd(ka), pisa = (lvb - mean(kb)) / stats::sd(kb),
           iea_A = (lva$vid_A - mean(ka)) / stats::sd(ka), pisa_A = (lvbA - mean(kb)) / stats::sd(kb))
    fik[[length(fik) + 1]] <- data.frame(joma = nm, iea = paste(p[1], p[2]), pisa = p[3], kopa_n = length(kopa),
      z_iea = z[["iea"]], z_pisa = z[["pisa"]], dz = z[["pisa"]] - z[["iea"]],
      z_iea_A = z[["iea_A"]], z_pisa_A = z[["pisa_A"]], dz_A = z[["pisa_A"]] - z[["iea_A"]])
    cat(sprintf("    %-13s -> PISA %s | visa Latvija z %+.2f -> %+.2f (Δ %+.2f) | latviešu plūsma z %+.2f -> %+.2f (Δ %+.2f)\n",
                paste(p[1], p[2]), p[3], z[["iea"]], z[["pisa"]], z[["pisa"]] - z[["iea"]],
                z[["iea_A"]], z[["pisa_A"]], z[["pisa_A"]] - z[["iea_A"]]))
  }
  # Jaunākais IEA mērījums (prognozei): tās pašas kopas valstis
  pr <- if (nm == "lasisana") c("PIRLS 4", "2021") else c("TIMSS 4", "2023")
  a <- iea[iea$rinda == pr[1] & iea$gads == pr[2], ]; ka <- a$vid[match(kopa, a$cnt)]
  lva <- a[a$cnt == "LVA", ]
  cat(sprintf("    %-13s -> PISA 2028 | visa Latvija z %+.2f | latviešu plūsma z %+.2f (kopā %d no kopas valstīm)\n",
              paste(pr, collapse = " "), (lva$vid - mean(ka, na.rm = TRUE)) / stats::sd(ka, na.rm = TRUE),
              (lva$vid_A - mean(ka, na.rm = TRUE)) / stats::sd(ka, na.rm = TRUE), sum(!is.na(ka))))
}
utils::write.csv(do.call(rbind, fik), "output/pisa2025/iea_kohortas_fiksets.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/iea_valstu_videjie.csv\n  output/pisa2025/iea_rindas.csv\n",
    " output/pisa2025/iea_kohortas.csv\n ", output_file, "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
