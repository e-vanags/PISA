# ============================================================================
# 18. Divi klasifikatori, proti, izglītības programma pret mājās runāto valodu
# ============================================================================
#
# Konteksts. LU IZPI 2026. gada 8. septembra prezentācijā valodu grupas
# definētas pēc izglītības programmas, proti, mazākumtautību pamatizglītības
# programma pret pamatizglītības programmu. Mūsu skripti 15 līdz 17 lieto
# mājās runāto valodu (LANGN). Šis skripts pārbauda, kā abi klasifikatori
# sakrīt un vai secinājumi no tā mainās.
#
# Kāpēc tas ir svarīgi. Mazākumtautību programma pati par sevi tiek pakāpeniski
# izbeigta, tāpēc tās īpatsvars starp cikliem krītas. Ja to lieto trenda
# salīdzinājumam, 2022. un 2025. gadā tiek salīdzinātas atšķirīgas skolēnu
# kopas. Mājās runātā valoda ar reformu nemainās, tāpēc trendam tā ir stabilāka.
#
# Skripts arī pārbauda, vai mūsu ķēde atražo LU prezentācijā publicētos
# skaitļus.
#
# Ievade:  data/pisa2025/processed/pisa_lva_2025_stu.rds
#          data/pisa2022/processed/pisa_lva_2022_stu.rds
# Izvade:  output/pisa2025/logs/18_klasifikatoru_salidzinajums_output.txt
#          output/pisa2025/klasifikatoru_salidzinajums.csv
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/18_klasifikatoru_salidzinajums_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

required_packages <- c("haven")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}
cat("Pakotnes ielādētas.\n\n")


# === 1. DATI UN KLASIFIKATORI ================================================
cat(strrep("=", 70), "\n")
cat("1. Dati un klasifikatori\n")
cat(strrep("=", 70), "\n\n")

d25 <- readRDS("data/pisa2025/processed/pisa_lva_2025_stu.rds")
d22 <- readRDS("data/pisa2022/processed/pisa_lva_2022_stu.rds")

sagatavo <- function(d) {
  d$krievu <- as.numeric(grepl("^Russian", as.character(as_factor(d$LANGN))))
  d$cita <- as.numeric(!grepl("^Russian|^Latvian",
                              as.character(as_factor(d$LANGN))))
  # Mazākumtautību programma. Etiķetes starp cikliem atšķiras vārdu secībā.
  prog <- as.character(as_factor(d$PROGN))
  d$mazakumt <- as.numeric(grepl("[Mm]inority", prog))
  # Pamatizglītības programmas, proti, abas pamatizglītības plūsmas bez
  # vidusskolas, profesionālās un speciālās programmas. LU prezentācija
  # salīdzina tieši šīs divas.
  d$pamatizgl <- as.numeric(grepl("[Bb]asic education", prog) &
                              !grepl("[Vv]ocational|[Ss]pecial", prog))
  d$latviski_majas <- as.numeric(grepl("^Latvian",
                                       as.character(as_factor(d$LANGN))))
  d$meitene <- as.numeric(as.numeric(d$ST004D01T) == 1)
  d$klase <- as.numeric(d$GRADE)
  d$imigrants <- as.numeric(grepl("^First|^Second",
                                  as.character(as_factor(d$IMMIG))))
  d$skola <- as.character(d$CNTSCHID)
  d
}
d25 <- sagatavo(d25); d22 <- sagatavo(d22)

for (gads in c("2022", "2025")) {
  d <- if (gads == "2022") d22 else d25
  w <- d$W_FSTUWT
  cat(gads, "\n")
  cat(sprintf("  Mazākumtautību programmā: %.1f %%\n",
              100 * sum(w[d$mazakumt == 1]) / sum(w)))
  cat(sprintf("  Mājās runā krieviski:     %.1f %%\n",
              100 * sum(w[d$krievu == 1]) / sum(w)))
  cat("  Krusttabula, svērtie īpatsvari:\n")
  print(round(prop.table(xtabs(w ~ paste0("majas_krievu=", d$krievu) +
                                 paste0("mazakumt_progr=", d$mazakumt))), 3))
  cat("\n")
}


# === 2. NOVĒRTĒŠANAS FUNKCIJA ================================================
# Tāda pati kā skriptā 16, proti, 10 PV un 80 BRR replikāti ar Fay 0.5.

FAY_DALITAJS <- 20

svertais_vidējais_pa_grupam <- function(x, g, w) {
  sw <- tapply(w, g, sum); swx <- tapply(w * x, g, sum)
  (swx / sw)[as.character(g)]
}

wls_koef <- function(X, y, w, mainigais, grupa = NULL) {
  if (!is.null(grupa)) {
    y <- y - svertais_vidējais_pa_grupam(y, grupa, w)
    X <- apply(X, 2, function(col) col - svertais_vidējais_pa_grupam(col, grupa, w))
    X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  }
  sw <- sqrt(w)
  cf <- .lm.fit(X * sw, y * sw)$coefficients
  names(cf) <- colnames(X)
  cf[[mainigais]]
}

novertē <- function(d, mainigais, kontroles = character(0),
                    absorb_skola = FALSE, domens = "READ") {
  pvs <- paste0("PV", 1:10, domens)
  rw <- grep("^W_FSTURWT", names(d), value = TRUE)
  mainigie <- c(mainigais, kontroles)
  pilni <- stats::complete.cases(d[, c(mainigie, pvs, "W_FSTUWT", "skola")])
  dd <- d[pilni, ]
  X <- stats::model.matrix(
    stats::as.formula(paste("~", paste(mainigie, collapse = " + "))), data = dd)
  grupa <- if (absorb_skola) dd$skola else NULL
  est_pv <- vapply(pvs, function(p)
    wls_koef(X, dd[[p]], dd$W_FSTUWT, mainigais, grupa), numeric(1))
  rep_est <- vapply(rw, function(r)
    wls_koef(X, dd[[pvs[1]]], dd[[r]], mainigais, grupa), numeric(1))
  list(est = mean(est_pv),
       se = sqrt(sum((rep_est - est_pv[1])^2) / FAY_DALITAJS +
                   (1 + 1 / length(pvs)) * stats::var(est_pv)),
       n = nrow(dd))
}


# === 3. STARPĪBA PĒC ABIEM KLASIFIKATORIEM ===================================
cat(strrep("=", 70), "\n")
cat("3. Sniegumu starpība pēc abiem klasifikatoriem\n")
cat(strrep("=", 70), "\n\n")

rez <- list()
for (gads in c("2022", "2025")) {
  d <- if (gads == "2022") d22 else d25
  for (kl in c("mazakumt", "krievu")) {
    for (dom in c("READ", "MATH", "SCIE")) {
      if (dom == "SCIE" && !("PV1SCIE" %in% names(d))) next
      neapstr <- novertē(d, kl, domens = dom)
      ar_ses  <- novertē(d, kl, "ESCS", domens = dom)
      ar_fe   <- novertē(d, kl, c("ESCS", "meitene", "klase", "imigrants"),
                         absorb_skola = TRUE, domens = dom)
      cat(sprintf("  %s %-9s %-4s neapstrādāta %7.1f (%.1f)  + ESCS %7.1f  skolas iekšienē %7.1f (%.1f)\n",
                  gads, kl, dom, neapstr$est, neapstr$se, ar_ses$est,
                  ar_fe$est, ar_fe$se))
      rez[[length(rez) + 1]] <- data.frame(
        gads = gads, klasifikators = kl, domens = dom,
        neapstradata = round(neapstr$est, 1), se_neapstr = round(neapstr$se, 1),
        ar_escs = round(ar_ses$est, 1),
        skolas_ieksiene = round(ar_fe$est, 1), se_fe = round(ar_fe$se, 1))
    }
  }
  cat("\n")
}
tab <- do.call(rbind, rez)
write.csv(tab, "output/pisa2025/klasifikatoru_salidzinajums.csv",
          row.names = FALSE, fileEncoding = "UTF-8")


# === 4. VAI ATRAŽOJAM LU PREZENTĀCIJAS SKAITĻUS ==============================
cat(strrep("=", 70), "\n")
cat("4. LU prezentācijas skaitļu pārbaude\n")
cat(strrep("=", 70), "\n\n")

cat("LU prezentācija, PISA 2025, starpība starp pamatizglītības un\n")
cat("mazākumtautību pamatizglītības programmu: lasīšana −91, dabaszinātnes\n")
cat("−80, matemātika −47.\n\n")
cat("Mūsu aprēķins, salīdzinot tikai abas pamatizglītības plūsmas savā starpā,\n")
cat("proti, izslēdzot vidusskolas, profesionālās un speciālās programmas:\n")
dp <- d25[d25$pamatizgl == 1, ]
for (dom in c("READ", "SCIE", "MATH")) {
  r <- novertē(dp, "mazakumt", domens = dom)
  cat(sprintf("  %-5s %7.1f (SE %.1f)   n = %d\n", dom, r$est, r$se, r$n))
}
cat("\n")
cat("Salīdzinājumam, tas pats uz visas izlases:\n")
for (dom in c("READ", "SCIE", "MATH")) {
  r <- tab[tab$gads == "2025" & tab$klasifikators == "mazakumt" &
             tab$domens == dom, ]
  if (nrow(r)) cat(sprintf("  %-5s %7.1f (SE %.1f)\n", dom, r$neapstradata,
                           r$se_neapstr))
}
cat("\n")

# Skolēnu īpatsvars zem 2. kompetences līmeņa lasīšanā, slieksnis 407 punkti.
cat("Skolēnu īpatsvars zem 2. līmeņa lasīšanā, slieksnis 407 punkti\n")
cat("LU prezentācija: visa Latvija 41.3 %, pamatizglītības programma un mājās\n")
cat("runā latviski 26.5 %, pamatizglītības programma un mājās nerunā latviski\n")
cat("58.6 %, mazākumtautību programma un mājās nerunā latviski 72.8 %.\n\n")

zem_limena <- function(d, i) {
  pv <- sapply(paste0("PV", 1:10, "READ"), function(p) d[[p]][i] < 407)
  w <- d$W_FSTUWT[i]
  100 * mean(apply(pv, 2, function(x) sum(w * x) / sum(w)))
}
cat(sprintf("  Visa Latvija:                                    %.1f %%  (LU 41.3)\n",
            zem_limena(d25, rep(TRUE, nrow(d25)))))
cat(sprintf("  Pamatizgl. programma, mājās latviski:            %.1f %%  (LU 26.5)\n",
            zem_limena(d25, d25$pamatizgl == 1 & d25$mazakumt == 0 &
                         d25$latviski_majas == 1)))
cat(sprintf("  Pamatizgl. programma, mājās nav latviski:        %.1f %%  (LU 58.6)\n",
            zem_limena(d25, d25$pamatizgl == 1 & d25$mazakumt == 0 &
                         d25$latviski_majas == 0)))
cat(sprintf("  Mazākumtautību programma, mājās nav latviski:    %.1f %%  (LU 72.8)\n",
            zem_limena(d25, d25$mazakumt == 1 & d25$latviski_majas == 0)))
cat("\n")


# === 5. KĀPĒC KLASIFIKATORA IZVĒLE MAINA TRENDU ==============================
cat(strrep("=", 70), "\n")
cat("5. Kāpēc klasifikatora izvēle maina trendu\n")
cat(strrep("=", 70), "\n\n")

for (kl in c("mazakumt", "krievu")) {
  d22_dala <- sum(d22$W_FSTUWT[d22[[kl]] == 1]) / sum(d22$W_FSTUWT)
  d25_dala <- sum(d25$W_FSTUWT[d25[[kl]] == 1]) / sum(d25$W_FSTUWT)
  cat(sprintf("  %-9s īpatsvars 2022 %.3f, 2025 %.3f, pārmaiņa %+.3f\n",
              kl, d22_dala, d25_dala, d25_dala - d22_dala))
}
cat("\n")
cat("Mazākumtautību programmas īpatsvars krītas tāpēc, ka programma tiek\n")
cat("izbeigta, nevis tāpēc, ka mainījies skolēnu sastāvs. Mājās runātās\n")
cat("valodas sadalījums tajā pašā laikā mainījies daudz mazāk.\n\n")
cat("Tas nozīmē, ka trendam pa programmām 2022 un 2025 tiek salīdzinātas\n")
cat("atšķirīgas kopas. Šķērsgriezumam vienā gadā programma ir derīgs\n")
cat("klasifikators, trendam starp cikliem drošāka ir mājās runātā valoda.\n\n")


# === 6. KOPSAVILKUMS =========================================================
cat(strrep("=", 70), "\n")
cat("6. Kopsavilkums\n")
cat(strrep("=", 70), "\n\n")
print(tab, row.names = FALSE)
cat("\nPabeigts:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
