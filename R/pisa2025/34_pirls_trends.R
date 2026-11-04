# ============================================================================
# 34. PIRLS 2016 pret 2021: cik liels bija kritums 4. klasē
# ============================================================================
#
# Jautājums. PIRLS 2021 nacionālais ziņojums 2023. gadā rakstīja, ka kopš
# 2016. gada Latvijas skolēnu lasītprasme "ievērojami pazeminājusies" un ka
# kritums noticis "visās skolēnu grupās, gan latviešu, gan krievu mācību
# valodām". Apjomu ziņojums pa grupām nenorāda. Šeit tas ir izrēķināts.
#
# Kāpēc tas ir svarīgi. Mikrodati rāda, ka PIRLS 2021 mēra to pašu dzimšanas
# kohortu, kas 2025. gadā kārtoja PISA: PIRLS vidējais vecums 11,31 gadi
# 2021. gada pavasarī, PISA 2025 visi dzimuši 2009. gadā. Tātad tas ir
# brīdinājums par tiem pašiem bērniem, publicēts divus gadus iepriekš.
#
# Metode. PIRLS standarts: 5 plausible values, jackknife JK2 ar 75 zonām.
#   izlases dispersija = summa pa zonām (aplēse_zonā - aplēse)^2
#   imputācijas dispersija = (1 + 1/5) * var(pieci PV)
#   kopā = izlases + imputācijas
#
# Ievade:  data/iea/pirls2016/asglvar4.sav
#          data/iea/pirls2021/spss/Data/ASGLVAR5.sav
# Izvade:  output/pisa2025/pirls_trends.csv
#          output/pisa2025/logs/34_pirls_trends_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/34_pirls_trends_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

library(haven)

nolasa <- function(cels) {
  d <- read_sav(cels)
  names(d) <- toupper(names(d))
  d
}
d16 <- nolasa("data/iea/pirls2016/asglvar4.sav")
d21 <- nolasa("data/iea/pirls2021/spss/Data/ASGLVAR5.sav")

PV <- paste0("ASRREA0", 1:5)

# PIRLS jackknife JK2. Katrai zonai viena replikācija: tās zonas skolēniem
# svars tiek dubultots vai anulēts atkarībā no JKREP, pārējiem nemainīgs.
jk_aplese <- function(d, i) {
  w <- as.numeric(d$TOTWGT)[i]
  zona <- as.numeric(d$JKZONE)[i]
  rep <- as.numeric(d$JKREP)[i]
  pv <- lapply(PV, function(p) as.numeric(d[[p]])[i])

  sv <- function(x, wt) sum(x * wt) / sum(wt)
  est_pv <- vapply(pv, sv, numeric(1), wt = w)
  est <- mean(est_pv)

  # Izlases dispersija uz pirmā PV, kā PIRLS rokasgrāmatā.
  zonas <- sort(unique(zona))
  rep_est <- vapply(zonas, function(z) {
    wr <- w
    wr[zona == z] <- w[zona == z] * 2 * rep[zona == z]
    sv(pv[[1]], wr)
  }, numeric(1))
  var_izl <- sum((rep_est - est_pv[1])^2)
  var_imp <- (1 + 1 / length(PV)) * stats::var(est_pv)

  c(est = est, se = sqrt(var_izl + var_imp), n = sum(i),
    dala = sum(w) / sum(as.numeric(d$TOTWGT)))
}

# 2016. gadā mainīgais saucas ITLANG, 2021. gadā ITLANG_SA.
valoda <- function(d) {
  lauks <- if ("ITLANG_SA" %in% names(d)) "ITLANG_SA" else "ITLANG"
  v <- as.character(as_factor(d[[lauks]]))
  ifelse(grepl("Latvian", v), "latviešu", ifelse(grepl("Russian", v), "krievu", NA))
}
d16$val <- valoda(d16); d21$val <- valoda(d21)

cat(strrep("=", 74), "\n1. Izlase\n", strrep("=", 74), "\n\n")
for (nos in c("2016", "2021")) {
  d <- if (nos == "2016") d16 else d21
  cat(nos, ": skolēni", nrow(d), "\n")
  print(table(d$val, useNA = "ifany"))
  cat("\n")
}

cat(strrep("=", 74), "\n2. Lasītprasmes sasniegumi un kritums\n",
    strrep("=", 74), "\n\n")
cat(sprintf("  %-16s %8s %6s   %8s %6s   %9s %6s\n",
            "grupa", "2016", "SE", "2021", "SE", "kritums", "SE"))

rez <- list()
grupas <- list(c("Visi", NA), c("Testa valoda latviešu", "latviešu"),
               c("Testa valoda krievu", "krievu"))
for (g in grupas) {
  i16 <- if (is.na(g[2])) rep(TRUE, nrow(d16)) else (!is.na(d16$val) & d16$val == g[2])
  i21 <- if (is.na(g[2])) rep(TRUE, nrow(d21)) else (!is.na(d21$val) & d21$val == g[2])
  a <- jk_aplese(d16, i16); b <- jk_aplese(d21, i21)
  d <- b["est"] - a["est"]; s <- sqrt(a["se"]^2 + b["se"]^2)
  zv <- if (abs(d / s) > 1.96) "*" else " "
  cat(sprintf("  %-16s %8.1f %6.1f   %8.1f %6.1f   %+9.1f %6.1f%s  n %d/%d\n",
              g[1], a["est"], a["se"], b["est"], b["se"], d, s, zv,
              a["n"], b["n"]))
  rez[[length(rez) + 1]] <- data.frame(
    grupa = g[1], y2016 = unname(a["est"]), se2016 = unname(a["se"]),
    y2021 = unname(b["est"]), se2021 = unname(b["se"]),
    kritums = unname(d), se_kritums = unname(s),
    n2016 = unname(a["n"]), n2021 = unname(b["n"]),
    dala2016 = unname(a["dala"]), dala2021 = unname(b["dala"]))
}

cat("\n", strrep("=", 74), "\n3. Grupu starpība un tās pārmaiņa\n",
    strrep("=", 74), "\n\n")
lv <- rez[[2]]; ru <- rez[[3]]
for (y in c("2016", "2021")) {
  d <- ru[[paste0("y", y)]] - lv[[paste0("y", y)]]
  s <- sqrt(lv[[paste0("se", y)]]^2 + ru[[paste0("se", y)]]^2)
  cat(sprintf("  %s  krievu mīnus latviešu: %+7.1f (SE %.1f)\n", y, d, s))
}
did <- ru$kritums - lv$kritums
did_se <- sqrt(lv$se_kritums^2 + ru$se_kritums^2)
cat(sprintf("\n  Kritumu starpība: %+.1f (SE %.1f)%s\n", did, did_se,
            if (abs(did / did_se) > 1.96) " *" else ""))

cat("\n  Grupu īpatsvars izlasē:\n")
cat(sprintf("    latviešu %.1f %% -> %.1f %%   krievu %.1f %% -> %.1f %%\n",
            100 * lv$dala2016, 100 * lv$dala2021,
            100 * ru$dala2016, 100 * ru$dala2021))

df <- do.call(rbind, rez); rownames(df) <- NULL
write.csv(df, "output/pisa2025/pirls_trends.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/pirls_trends.csv\n ", output_file,
    "\n\nPabeigts:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
