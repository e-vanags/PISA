# ============================================================================
# 45. Scenāriju aprēķins: cik gados A plūsma sasniegtu šī brīža valsts vidējo
# ============================================================================
#
# JAUTĀJUMS. Ja A plūsmā turpinās lēnā lejupslīde, pēc cik gadiem tās vidējais
# lasītprasmes rezultāts sasniegtu Latvijas 2025. gada vidējo?
#
#   A plūsma 2025          459,2
#   Latvija kopā 2025      429,7
#   starpība               29,5 punkti
#
# ŠIS NAV PROGNOZĒŠANAS MODELIS. Tā ir lineāra ekstrapolācija, kas atbild uz
# jautājumu "kas būtu, ja", nevis "kas būs". Tā pieņem, ka tempu nemaina ne
# politika, ne demogrāfija, ne mērījuma pārmaiņas.
#
# LĒNĀS LEJUPSLĪDES DEFINĪCIJA. Temps ir jutīgs pret to, kuru periodu ņem,
# tāpēc tiek rēķināti vairāki varianti. Galvenais ir 2009-2022, jo:
#   - 2003 -> 2006 ir atsevišķs kritums, kam seko plato (`docs/48`);
#   - 2022 -> 2025 kritums (-19,4) nav lēns, un daļa no tā, iespējams, ir
#     testa formāta efekts (`docs/23`, `docs/40`, `docs/49`);
#   - apakšgala erozija sākās 2009. gadā.
#
# METODE. Svērtā lineārā regresija ar svariem 1/SE^2. Tempa standartkļūda
# no zināmajām dispersijām. Otrs variants pieskaita katra punkta dispersijai
# saites kļūdu LE = 3, kas ir vidus vērtība no `docs/49` atvasinātajām
# 1,5 līdz 8,6, jo trenda regresija starp cikliem to prasa.
#
# Gadi līdz mērķim = starpība / |temps|. Ja tempa intervāls pietuvojas nullei,
# gadu skaits tiecas uz bezgalību, tāpēc intervāls ir asimetrisks.
#
# Izvade:  output/pisa2025/logs/45_scenariji_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/45_scenariji_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)

v <- read.csv("output/pisa2025/verifikacija.csv")
A <- v[v$grupa == "A", c("gads", "aplese", "se")]
V <- v[v$grupa == "visi", c("gads", "aplese", "se")]
A$gads <- as.numeric(A$gads)
sakums <- A$aplese[A$gads == 2025]
merkis <- V$aplese[V$gads == "2025"]
starpiba <- sakums - merkis

cat(sprintf("A plūsma 2025:      %.1f\nLatvija kopā 2025:  %.1f\nStarpība:           %.1f punkti\n\n",
            sakums, merkis, starpiba))

tempis <- function(g, le = 0) {
  d <- A[A$gads %in% g, ]
  w <- 1 / (d$se^2 + le^2)
  xb <- sum(w * d$gads) / sum(w)
  b <- sum(w * (d$gads - xb) * d$aplese) / sum(w * (d$gads - xb)^2)
  se <- sqrt(1 / sum(w * (d$gads - xb)^2))
  c(b = b, se = se)
}

VARIANTI <- list(
  "Lēnā lejupslīde 2009–2022"  = c(2009, 2012, 2015, 2018, 2022),
  "Lēnā lejupslīde 2012–2022"  = c(2012, 2015, 2018, 2022),
  "Visa rinda 2003–2025"       = c(2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025),
  "Pēdējā desmitgade 2015–2025"= c(2015, 2018, 2022, 2025))

gadi <- function(b) if (b >= 0) Inf else starpiba / abs(b)
fmt <- function(x) if (!is.finite(x)) "nekad" else sprintf("%.0f", x)

for (le in c(0, 3)) {
  cat(strrep("=", 78), "\n",
      if (le == 0) "Bez saites kļūdas" else "Ar saites kļūdu LE = 3", "\n",
      strrep("=", 78), "\n\n", sep = "")
  cat(sprintf("  %-30s %9s %7s %13s   %8s  %-13s %s\n", "periods", "temps/g", "SE",
              "95 % TI", "gadi", "95 % TI gadi", "sasniegtu ap"))
  for (n in names(VARIANTI)) {
    t <- tempis(VARIANTI[[n]], le)
    lo <- t["b"] - 1.96 * t["se"]; hi <- t["b"] + 1.96 * t["se"]
    g <- gadi(t["b"]); g_ats <- gadi(lo); g_lēn <- gadi(hi)
    cat(sprintf("  %-30s %+9.2f %7.2f  [%+5.2f; %+5.2f]   %8s  %5s–%-7s %s\n",
                n, t["b"], t["se"], lo, hi, fmt(g), fmt(g_ats), fmt(g_lēn),
                if (is.finite(g)) sprintf("%.0f", 2025 + g) else "—"))
  }
  cat("\n")
}
while (sink.number() > 0) sink()
close(log_con)
