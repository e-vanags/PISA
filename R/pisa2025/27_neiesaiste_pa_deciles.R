# ============================================================================
# 27. Kur tieši pieauga neiesaistīšanās
# ============================================================================
#
# Jautājums. 2025. gadā sadalījuma apakšgals sabruka daudz vairāk nekā augšgals:
# P10 kritās par 60.8 punktiem, P90 par 30.6. `docs/23` es brīdināju, ka tas
# var būt tas pats notikums, ko mēra neiesaistīšanās, nevis neatkarīgs
# pierādījums. Šeit to pārbauda.
#
# Kāpēc tas nav triviāli. Ja skolēnus sakārto pēc PISA rezultāta, tad
# secinājums ir cirkulārs: ātrā minēšana tiek vērtēta kā nepareiza, tāpēc
# minētājs mehāniski nonāk zemākā decilē. Tāpēc kārtojumam vajag mēru, kas
# nav atkarīgs no minēšanas apjoma.
#
# Divi neatkarīgāki kārtojumi:
#   1. Precizitāte TIKAI iesaistītajās atbildēs. Mēra, cik labi skolēns tiek
#      galā ar uzdevumiem, ko tiešām pilda. Daļēji joprojām saistīts, jo vājš
#      lasītājs var padoties tāpēc, ka uzdevums ir par grūtu.
#   2. ESCS. Pilnīgi neatkarīgs no testa uzvedības.
#
# Ievade:  data/pisa2025/processed/uzdevumu_limenis.rds  (skripta 23 kešs)
# Izvade:  output/pisa2025/neiesaiste_deciles.csv
#          output/pisa2025/logs/27_neiesaiste_deciles_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/27_neiesaiste_deciles_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

library(haven)

U <- readRDS("data/pisa2025/processed/uzdevumu_limenis.rds"); uzd <- U$uzd
PV <- paste0("PV", 1:10, "READ")

klase <- function(x) {
  lab <- as.character(as_factor(x)); o <- rep(NA_character_, length(lab))
  o[grepl("^0", lab)] <- "n"; o[grepl("^1", lab)] <- "p"
  o[grepl("No Response", lab)] <- "n"; o[grepl("Not Reached", lab)] <- "nr"; o
}

sagatavo <- function(d, gads) {
  i <- as.character(d$CNT) == "LVA"
  S <- vapply(d[paste0(uzd, "S")], klase, character(nrow(d)))[i, ]
  T <- vapply(d[paste0(uzd, "TT")], function(x) {
    v <- as.numeric(x); v[v <= 0 | v >= 1e7] <- NA; v }, numeric(nrow(d)))[i, ]
  colnames(S) <- uzd; colnames(T) <- uzd
  s <- readRDS(sprintf("data/pisa%d/processed/pisa_lva_%d_stu.rds", gads, gads))
  j <- match(as.numeric(d$CNTSTUID[i]), as.numeric(s$CNTSTUID))
  list(S = S, T = T, gads = gads,
       w = as.numeric(s$W_FSTUWT)[j],
       escs = as.numeric(s$ESCS)[j],
       pv = rowMeans(sapply(PV, function(p) s[[p]]))[j],
       grupa = ifelse(grepl("^Latvian", as.character(as_factor(s$LANGN))[j]), "Latviesu",
               ifelse(grepl("^Russian", as.character(as_factor(s$LANGN))[j]), "Krievu", "Cita")))
}
A <- sagatavo(U$d22, 2022); B <- sagatavo(U$d25, 2025)

# Slieksnis no 2022. gada, kā skriptā 23.
SL <- pmin(0.10 * apply(A$T, 2, mean, na.rm = TRUE), 10000)

mera <- function(z) {
  M <- sweep(z$T, 2, SL, "<"); M[is.na(M)] <- FALSE
  vert <- !is.na(z$S) & z$S %in% c("p", "n")
  atr  <- M & vert
  ies  <- vert & !atr
  n_ies <- rowSums(ies); n_v <- rowSums(vert)
  # Spēja, kas nav atkarīga no tā, cik daudz skolēns minējis.
  z$prec_ies <- ifelse(n_ies >= 8, rowSums(ies & z$S == "p") / n_ies, NA)
  z$rg <- ifelse(n_v >= 10, rowSums(atr) / n_v, NA)
  z
}
A <- mera(A); B <- mera(B)


# === 1. ADAPTIVITĀTES DIAGNOSTIKA ===========================================
cat(strrep("=", 70), "\n1. Vai uzdevumu piešķīrums ir atkarīgs no spējas\n",
    strrep("=", 70), "\n\n")

cat("Ja tests maršrutē uzdevumus pēc spējas, tad dažādus uzdevumus saņem\n")
cat("dažāda līmeņa skolēni. To rāda saņēmēju vidējās spējas izkliede.\n")
cat("Nejaušā bukletu piešķīrumā tai jābūt tuvu izlases troksnim.\n\n")

for (z in list(A, B)) {
  san <- apply(z$S, 2, function(x) mean(z$pv[!is.na(x)], na.rm = TRUE))
  n_vid <- mean(colSums(!is.na(z$S)))
  troksnis <- sd(z$pv, na.rm = TRUE) / sqrt(n_vid)
  cat(sprintf("  %d  saņēmēju PV vidējo SD = %5.1f   sagaidāmais troksnis = %4.1f\n",
              z$gads, sd(san, na.rm = TRUE), troksnis))
}
cat("\nAbos gados izkliede krietni pārsniedz troksni, tātad maršrutēšana pēc\n")
cat("spējas notiek JAU 2022. gadā. Lasīšanas tests bija adaptīvs abos ciklos.\n")
cat("Sekas: neapstrādātu pareizo atbilžu īpatsvaru NEDRĪKST salīdzināt starp\n")
cat("spēju līmeņiem, tāpēc procentiļu analīze uz neapstrādātiem datiem atkrīt.\n")
cat("Grupu vidējo salīdzinājums paliek spēkā, sk. dokumentu.\n")


# === 2. NEIESAISTĪŠANĀS PA DECILĒM ==========================================
cat("\n", strrep("=", 70), "\n2. Ātrās minēšanas īpatsvars pa decilēm\n",
    strrep("=", 70), "\n\n")

rez <- list()
pa_decilem <- function(lauks, nosaukums) {
  cat("Kārtojums pēc:", nosaukums, "\n")
  cat(sprintf("  %-7s %9s %9s %10s\n", "decile", "2022 %", "2025 %", "pārmaiņa"))
  for (k in 1:10) {
    v <- numeric(2)
    for (gi in 1:2) {
      z <- if (gi == 1) A else B
      x <- z[[lauks]]
      q <- quantile(x, c((k - 1) / 10, k / 10), na.rm = TRUE)
      i <- !is.na(x) & !is.na(z$rg) & x >= q[1] & x <= q[2]
      v[gi] <- 100 * sum(z$rg[i] * z$w[i], na.rm = TRUE) / sum(z$w[i])
    }
    cat(sprintf("  %-7d %9.2f %9.2f %+10.2f\n", k, v[1], v[2], v[2] - v[1]))
    rez[[length(rez) + 1]] <<- data.frame(kartojums = nosaukums, decile = k,
                                          y2022 = v[1], y2025 = v[2],
                                          parmaina = v[2] - v[1])
  }
  cat(sprintf("  korelācija ar ātro minēšanu: 2022 %+.3f, 2025 %+.3f\n\n",
              cor(A[[lauks]], A$rg, use = "complete.obs"),
              cor(B[[lauks]], B$rg, use = "complete.obs")))
}

pa_decilem("pv",        "PISA lasīšanas rezultāts (CIRKULĀRS)")
pa_decilem("prec_ies",  "precizitāte tikai iesaistītajās atbildēs")
pa_decilem("escs",      "ESCS, neatkarīgs no testa uzvedības")


# === 3. PA VALODU GRUPĀM ====================================================
cat(strrep("=", 70), "\n3. Ātrās minēšanas īpatsvars pa valodu grupām\n",
    strrep("=", 70), "\n\n")
cat(sprintf("  %-10s %9s %9s %10s\n", "grupa", "2022 %", "2025 %", "pārmaiņa"))
for (g in c("Latviesu", "Krievu", "Cita")) {
  v <- numeric(2)
  for (gi in 1:2) {
    z <- if (gi == 1) A else B
    i <- !is.na(z$grupa) & z$grupa == g & !is.na(z$rg)
    v[gi] <- 100 * sum(z$rg[i] * z$w[i]) / sum(z$w[i])
  }
  cat(sprintf("  %-10s %9.2f %9.2f %+10.2f\n", g, v[1], v[2], v[2] - v[1]))
  rez[[length(rez) + 1]] <- data.frame(kartojums = "valodu grupa", decile = NA,
                                       y2022 = v[1], y2025 = v[2],
                                       parmaina = v[2] - v[1])
}

df <- do.call(rbind, rez); rownames(df) <- NULL
write.csv(df, "output/pisa2025/neiesaiste_deciles.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/neiesaiste_deciles.csv\n ",
    output_file, "\n\nPabeigts:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
