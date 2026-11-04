# ============================================================================
# 25. Igaunija kā kontrole: vai apmaiņas artefakts ir vispārējs
# ============================================================================
#
# Jautājums. Skripts 23 parādīja, ka Latvijā korekcija par nesasniegto un ātrās
# minēšanas apmaiņu noņem 3.2 procentpunktus no krituma. Vai tas ir vispārējs
# formāta efekts, vai kaut kas Latvijai specifisks.
#
# Igaunija ir gandrīz ideāla kontrole. Tai bija tā pati formāta maiņa, tā pati
# adaptīvā testēšana, tas pats gads un tā pati saistīšana. Bet valodas politika
# nemainījās, un 2025. gadā 1317 igauņu skolēni joprojām kārtoja testu
# krieviski.
#
# Tas ļauj izdalīt grupu, kurai mainījās TIKAI formāts, ne valoda:
# krieviski runājošie Igaunijā, kas abos gados testu kārtoja krieviski.
# Viņu korekcijas lielums ir tīra formāta artefakta aplēse.
#
# Ievade:  data/pisa2025/processed/uzdevumu_limenis.rds  (skripta 23 kešs)
#          data/pisa20xx/processed/pisa_{lva,est}_20xx_stu.rds
# Izvade:  output/pisa2025/igaunija_kontrole.csv
#          output/pisa2025/logs/25_igaunija_kontrole_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/25_igaunija_kontrole_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

library(haven)
source("R/pisa2025/00_kopigas_funkcijas.R")

KESS <- "data/pisa2025/processed/uzdevumu_limenis.rds"
if (!file.exists(KESS)) stop("Vispirms jāpalaiž skripts 23.")
U <- readRDS(KESS); uzd <- U$uzd

izgust <- function(d) {
  S <- vapply(d[paste0(uzd, "S")], klasificet, character(nrow(d)))
  T <- vapply(d[paste0(uzd, "TT")], tirs_laiks, numeric(nrow(d)))
  colnames(S) <- uzd; colnames(T) <- uzd
  list(S = S, P = punkti_matrica(S), T = T,
       cnt = as.character(d$CNT), id = as.numeric(d$CNTSTUID))
}
E22 <- izgust(U$d22); E25 <- izgust(U$d25)

RW <- paste0("W_FSTURWT", 1:80)
svari <- function(valsts, gads) {
  f <- sprintf("data/pisa%d/processed/pisa_%s_%d_stu.rds", gads, tolower(valsts), gads)
  d <- readRDS(f)
  cbind(data.frame(CNTSTUID = as.numeric(d$CNTSTUID), w = as.numeric(d$W_FSTUWT),
                   lab = as.character(as_factor(d$LANGN)),
                   tval = as.character(as_factor(d$LANGTEST_COG)),
                   stringsAsFactors = FALSE),
        as.data.frame(lapply(d[RW], as.numeric)))
}
SV <- list(LVA = list("2022" = svari("LVA", 2022), "2025" = svari("LVA", 2025)),
           EST = list("2022" = svari("EST", 2022), "2025" = svari("EST", 2025)))

# Slieksnis no 2022. gada, katrai valstij savs, jo uzdevumu grūtība un
# valodas versijas atšķiras.
slieksnis <- function(valsts) {
  i <- E22$cnt == valsts
  pmin(0.10 * apply(E22$T[i, ], 2, mean, na.rm = TRUE), 10000)
}
SL <- list(LVA = slieksnis("LVA"), EST = slieksnis("EST"))

pct <- function(E, valsts, sl, wt, keep) {
  i <- E$cnt == valsts
  M0 <- sweep(E$T[i, ], 2, sl, "<"); M0[is.na(M0)] <- FALSE
  S <- E$S[i, ]; P <- E$P[i, ]
  A <- M0 & !is.na(S) & S %in% VERTETAS
  S <- S[keep, ]; P <- P[keep, ]; A <- A[keep, ]
  wt <- wt[keep]; wt[is.na(wt)] <- 0
  W <- matrix(wt, nrow = nrow(S), ncol = ncol(S)); b <- !is.na(S)
  f <- function(s) 100 * sum(W[s] * P[s], na.rm = TRUE) / sum(W[s])
  c(R1 = f(b & S %in% VERTETAS),
    R5 = f(b & S %in% VERTETAS_AR_NESASNIEGTIEM & !A))
}

# nosac(m) atgriež loģisko vektoru pār attiecīgā gada skolēniem
analize <- function(valsts, nosac) {
  res <- list()
  for (gads in c("2022", "2025")) {
    E <- if (gads == "2022") E22 else E25
    w <- SV[[valsts]][[gads]]
    m <- w[match(E$id[E$cnt == valsts], w$CNTSTUID), ]
    keep <- nosac(m, gads)
    keep[is.na(keep)] <- FALSE
    if (sum(keep) < 30) return(NULL)
    est <- pct(E, valsts, SL[[valsts]], m$w, keep)
    rp <- vapply(RW, function(r) pct(E, valsts, SL[[valsts]], m[[r]], keep), numeric(2))
    res[[gads]] <- list(est = est, var = rowSums((rp - est)^2) / 20, n = sum(keep))
  }
  d1 <- res[["2025"]]$est["R1"] - res[["2022"]]$est["R1"]
  d5 <- res[["2025"]]$est["R5"] - res[["2022"]]$est["R5"]
  c(n22 = res[["2022"]]$n, n25 = res[["2025"]]$n,
    d1 = unname(d1), s1 = unname(sqrt(res[["2025"]]$var["R1"] + res[["2022"]]$var["R1"])),
    d5 = unname(d5), s5 = unname(sqrt(res[["2025"]]$var["R5"] + res[["2022"]]$var["R5"])))
}

vis  <- function(m, g) rep(TRUE, nrow(m))
titul <- function(pat) function(m, g) grepl(pat, m$lab)
kriev <- function(m, g) grepl("^Russian", m$lab)
# Igaunijā: krieviski runājošie, kas TESTU kārtoja krieviski abos gados
kriev_ru_tests <- function(m, g) grepl("^Russian", m$lab) & grepl("^Russian", m$tval)
kriev_lv_tests <- function(m, g) grepl("^Russian", m$lab) & !grepl("^Russian", m$tval)

GRUPAS <- list(
  list("LVA", "Latvija, visi",                         vis),
  list("LVA", "Latvija, latviski runājošie",           titul("^Latvian")),
  list("LVA", "Latvija, krieviski runājošie",          kriev),
  list("EST", "Igaunija, visi",                        vis),
  list("EST", "Igaunija, igauniski runājošie",         titul("^Estonian")),
  list("EST", "Igaunija, krieviski runājošie",         kriev),
  list("EST", "Igaunija, krievu val., tests krieviski", kriev_ru_tests),
  list("EST", "Igaunija, krievu val., tests igauniski", kriev_lv_tests)
)

cat(strrep("=", 92), "\n")
cat(sprintf("%-38s %6s %6s %17s %17s %8s\n", "grupa", "n22", "n25",
            "R1 pārmaiņa", "R5 pārmaiņa", "korekcija"))
cat(strrep("=", 92), "\n")
rez <- list()
for (g in GRUPAS) {
  r <- analize(g[[1]], g[[3]])
  if (is.null(r)) { cat(sprintf("%-38s  par mazu izlase\n", g[[2]])); next }
  zv <- if (abs(r["d5"] / r["s5"]) > 1.96) "*" else " "
  cat(sprintf("%-38s %6d %6d %8.2f (%.2f) %8.2f (%.2f)%s %7.2f\n",
              g[[2]], r["n22"], r["n25"], r["d1"], r["s1"], r["d5"], r["s5"], zv,
              r["d1"] - r["d5"]))
  rez[[length(rez) + 1]] <- data.frame(
    grupa = g[[2]], n_2022 = r["n22"], n_2025 = r["n25"],
    r1_parmaina = r["d1"], r1_se = r["s1"],
    r5_parmaina = r["d5"], r5_se = r["s5"],
    korekcija_pp = r["d1"] - r["d5"])
}
cat(strrep("=", 92), "\n")
cat("\n* nozīmīgs pie 95 %. Korekcija ir R1 un R5 starpība procentpunktos,\n")
cat("proti, cik daudz vērtēšanas mehānikas maiņa un ātrā minēšana kopā\n")
cat("pazemināja izmērīto rezultātu.\n")

cat("\n", strrep("=", 70), "\nInterpretācija\n", strrep("=", 70), "\n\n", sep = "")
df <- do.call(rbind, rez); rownames(df) <- NULL
kor <- df$korekcija_pp
cat(sprintf("Korekcijas lielums svārstās no %.2f līdz %.2f procentpunktiem.\n",
            min(kor), max(kor)))
cat("Ja tas visās grupās un abās valstīs ir līdzīgs, tad tas ir vispārējs\n")
cat("formāta efekts, nevis kaut kas Latvijai vai kādai grupai specifisks.\n")

write.csv(df, "output/pisa2025/igaunija_kontrole.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/igaunija_kontrole.csv\n ",
    output_file, "\n\nPabeigts:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
