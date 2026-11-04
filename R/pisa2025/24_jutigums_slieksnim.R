# ============================================================================
# 24. Jutīguma analīze: cik atkarīgs rezultāts no ātrās minēšanas sliekšņa
# ============================================================================
#
# Jautājums. Skripts 23 secināja, ka latviski runājošo lasītprasmes kritums
# pēc korekcijas ir −0.53 procentpunkti, proti, praktiski nulle, un ka 85 %
# no izmērītā krituma ir vērtēšanas mehānika. Šis secinājums balstās uz vienu
# patvaļīgu izvēli: ātrās minēšanas slieksni 10 % no uzdevuma vidējā atbildes
# laika, ne vairāk par 10 sekundēm.
#
# Ja secinājums mainās, mainot slieksni, tad tas nav publicējams.
#
# Pārbaudītie sliekšņi:
#   normatīvie   5, 10, 15, 20 % no 2022. gada vidējā laika, griesti 10 s
#   bez griestiem 10 % bez griestiem
#   fiksētie     2, 3, 5, 10 sekundes visiem uzdevumiem
#   no 2025      10 % no 2025. gada vidējā, lai parādītu bāzes gada izvēles virzienu
#
# Ievade:  data/pisa2025/processed/uzdevumu_limenis.rds  (skripta 23 kešs)
# Izvade:  output/pisa2025/jutigums_slieksnim.csv
#          output/pisa2025/logs/24_jutigums_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/24_jutigums_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

library(haven)
source("R/pisa2025/00_kopigas_funkcijas.R")

KESS <- "data/pisa2025/processed/uzdevumu_limenis.rds"
if (!file.exists(KESS)) stop("Vispirms jāpalaiž skripts 23, lai izveidotu kešu.")
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
svari <- function(gads) {
  d <- readRDS(sprintf("data/pisa%d/processed/pisa_lva_%d_stu.rds", gads, gads))
  cbind(data.frame(CNTSTUID = as.numeric(d$CNTSTUID), w = as.numeric(d$W_FSTUWT),
                   lab = as.character(as_factor(d$LANGN)), stringsAsFactors = FALSE),
        as.data.frame(lapply(d[RW], as.numeric)))
}
W22 <- svari(2022); W25 <- svari(2025)
grupa <- function(l) ifelse(grepl("^Latvian", l), "Latviesu",
                     ifelse(grepl("^Russian", l), "Krievu", "Cita"))

# Vidējie atbilžu laiki katram uzdevumam, abos gados atsevišķi.
vid22 <- apply(E22$T[E22$cnt == "LVA", ], 2, mean, na.rm = TRUE)
vid25 <- apply(E25$T[E25$cnt == "LVA", ], 2, mean, na.rm = TRUE)

SLIEKSNI <- list(
  "normatīvs 5 %, griesti 10 s"   = pmin(0.05 * vid22, 10000),
  "normatīvs 10 %, griesti 10 s"  = pmin(0.10 * vid22, 10000),
  "normatīvs 15 %, griesti 10 s"  = pmin(0.15 * vid22, 10000),
  "normatīvs 20 %, griesti 10 s"  = pmin(0.20 * vid22, 10000),
  "normatīvs 10 %, bez griestiem" = 0.10 * vid22,
  "fiksēts 2 s"                   = rep(2000,  length(uzd)),
  "fiksēts 3 s"                   = rep(3000,  length(uzd)),
  "fiksēts 5 s"                   = rep(5000,  length(uzd)),
  "fiksēts 10 s"                  = rep(10000, length(uzd)),
  "normatīvs 10 %, bāze 2025"     = pmin(0.10 * vid25, 10000)
)

atrs <- function(E, sl) {
  M <- sweep(E$T, 2, sl, "<"); M[is.na(M)] <- FALSE
  M & !is.na(E$S) & E$S %in% VERTETAS
}

pct <- function(E, A, wt, keep) {
  i <- E$cnt == "LVA"
  S <- E$S[i, ][keep, ]; P <- E$P[i, ][keep, ]; Ai <- A[i, ][keep, ]
  wt <- wt[keep]; wt[is.na(wt)] <- 0
  M <- matrix(wt, nrow = nrow(S), ncol = ncol(S)); b <- !is.na(S)
  f <- function(s) 100 * sum(M[s] * P[s], na.rm = TRUE) / sum(M[s])
  c(R1 = f(b & S %in% VERTETAS),
    R5 = f(b & S %in% VERTETAS_AR_NESASNIEGTIEM & !Ai))
}

viens <- function(sl, g, ar_se = TRUE) {
  A22 <- atrs(E22, sl); A25 <- atrs(E25, sl)
  res <- list()
  for (nos in c("2022", "2025")) {
    E <- if (nos == "2022") E22 else E25
    A <- if (nos == "2022") A22 else A25
    w <- if (nos == "2022") W22 else W25
    m <- w[match(E$id[E$cnt == "LVA"], w$CNTSTUID), ]
    keep <- if (is.null(g)) rep(TRUE, nrow(m)) else
      (!is.na(grupa(m$lab)) & grupa(m$lab) == g)
    est <- pct(E, A, m$w, keep)
    v <- c(R1 = NA, R5 = NA)
    if (ar_se) {
      rp <- vapply(RW, function(r) pct(E, A, m[[r]], keep), numeric(2))
      v <- rowSums((rp - est)^2) / 20
    }
    i <- E$cnt == "LVA"
    res[[nos]] <- list(est = est, var = v,
                       atrs = 100 * mean(A[i, ][keep, ][
                         !is.na(E$S[i, ][keep, ]) &
                           E$S[i, ][keep, ] %in% VERTETAS]))
  }
  d1 <- res[["2025"]]$est["R1"] - res[["2022"]]$est["R1"]
  d5 <- res[["2025"]]$est["R5"] - res[["2022"]]$est["R5"]
  c(d1 = unname(d1), d5 = unname(d5),
    s5 = unname(sqrt(res[["2025"]]$var["R5"] + res[["2022"]]$var["R5"])),
    a22 = res[["2022"]]$atrs, a25 = res[["2025"]]$atrs)
}

rez <- list()
for (g in list(NULL, "Latviesu", "Krievu")) {
  nos <- if (is.null(g)) "Visi" else g
  cat(strrep("=", 78), "\n", nos, "\n", strrep("=", 78), "\n", sep = "")
  cat(sprintf("  %-30s %7s %7s %6s %8s %14s\n",
              "slieksnis", "R1 Δ", "R5 Δ", "SE", "noņemts", "ātrie 22→25 %"))
  for (sn in names(SLIEKSNI)) {
    r <- viens(SLIEKSNI[[sn]], g)
    zv <- if (!is.na(r["s5"]) && abs(r["d5"] / r["s5"]) > 1.96) "*" else " "
    cat(sprintf("  %-30s %7.2f %6.2f%s %6.2f %7.1f %% %6.1f → %5.1f\n",
                sn, r["d1"], r["d5"], zv, r["s5"],
                100 * (1 - r["d5"] / r["d1"]), r["a22"], r["a25"]))
    rez[[length(rez) + 1]] <- data.frame(
      grupa = nos, slieksnis = sn, r1_parmaina = r["d1"], r5_parmaina = r["d5"],
      r5_se = r["s5"], nonemts_proc = 100 * (1 - r["d5"] / r["d1"]),
      atrie_2022 = r["a22"], atrie_2025 = r["a25"])
  }
  cat("\n")
}

cat(strrep("=", 78), "\nKopsavilkums\n", strrep("=", 78), "\n\n", sep = "")
df <- do.call(rbind, rez); rownames(df) <- NULL
for (g in unique(df$grupa)) {
  s <- df[df$grupa == g & df$slieksnis != "normatīvs 10 %, bāze 2025", ]
  cat(sprintf("  %-10s R5 pārmaiņa no %6.2f līdz %6.2f, noņemts no %.0f līdz %.0f %%\n",
              g, min(s$r5_parmaina), max(s$r5_parmaina),
              min(s$nonemts_proc), max(s$nonemts_proc)))
}

write.csv(df, "output/pisa2025/jutigums_slieksnim.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/jutigums_slieksnim.csv\n ",
    output_file, "\n\nPabeigts:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
