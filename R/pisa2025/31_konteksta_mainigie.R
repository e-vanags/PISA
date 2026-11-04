# ============================================================================
# 31. Vai konteksta mainīgie rādīja kaut ko pirms 2025. gada
# ============================================================================
#
# Jautājums. Skripts 29 parādīja, ka Latvijas krievvalodīgo LASĪŠANA no 2018.
# līdz 2022. gadam nekustējās vispār, un pēc tam sabruka. Bet PISA mēra ne
# tikai lasīšanu. Vai anketas mainīgajos tajā pašā periodā jau bija redzams
# kaut kas, kas 2025. gadu paredzēja.
#
# Tas ir īstais prognozējamības jautājums: nevis vai lasīšanas trends to
# rādīja, bet vai kaut kas cits no tā, ko Latvija jau mērīja, to rādīja.
#
# Mainīgie. Visos trijos ciklos kopīgi ir 9 indeksi un 23 anketas jautājumi.
# Šeit lietoti tie, kas nav tīri identifikatori:
#
#   iesaiste testā     EFFORT1, EFFORT2
#   piederība skolai   BELONG un seši ST034 panti
#   labizjūta          ST016 dzīves apmierinātība
#   pāridarījumi       četri ST038 panti
#   kavējumi           trīs ST062 panti
#   fons               ESCS, HOMEPOS, HISEI, PAREDINT, BSMJ
#
# UZMANĪBU. TEACHSUP nav salīdzināms, jo 2018. gadā tas attiecās uz testa
# valodas stundām, bet 2025. gadā uz dabaszinātņu stundām. Tas ir izslēgts.
#
# Ievade:  data/pisa2018/raw/STU/CY07_MSU_STU_QQQ.sav
#          data/pisa20xx/processed/pisa_{lva,est}_20xx_stu.rds
# Izvade:  output/pisa2025/konteksta_mainigie.csv
#          output/pisa2025/logs/31_konteksta_mainigie_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/31_konteksta_mainigie_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

library(haven); library(tidyselect)

RW <- paste0("W_FSTURWT", 1:80)
MAIN <- c("EFFORT1", "EFFORT2", "BELONG", "ST016Q01NA",
          "ST034Q01TA", "ST034Q02TA", "ST034Q03TA", "ST034Q04TA",
          "ST034Q05TA", "ST034Q06TA",
          "ST038Q04NA", "ST038Q05NA", "ST038Q07NA", "ST038Q08NA",
          "ST062Q01TA", "ST062Q02TA", "ST062Q03TA",
          "ESCS", "HOMEPOS", "HISEI", "PAREDINT", "BSMJ")

KESS <- "data/pisa2018/processed/stu_2018_konteksts.rds"
if (file.exists(KESS)) {
  s18 <- readRDS(KESS); cat("Nolasīts kešs:", KESS, "\n")
} else {
  cat("Lasu 2018 STU failu ar konteksta mainīgajiem...\n")
  s18 <- read_sav("data/pisa2018/raw/STU/CY07_MSU_STU_QQQ.sav",
                  col_select = all_of(c("CNT", "CNTSTUID", "LANGN",
                                        "W_FSTUWT", RW, MAIN)))
  s18 <- s18[as.character(s18$CNT) %in% c("LVA", "EST"), ]
  saveRDS(s18, KESS); cat("  saglabāts, rindas:", nrow(s18), "\n")
}

nolasa <- function(valsts, gads) {
  if (gads == 2018) return(s18[as.character(s18$CNT) == valsts, ])
  d <- readRDS(sprintf("data/pisa%d/processed/pisa_%s_%d_stu.rds",
                       gads, tolower(valsts), gads))
  d$CNT <- valsts
  d
}

grupas <- function(d, valsts) {
  lab <- as.character(as_factor(d$LANGN))
  titul <- if (valsts == "LVA") "^Latvian" else "^Estonian"
  ifelse(grepl(titul, lab), "titulara",
  ifelse(grepl("^Russian", lab), "krievu", "cita"))
}

# Svērtais vidējais ar BRR. Anketas panti ir Likerta skalas, tāpēc vidējais
# ir jāsaprot kā skalas punkti, ne kā procenti.
videjais <- function(d, v, keep) {
  if (!(v %in% names(d))) return(c(est = NA, var = NA, n = 0))
  x <- as.numeric(d[[v]])
  x[!is.finite(x)] <- NA
  i <- keep & !is.na(x)
  if (sum(i) < 30) return(c(est = NA, var = NA, n = sum(i)))
  w <- as.numeric(d$W_FSTUWT)
  sv <- function(wt) sum(x[i] * wt[i]) / sum(wt[i])
  est <- sv(w)
  rp <- vapply(RW, function(r) sv(as.numeric(d[[r]])), numeric(1))
  c(est = est, var = sum((rp - est)^2) / 20, n = sum(i))
}

cat("\n", strrep("=", 78), "\n")
cat("Konteksta mainīgie, svērtie vidējie ar BRR standartkļūdām\n")
cat(strrep("=", 78), "\n")

rez <- list()
for (valsts in c("LVA", "EST")) {
  D <- lapply(c("2018", "2022", "2025"), function(g) nolasa(valsts, as.numeric(g)))
  names(D) <- c("2018", "2022", "2025")
  G <- lapply(names(D), function(g) grupas(D[[g]], valsts)); names(G) <- names(D)

  for (gr in c("titulara", "krievu")) {
    cat("\n", strrep("-", 78), "\n", valsts, " / ", gr, "\n",
        strrep("-", 78), "\n", sep = "")
    cat(sprintf("  %-12s %8s %8s %8s   %14s %14s\n", "mainīgais",
                "2018", "2022", "2025", "2018→2022", "2022→2025"))
    for (v in MAIN) {
      a <- lapply(names(D), function(g) videjais(D[[g]], v, G[[g]] == gr))
      names(a) <- names(D)
      if (any(sapply(a, function(z) is.na(z["est"])))) next
      d1 <- a[["2022"]]["est"] - a[["2018"]]["est"]
      s1 <- sqrt(a[["2022"]]["var"] + a[["2018"]]["var"])
      d2 <- a[["2025"]]["est"] - a[["2022"]]["est"]
      s2 <- sqrt(a[["2025"]]["var"] + a[["2022"]]["var"])
      z1 <- if (abs(d1 / s1) > 1.96) "*" else " "
      z2 <- if (abs(d2 / s2) > 1.96) "*" else " "
      cat(sprintf("  %-12s %8.2f %8.2f %8.2f   %+7.2f (%.2f)%s %+7.2f (%.2f)%s\n",
                  v, a[["2018"]]["est"], a[["2022"]]["est"], a[["2025"]]["est"],
                  d1, s1, z1, d2, s2, z2))
      rez[[length(rez) + 1]] <- data.frame(
        valsts = valsts, grupa = gr, mainigais = v,
        y2018 = unname(a[["2018"]]["est"]), y2022 = unname(a[["2022"]]["est"]),
        y2025 = unname(a[["2025"]]["est"]),
        d_18_22 = unname(d1), se_18_22 = unname(s1),
        d_22_25 = unname(d2), se_22_25 = unname(s2))
    }
  }
}

cat("\n", strrep("=", 78), "\n* nozīmīgs pie 95 %\n", sep = "")
cat("\nSkalu virzieni atšķiras. ST038 pāridarījumi un ST062 kavējumi ir\n")
cat("biežuma skalas, kur AUGSTĀKS skaitlis nozīmē biežāk. ST034 piederība un\n")
cat("ST016 apmierinātība ir tā, ka augstāks ir labāk. Zīme jāinterpretē\n")
cat("katram mainīgajam atsevišķi.\n")

df <- do.call(rbind, rez); rownames(df) <- NULL
write.csv(df, "output/pisa2025/konteksta_mainigie.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/konteksta_mainigie.csv\n ", output_file,
    "\n\nPabeigts:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
