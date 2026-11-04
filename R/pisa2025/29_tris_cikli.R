# ============================================================================
# 29. Trīs cikli: 2018, 2022, 2025 uz tiem pašiem uzdevumiem
# ============================================================================
#
# Jautājums. Vai lasītprasmes kritums sākās jau pirms 2025. gada. OECD
# publicētie valstu vidējie rāda, ka 2018 un 2022 ir gandrīz identiski, bet
# tie ir IRT skalā. Šeit to pārbauda uz neapstrādātiem uzdevumu līmeņa
# datiem, tajos pašos 196 lasīšanas uzdevumos, kas ir visos trijos ciklos.
#
# IEROBEŽOJUMS, KAS NOSAKA DIZAINU. 2018. gada failos NAV kognitīvo uzdevumu
# atbildes laiku. Pārbaudīts: COG failā CR..TT kolonnu ir nulle, un TIM fails
# satur tikai anketas laikus. Tāpēc ātrās minēšanas korekciju 2018. gadam
# aprēķināt nevar, un trīs ciklu salīdzinājums iespējams tikai pēc diviem
# noteikumiem:
#
#   R1  nesasniegtie netiek vērtēti, kā to dara PISA
#   R2  nesasniegtie skaitīti kā nepareizi, konsekventi visos ciklos
#
# R2 ir svarīgāks, jo tieši nesasniegto uzdevumu izzušana ir viena no divām
# formāta artefakta sastāvdaļām, un R2 to izlīdzina visos trijos ciklos.
#
# Ievade:  data/pisa2018/raw/COG/CY07_MSU_STU_COG.sav
#          data/pisa2018/raw/STU/CY07_MSU_STU_QQQ.sav
#          data/pisa2025/processed/uzdevumu_limenis.rds  (2022 un 2025)
# Izvade:  data/pisa2018/processed/pisa_{lva,est}_2018_stu.rds
#          data/pisa2018/processed/uzdevumi_2018.rds
#          output/pisa2025/tris_cikli.csv
#          output/pisa2025/logs/29_tris_cikli_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
dir.create("data/pisa2018/processed", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/29_tris_cikli_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

library(haven); library(tidyselect)
source("R/pisa2025/00_kopigas_funkcijas.R")

U <- readRDS("data/pisa2025/processed/uzdevumu_limenis.rds"); uzd <- U$uzd
VALSTIS <- c("LVA", "EST")
RW <- paste0("W_FSTURWT", 1:80)
PV <- paste0("PV", 1:10, "READ")


# === 1. 2018. GADA DATU SAGATAVOŠANA ========================================
cat(strrep("=", 70), "\n1. 2018. gada dati\n", strrep("=", 70), "\n\n")

KESS_STU <- "data/pisa2018/processed/stu_2018.rds"
KESS_UZD <- "data/pisa2018/processed/uzdevumi_2018.rds"

if (file.exists(KESS_STU)) {
  stu18 <- readRDS(KESS_STU); cat("Nolasīts kešs:", KESS_STU, "\n")
} else {
  cat("Lasu 2018 STU failu...\n")
  vajag <- c("CNT", "CNTSTUID", "CNTSCHID", "LANGN", "LANGTEST_COG",
             "W_FSTUWT", "ESCS", RW, PV)
  stu18 <- read_sav("data/pisa2018/raw/STU/CY07_MSU_STU_QQQ.sav",
                    col_select = all_of(vajag))
  stu18 <- stu18[as.character(stu18$CNT) %in% VALSTIS, ]
  saveRDS(stu18, KESS_STU)
  cat("  saglabāts, rindas:", nrow(stu18), "\n")
}

if (file.exists(KESS_UZD)) {
  d18 <- readRDS(KESS_UZD); cat("Nolasīts kešs:", KESS_UZD, "\n")
} else {
  cat("Lasu 2018 COG failu, 196 uzdevumi...\n")
  d18 <- read_sav("data/pisa2018/raw/COG/CY07_MSU_STU_COG.sav", user_na = TRUE,
                  col_select = all_of(c("CNT", "CNTSTUID", paste0(uzd, "S"))))
  d18 <- d18[as.character(d18$CNT) %in% VALSTIS, ]
  saveRDS(d18, KESS_UZD)
  cat("  saglabāts, rindas:", nrow(d18), "\n")
}

cat("\n2018 izlase:\n"); print(table(as.character(stu18$CNT)))
cat("\nLatvijas testa valoda 2018:\n")
print(table(as.character(as_factor(stu18$LANGTEST_COG[as.character(stu18$CNT) == "LVA"]))))


# === 2. VIENOTA SAGATAVOŠANA VISIEM TRIM CIKLIEM =============================
cat("\n", strrep("=", 70), "\n2. Uzdevumu klasifikācija\n", strrep("=", 70), "\n\n")

grupa_no <- function(lab, valsts) {
  titul <- if (valsts == "LVA") "^Latvian" else "^Estonian"
  ifelse(grepl(titul, lab), "titulara",
  ifelse(grepl("^Russian", lab), "krievu", "cita"))
}

# Katram ciklam: kategoriju matrica, punktu matrica, svari, grupa.
sagatavo <- function(cog, stu, valsts, gads) {
  i <- as.character(cog$CNT) == valsts
  S <- vapply(cog[paste0(uzd, "S")], klasificet, character(nrow(cog)))[i, ]
  colnames(S) <- uzd
  j <- match(as.numeric(cog$CNTSTUID[i]),
             as.numeric(stu$CNTSTUID[as.character(stu$CNT) == valsts]))
  sv <- stu[as.character(stu$CNT) == valsts, ]
  list(gads = gads, valsts = valsts, S = S, P = punkti_matrica(S),
       w = as.numeric(sv$W_FSTUWT)[j],
       rw = as.data.frame(lapply(sv[RW], function(z) as.numeric(z)[j])),
       g = grupa_no(as.character(as_factor(sv$LANGN))[j], valsts),
       pv = rowMeans(sapply(PV, function(p) as.numeric(sv[[p]])))[j])
}

stu22 <- readRDS("data/pisa2022/processed/pisa_lva_2022_stu.rds")
stu25 <- readRDS("data/pisa2025/processed/pisa_lva_2025_stu.rds")
est22 <- readRDS("data/pisa2022/processed/pisa_est_2022_stu.rds")
est25 <- readRDS("data/pisa2025/processed/pisa_est_2025_stu.rds")
pieliek_cnt <- function(d, v) { d$CNT <- v; d }

C <- list(
  LVA = list(
    "2018" = sagatavo(d18, stu18, "LVA", 2018),
    "2022" = sagatavo(U$d22, pieliek_cnt(stu22, "LVA"), "LVA", 2022),
    "2025" = sagatavo(U$d25, pieliek_cnt(stu25, "LVA"), "LVA", 2025)),
  EST = list(
    "2018" = sagatavo(d18, stu18, "EST", 2018),
    "2022" = sagatavo(U$d22, pieliek_cnt(est22, "EST"), "EST", 2022),
    "2025" = sagatavo(U$d25, pieliek_cnt(est25, "EST"), "EST", 2025)))

cat("Uzdoto kopīgo uzdevumu skaits vienam skolēnam:\n")
for (v in VALSTIS) for (g in c("2018", "2022", "2025")) {
  z <- C[[v]][[g]]
  cat(sprintf("  %s %s  vidēji %5.1f uzdevumi, skolēni %d\n",
              v, g, mean(rowSums(!is.na(z$S))), nrow(z$S)))
}


# === 3. IEGŪTO PUNKTU ĪPATSVARS =============================================
cat("\n", strrep("=", 70), "\n3. Iegūto punktu īpatsvars no maksimālā\n",
    strrep("=", 70), "\n\n")

pct <- function(z, wt, keep) {
  S <- z$S[keep, ]; P <- z$P[keep, ]; wt <- wt[keep]; wt[is.na(wt)] <- 0
  W <- matrix(wt, nrow = nrow(S), ncol = ncol(S))
  f <- function(sel) 100 * sum(W[sel] * P[sel], na.rm = TRUE) / sum(W[sel])
  c(R1 = f(!is.na(S) & S %in% VERTETAS),
    R2 = f(!is.na(S) & S %in% VERTETAS_AR_NESASNIEGTIEM))
}

aplese <- function(z, g) {
  keep <- if (is.null(g)) rep(TRUE, length(z$w)) else (!is.na(z$g) & z$g == g)
  keep[is.na(keep)] <- FALSE
  est <- pct(z, z$w, keep)
  rp <- vapply(RW, function(r) pct(z, z$rw[[r]], keep), numeric(2))
  list(est = est, var = rowSums((rp - est)^2) / 20, n = sum(keep))
}

rez <- list()
for (v in VALSTIS) {
  cat(strrep("-", 70), "\n", v, "\n", strrep("-", 70), "\n", sep = "")
  for (g in list(NULL, "titulara", "krievu")) {
    nos <- if (is.null(g)) "visi" else g
    a <- lapply(C[[v]], aplese, g = g)
    cat(sprintf("\n  %s\n", nos))
    cat(sprintf("    %-4s %8s %8s   %8s %8s\n", "gads", "R1", "SE", "R2", "SE"))
    for (gd in c("2018", "2022", "2025"))
      cat(sprintf("    %-4s %8.2f %8.2f   %8.2f %8.2f   n = %d\n", gd,
                  a[[gd]]$est["R1"], sqrt(a[[gd]]$var["R1"]),
                  a[[gd]]$est["R2"], sqrt(a[[gd]]$var["R2"]), a[[gd]]$n))
    for (par in list(c("2018", "2022"), c("2022", "2025"), c("2018", "2025"))) {
      for (r in c("R1", "R2")) {
        d <- a[[par[2]]]$est[r] - a[[par[1]]]$est[r]
        s <- sqrt(a[[par[2]]]$var[r] + a[[par[1]]]$var[r])
        zv <- if (abs(d / s) > 1.96) "*" else " "
        cat(sprintf("    %s -> %s  %s  %+7.2f (SE %.2f)%s\n",
                    par[1], par[2], r, d, s, zv))
        rez[[length(rez) + 1]] <- data.frame(
          valsts = v, grupa = nos, noteikums = r,
          no = par[1], lidz = par[2], parmaina = unname(d), se = unname(s))
      }
    }
  }
  cat("\n")
}

df <- do.call(rbind, rez); rownames(df) <- NULL
write.csv(df, "output/pisa2025/tris_cikli.csv", row.names = FALSE)
cat("Izveidotie faili:\n  output/pisa2025/tris_cikli.csv\n ", output_file,
    "\n\nPabeigts:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
