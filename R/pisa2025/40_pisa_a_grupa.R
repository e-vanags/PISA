# ============================================================================
# 40. A6. PISA lasītprasme A grupai, 2015 līdz 2025
# ============================================================================
#
# Plāna solis A6. `docs/38` dod 4. klases līkni no 2001. gada. Šis dod to pašu
# 15 gadu vecumā, cik tālu PISA dati atļauj, proti, no 2015. gada. PISA
# 2000.–2012. gada dati ir aiz OECD reģistrācijas formas, un tos var iegūt
# tikai lietotājs pats.
#
# GRUPAS DEFINĪCIJA. Vienāda visos četros ciklos, tāpēc visi cikli tiek
# izvilkti no izejas failiem ar vienu un to pašu mainīgo sarakstu. 2018. gada
# iepriekšējā izvilkumā `PROGN` nebija, tāpēc to nedrīkst lietot.
#
#   A grupa   LANGN latviešu UN programma nav mazākumtautību
#   B grupa   LANGN krievu   UN programma ir mazākumtautību
#   trešā     pārējie, proti, valodas un programmas nesakritība
#
# ATRADUMS, KAS MAINA DEFINĪCIJU. PISA 2015. gada datos mazākumtautību
# programmas koda NAV VISPĀR: visi Latvijas skolēni ir "Latvia: Basic
# education" bez mazākumtautību varianta. Administratīvā pazīme parādās
# tikai 2018. gadā. Tāpēc četru ciklu rindai definīcija var būt TIKAI pēc
# valodas. Abas definīcijas tiek rēķinātas blakus, un 2018.–2025. gadā tās
# gandrīz sakrīt, kas attaisno valodas definīcijas lietošanu no 2015. gada.
#
# KONTROLE. Lietuva ar titulvalodu mājās, tāpat kā PIRLS analīzē skriptā 39.
#
# METODE. 10 plausible values, BRR ar Fay koeficientu 0.5, proti, dalītājs 20,
# kā OECD rokasgrāmatā. Imputācijas dispersija pēc Rubina likuma.
#
# UZMANĪBU. Šī ir OFICIĀLĀ PV skala. `docs/23` un `docs/28` rāda, ka daļa no
# 2025. gada krituma ir formāta artefakts, kas oficiālajā skalā ir iekšā.
# Tāpēc šī līkne ir jālasa kopā ar uzdevumu līmeņa korekciju, ne tās vietā.
#
# Izvade:  data/pisa2025/processed/a_grupa_pisa.rds  (kešs)
#          output/pisa2025/pisa_a_grupa.csv
#          output/pisa2025/logs/40_pisa_a_grupa_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/40_pisa_a_grupa_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

library(haven)

VALSTIS <- c("LVA", "LTU", "EST")
PV  <- paste0("PV", 1:10, "READ")
RW  <- paste0("W_FSTURWT", 1:80)
PAMAT <- c("CNT", "CNTSTUID", "CNTSCHID", "LANGN", "PROGN", "ST004D01T",
           "ESCS", "W_FSTUWT")
VAJAG <- c(PAMAT, PV, RW)

KESS <- "data/pisa2025/processed/a_grupa_pisa.rds"

izvelk_sav <- function(cels) {
  n <- names(read_sav(cels, n_max = 0))
  sel <- n[toupper(n) %in% VAJAG]
  d <- read_sav(cels, col_select = all_of(sel))
  names(d) <- toupper(names(d))
  d[as.character(d$CNT) %in% VALSTIS, ]
}

if (file.exists(KESS)) {
  D <- readRDS(KESS)
  cat("Kešs nolasīts:", KESS, "\n")
} else {
  cat("Izejas failu lasīšana, tas prasa laiku.\n")
  D <- list()
  D[["2015"]] <- izvelk_sav("data/pisa2015/raw/CY6_MS_CMB_STU_QQQ.sav")
  cat("  2015 gatavs\n")
  D[["2018"]] <- izvelk_sav("data/pisa2018/raw/STU/CY07_MSU_STU_QQQ.sav")
  cat("  2018 gatavs\n")
  D[["2022"]] <- izvelk_sav("data/pisa2022/raw/CY08MSP_STU_QQQ.SAV")
  cat("  2022 gatavs\n")
  d25 <- do.call(rbind, lapply(
    c("data/pisa2025/processed/pisa_lva_2025_stu.rds",
      "data/pisa2025/processed/pisa_ltu_2025_stu.rds",
      "data/pisa2025/processed/pisa_est_2025_stu.rds"),
    function(f) { s <- readRDS(f); names(s) <- toupper(names(s))
                  s[, intersect(VAJAG, names(s))] }))
  D[["2025"]] <- d25
  cat("  2025 gatavs\n")
  saveRDS(D, KESS)
}

# ---------------------------------------------------------------------------
# Grupas. Definīcija ir viena visos ciklos.
# ---------------------------------------------------------------------------
mazakumtautibu <- function(prog) {
  p <- as.character(as_factor(prog))
  grepl("minorit|mazākumt|Russian|bilingual", p, ignore.case = TRUE)
}
grupa <- function(d) {
  lang <- as.character(as_factor(d$LANGN))
  mt <- mazakumtautibu(d$PROGN)
  g <- rep(NA_character_, nrow(d))
  g[grepl("Latvian|Lithuanian|Estonian", lang) & !mt] <- "A"
  g[grepl("Russian", lang) & mt] <- "B"
  g[is.na(g) & !is.na(lang) & lang != "Missing"] <- "tresa"
  g
}

# Tikai valoda. Vienīgā definīcija, kas ir iespējama visos četros ciklos.
grupa_valoda <- function(d) {
  lang <- as.character(as_factor(d$LANGN))
  g <- rep(NA_character_, nrow(d))
  g[grepl("Latvian|Lithuanian|Estonian", lang)] <- "A"
  g[grepl("Russian", lang)] <- "B"
  g
}

cat("\n", strrep("=", 76), "\n1. Izlase un grupas\n", strrep("=", 76), "\n\n")
for (g in names(D)) {
  d <- D[[g]]; d <- d[as.character(d$CNT) == "LVA", ]
  gr <- grupa(d)
  cat(sprintf("%-6s Latvija n=%4d   A=%4d (%4.1f %%)   B=%4d (%4.1f %%)   trešā=%4d   nav=%3d\n",
              g, nrow(d), sum(gr == "A", na.rm = TRUE), 100 * mean(gr == "A", na.rm = TRUE),
              sum(gr == "B", na.rm = TRUE), 100 * mean(gr == "B", na.rm = TRUE),
              sum(gr == "tresa", na.rm = TRUE), sum(is.na(gr))))
}

# ---------------------------------------------------------------------------
# BRR ar Fay 0.5. Dalītājs 20.
# ---------------------------------------------------------------------------
brr <- function(d, i, fun) {
  if (sum(i, na.rm = TRUE) < 30) return(c(est = NA, se = NA, n = sum(i, na.rm = TRUE)))
  i[is.na(i)] <- FALSE
  w  <- as.numeric(d$W_FSTUWT)[i]
  pv <- lapply(PV, function(p) as.numeric(d[[p]])[i])
  e  <- vapply(pv, function(x) fun(x, w), numeric(1))
  rv <- vapply(RW, function(r) fun(pv[[1]], as.numeric(d[[r]])[i]), numeric(1))
  c(est = mean(e),
    se = sqrt(sum((rv - e[1])^2) / 20 + (1 + 1 / length(PV)) * stats::var(e)),
    n = sum(i))
}
VIDEJAIS <- function(x, w) sum(x * w) / sum(w)
ZEM2 <- function(x, w) 100 * sum(w * (x < 407.47)) / sum(w)   # zem 2. līmeņa
P10 <- function(x, w) { o <- order(x); x <- x[o]; w <- w[o]
  stats::approx((cumsum(w) - 0.5 * w) / sum(w), x, xout = 0.10, rule = 2,
                ties = "ordered")$y }

rindas <- list()
apleses <- function(nos, valsts, gr_nos, pec_valodas = FALSE) {
  for (g in names(D)) {
    d <- D[[g]]; d <- d[as.character(d$CNT) == valsts, ]
    fn <- if (pec_valodas) grupa_valoda else grupa
    i <- if (is.na(gr_nos)) rep(TRUE, nrow(d)) else fn(d) == gr_nos
    a <- brr(d, i, VIDEJAIS); z <- brr(d, i, ZEM2); p <- brr(d, i, P10)
    rindas[[length(rindas) + 1]] <<- data.frame(
      rinda = nos, gads = g, aplese = unname(a["est"]), se = unname(a["se"]),
      zem2 = unname(z["est"]), zem2_se = unname(z["se"]),
      p10 = unname(p["est"]), p10_se = unname(p["se"]), n = unname(a["n"]))
  }
}
apleses("LV A, tikai valoda",  "LVA", "A", TRUE)
apleses("LV A, ar programmu",  "LVA", "A")
apleses("LV visi",             "LVA", NA)
apleses("LV B, tikai valoda",  "LVA", "B", TRUE)
apleses("LT A, tikai valoda",  "LTU", "A", TRUE)
apleses("EE A, tikai valoda",  "EST", "A", TRUE)

df <- do.call(rbind, rindas)

cat("\n", strrep("=", 76), "\n2. Lasītprasme, PISA skala\n", strrep("=", 76), "\n\n")
for (r in unique(df$rinda)) {
  x <- df[df$rinda == r, ]
  cat(sprintf("  %-20s", r))
  for (i in seq_len(nrow(x))) cat(sprintf(" %s %6.1f (%.1f)", x$gads[i], x$aplese[i], x$se[i]))
  cat("\n")
}

cat("\n", strrep("=", 76), "\n3. Pārmaiņas, A grupa\n", strrep("=", 76), "\n\n")
for (r in unique(df$rinda)) {
  x <- df[df$rinda == r, ]
  cat(sprintf("  %-20s", r))
  for (i in 2:nrow(x)) {
    d <- x$aplese[i] - x$aplese[i - 1]; s <- sqrt(x$se[i]^2 + x$se[i - 1]^2)
    cat(sprintf("  %s->%s %+6.1f%s", substr(x$gads[i - 1], 3, 4), substr(x$gads[i], 3, 4),
                d, if (!is.na(d) && abs(d / s) > 1.96) "*" else " "))
  }
  cat("\n")
}

cat("\n", strrep("=", 76),
    "\n3b. Uzkrātā pārmaiņa pret 2015. gadu\n", strrep("=", 76), "\n\n")
cat("  Kolonna 2022 ir tas, kas bija zināms 2023. gada decembrī, proti,\n",
    " PIRMS PISA 2025 mērījuma.\n\n")
cat(sprintf("  %-20s %14s %14s %14s\n", "", "2018-2015", "2022-2015", "2025-2015"))
for (r in unique(df$rinda)) {
  x <- df[df$rinda == r, ]
  cat(sprintf("  %-20s", r))
  for (i in 2:nrow(x)) {
    d <- x$aplese[i] - x$aplese[1]; s2 <- sqrt(x$se[i]^2 + x$se[1]^2)
    cat(sprintf(" %+8.1f (%.1f)%s", d, s2,
                if (!is.na(d) && abs(d / s2) > 1.96) "*" else " "))
  }
  cat("\n")
}

cat("\n", strrep("=", 76), "\n4. Apakšgals: zem 2. līmeņa un P10\n", strrep("=", 76), "\n\n")
for (r in unique(df$rinda)) {
  x <- df[df$rinda == r, ]
  cat(sprintf("  %-20s zem 2. lim.", r))
  for (i in seq_len(nrow(x))) cat(sprintf("  %s %4.1f (%.1f)", x$gads[i], x$zem2[i], x$zem2_se[i]))
  d <- x$zem2[3] - x$zem2[1]; s2 <- sqrt(x$zem2_se[3]^2 + x$zem2_se[1]^2)
  cat(sprintf("   | 2015->2022 %+.1f pp%s\n", d,
              if (!is.na(d) && abs(d / s2) > 1.96) "*" else " "))
  cat(sprintf("  %-20s P10        ", ""))
  for (i in seq_len(nrow(x))) cat(sprintf("  %s %5.1f", x$gads[i], x$p10[i]))
  d <- x$p10[3] - x$p10[1]; s2 <- sqrt(x$p10_se[3]^2 + x$p10_se[1]^2)
  cat(sprintf("   | 2015->2022 %+.1f%s\n", d,
              if (!is.na(d) && abs(d / s2) > 1.96) "*" else " "))
}

write.csv(df, "output/pisa2025/pisa_a_grupa.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/pisa_a_grupa.csv\n ", output_file,
    "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
