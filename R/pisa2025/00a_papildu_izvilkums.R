# 0. posms (docs/51): papildu mainīgie sastāva korekcijām 2015.-2022. gadam,
# kuru nav kešā a_grupa_pisa.rds, un ESCS skalas pārbaude 2022. pret 2025. gadu.
#
# IZVADE
#   data/pisa2025/processed/ab_papildu_2015_2022.rds   Latvijas skolēni, pēc CNTSTUID
#   output/pisa2025/escs_skalas_parbaude.csv
#
# SKALAS PĀRBAUDES LOĢIKA. ESCS ir viena kopēja formula visām valstīm, kas
# apvieno HISEI, PAREDINT un HOMEPOS. HISEI (profesijas statuss) un PAREDINT
# (vecāku izglītības gadi) ir fiksētās mērvienībās. Ja 2025. gada ESCS ir tajā
# pašā skalā kā 2022. gadā, tad regresijā ESCS ~ HISEI + PAREDINT + HOMEPOS
# koeficientiem pie HISEI un PAREDINT un brīvajam loceklim jābūt gandrīz
# vienādiem abos ciklos. Koeficients pie HOMEPOS var atšķirties, ja HOMEPOS
# ir skalēts no jauna. Tas pats tiek pārbaudīts trenda failam (2012-2018).

suppressPackageStartupMessages(library(haven))

F <- c("2015" = "data/pisa2015/raw/CY6_MS_CMB_STU_QQQ.sav",
       "2018" = "data/pisa2018/raw/STU/CY07_MSU_STU_QQQ.sav",
       "2022" = "data/pisa2022/raw/CY08MSP_STU_QQQ.SAV")
V <- c("CNT", "CNTSTUID", "ST001D01T", "GRADE", "LANGTEST_COG", "IMMIG",
       "HISEI", "PAREDINT", "HOMEPOS", "EFFORT1", "EFFORT2", "ESCS", "W_FSTUWT")
VALSTIS <- c("LVA", "LTU", "EST")

koef <- function(d, gads, cnt, avots) {
  d <- d[stats::complete.cases(d[, c("ESCS", "HISEI", "PAREDINT", "HOMEPOS", "W")]), ]
  m <- stats::lm(ESCS ~ HISEI + PAREDINT + HOMEPOS, data = d, weights = W)
  k <- stats::coef(m)
  data.frame(gads = gads, cnt = cnt, avots = avots, n = nrow(d),
             brivais = k[[1]], hisei = k[["HISEI"]], paredint = k[["PAREDINT"]],
             homepos = k[["HOMEPOS"]], r2 = summary(m)$r.squared,
             escs_vid = sum(d$W * d$ESCS) / sum(d$W),
             hisei_vid = sum(d$W * d$HISEI) / sum(d$W),
             paredint_vid = sum(d$W * d$PAREDINT) / sum(d$W),
             homepos_vid = sum(d$W * d$HOMEPOS) / sum(d$W))
}

papildu <- list(); skala <- list()
for (g in names(F)) {
  v <- intersect(V, names(read_sav(F[[g]], n_max = 0)))
  d <- read_sav(F[[g]], col_select = tidyselect::all_of(v))
  d <- d[as.character(d$CNT) %in% VALSTIS, ]
  # Testa valodas etiķete jāsaglabā pirms pārveides par skaitli, 46. skripts
  # plūsmu testa valodu nosaka pēc tās ("Latvian" / "Russian").
  lab <- as.character(as_factor(d$LANGTEST_COG))
  d <- as.data.frame(lapply(d, function(x) if (is.character(x)) x else as.numeric(x)))
  d$LANGTEST_LAB <- lab
  # 2015. gada failā nav HISEI un PAREDINT; tam gadam tos dod trenda fails.
  for (k in setdiff(V, names(d))) d[[k]] <- NA_real_
  lv <- d[d$CNT == "LVA", c(setdiff(V, c("ESCS", "W_FSTUWT")), "LANGTEST_LAB")]
  lv$gads <- g
  papildu[[g]] <- lv
  cat(sprintf("%s: %d Latvijas skolēni, mainīgie: %s\n", g, nrow(lv), paste(v, collapse = " ")))
  if (g == "2022") {
    d$W <- d$W_FSTUWT
    for (cn in VALSTIS) skala[[paste(g, cn)]] <- koef(d[d$CNT == cn, ], g, cn, "fails")
  }
}
saveRDS(do.call(rbind, papildu), "data/pisa2025/processed/ab_papildu_2015_2022.rds")

for (cn in VALSTIS) {
  d <- readRDS(sprintf("data/pisa2025/processed/pisa_%s_2025_stu.rds", tolower(cn)))
  names(d) <- toupper(names(d))
  d <- data.frame(ESCS = as.numeric(d$ESCS), HISEI = as.numeric(d$HISEI),
                  PAREDINT = as.numeric(d$PAREDINT), HOMEPOS = as.numeric(d$HOMEPOS),
                  W = as.numeric(d$W_FSTUWT))
  skala[[paste("2025", cn)]] <- koef(d, "2025", cn, "fails")
}

t <- utils::read.csv("data/pisa2022/escs_trend/escs_trend.csv")
t <- t[t$cnt == "LVA", ]
for (k in c(5, 6, 7)) {
  x <- t[t$cycle == k, ]
  d <- data.frame(ESCS = x$escs_trend, HISEI = x$hisei_trend, PAREDINT = x$paredint_trend,
                  HOMEPOS = x$homepos_trend, W = 1)
  skala[[paste("trend", k)]] <- koef(d, c("5" = "2012", "6" = "2015", "7" = "2018")[[as.character(k)]],
                                     "LVA", "trenda fails (nesvērts)")
}

s <- do.call(rbind, skala); rownames(s) <- NULL
utils::write.csv(s, "output/pisa2025/escs_skalas_parbaude.csv", row.names = FALSE)
print(format(s, digits = 4), row.names = FALSE)
