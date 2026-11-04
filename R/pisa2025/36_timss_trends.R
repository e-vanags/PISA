# ============================================================================
# 36. TIMSS 2015 pret 2019: vienīgais PISA 2025 kohortas agrīnais mērījums
# ============================================================================
#
# Kāpēc tieši TIMSS. Kohortu saskaņošana (`docs/35`) rāda, ka PISA 2025
# kohortai lasītprasmes mērījuma sākumskolā nav vispār: PIRLS 2016 mēra
# 2005. gada kohortu, PIRLS 2021 mēra 2010. gada kohortu, bet PISA 2025 ir
# 2009. gads. Vienīgais šīs kohortas mērījums 4. klasē ir TIMSS 2019.
#
# Un tas sader vēl labāk, nekā izskatās:
#
#   TIMSS 2015, 4. klase, dzimuši ~2004.5  ->  PISA 2022, dzimuši 2006
#   TIMSS 2019, 4. klase, dzimuši ~2008.5  ->  PISA 2025, dzimuši 2009
#
# Tātad TIMSS trends 2015 pret 2019 ir tas pats kohortu pāris, kas PISA
# trendā 2022 pret 2025. Ja TIMSS jau rādīja kritumu, tad kohorta bija
# vājāka jau 4. klasē, un PISA 2025 rezultāts nav pēkšņs.
#
# UZMANĪBU. TIMSS mēra matemātiku un dabaszinātnes, ne lasīšanu. Tāpēc tas
# nav tiešs lasītprasmes brīdinājums, bet tas ir vispārējs kohortas
# rādītājs.
#
# Metode: 5 plausible values, jackknife JK2, kā skriptā 34.
#
# Izvade:  output/pisa2025/timss_trends.csv
#          output/pisa2025/logs/36_timss_trends_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/36_timss_trends_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

library(haven)

atrod <- function(mape) {
  f <- list.files(mape, pattern = "^asglva", recursive = TRUE,
                  full.names = TRUE, ignore.case = TRUE)
  if (!length(f)) return(NA_character_)
  f[1]
}
nolasa <- function(cels) { d <- read_sav(cels); names(d) <- toupper(names(d)); d }

CELI <- c("2015" = atrod("data/iea/timss2015"),
          "2019" = atrod("data/iea/timss2019"),
          "2023" = atrod("data/iea/timss2023/g4"))
cat("Faili:\n"); for (g in names(CELI)) cat(sprintf("  %s: %s\n", g, CELI[[g]]))
if (is.na(CELI[["2019"]])) stop("Nav TIMSS 2019 faila")

D <- list()
for (g in names(CELI)) if (!is.na(CELI[[g]])) D[[g]] <- nolasa(CELI[[g]])

# Jackknife JK2, tāpat kā PIRLS.
jk <- function(d, i, PV) {
  w <- as.numeric(d$TOTWGT)[i]
  zona <- as.numeric(d$JKZONE)[i]; rep <- as.numeric(d$JKREP)[i]
  pv <- lapply(PV, function(p) as.numeric(d[[p]])[i])
  sv <- function(x, wt) sum(x * wt) / sum(wt)
  est_pv <- vapply(pv, sv, numeric(1), wt = w)
  est <- mean(est_pv)
  rep_est <- vapply(sort(unique(zona)), function(z) {
    wr <- w; wr[zona == z] <- w[zona == z] * 2 * rep[zona == z]
    sv(pv[[1]], wr)
  }, numeric(1))
  c(est = est,
    se = sqrt(sum((rep_est - est_pv[1])^2) +
              (1 + 1 / length(PV)) * stats::var(est_pv)),
    n = sum(i))
}

# TIMSS 2023 failā testa valodas mainīgais ir ITLANG_SQ, agrākajos ITLANG_SA.
valoda <- function(d) {
  lauks <- intersect(c("ITLANG_SA", "ITLANG_SQ", "ITLANG"), names(d))[1]
  v <- as.character(as_factor(d[[lauks]]))
  ifelse(grepl("Latvian", v), "latviešu", ifelse(grepl("Russian", v), "krievu", NA))
}
for (g in names(D)) D[[g]]$VAL <- valoda(D[[g]])

cat("\n", strrep("=", 74), "\n1. Izlase\n", strrep("=", 74), "\n\n")
for (g in names(D)) { cat(g, ": ", nrow(D[[g]]), " skolēni\n", sep = "")
  print(table(D[[g]]$VAL, useNA = "ifany")); cat("\n") }

DOMENI <- list(Matemātika = paste0("ASMMAT0", 1:5),
               Dabaszinātnes = paste0("ASSSCI0", 1:5))
GRUPAS <- list(c("Visi", NA), c("Mācību valoda latviešu", "latviešu"),
               c("Mācību valoda krievu", "krievu"))

rez <- list()
for (dn in names(DOMENI)) {
  cat(strrep("=", 74), "\n", dn, "\n", strrep("=", 74), "\n\n", sep = "")
  cat(sprintf("  %-24s %8s %6s   %8s %6s   %9s\n", "grupa",
              "2015", "SE", "2019", "SE", "pārmaiņa"))
  for (gr in GRUPAS) {
    a <- b <- d23 <- NULL
    for (g in names(D)) {
      i <- if (is.na(gr[2])) rep(TRUE, nrow(D[[g]])) else
           (!is.na(D[[g]]$VAL) & D[[g]]$VAL == gr[2])
      r <- jk(D[[g]], i, DOMENI[[dn]])
      if (g == "2015") a <- r else if (g == "2019") b <- r else d23 <- r
    }
    if (is.null(a)) {
      cat(sprintf("  %-24s %8s %6s   %8.1f %6.1f   %9s  n %d", 
                  gr[1], "—", "—", b["est"], b["se"], "—", b["n"]))
      if (!is.null(d23)) { dd <- d23["est"] - b["est"]; ss <- sqrt(b["se"]^2 + d23["se"]^2)
        cat(sprintf("   | 2023: %.1f (%.1f)  2019->2023 %+.1f (%.1f)%s", d23["est"], d23["se"],
                    dd, ss, if (abs(dd/ss) > 1.96) "*" else " ")) }
      cat("\n")
      rez[[length(rez) + 1]] <- data.frame(domens = dn, grupa = gr[1],
        y2015 = NA, se2015 = NA, y2019 = unname(b["est"]),
        se2019 = unname(b["se"]), parmaina = NA, se_parm = NA)
    } else {
      d <- b["est"] - a["est"]; s <- sqrt(a["se"]^2 + b["se"]^2)
      zv <- if (abs(d / s) > 1.96) "*" else " "
      cat(sprintf("  %-24s %8.1f %6.1f   %8.1f %6.1f   %+7.1f (%.1f)%s  n %d/%d\n",
                  gr[1], a["est"], a["se"], b["est"], b["se"], d, s, zv,
                  a["n"], b["n"]))
      rez[[length(rez) + 1]] <- data.frame(domens = dn, grupa = gr[1],
        y2015 = unname(a["est"]), se2015 = unname(a["se"]),
        y2019 = unname(b["est"]), se2019 = unname(b["se"]),
        parmaina = unname(d), se_parm = unname(s))
    }
  }
  cat("\n")
}

df <- do.call(rbind, rez); rownames(df) <- NULL
write.csv(df, "output/pisa2025/timss_trends.csv", row.names = FALSE)
cat("Izveidotie faili:\n  output/pisa2025/timss_trends.csv\n ", output_file,
    "\n\nPabeigts:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
