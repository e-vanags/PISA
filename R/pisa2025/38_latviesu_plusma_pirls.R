# ============================================================================
# 38. A3. Latviešu valodas plūsmas lasītprasme 4. klasē, 2001 līdz 2021
# ============================================================================
#
# Plāna solis A3. Jautājums: kad datos pirmo reizi parādījās pazīmes, ka
# latviešu valodas plūsmas lasītprasme pasliktinās.
#
# LATVIJAS DALĪBA. Pārbaudīts valstu kodos pašos datos:
#   PIRLS 2001  piedalījās
#   PIRLS 2006  piedalījās
#   PIRLS 2011  NEPIEDALĪJĀS
#   PIRLS 2016  piedalījās
#   PIRLS 2021  piedalījās
#   TIMSS 2011 un 2015 4. klasē NEPIEDALĪJĀS, 2019 piedalījās
#
# Tātad 4. klases lasītprasmes rindā ir četri punkti un desmit gadu caurums
# starp 2006. un 2016. gadu. Tas ir jāatzīmē rakstā, jo tieši tajā desmitgadē
# varēja notikt pārmaiņa, ko neviens neizmērīja.
#
# DEFINĪCIJAS. 2001. gada failā mājās runātās valodas mainīgā nav, ir tikai
# testa valoda. Tāpēc:
#   pamatdefinīcija    testa valoda latviešu, pieejama visos četros ciklos
#   stingrā definīcija testa valoda latviešu UN mājās bieži vai vienmēr runā
#                      testa valodā, pieejama no 2006. gada
#
# SKALA. PIRLS trenda skala ir saistīta ar 2001. gadu, tāpēc sasniegumi ir
# salīdzināmi. Tas ir IEA apgalvojums, un rakstā uz to jāatsaucas.
#
# DZĪVESVIETA. Mainīgais atšķiras katrā ciklā un joslas nesakrīt:
#   2001 ACBGCLOS  četras joslas
#   2006 ACBGCTAS  sešas joslas
#   2016 un 2021 ACBG05A  septiņas joslas
# Vienīgais griezums, kas ir visos četros, ir 100 000 iedzīvotāju. Praksē tas
# nodala Rīgu un Daugavpili no pārējās Latvijas. Smalkāks dalījums nav
# iespējams, un tas ir jāpasaka rakstā.
#
# Izvade:  output/pisa2025/latviesu_plusma_pirls.csv
#          output/pisa2025/latviesu_plusma_pirls_percentiles.csv
#          output/pisa2025/logs/38_latviesu_plusma_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/38_latviesu_plusma_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

library(haven)

FAILI <- c("2001" = "data/iea/pirls2001/ASGLVAR1.sav",
           "2006" = "data/iea/pirls2006/asgLVAr2.sav",
           "2016" = "data/iea/pirls2016/asglvar4.sav",
           "2021" = "data/iea/pirls2021/spss/Data/ASGLVAR5.sav")
PV <- paste0("ASRREA0", 1:5)
SLIEKSNIS <- 400   # PIRLS zemākais atskaites līmenis

SKOLU_FAILI <- c("2001" = "data/iea/pirls2001/ACGLVAr1.sav",
                 "2006" = "data/iea/pirls2006/acglvar2.sav",
                 "2016" = "data/iea/pirls2016/acglvar4.sav",
                 "2021" = "data/iea/pirls2021/spss/Data/ACGLVAR5.sav")
IEDZ <- c("2001" = "ACBGCLOS", "2006" = "ACBGCTAS",
          "2016" = "ACBG05A",  "2021" = "ACBG05A")

nolasa <- function(p) { d <- read_sav(p); names(d) <- toupper(names(d)); d }
D <- lapply(FAILI, nolasa)

# Skolas atrašanās vietas lielums, pievienots skolēnam pēc IDSCHOOL.
# TRUE nozīmē vairāk nekā 100 000 iedzīvotāju.
for (g in names(D)) {
  sk <- nolasa(SKOLU_FAILI[[g]])
  lab <- as.character(as_factor(sk[[IEDZ[[g]]]]))
  liela <- grepl("^100,?001|More than 500", lab, ignore.case = TRUE)
  liela[grepl("Omitted|invalid|NOT APPLICABLE", lab, ignore.case = TRUE)] <- NA
  D[[g]]$LIELPILSETA <- liela[match(as.numeric(D[[g]]$IDSCHOOL),
                                    as.numeric(sk$IDSCHOOL))]
}

# 2001. un 2006. gadā etiķetes ir vietturi bez valodu nosaukumiem, bet kods 1
# ir valsts galvenā testa valoda, proti, latviešu, un kods 2 ir krievu. To
# apstiprina grupu lielumi un sasniegumu līmeņi, kas sakrīt ar 2016. un 2021.
# gada ainu.
latviesu_tests <- function(d, gads) {
  lauks <- if ("ITLANG_SA" %in% names(d)) "ITLANG_SA" else "ITLANG"
  if (gads %in% c("2001", "2006")) return(as.numeric(d[[lauks]]) == 1)
  grepl("Latvian", as.character(as_factor(d[[lauks]])))
}

# Mājās runā testa valodā bieži vai vienmēr. Mainīgais atšķiras pa cikliem.
majas_testa_valoda <- function(d) {
  lauks <- intersect(c("ASBG03", "ASBGLNGH"), names(d))
  if (!length(lauks)) return(rep(NA, nrow(d)))
  v <- as.character(as_factor(d[[lauks[1]]]))
  grepl("always", v, ignore.case = TRUE)
}

jk <- function(d, i, fun) {
  w <- as.numeric(d$TOTWGT)[i]
  z <- as.numeric(d$JKZONE)[i]; r <- as.numeric(d$JKREP)[i]
  pv <- lapply(PV, function(p) as.numeric(d[[p]])[i])
  e <- vapply(pv, function(x) fun(x, w), numeric(1))
  est <- mean(e)
  re <- vapply(sort(unique(z)), function(zz) {
    wr <- w; wr[z == zz] <- w[z == zz] * 2 * r[z == zz]; fun(pv[[1]], wr)
  }, numeric(1))
  c(est = est,
    se = sqrt(sum((re - e[1])^2) + (1 + 1 / length(PV)) * stats::var(e)),
    n = sum(i))
}
# Tas pats, bet statistikai, kas atgriež vektoru, piemēram, percentilēm.
jk_vec <- function(d, i, fun) {
  w <- as.numeric(d$TOTWGT)[i]
  z <- as.numeric(d$JKZONE)[i]; r <- as.numeric(d$JKREP)[i]
  pv <- lapply(PV, function(p) as.numeric(d[[p]])[i])
  e <- vapply(pv, function(x) fun(x, w), numeric(length(fun(pv[[1]], w))))
  est <- rowMeans(e)
  re <- vapply(sort(unique(z)), function(zz) {
    wr <- w; wr[z == zz] <- w[z == zz] * 2 * r[z == zz]; fun(pv[[1]], wr)
  }, numeric(nrow(e)))
  list(est = est,
       se = sqrt(rowSums((re - e[, 1])^2) +
                 (1 + 1 / length(PV)) * apply(e, 1, stats::var)))
}

# Svērtā percentile ar lineāru interpolāciju pa svaru kumulatīvo sadalījumu.
sverta_kvantile <- function(x, w, p) {
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- (cumsum(w) - 0.5 * w) / sum(w)
  stats::approx(cw, x, xout = p, rule = 2, ties = "ordered")$y
}
PROC <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90)
PERCENTILES <- function(x, w) sverta_kvantile(x, w, PROC)

VIDEJAIS <- function(x, w) sum(x * w) / sum(w)
ZEM <- function(x, w) 100 * sum(w * (x < SLIEKSNIS)) / sum(w)

cat(strrep("=", 76), "\n1. Izlase un definīcijas\n", strrep("=", 76), "\n\n")
for (g in names(D)) {
  d <- D[[g]]; lv <- latviesu_tests(d, g); mv <- majas_testa_valoda(d)
  cat(sprintf("%-6s kopā %4d   latviešu tests %4d   no tiem mājās latviski %s\n",
              g, nrow(d), sum(lv),
              if (all(is.na(mv))) "nav mainīgā" else as.character(sum(lv & mv, na.rm = TRUE))))
}

cat("\n", strrep("=", 76), "\n2. Latviešu valodas plūsma, vidējie sasniegumi\n",
    strrep("=", 76), "\n\n")
cat(sprintf("  %-6s %9s %6s   %9s %6s   %9s\n", "gads",
            "pamatdef.", "SE", "stingrā", "SE", "zem 400 %"))
rez <- list()
for (g in names(D)) {
  d <- D[[g]]; lv <- latviesu_tests(d, g); mv <- majas_testa_valoda(d)
  a <- jk(d, lv, VIDEJAIS)
  b <- if (all(is.na(mv))) c(est = NA, se = NA, n = NA) else jk(d, lv & mv, VIDEJAIS)
  z <- jk(d, lv, ZEM)
  cat(sprintf("  %-6s %9.1f %6.1f   %9s %6s   %9.1f   n %d\n", g,
              a["est"], a["se"],
              if (is.na(b["est"])) "—" else sprintf("%.1f", b["est"]),
              if (is.na(b["se"]))  "—" else sprintf("%.1f", b["se"]),
              z["est"], a["n"]))
  rez[[length(rez) + 1]] <- data.frame(
    gads = g, pamat = unname(a["est"]), pamat_se = unname(a["se"]),
    stingra = unname(b["est"]), stingra_se = unname(b["se"]),
    zem400 = unname(z["est"]), n = unname(a["n"]))
}

cat("\n", strrep("=", 76), "\n3. Pārmaiņas starp secīgiem mērījumiem\n",
    strrep("=", 76), "\n\n")
df <- do.call(rbind, rez)
for (i in 2:nrow(df)) {
  d <- df$pamat[i] - df$pamat[i - 1]
  s <- sqrt(df$pamat_se[i]^2 + df$pamat_se[i - 1]^2)
  zv <- if (!is.na(d) && !is.na(s) && abs(d / s) > 1.96) "*" else " "
  cat(sprintf("  %s -> %s   %+7.1f (SE %.1f)%s   zem 400: %.1f %% -> %.1f %%\n",
              df$gads[i - 1], df$gads[i], d, s, zv, df$zem400[i - 1], df$zem400[i]))
}
cat(sprintf("\n  2001 -> 2021 kopā  %+7.1f (SE %.1f)\n",
            df$pamat[nrow(df)] - df$pamat[1],
            sqrt(df$pamat_se[nrow(df)]^2 + df$pamat_se[1]^2)))

cat("\n", strrep("=", 76), "\n4. Pa dzimumiem\n", strrep("=", 76), "\n\n")
cat(sprintf("  %-6s %9s %9s %10s\n", "gads", "meitenes", "zēni", "starpība"))
dz <- list()
for (g in names(D)) {
  d <- D[[g]]; lv <- latviesu_tests(d, g); s <- as.character(as_factor(d$ITSEX))
  a <- jk(d, lv & grepl("^Girl|GIRL", s), VIDEJAIS)
  b <- jk(d, lv & grepl("^Boy|BOY", s), VIDEJAIS)
  dd <- a["est"] - b["est"]
  cat(sprintf("  %-6s %9.1f %9.1f %+10.1f (SE %.1f)\n", g, a["est"], b["est"],
              dd, sqrt(a["se"]^2 + b["se"]^2)))
  dz[[g]] <- c(meitenes = unname(a["est"]), zeni = unname(b["est"]))
}
cat(sprintf("\n  zēni 2016 -> 2021  %+.1f     meitenes 2016 -> 2021  %+.1f\n",
            dz[["2021"]]["zeni"] - dz[["2016"]]["zeni"],
            dz[["2021"]]["meitenes"] - dz[["2016"]]["meitenes"]))

# ---------------------------------------------------------------------------
# A4. Vai apakšgals atdalījās agrāk nekā vidējais.
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 76), "\n5. Sadalījums: percentiles\n", strrep("=", 76), "\n\n")
cat(sprintf("  %-6s %s\n", "gads",
            paste(sprintf("%8s", paste0("P", PROC * 100)), collapse = "")))
P <- list()
for (g in names(D)) {
  d <- D[[g]]; r <- jk_vec(d, latviesu_tests(d, g), PERCENTILES)
  P[[g]] <- r
  cat(sprintf("  %-6s %s\n", g, paste(sprintf("%8.1f", r$est), collapse = "")))
  cat(sprintf("  %-6s %s\n", "(SE)", paste(sprintf("%8s", sprintf("(%.1f)", r$se)),
                                           collapse = "")))
}
cat("\n  Pārmaiņas pa percentilēm\n")
for (i in 2:length(names(D))) {
  g1 <- names(D)[i - 1]; g2 <- names(D)[i]
  dd <- P[[g2]]$est - P[[g1]]$est
  ss <- sqrt(P[[g2]]$se^2 + P[[g1]]$se^2)
  cat(sprintf("  %s -> %s %s\n", g1, g2,
              paste(sprintf("%8s", sprintf("%+.1f%s", dd,
                    ifelse(abs(dd / ss) > 1.96, "*", " "))), collapse = "")))
}
cat(sprintf("\n  P90 mīnus P5, izplētums:  %s\n",
    paste(sprintf("%s %.1f  ", names(D),
                  vapply(P, function(x) x$est[6] - x$est[1], numeric(1))),
          collapse = "")))

pdf_rez <- do.call(rbind, lapply(names(P), function(g) data.frame(
  gads = g, percentile = PROC * 100, aplese = P[[g]]$est, se = P[[g]]$se)))
write.csv(pdf_rez, "output/pisa2025/latviesu_plusma_pirls_percentiles.csv",
          row.names = FALSE)

# ---------------------------------------------------------------------------
# A5. Dzīvesvieta. Vienīgais griezums, kas ir visos četros ciklos.
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 76), "\n6. Pēc skolas atrašanās vietas lieluma\n",
    strrep("=", 76), "\n\n")
cat(sprintf("  %-6s %12s %6s   %12s %6s   %9s\n", "gads",
            "virs 100 tūkst", "SE", "līdz 100 tūkst", "SE", "starpība"))
LV <- list()
for (g in names(D)) {
  d <- D[[g]]; lv <- latviesu_tests(d, g); l <- d$LIELPILSETA
  a <- jk(d, lv & !is.na(l) & l,  VIDEJAIS)
  b <- jk(d, lv & !is.na(l) & !l, VIDEJAIS)
  LV[[g]] <- list(a = a, b = b)
  dd <- a["est"] - b["est"]; ss <- sqrt(a["se"]^2 + b["se"]^2)
  cat(sprintf("  %-6s %12.1f %6.1f   %12.1f %6.1f   %+7.1f (%.1f)%s  n %d/%d\n",
              g, a["est"], a["se"], b["est"], b["se"], dd, ss,
              if (abs(dd / ss) > 1.96) "*" else " ", a["n"], b["n"]))
}
cat("\n  Pārmaiņas 2016 -> 2021\n")
cat(sprintf("    virs 100 tūkst   %+.1f (SE %.1f)\n",
            LV[["2021"]]$a["est"] - LV[["2016"]]$a["est"],
            sqrt(LV[["2021"]]$a["se"]^2 + LV[["2016"]]$a["se"]^2)))
cat(sprintf("    līdz 100 tūkst   %+.1f (SE %.1f)\n",
            LV[["2021"]]$b["est"] - LV[["2016"]]$b["est"],
            sqrt(LV[["2021"]]$b["se"]^2 + LV[["2016"]]$b["se"]^2)))

write.csv(df, "output/pisa2025/latviesu_plusma_pirls.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/latviesu_plusma_pirls.csv",
    "\n  output/pisa2025/latviesu_plusma_pirls_percentiles.csv\n ",
    output_file, "\n\nPabeigts:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
