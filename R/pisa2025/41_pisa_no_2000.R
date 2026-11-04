# ============================================================================
# 41. PISA lasītprasme Latvijā no 2000. gada: kas bija zem vidējā
# ============================================================================
#
# KĀPĒC ŠIS SKRIPTS IR. Lietotāja iebildums pret `docs/42` bija tiešs:
# apgalvojumu, ka 2017. un 2019. gadā ziņojumi pamatoti teica "uzlabojas" un
# "stabils", nedrīkst pieņemt no valsts vidējiem rādītājiem. Lasītprasmes
# procesi ir lēni un inerti, pasliktināšanās nesākas vienā gadā, tāpēc
# jāskatās zem virsraksta: sadalījuma apakšgals, dzimumi, resursu grupas.
#
# DATU AVOTS UN TĀ ROBEŽAS. PISA 2000.-2012. gada mikrodati ir aiz OECD
# reģistrācijas formas. Bet CRAN pakotnē `learningtower` ir skolēnu līmeņa
# apakškopa no PISA 2000 līdz 2022, ieskaitot Latviju.
#
#   IR:   lasīšanas rezultāts, skolēna svars, skola, dzimums, ESCS,
#         grāmatu skaits mājās, vecāku izglītība
#   NAV:  mājās runātā valoda, izglītības programma, replikātsvari,
#         10 plausible values
#
# Tātad ar šo NEVAR sadalīt latviešu un krievu valodas plūsmu. Tas paliek atkarīgs no
# īstajiem mikrodatiem. Ar šo VAR redzēt valsts sadalījuma formu no 2000.
#
# PĀRBAUDE. `read` ir viena plausible value, ne desmit vidējais. Pret mūsu
# pašu aprēķinu no īstajiem datiem:
#   2015  488.7 pret 487.8    2018  478.1 pret 478.7    2022  474.9 pret 474.6
# Atšķirība nepārsniedz 0.9 punktus, tātad trendam tas der.
#
# STANDARTKĻŪDAS. Replikātsvaru nav, tāpēc BRR nav iespējams. Lietots
# sāknēšanas atkārtojums pa SKOLĀM, kas atveido divpakāpju izlases galveno
# daļu. Tas nav OECD metode, un rakstā tas ir jāatzīmē.
#
# Izvade:  output/pisa2025/pisa_no_2000.csv
#          output/pisa2025/logs/41_pisa_no_2000_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/41_pisa_no_2000_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

suppressMessages(library(learningtower))
set.seed(20260910)
B <- 300                      # sāknēšanas atkārtojumu skaits
L2 <- 407.47                  # PISA 2. kompetences līmeņa robeža

d <- load_student(year = "all")
lv <- d[d$country == "LVA", ]
lv$read <- as.numeric(lv$read)
lv$w <- as.numeric(lv$stu_wgt)
lv$skola <- as.character(lv$school_id)
lv <- lv[!is.na(lv$read) & !is.na(lv$w), ]
GADI <- sort(unique(as.character(lv$year)))

vid <- function(x, w) sum(x * w) / sum(w)
kvant <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]
  stats::approx((cumsum(w) - 0.5 * w) / sum(w), x, xout = p, rule = 2,
                ties = "ordered")$y }
zem <- function(x, w) 100 * sum(w * (x < L2)) / sum(w)

# Sāknēšana pa skolām: no katras izlases nejauši izvēlas tikpat skolu ar
# atkārtošanu un pārrēķina visu statistiku kopu.
statistikas <- function(x, w) {
  c(videjais = vid(x, w),
    P10 = kvant(x, w, 0.10), P25 = kvant(x, w, 0.25),
    P50 = kvant(x, w, 0.50), P75 = kvant(x, w, 0.75),
    P90 = kvant(x, w, 0.90),
    zem_L2 = zem(x, w),
    izpletums = kvant(x, w, 0.90) - kvant(x, w, 0.10))
}

aplese <- function(sub) {
  est <- statistikas(sub$read, sub$w)
  sk <- unique(sub$skola)
  bs <- replicate(B, {
    izv <- sample(sk, length(sk), replace = TRUE)
    i <- unlist(lapply(izv, function(s) which(sub$skola == s)))
    statistikas(sub$read[i], sub$w[i])
  })
  list(est = est, se = apply(bs, 1, stats::sd))
}

cat(strrep("=", 78), "\n1. Latvijas PISA lasītprasme 2000-2022, viss sadalījums\n",
    strrep("=", 78), "\n\n")
cat(sprintf("  %-6s %8s %8s %8s %8s %8s %8s %9s %10s\n", "gads", "vidējais",
            "P10", "P25", "P50", "P75", "P90", "zem 2.līm", "izplētums"))
R <- list()
for (g in GADI) {
  a <- aplese(lv[as.character(lv$year) == g, ])
  R[[g]] <- a
  cat(sprintf("  %-6s %8.1f %8.1f %8.1f %8.1f %8.1f %8.1f %8.1f %% %10.1f\n", g,
              a$est["videjais"], a$est["P10"], a$est["P25"], a$est["P50"],
              a$est["P75"], a$est["P90"], a$est["zem_L2"], a$est["izpletums"]))
}
cat(sprintf("\n  (SE)   %8.1f %8.1f %8.1f %8.1f %8.1f %8.1f %8.1f   %10.1f  <- pēdējā gada piemērs\n",
            R[[length(R)]]$se["videjais"], R[[length(R)]]$se["P10"],
            R[[length(R)]]$se["P25"], R[[length(R)]]$se["P50"],
            R[[length(R)]]$se["P75"], R[[length(R)]]$se["P90"],
            R[[length(R)]]$se["zem_L2"], R[[length(R)]]$se["izpletums"]))

cat("\n", strrep("=", 78), "\n2. Pārmaiņas starp secīgiem cikliem\n",
    strrep("=", 78), "\n\n")
cat(sprintf("  %-14s %10s %10s %10s %12s\n", "periods", "vidējais", "P10", "P90", "zem 2.līm"))
for (i in 2:length(GADI)) {
  a <- R[[GADI[i]]]; b <- R[[GADI[i - 1]]]
  rinda <- sprintf("  %-14s", paste0(GADI[i - 1], "->", GADI[i]))
  for (v in c("videjais", "P10", "P90", "zem_L2")) {
    dd <- a$est[v] - b$est[v]; ss <- sqrt(a$se[v]^2 + b$se[v]^2)
    rinda <- paste0(rinda, sprintf(" %9s", sprintf("%+.1f%s", dd,
                    if (abs(dd / ss) > 1.96) "*" else " ")))
  }
  cat(rinda, "\n")
}

cat("\n", strrep("=", 78),
    "\n3. Pārbaude: vai lasīšana samazinājās KATRĀ ciklā kopš 2009. gada\n",
    strrep("=", 78), "\n\n")
cat("  Sekundāro pētījumu krājums (2023-12-31) apgalvo tieši to.\n\n")
for (i in which(GADI == "2009"):length(GADI)) {
  if (i == which(GADI == "2009")) { cat(sprintf("  %s  %6.1f   atskaite\n", GADI[i], R[[GADI[i]]]$est["videjais"])); next }
  dd <- R[[GADI[i]]]$est["videjais"] - R[[GADI[i - 1]]]$est["videjais"]
  ss <- sqrt(R[[GADI[i]]]$se["videjais"]^2 + R[[GADI[i - 1]]]$se["videjais"]^2)
  cat(sprintf("  %s  %6.1f   pret iepriekšējo %+6.1f (SE %.1f) %s\n", GADI[i],
              R[[GADI[i]]]$est["videjais"], dd, ss,
              if (dd > 0) "  <- PIEAUGUMS, ne samazinājums" else ""))
}

cat("\n", strrep("=", 78), "\n4. Pa dzimumiem\n", strrep("=", 78), "\n\n")
cat(sprintf("  %-6s %10s %10s %10s   %12s %12s\n", "gads", "meitenes", "zēni",
            "starpība", "zēni zem L2", "meit. zem L2"))
for (g in GADI) {
  s <- lv[as.character(lv$year) == g, ]
  gm <- as.character(s$gender)
  a <- s[gm == "female" & !is.na(gm), ]; b <- s[gm == "male" & !is.na(gm), ]
  if (nrow(a) < 50 || nrow(b) < 50) { cat(sprintf("  %-6s dzimums nav pieejams\n", g)); next }
  cat(sprintf("  %-6s %10.1f %10.1f %+10.1f   %11.1f %% %11.1f %%\n", g,
              vid(a$read, a$w), vid(b$read, b$w),
              vid(a$read, a$w) - vid(b$read, b$w),
              zem(b$read, b$w), zem(a$read, a$w)))
}

cat("\n", strrep("=", 78), "\n5. Pa grāmatu skaitam mājās\n", strrep("=", 78), "\n\n")
lv$gram <- as.character(lv$book)
kat <- names(sort(table(lv$gram), decreasing = TRUE))
kat <- kat[!is.na(kat)][1:6]
cat(sprintf("  %-6s %s\n", "gads", paste(sprintf("%14s", kat), collapse = "")))
for (g in GADI) {
  s <- lv[as.character(lv$year) == g, ]
  v <- vapply(kat, function(k) { x <- s[!is.na(s$gram) & s$gram == k, ]
    if (nrow(x) < 40) NA_real_ else vid(x$read, x$w) }, numeric(1))
  cat(sprintf("  %-6s %s\n", g, paste(sprintf("%14s", ifelse(is.na(v), "-", sprintf("%.1f", v))), collapse = "")))
}

df <- do.call(rbind, lapply(GADI, function(g) data.frame(
  gads = g, statistika = names(R[[g]]$est),
  aplese = unname(R[[g]]$est), se = unname(R[[g]]$se))))
write.csv(df, "output/pisa2025/pisa_no_2000.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/pisa_no_2000.csv\n ", output_file,
    "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
