# ============================================================================
# 48. Izslēgšanas nobīdes jutīguma robežas
# ============================================================================
#
# IDEJA. Ja no izlases izslēgtie skolēni ir vājāki lasītāji, tad izlases
# vidējais ir pārvērtēts. Cik stipri, nav zināms, jo nav zināma sakarība
# starp izslēgšanu un sniegumu. Tāpēc vienas labotas vērtības vietā tiek
# rēķināts diapazons:
#   rho = 0    izslēgtie ir tādi paši kā pārējie, nobīdes nav
#   rho = 0,5  OECD ilustratīvais pieņēmums (PISA 2022 Vol. I, A2 pielikums)
#
# FORMULA. Nogriezts normālais sadalījums: izslēdz daļu e ar zemāko latento
# "izslēgšanas iespējamību", kas ar sniegumu korelē ar rho. Palikušo vidējais
# ir pārvērtēts par
#       nobīde = rho * SD * dnorm(qnorm(e)) / (1 - e)
# Pie SD = 100 un rho = 0,5 tas dod 1,4 / 5,4 / 9,8 punktus pie 1 / 5 / 10 %,
# kas atbilst OECD noapaļotajiem 1 / 5 / 10.
#
# PIEŅĒMUMI, KO NEDRĪKST AIZMIRST
#   - rho ir vienāds visos ciklos (tas pats mehānisms); tas ir vienkāršojums,
#     jo 2022. gadā izslēgti galvenokārt attālināto mācību skolēni
#   - izslēgšanas līmenis plūsmās pieņemts vienāds ar valsts līmeni, jo
#     sadalījums pa plūsmām nav publisks
#   - 2025. gada izslēgšana nav publicēta; lietots diapazons 3,21-5,07 %, kas
#     ir Latvijas neanotēto ciklu 2003-2018 robežas
#   - statistiskā nozīmība: SE ar katra pāra saites kļūdu no vienotās tabulas
#     output/pisa2025/saites_kludas.csv (78. skripts; līdz 2026-09-13 visiem
#     pāriem bija pagaidu 3)
#
# Ievade:  output/pisa2025/verifikacija.csv, ti_platums.csv, izlases_kvalitate.csv,
#          saites_kludas.csv
# Izvade:  output/pisa2025/izslegsanas_robezas.csv
#          output/pisa2025/logs/48_izslegsanas_robezas_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/48_izslegsanas_robezas_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)

v  <- read.csv("output/pisa2025/verifikacija.csv")
sd <- read.csv("output/pisa2025/ti_platums.csv")
q  <- read.csv("output/pisa2025/izlases_kvalitate.csv", check.names = FALSE,
               fileEncoding = "UTF-8")
EXCL <- setNames(q[["izslēgšana_kopā_pct"]] / 100, q$gads)
E25 <- c(zema = 0.0321, augsta = 0.0507)
LE_TAB <- read.csv("output/pisa2025/saites_kludas.csv", stringsAsFactors = FALSE)   # viens avots, 78. skripts
le_tab <- function(no, uz, joma = "READ") {
  i <- which(LE_TAB$joma == joma & LE_TAB$no == as.numeric(no) & LE_TAB$uz == as.numeric(uz))
  if (!length(i)) stop("Nav saites kļūdas pārim ", no, " -> ", uz); LE_TAB$LE[i[1]] }

nobide <- function(e, rho, s) rho * s * dnorm(qnorm(e)) / (1 - e)
cat("Formulas pārbaude pret OECD (SD = 100, rho = 0,5):",
    sprintf("%.1f", nobide(c(.01, .05, .10), .5, 100)), "punkti\n\n")

vert <- function(g, k) {
  r <- v[v$gads == g & v$grupa == k, ]; s <- sd[sd$gads == g & sd$grupa == k, ]
  list(m = r$aplese, se = r$se, sd = s$sd)
}

cat(strrep("=", 78), "\n1. Līmeņi: novērotais un labotais pie rho = 0,5\n", strrep("=", 78), "\n\n")
cat(sprintf("  %-5s %8s %8s %8s | %8s %8s | %8s %8s\n", "gads", "izsl.%",
            "A nov.", "A lab.", "B nov.", "B lab.", "visi nov.", "visi lab."))
GADI <- c(2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025)
for (g in GADI) {
  e <- if (g == 2025) mean(E25) else EXCL[[as.character(g)]]
  lab <- sapply(c("A", "B", "visi"), function(k) { x <- vert(g, k); c(x$m, x$m - nobide(e, .5, x$sd)) })
  cat(sprintf("  %-5d %8s %8.1f %8.1f | %8.1f %8.1f | %8.1f %8.1f\n", g,
              if (g == 2025) "3,2-5,1" else sprintf("%.2f", 100 * e),
              lab[1, "A"], lab[2, "A"], lab[1, "B"], lab[2, "B"], lab[1, "visi"], lab[2, "visi"]))
}

cat("\n", strrep("=", 78), "\n2. Pārmaiņas starp cikliem: novērotā un robežas\n", strrep("=", 78), "\n\n")
PARI <- list(c(2015, 2018), c(2018, 2022), c(2015, 2022), c(2022, 2025), c(2018, 2025), c(2003, 2025))
rez <- list()
cat(sprintf("  %-5s %-12s %9s   %-19s %-19s %7s  %s\n", "grupa", "periods", "novērotā",
            "rho=0,25", "rho=0,5", "SE(LE)", "noturīgs?"))
for (k in c("A", "B", "visi")) for (p in PARI) {
  a <- vert(p[1], k); b <- vert(p[2], k)
  le <- le_tab(p[1], p[2])
  d <- b$m - a$m; se <- sqrt(a$se^2 + b$se^2 + le^2)
  e1 <- EXCL[[as.character(p[1])]]
  e2s <- if (p[2] == 2025) E25 else EXCL[[as.character(p[2])]]
  lab <- function(rho) sapply(e2s, function(e2) d - (nobide(e2, rho, b$sd) - nobide(e1, rho, a$sd)))
  l25 <- lab(.25); l50 <- lab(.5)
  visi <- c(d, l25, l50)
  noz <- abs(visi) > 1.96 * se
  verd <- if (all(noz)) "jā, nozīmīgs visur" else if (!any(noz)) "nav nozīmīgs nekur" else "TRAUSLS"
  fmt <- function(x) if (length(x) == 1) sprintf("%+.1f", x) else sprintf("%+.1f..%+.1f", min(x), max(x))
  cat(sprintf("  %-5s %-12s %+9.1f   %-19s %-19s %7.2f  %s\n", k, paste0(p[1], "->", p[2]), d,
              fmt(l25), fmt(l50), se, verd))
  rez[[length(rez) + 1]] <- data.frame(grupa = k, no = p[1], lidz = p[2], noverota = d,
    rho025_min = min(l25), rho025_max = max(l25), rho05_min = min(l50), rho05_max = max(l50),
    le = le, se_le = se, vertejums = verd)
}
write.csv(do.call(rbind, rez), "output/pisa2025/izslegsanas_robezas.csv", row.names = FALSE)
cat("\nIzveidots: output/pisa2025/izslegsanas_robezas.csv\n")
while (sink.number() > 0) sink()
close(log_con)
