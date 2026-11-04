# ============================================================================
# 42. Atskaites punkti un 2003 -> 2006 epizode
# ============================================================================
#
# DIVI LIETOTĀJA IEBILDUMI, KAS ŠO SKRIPTU IZRAISĪJA.
#
# 1. `docs/43` un `docs/45` apgalvoja, ka 2016. gada ziņojuma vērtējums bija
#    pareizs, jo Latvijā zem 2. līmeņa bija 17,1 %, "mazāk nekā OECD vidēji".
#    Iebildums: tas balstās uz OECD atskaites punktu. Pārmaiņas jāmēra
#    primāri pret pašu valsti, ne pret reģiona vidējo, jo Latvija varēja
#    startēt augstu.
#
#    Piebilde, kas iebildumu padara vēl svarīgāku: "OECD vidējais" nav
#    nemainīgs. OECD dalībvalstu sastāvs starp cikliem mainās, tāpēc
#    salīdzinājums ar to ir kustīgs mērs. Tāpēc šeit lietoti TRĪS atskaites
#    punkti, un pirmais ir galvenais:
#
#      A. Latvija pret sevi pašu laikā          <- galvenais
#      B. Latvija pret FIKSĒTU astoņu valstu grupu, kas piedalījusies visos
#         astoņos ciklos: FIN SWE POL DEU DNK CZE HUN NOR
#      C. Latvija pret Igauniju un Lietuvu, iespējams tikai no 2006. gada
#
# 2. 2003 -> 2006 kritums (-11,4) tika novērtēts kā "vāji" ar pamatojumu, ka
#    2003. gads izskatās aizdomīgi augsts. Iebildums: tas ir vispārīgs
#    aprakstošs spriedums, un te jāiet dziļumā.
#
#    Tāpēc šeit tiek pārbaudīts konkrēts izskaidrojums: vai pārmaiņa ir
#    IZLASES SASTĀVA pārmaiņa. Metode ir tiešā standartizācija. Katram gadam
#    tiek rēķināti šūnu vidējie pēc dzimuma un grāmatu skaita mājās, un tad
#    tiem uzlikts VIENS UN TAS PATS sastāvs. Ja standartizētā rinda izlīdzinās,
#    tad pārmaiņa bija sastāvā, ne prasmē.
#
#    Otrā pārbaude ir dispersijas sadalīšana starp skolām un skolu iekšienē.
#    Ja kritums nāk no starpskolu daļas, tas norāda uz izlasi vai segregāciju,
#    nevis uz vispārēju prasmes pārmaiņu.
#
# DATU AVOTS UN ROBEŽAS. `learningtower`, sk. `docs/43`. Viena plausible value,
# nav replikātsvaru, nav mājās runātās valodas. Standartkļūdas ar sāknēšanu
# pa skolām. Tas visu neizšķir, un skripts pasaka, kas paliek neizšķirts.
#
# Izvade:  output/pisa2025/atskaites_punkti.csv
#          output/pisa2025/logs/42_atskaites_punkti_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/42_atskaites_punkti_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

suppressMessages(library(learningtower))
set.seed(20260910)
B  <- 300
L2 <- 407.47
FIKSETA <- c("FIN", "SWE", "POL", "DEU", "DNK", "CZE", "HUN", "NOR")
BALTIJA <- c("EST", "LTU")

d <- load_student(year = "all")
d$read <- as.numeric(d$read); d$w <- as.numeric(d$stu_wgt)
d$skola <- paste0(d$country, "_", d$school_id)
d <- d[!is.na(d$read) & !is.na(d$w), ]
GADI <- sort(unique(as.character(d$year)))

vid <- function(x, w) sum(x * w) / sum(w)
zem <- function(x, w) 100 * sum(w * (x < L2)) / sum(w)
kv  <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]
  stats::approx((cumsum(w) - 0.5 * w) / sum(w), x, xout = p, rule = 2, ties = "ordered")$y }

sakne <- function(sub, fun) {
  est <- fun(sub$read, sub$w)
  sk <- unique(sub$skola)
  bs <- replicate(B, { izv <- sample(sk, length(sk), replace = TRUE)
    i <- unlist(lapply(izv, function(s) which(sub$skola == s)))
    fun(sub$read[i], sub$w[i]) })
  c(est = est, se = stats::sd(bs))
}

# ---------------------------------------------------------------------------
cat(strrep("=", 78), "\n1. Trīs atskaites punkti: vidējais\n", strrep("=", 78), "\n\n")
cat(sprintf("  %-6s %10s %14s %12s %12s\n", "gads", "Latvija",
            "fiksētā grupa", "starpība", "Igaunija/Lietuva"))
A <- list()
for (g in GADI) {
  lv <- d[d$country == "LVA" & as.character(d$year) == g, ]
  fx <- d[d$country %in% FIKSETA & as.character(d$year) == g, ]
  bl <- d[d$country %in% BALTIJA & as.character(d$year) == g, ]
  a <- vid(lv$read, lv$w); b <- vid(fx$read, fx$w)
  cc <- if (nrow(bl) > 100) vid(bl$read, bl$w) else NA
  A[[g]] <- c(lv = a, fx = b, bl = cc)
  cat(sprintf("  %-6s %10.1f %14.1f %+12.1f %12s\n", g, a, b, a - b,
              if (is.na(cc)) "—" else sprintf("%.1f", cc)))
}

cat("\n  Tas pats zem 2. līmeņa, procentos\n\n")
cat(sprintf("  %-6s %10s %14s %12s %12s\n", "gads", "Latvija",
            "fiksētā grupa", "starpība", "Igaunija/Lietuva"))
Z <- list()
for (g in GADI) {
  lv <- d[d$country == "LVA" & as.character(d$year) == g, ]
  fx <- d[d$country %in% FIKSETA & as.character(d$year) == g, ]
  bl <- d[d$country %in% BALTIJA & as.character(d$year) == g, ]
  a <- zem(lv$read, lv$w); b <- zem(fx$read, fx$w)
  cc <- if (nrow(bl) > 100) zem(bl$read, bl$w) else NA
  Z[[g]] <- c(lv = a, fx = b, bl = cc)
  cat(sprintf("  %-6s %9.1f %% %13.1f %% %+11.1f %12s\n", g, a, b, a - b,
              if (is.na(cc)) "—" else sprintf("%.1f %%", cc)))
}

cat("\n  Pārbaude, ko iebildums prasīja: vai apgalvojums \"Latvijā mazāk vājo\n",
    " lasītāju nekā salīdzinājuma grupā\" ir patiess katrā ciklā\n\n")
for (g in GADI) {
  st <- Z[[g]]["lv"] - Z[[g]]["fx"]
  cat(sprintf("  %-6s Latvija %5.1f %%   fiksētā grupa %5.1f %%   %s\n", g,
              Z[[g]]["lv"], Z[[g]]["fx"],
              if (st < 0) "Latvijā MAZĀK vājo" else "Latvijā VAIRĀK vājo"))
}

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78),
    "\n2. Izlases sastāvs: grāmatu skaits mājās, procentos no izlases\n",
    strrep("=", 78), "\n\n")
lv <- d[d$country == "LVA", ]
lv$gram <- as.character(lv$book)
KAT <- c("0-10", "11-25", "26-100", "101-200", "201-500", "More than 500")
cat(sprintf("  %-6s %s %8s\n", "gads",
            paste(sprintf("%14s", KAT), collapse = ""), "trūkst"))
for (g in GADI) {
  s <- lv[as.character(lv$year) == g, ]
  tot <- sum(s$w)
  v <- vapply(KAT, function(k) 100 * sum(s$w[!is.na(s$gram) & s$gram == k]) / tot, numeric(1))
  cat(sprintf("  %-6s %s %7.1f %%\n", g,
              paste(sprintf("%13.1f %%", v), collapse = ""),
              100 * sum(s$w[is.na(s$gram)]) / tot))
}

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78),
    "\n3. Tiešā standartizācija: cik no pārmaiņas ir sastāvs\n",
    strrep("=", 78), "\n\n")
cat("  Šūnas: dzimums x grāmatu skaits. Fiksētais sastāvs: visu gadu vidējais.\n\n")

lv$dz <- as.character(lv$gender)
lv$sun <- ifelse(is.na(lv$gram) | is.na(lv$dz), NA, paste(lv$dz, lv$gram))
ok <- !is.na(lv$sun)
sastavs <- tapply(lv$w[ok], lv$sun[ok], sum); sastavs <- sastavs / sum(sastavs)

cat(sprintf("  %-6s %12s %14s %12s\n", "gads", "novērotais", "standartizētais",
            "sastāva daļa"))
S <- list()
for (g in GADI) {
  s <- lv[ok & as.character(lv$year) == g, ]
  if (!nrow(s)) { cat(sprintf("  %-6s %12s %14s %12s\n", g, "—", "—", "—")); next }
  suni <- names(sastavs)
  m <- vapply(suni, function(k) { x <- s[s$sun == k, ]
    if (nrow(x) < 25) NA_real_ else vid(x$read, x$w) }, numeric(1))
  der <- !is.na(m)
  std <- sum(m[der] * sastavs[der]) / sum(sastavs[der])
  nov <- vid(s$read, s$w)
  S[[g]] <- c(nov = nov, std = std)
  cat(sprintf("  %-6s %12.1f %14.1f %12.1f\n", g, nov, std, nov - std))
}
cat("\n  Pārmaiņas: novērotā pret standartizēto\n\n")
gg <- names(S)
for (i in 2:length(gg)) {
  dn <- S[[gg[i]]]["nov"] - S[[gg[i - 1]]]["nov"]
  ds <- S[[gg[i]]]["std"] - S[[gg[i - 1]]]["std"]
  cat(sprintf("  %s -> %s   novērotā %+6.1f   standartizētā %+6.1f   sastāva ieguldījums %+6.1f\n",
              gg[i - 1], gg[i], dn, ds, dn - ds))
}

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78),
    "\n4. Dispersija starp skolām un skolu iekšienē\n", strrep("=", 78), "\n\n")
cat(sprintf("  %-6s %12s %14s %14s %10s\n", "gads", "kopējā SD",
            "starp skolām", "skolu iekšienē", "ICC"))
for (g in GADI) {
  s <- lv[as.character(lv$year) == g, ]
  sk <- split(seq_len(nrow(s)), s$skola)
  sk <- sk[vapply(sk, length, integer(1)) >= 5]
  if (length(sk) < 20) next
  mu <- vapply(sk, function(i) vid(s$read[i], s$w[i]), numeric(1))
  nn <- vapply(sk, function(i) sum(s$w[i]), numeric(1))
  kop <- vid(s$read, s$w)
  starp <- sqrt(sum(nn * (mu - kop)^2) / sum(nn))
  ieksh <- sqrt(vapply(seq_along(sk), function(j) {
    i <- sk[[j]]; sum(s$w[i] * (s$read[i] - mu[j])^2) }, numeric(1)) |> sum() /
      sum(vapply(sk, function(i) sum(s$w[i]), numeric(1))))
  kopsd <- sqrt(sum(s$w * (s$read - kop)^2) / sum(s$w))
  cat(sprintf("  %-6s %12.1f %14.1f %14.1f %10.3f\n", g, kopsd, starp, ieksh,
              starp^2 / (starp^2 + ieksh^2)))
}

cat("\n", strrep("=", 78), "\n5. Divas epizodes tuvplānā\n", strrep("=", 78), "\n\n")
for (pari in list(c("2003", "2006"), c("2015", "2018"))) {
  cat(sprintf("\n  --- %s -> %s ---\n", pari[1], pari[2]))
  for (st in list(c("vidējais", "vid"), c("P10", "p10"), c("P50", "p50"),
                  c("P90", "p90"), c("zem 2. līm.", "zem"))) {
    fn <- switch(st[2], vid = vid, zem = zem,
                 p10 = function(x, w) kv(x, w, .10),
                 p50 = function(x, w) kv(x, w, .50),
                 p90 = function(x, w) kv(x, w, .90))
    a <- sakne(lv[as.character(lv$year) == pari[1], ], fn)
    b <- sakne(lv[as.character(lv$year) == pari[2], ], fn)
    dd <- b["est"] - a["est"]; ss <- sqrt(a["se"]^2 + b["se"]^2)
    cat(sprintf("    %-12s %8.1f -> %8.1f   %+7.1f (SE %.1f)%s\n", st[1],
                a["est"], b["est"], dd, ss, if (abs(dd / ss) > 1.96) "*" else " "))
  }
}

df <- do.call(rbind, lapply(GADI, function(g) data.frame(
  gads = g, lv_vid = A[[g]]["lv"], fiks_vid = A[[g]]["fx"], balt_vid = A[[g]]["bl"],
  lv_zem = Z[[g]]["lv"], fiks_zem = Z[[g]]["fx"], balt_zem = Z[[g]]["bl"])))
write.csv(df, "output/pisa2025/atskaites_punkti.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/atskaites_punkti.csv\n ", output_file,
    "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
