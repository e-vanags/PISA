# ============================================================================
# 39. A7. Starptautiskais fons: vai 2016 -> 2021 kritums ir Latvijas vai visu
# ============================================================================
#
# Plāna solis A7. `docs/38` parāda, ka latviešu valodas plūsmas 4. klases lasītprasme
# no 2016. līdz 2021. gadam nokrita par 30,1 punktu. Bet PIRLS 2021 visā
# pasaulē notika pandēmijas apstākļos. Tāpēc jautājums ir:
#
#   cik no šī krituma ir Latvijas, un cik ir tas pats, kas visur citur
#
# Divas atbildes tiek meklētas atsevišķi:
#
#   1. VISAS SISTĒMAS. Katrai sistēmai, kas piedalījās gan 2016., gan
#      2021. gadā, tiek rēķināta pārmaiņa. Latvija tiek ielikta šajā
#      sadalījumā. Tas parāda, vai −30 ir tipiski vai galējs.
#   2. LIETUVA GARAJĀ RINDĀ. Lietuva ir vienīgā valsts, kas piedalījās
#      visos četros ciklos, kuros piedalījās Latvija, un tā ir tuvākais
#      iespējamais dvīnis: tā pati vēsture, tā pati reforma, tā pati
#      pandēmija. Igaunija PIRLS nepiedalās vispār, tāpēc kā PISA analīzē
#      to izmantot nevar.
#
# Salīdzinājumam tiek lietota titulvalodas plūsma abās valstīs, ne valsts
# vidējais, jo `docs/38` visa loģika ir par plūsmu, ne par valsti.
#
# UZMANĪBU, KĻŪDA, KAS BIJA JĀNOVĒRŠ. PIRLS 2021 vairākām valstīm ir divi
# faili: pamatizlase digitālajā formātā (ASGxxxR5) un tilta izlase papīrā
# (ASGxxxA5), kas kalpo tikai skalu saistīšanai. Ja paņem tilta failu, valsts
# vidējais ir par 5 līdz 8 punktiem nepareizs. Zviedrijai tilta fails deva
# 535,9 pretī pareizajiem 544,0. Tāpēc failu šablons ir ierobežots ar "R".
# Pārbaude: Latvija 527,7 un Polija 549,1 sakrīt ar IEA publicēto.
#
# Metode: 5 plausible values, jackknife JK2, tāpat kā skriptā 38.
#
# Izvade:  output/pisa2025/starptautiskais_fons.csv
#          output/pisa2025/logs/39_starptautiskais_fons_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/39_starptautiskais_fons_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

library(haven)
PV <- paste0("ASRREA0", 1:5)
SLIEKSNIS <- 400

# Apakšnacionālie un atkārtoto klašu dalībnieki. Tie nav valstis, un valstu
# sadalījumā tie nedrīkst būt, citādi rangs ir bezjēdzīgs.
NAV_VALSTIS <- c("aad", "adu", "cab", "cbc", "cnl", "cqu", "cot", "rmo",
                 "za5", "za6", "is5", "no4", "no5", "se3", "ean", "ema",
                 "ma6", "mln", "can")

# Tikai pamatizlases faili, proti, tie, kas beidzas ar R un cikla numuru.
faili <- function(mape) {
  f <- list.files(mape, pattern = "^asg[a-z0-9]{3}r[0-9]\\.sav$",
                  recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  kods <- tolower(substr(sub("^(?i)asg", "", basename(f)), 1, 3))
  data.frame(kods = kods, cels = f, stringsAsFactors = FALSE)
}

NOSAUKUMI <- c(
  nor = "Norvēģija", aze = "Azerbaidžāna", kaz = "Kazahstāna", lva = "Latvija",
  bfl = "Beļģija (flāmu)", svn = "Slovēnija", fin = "Somija", isr = "Izraēla",
  swe = "Zviedrija", irn = "Irāna", nld = "Nīderlande", pol = "Polija",
  esp = "Spānija", sau = "Saūda Arābija", hun = "Ungārija", bgr = "Bulgārija",
  aut = "Austrija", mac = "Makao", svk = "Slovākija", dnk = "Dānija",
  rus = "Krievija", ltu = "Lietuva", nzl = "Jaunzēlande", deu = "Vācija",
  aus = "Austrālija", ita = "Itālija", bfr = "Beļģija (franču)", usa = "ASV",
  eng = "Anglija", twn = "Taivāna", nir = "Ziemeļīrija", cze = "Čehija",
  fra = "Francija", prt = "Portugāle", hkg = "Honkonga", sgp = "Singapūra",
  geo = "Gruzija", irl = "Īrija", omn = "Omāna", bhr = "Bahreina",
  are = "AAE", mar = "Maroka", qat = "Katara", mlt = "Malta",
  cyp = "Kipra", hrv = "Horvātija", mkd = "Ziemeļmaķedonija", srb = "Serbija",
  tur = "Turcija", alb = "Albānija", uzb = "Uzbekistāna", egy = "Ēģipte",
  jor = "Jordānija", bra = "Brazīlija", chl = "Čīle", mne = "Melnkalne",
  mda = "Moldova", rom = "Rumānija", zaf = "Dienvidāfrika", tto = "Trinidada",
  kwt = "Kuveita", idn = "Indonēzija", col = "Kolumbija", sco = "Skotija",
  isl = "Islande", lux = "Luksemburga", dnl = "Dānija", pol2 = "")
vards <- function(k) if (!is.na(NOSAUKUMI[k])) unname(NOSAUKUMI[k]) else toupper(k)
F16 <- faili("data/iea/pirls2016")
F21 <- faili("data/iea/pirls2021")

nolasa <- function(cels) {
  d <- read_sav(cels)
  names(d) <- toupper(names(d))
  d
}

jk <- function(d, i, fun) {
  if (sum(i) < 50) return(c(est = NA, se = NA, n = sum(i)))
  w <- as.numeric(d$TOTWGT)[i]
  z <- as.numeric(d$JKZONE)[i]; r <- as.numeric(d$JKREP)[i]
  pv <- lapply(PV, function(p) as.numeric(d[[p]])[i])
  e <- vapply(pv, function(x) fun(x, w), numeric(1))
  re <- vapply(sort(unique(z)), function(zz) {
    wr <- w; wr[z == zz] <- w[z == zz] * 2 * r[z == zz]; fun(pv[[1]], wr)
  }, numeric(1))
  c(est = mean(e),
    se = sqrt(sum((re - e[1])^2) + (1 + 1 / length(PV)) * stats::var(e)),
    n = sum(i))
}
VIDEJAIS <- function(x, w) sum(x * w) / sum(w)
ZEM <- function(x, w) 100 * sum(w * (x < SLIEKSNIS)) / sum(w)

# ---------------------------------------------------------------------------
# 1. Visas sistēmas, kas piedalījās abos ciklos
# ---------------------------------------------------------------------------
kopigie <- sort(intersect(F16$kods, F21$kods))
kopigie <- setdiff(kopigie, NAV_VALSTIS)
cat(strrep("=", 76), "\n1. Sistēmas, kas piedalījās gan 2016., gan 2021. gadā:",
    length(kopigie), "\n", strrep("=", 76), "\n\n")

rez <- list()
for (k in kopigie) {
  a <- try(nolasa(F16$cels[F16$kods == k][1]), silent = TRUE)
  b <- try(nolasa(F21$cels[F21$kods == k][1]), silent = TRUE)
  if (inherits(a, "try-error") || inherits(b, "try-error")) next
  if (!all(PV %in% names(a)) || !all(PV %in% names(b))) next
  nos <- vards(k)
  v16 <- jk(a, rep(TRUE, nrow(a)), VIDEJAIS)
  v21 <- jk(b, rep(TRUE, nrow(b)), VIDEJAIS)
  z16 <- jk(a, rep(TRUE, nrow(a)), ZEM)
  z21 <- jk(b, rep(TRUE, nrow(b)), ZEM)
  rez[[k]] <- data.frame(kods = k, valsts = nos,
    y2016 = unname(v16["est"]), se2016 = unname(v16["se"]),
    y2021 = unname(v21["est"]), se2021 = unname(v21["se"]),
    parmaina = unname(v21["est"] - v16["est"]),
    se_parm = unname(sqrt(v16["se"]^2 + v21["se"]^2)),
    zem2016 = unname(z16["est"]), zem2021 = unname(z21["est"]),
    stringsAsFactors = FALSE)
  cat(sprintf("  %-3s %-34s %6.1f -> %6.1f   %+6.1f\n", k, substr(nos, 1, 34),
              v16["est"], v21["est"], v21["est"] - v16["est"]))
}
df <- do.call(rbind, rez); rownames(df) <- NULL
df <- df[order(df$parmaina), ]

cat("\n", strrep("=", 76), "\n2. Sarindots pēc pārmaiņas\n", strrep("=", 76), "\n\n")
cat(sprintf("  %-4s %-32s %8s %8s %9s %10s\n", "", "sistēma", "2016", "2021",
            "pārmaiņa", "zem 400 %"))
for (i in seq_len(nrow(df))) {
  zv <- if (!is.na(df$se_parm[i]) && abs(df$parmaina[i] / df$se_parm[i]) > 1.96) "*" else " "
  izc <- if (df$kods[i] %in% c("lva", "ltu")) ">>" else "  "
  cat(sprintf("%s %2d. %-32s %8.1f %8.1f %+8.1f%s  %4.1f -> %4.1f\n",
              izc, i, substr(df$valsts[i], 1, 32), df$y2016[i], df$y2021[i],
              df$parmaina[i], zv, df$zem2016[i], df$zem2021[i]))
}

lv <- which(df$kods == "lva")
cat(sprintf("\n  Latvijas vieta: %d. no %d, no apakšas\n", lv, nrow(df)))
cat(sprintf("  Mediāna:  %+.1f     vidējais:  %+.1f     Latvija:  %+.1f\n",
            median(df$parmaina, na.rm = TRUE), mean(df$parmaina, na.rm = TRUE),
            df$parmaina[lv]))
cat(sprintf("  Sistēmu skaits ar lielāku kritumu nekā Latvijai: %d\n", lv - 1))
cat(sprintf("  Latvija standartizēti pret sadalījumu: z = %.2f\n",
            (df$parmaina[lv] - mean(df$parmaina, na.rm = TRUE)) /
              sd(df$parmaina, na.rm = TRUE)))

# ---------------------------------------------------------------------------
# 3. Lietuva garajā rindā, titulvalodas plūsma abās valstīs
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 76),
    "\n3. Latvija pret Lietuvu, titulvalodas plūsma, četri cikli\n",
    strrep("=", 76), "\n\n")

GARIE <- list(
  "2001" = c(lva = "data/iea/pirls2001/ASGLVAR1.sav",
             ltu = "data/iea/pirls2001/ASGLTUR1.sav"),
  "2006" = c(lva = "data/iea/pirls2006/asgLVAr2.sav",
             ltu = "data/iea/pirls2006/asgLTUr2.sav"),
  "2016" = c(lva = "data/iea/pirls2016/asglvar4.sav",
             ltu = "data/iea/pirls2016/asgltur4.sav"),
  "2021" = c(lva = "data/iea/pirls2021/spss/Data/ASGLVAR5.sav",
             ltu = "data/iea/pirls2021/spss/Data/ASGLTUR5.sav"))
TITULS <- c(lva = "Latvian", ltu = "Lithuanian")

# 2001. un 2006. gadā testa valodas etiķetes ir vietturi, un kods 1 ir valsts
# galvenā testa valoda. No 2016. gada etiķetes ir īstas. Tas pats pieņēmums,
# kas skriptā 38, tikai piemērots abām valstīm.
titulvaloda <- function(d, gads, kods) {
  lauks <- if ("ITLANG_SA" %in% names(d)) "ITLANG_SA" else "ITLANG"
  if (gads %in% c("2001", "2006")) return(as.numeric(d[[lauks]]) == 1)
  grepl(TITULS[[kods]], as.character(as_factor(d[[lauks]])))
}

cat(sprintf("  %-6s %10s %6s   %10s %6s   %10s\n", "gads",
            "Latvija", "SE", "Lietuva", "SE", "starpība"))
G <- list()
for (g in names(GARIE)) {
  v <- list()
  for (k in c("lva", "ltu")) {
    d <- nolasa(GARIE[[g]][[k]])
    v[[k]] <- jk(d, titulvaloda(d, g, k), VIDEJAIS)
  }
  G[[g]] <- v
  dd <- v$lva["est"] - v$ltu["est"]; ss <- sqrt(v$lva["se"]^2 + v$ltu["se"]^2)
  cat(sprintf("  %-6s %10.1f %6.1f   %10.1f %6.1f   %+7.1f (%.1f)%s\n", g,
              v$lva["est"], v$lva["se"], v$ltu["est"], v$ltu["se"], dd, ss,
              if (abs(dd / ss) > 1.96) "*" else " "))
}
cat("\n  Pārmaiņas\n")
gg <- names(GARIE)
for (i in 2:length(gg)) {
  a <- G[[gg[i]]]; b <- G[[gg[i - 1]]]
  dl <- a$lva["est"] - b$lva["est"]; sl <- sqrt(a$lva["se"]^2 + b$lva["se"]^2)
  dt <- a$ltu["est"] - b$ltu["est"]; st <- sqrt(a$ltu["se"]^2 + b$ltu["se"]^2)
  dd <- dl - dt; ss <- sqrt(sl^2 + st^2)
  cat(sprintf("  %s -> %s   Latvija %+6.1f (%.1f)   Lietuva %+6.1f (%.1f)   starpību starpība %+6.1f (%.1f)%s\n",
              gg[i - 1], gg[i], dl, sl, dt, st, dd, ss,
              if (abs(dd / ss) > 1.96) "*" else " "))
}

write.csv(df, "output/pisa2025/starptautiskais_fons.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/starptautiskais_fons.csv\n ",
    output_file, "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
