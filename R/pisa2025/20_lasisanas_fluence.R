# ============================================================================
# 20. Lasīšanas ātrums un kļūdu skaits fluences blokā
# ============================================================================
#
# Jautājums. Skripti 15 līdz 17 rāda, ka krieviski runājošo lasīšanas rezultāts
# 2025. gadā nokrita par 97 punktiem. Tas ir summārs testa rezultāts. Šeit
# skatāmies zem tā, uz pašu lasīšanas mehāniku, proti, cik ātri skolēns lasa
# un cik kļūdas pieļauj.
#
# Kāpēc tas ir svarīgi. Fluences bloks (RF, uzdevumi CR590Q01 līdz Q66) ir
# vienkārši teikumi, par kuriem jāpasaka, vai tie ir jēdzīgi. Tas mēra
# dekodēšanas ātrumu un precizitāti, nevis teksta izpratni. Ja krieviski
# runājošo kritums ir arī šeit, tad runa ir par pašu lasīšanas prasmi latviešu
# valodā, nevis tikai par sarežģītāku tekstu izpratni.
#
# Divi rādītāji katram skolēnam:
#   ātrums    mediānais laiks uz vienu teikumu, sekundēs, un teikumi minūtē
#   kļūdas    nepareizo atbilžu skaits un īpatsvars no atbildētajiem
#
# Ievade:  ~/pisa2025_sav/CY09_MS_COG_20260806.sav          (CR590Q##S)
#          ~/pisa2025_sav/CY09_MS_COG_PROCESS_20260806.sav  (CR590Q##TT)
#          data/pisa2025/processed/pisa_lva_2025_stu.rds    (LANGN, svari)
# Izvade:  data/pisa2025/processed/fluence_2025.rds         (kešs)
#          output/pisa2025/fluence_valodu_grupas.csv
#          output/pisa2025/logs/20_lasisanas_fluence_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/20_lasisanas_fluence_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

required_packages <- c("haven", "tidyselect")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}

SAV_DIR <- Sys.getenv("PISA2025_SAV_DIR", unset = "")
if (!nzchar(SAV_DIR)) {
  kandidati <- c(file.path(path.expand("~"), "pisa2025_sav"),
                 file.path(path.expand("~"), "Documents", "pisa2025_sav"),
                 "/Volumes/PISA/pisa2025_sav")
  atrasts <- kandidati[dir.exists(kandidati)]
  SAV_DIR <- if (length(atrasts)) atrasts[1] else kandidati[1]
}

atrod_failu <- function(sablons) {
  f <- list.files(SAV_DIR, pattern = sablons, full.names = TRUE)
  if (!length(f)) stop("Nav atrasts fails pēc šablona ", sablons, " mapē ", SAV_DIR)
  f[1]
}

VALSTIS <- c("LVA", "EST")
KESS <- "data/pisa2025/processed/fluence_2025.rds"


# === 1. IZGŪŠANA NO LIELAJIEM FAILIEM =======================================
cat(strrep("=", 70), "\n")
cat("1. Fluences bloka izgūšana\n")
cat(strrep("=", 70), "\n\n")

if (file.exists(KESS)) {
  fl <- readRDS(KESS)
  cat("Nolasīts kešs:", KESS, "\n")
} else {
  cat("Keša nav, lasu .sav failus. Tas prasa vairākas minūtes.\n\n")

  # --- Pareizība no COG ---
  f_cog <- atrod_failu("^CY09_MS_COG_[0-9]+\\.sav$")
  cat("COG:", basename(f_cog), "\n")
  galvene <- read_sav(f_cog, n_max = 1)
  s_kol <- grep("^CR590Q[0-9]+S$", names(galvene), value = TRUE)
  cat("  fluences vērtējuma mainīgie:", length(s_kol), "\n")
  cog <- read_sav(f_cog, col_select = all_of(c("CNT", "CNTSTUID", s_kol)))
  cog <- cog[as.character(cog$CNT) %in% VALSTIS, ]
  cat("  izgūtas rindas:", nrow(cog), "\n\n")

  # --- Laiki no COG_PROCESS ---
  f_prc <- atrod_failu("^CY09_MS_COG_PROCESS_[0-9]+\\.sav$")
  cat("COG_PROCESS:", basename(f_prc), "\n")
  galvene <- read_sav(f_prc, n_max = 1)
  t_kol <- grep("^CR590Q[0-9]+TT$", names(galvene), value = TRUE)
  cat("  fluences laika mainīgie:", length(t_kol), "\n")
  prc <- read_sav(f_prc, col_select = all_of(c("CNT", "CNTSTUID", t_kol)))
  prc <- prc[as.character(prc$CNT) %in% VALSTIS, ]
  cat("  izgūtas rindas:", nrow(prc), "\n\n")

  fl <- list(cog = cog, prc = prc, s_kol = s_kol, t_kol = t_kol)
  saveRDS(fl, KESS)
  cat("Saglabāts kešs:", KESS, "\n")
}

cog <- fl$cog; prc <- fl$prc; s_kol <- fl$s_kol; t_kol <- fl$t_kol


# === 2. RĀDĪTĀJI KATRAM SKOLĒNAM ============================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("2. Rādītāji katram skolēnam\n")
cat(strrep("=", 70), "\n\n")

# Vērtējums. Fluences atbildes .sav failā ir teksta virknes ar etiķetēm
# "0 - No credit", "1 - Full credit" un "7 - Not Applicable", tāpēc vispirms
# jāpārveido par skaitli un tikai tad jāizmet trūkstošie kodi 7, 8, 9.
tiri_vertejumu <- function(x) {
  v <- suppressWarnings(as.numeric(as.character(x)))
  v[!v %in% c(0, 1)] <- NA
  v
}

# Laiks. TT ir milisekundes. Nulle vai negatīvs nozīmē, ka uzdevums nav rādīts.
# Ļoti garas vērtības ir pauzes, tāpēc tās neieskaita mediānā, bet neizmet
# skolēnu, jo mediāna pret tām ir izturīga.
tiri_laiku <- function(x) {
  v <- as.numeric(x)
  v[v <= 0] <- NA
  v / 1000
}

S <- vapply(cog[s_kol], tiri_vertejumu, numeric(nrow(cog)))
LAIKI <- vapply(prc[t_kol], tiri_laiku, numeric(nrow(prc)))

# Vienoju abas tabulas pēc skolēna, jo rindu secība failos nesakrīt.
sk_cog <- data.frame(
  CNT       = as.character(cog$CNT),
  CNTSTUID  = cog$CNTSTUID,
  atbildeti = rowSums(!is.na(S)),
  pareizi   = rowSums(S == 1, na.rm = TRUE)
)
sk_cog$kludas <- sk_cog$atbildeti - sk_cog$pareizi
sk_cog$precizitate <- ifelse(sk_cog$atbildeti > 0,
                             sk_cog$pareizi / sk_cog$atbildeti, NA)

sk_prc <- data.frame(
  CNTSTUID   = prc$CNTSTUID,
  sek_median = apply(LAIKI, 1, median, na.rm = TRUE),
  laiki_n    = rowSums(!is.na(LAIKI))
)

fl_sk <- merge(sk_cog, sk_prc, by = "CNTSTUID")
fl_sk <- fl_sk[fl_sk$atbildeti >= 10 & fl_sk$laiki_n >= 10 &
                 is.finite(fl_sk$sek_median), ]

# Ātrā minēšana. Teikumu nevar izlasīt un novērtēt ātrāk par aptuveni sekundi.
# Šiem skolēniem precizitāte ir 0.56, proti, tuvu nejaušībai, jo atbilde ir
# jā vai nē. Tie nemēra lasīšanas ātrumu, tie mēra neiesaistīšanos, tāpēc
# ātruma un kļūdu aplēses rēķinām atsevišķi.
SLIEKSNIS_SEK <- 1.0
fl_sk$minetajs <- as.numeric(fl_sk$sek_median < SLIEKSNIS_SEK)

# UZMANĪBU. Teikumus minūtē nedrīkst rēķināt katram skolēnam un tad vidējot,
# jo 60/x ir izliekta funkcija un daži ļoti ātri skolēni pavelk vidējo uz augšu.
# Ātrumu izsakām kā 60, dalītu ar grupas vidējo sekunžu skaitu.

cat("Skolēni ar fluences datiem:\n")
print(table(fl_sk$CNT))
cat("\nVidēji atbildēti teikumi:", round(mean(fl_sk$atbildeti), 1),
    " laika mērījumi:", round(mean(fl_sk$laiki_n), 1), "\n")
cat("Ātrās minēšanas slieksnis:", SLIEKSNIS_SEK, "s uz teikumu\n")
cat("Precizitāte zem sliekšņa:",
    round(mean(fl_sk$precizitate[fl_sk$minetajs == 1]), 3),
    " virs sliekšņa:",
    round(mean(fl_sk$precizitate[fl_sk$minetajs == 0]), 3), "\n\n")


# === 3. VALODU GRUPAS =======================================================
cat(strrep("=", 70), "\n")
cat("3. Latvija pa mājās runātās valodas grupām\n")
cat(strrep("=", 70), "\n\n")

d25 <- readRDS("data/pisa2025/processed/pisa_lva_2025_stu.rds")

grupa_no_langn <- function(d) {
  lab <- as.character(as_factor(d$LANGN))
  g <- rep("Cita vai nezināma", length(lab))
  g[grepl("^Latvian", lab)] <- "Latviešu"
  g[grepl("^Russian", lab)] <- "Krievu"
  factor(g, levels = c("Latviešu", "Krievu", "Cita vai nezināma"))
}

rw_kol <- grep("^W_FSTURWT", names(d25), value = TRUE)
stu <- data.frame(
  CNTSTUID = d25$CNTSTUID,
  grupa    = grupa_no_langn(d25),
  w        = d25$W_FSTUWT,
  escs     = d25$ESCS,
  skola    = d25$CNTSCHID,
  read     = rowMeans(sapply(paste0("PV", 1:10, "READ"), function(p) d25[[p]]))
)
stu <- cbind(stu, d25[rw_kol])

lva <- merge(fl_sk[fl_sk$CNT == "LVA", ], stu, by = "CNTSTUID")
cat("Sapārotie Latvijas skolēni:", nrow(lva), "no", sum(fl_sk$CNT == "LVA"), "\n\n")

# Fay koeficients 0.5, tāpēc dalītājs ir 80 * (1 - 0.5)^2 = 20.
FAY_DALITAJS <- 20

svertais <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)])

videjais_brr <- function(d, mainigais, i = rep(TRUE, nrow(d))) {
  x <- d[[mainigais]][i]
  est <- svertais(x, d$w[i])
  rep_est <- vapply(rw_kol, function(r) svertais(x, d[[r]][i]), numeric(1))
  c(est = est, se = sqrt(sum((rep_est - est)^2) / FAY_DALITAJS), n = sum(i))
}

starpiba_brr <- function(d, mainigais, i1, i2) {
  f <- function(w) svertais(d[[mainigais]][i1], w[i1]) -
    svertais(d[[mainigais]][i2], w[i2])
  est <- f(d$w)
  rep_est <- vapply(rw_kol, function(r) f(d[[r]]), numeric(1))
  c(est = est, se = sqrt(sum((rep_est - est)^2) / FAY_DALITAJS))
}

rez <- list()

parada <- function(d, raditaji, virsraksts) {
  cat(strrep("-", 70), "\n", virsraksts, "\n", strrep("-", 70), "\n\n", sep = "")
  for (m in names(raditaji)) {
    cat(raditaji[[m]], "\n")
    for (g in levels(d$grupa)) {
      r <- videjais_brr(d, m, d$grupa == g)
      cat(sprintf("  %-18s %8.2f  (SE %5.2f)  n = %4d\n",
                  g, r["est"], r["se"], r["n"]))
      rez[[length(rez) + 1]] <<- data.frame(izlase = virsraksts,
                                            raditajs = raditaji[[m]], grupa = g,
                                            est = r["est"], se = r["se"], n = r["n"])
    }
    s <- starpiba_brr(d, m, d$grupa == "Krievu", d$grupa == "Latviešu")
    zvg <- if (abs(s["est"] / s["se"]) > 1.96) "*" else " "
    cat(sprintf("  %-18s %8.2f  (SE %5.2f) %s\n", "Krievu - latviešu",
                s["est"], s["se"], zvg))
    if (m == "sek_median") {
      cat(sprintf("  %-18s %8.1f  pret %.1f teikumiem minūtē\n", "Ātrums",
                  60 / videjais_brr(d, m, d$grupa == "Krievu")["est"],
                  60 / videjais_brr(d, m, d$grupa == "Latviešu")["est"]))
    }
    cat("\n")
    rez[[length(rez) + 1]] <<- data.frame(izlase = virsraksts,
                                          raditajs = raditaji[[m]],
                                          grupa = "Starpība", est = s["est"],
                                          se = s["se"], n = NA)
  }
}

parada(lva, c(minetajs = "Ātro minētāju daļa"), "Visi skolēni")

lva_ies <- lva[lva$minetajs == 0, ]
parada(lva_ies,
       c(sek_median  = "Sekundes uz teikumu",
         kludas      = "Kļūdu skaits",
         precizitate = "Pareizo daļa",
         atbildeti   = "Atbildētie teikumi"),
       "Tikai iesaistījušies, proti, virs 1 s uz teikumu")


# === 4. VAI ĀTRUMS IR TAS PATS, KAS KĻŪDAS ==================================
cat(strrep("=", 70), "\n")
cat("4. Ātrums pret precizitāti\n")
cat(strrep("=", 70), "\n\n")

cat("Korelācijas visā Latvijas fluences izlasē:\n")
cat(sprintf("  sekundes uz teikumu pret precizitāti   r = %5.2f\n",
            cor(lva$sek_median, lva$precizitate, use = "complete.obs")))
cat(sprintf("  sekundes uz teikumu pret lasīšanu      r = %5.2f\n",
            cor(lva$sek_median, lva$read, use = "complete.obs")))
cat(sprintf("  precizitāte pret lasīšanas rezultātu   r = %5.2f\n\n",
            cor(lva$precizitate, lva$read, use = "complete.obs")))

cat("Tas pats tikai iesaistījušajiem:\n")
cat(sprintf("  sekundes uz teikumu pret precizitāti   r = %5.2f\n",
            cor(lva_ies$sek_median, lva_ies$precizitate, use = "complete.obs")))
cat(sprintf("  precizitāte pret lasīšanas rezultātu   r = %5.2f\n\n",
            cor(lva_ies$precizitate, lva_ies$read, use = "complete.obs")))

cat("Vai valodu grupu starpība paliek pēc kontrolēm.\n")
cat("Atsauces grupa ir latviski runājošie, izlase ir iesaistījušies.\n\n")
lva$krievu <- as.numeric(lva$grupa == "Krievu")
lva_ies$krievu <- as.numeric(lva_ies$grupa == "Krievu")
for (m in c("sek_median", "kludas", "precizitate")) {
  m0 <- lm(as.formula(paste(m, "~ krievu")), data = lva_ies, weights = w)
  m1 <- lm(as.formula(paste(m, "~ krievu + escs")), data = lva_ies, weights = w)
  m2 <- lm(as.formula(paste(m, "~ krievu + escs + factor(skola)")),
           data = lva_ies, weights = w)
  cat(sprintf("  %-14s M0 %7.3f   M1 (+ESCS) %7.3f   M2 (+skola) %7.3f\n",
              m, coef(m0)["krievu"], coef(m1)["krievu"], coef(m2)["krievu"]))
}


# === 5. IGAUNIJAS ATSKAITES PUNKTS ==========================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("5. Igaunija salīdzinājumam\n")
cat(strrep("=", 70), "\n\n")

est <- fl_sk[fl_sk$CNT == "EST", ]
for (nos in c("Igaunija", "Latvija")) {
  d <- if (nos == "Igaunija") est else fl_sk[fl_sk$CNT == "LVA", ]
  di <- d[d$minetajs == 0, ]
  cat(sprintf("  %-9s ātro minētāju daļa %4.1f %%   iesaistījušies: %.2f s uz teikumu, %.1f kļūdas, n = %d\n",
              nos, 100 * mean(d$minetajs), mean(di$sek_median),
              mean(di$kludas), nrow(di)))
}
cat("\nŠie ir nesvērti, tāpēc tie ir orientieris, nevis aplēse.\n")


# === 6. SAGLABĀŠANA =========================================================
rez_df <- do.call(rbind, rez)
rownames(rez_df) <- NULL
write.csv(rez_df, "output/pisa2025/fluence_valodu_grupas.csv", row.names = FALSE)

cat("\nIzveidotie faili:\n")
cat("  output/pisa2025/fluence_valodu_grupas.csv\n")
cat(" ", output_file, "\n")
cat("\nPabeigts:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
