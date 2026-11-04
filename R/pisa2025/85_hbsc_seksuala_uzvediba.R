# ============================================================================
# 85. HBSC 15 gadīgo seksuālā uzvedība Latvijā 2001/02 līdz 2021/22
# PISA datu analīze 2025
# ============================================================================
#
# JAUTĀJUMS: vai un kā Latvijā mainās pusaudžu seksuālā uzvedība (bijuši
# dzimumsakari, agrīna pieredze, prezervatīvs un tablete pēdējos dzimumsakaros),
# un kur Latvija ir pret 7 salīdzinājuma valstīm (EST, LTU, POL, FIN, CZE, HUN,
# DEU) un visām HBSC valstīm. Konteksts: seksualitātes mācība un tikumiskā
# audzināšana; OECD mērījumos tā nav mērīta, HBSC ir vienīgais avots.
#
# DATI: HBSC atvērtās piekļuves mikrodati HBSC2001OAed1.0, HBSC2006OAed1.0,
#   HBSC2010OAed1.0, HBSC2014OAed1.1, HBSC2018OAed1.1 (HBSC Data Management
#   Centre, Bergenas Universitāte; data/Allrounds_SPSS/, skat. AVOTS.md) un
#   HBSC Data Browser apkopojumi 2014, 2018, 2022 (data/hbsc/). 2021/22
#   mikrodati ir embargo (docs/44), tāpēc 2022 tikai no pārlūka.
#   Mikrodatus nedrīkst izplatīt; izvadēs tikai apkopojumi valsts × kārta ×
#   dzimums. Kešs data/Allrounds_SPSS/processed/ ir privāts.
# VECUMS: tikai 15 gadīgie (agecat = 3); jautājumi par seksuālo uzvedību citām
#   vecuma grupām netiek uzdoti.
# RĀDĪTĀJI (pa dzimumiem un kopā):
#   dzimumsakari: bijuši dzimumsakari, % no visiem 15 gadīgajiem (hadsex);
#   agrini:       pirmie dzimumsakari 13 gados vai agrāk, % no visiem (agesex);
#   prezervativs: prezervatīvs pēdējos dzimumsakaros, % no tiem, kam bijis
#                 (2001 līdz 2010 condomuse; 2014 līdz 2018 contraceptcondom);
#   tablete:      tablete pēdējos dzimumsakaros, % no tiem, kam bijis;
#   neviens:      ne prezervatīvs, ne tablete, % no tiem, kam bijis.
#   Papildus 2001 līdz 2010: prezervativs_k (prezervatīvs no kontracepcijas
#   saraksta, ne no atsevišķā jautājuma), lai redzētu abu formulējumu starpību.
# SVARI: sampleweights (2002, 2006, 2010), M137 (2014; failā ir, bet = 1 visām
#   valstīm, izņemot DEU, tātad faktiski nesvērts), weight (2018). Latvija visās
#   kārtās nesvērta (svars = 1 pēc dizaina).
# SE: svērts vidējais ar skolu klasteru (Teilora) SE, wmean_se kā 65c; kur
#   skolas nav, klase; kur arī klases nav, indivīds. Pārlūka rindām SE
#   tuvinājums (binomiāls ar n no 2018 mikrodatiem tai pašai grupai).
# PAAUDZE: 15 gadīgie 2001/02 dz. ap 1986, 2005/06 dz. 1990, 2009/10 dz. 1994,
#   2013/14 dz. 1998, 2017/18 dz. 2002, 2021/22 dz. 2006 (PISA 2022 paaudze).
#
# Izvade: output/pisa2025/hbsc_seks_rinda.csv, hbsc_seks_latvija.csv,
#   hbsc_seks_harmonizacija.csv, hbsc_seks_parbaude.csv;
#   output/pisa2025/plots/85_hbsc_seks_rinda.png; docs/88.
# ============================================================================

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
if (!file.exists("R/pisa2025") && file.exists("../../R/pisa2025")) setwd("../..")
output_file <- "output/pisa2025/logs/85_hbsc_seksuala_uzvediba_output.txt"
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
cat("Darba direktorija:", getwd(), "\n")
cat("Sākts:", format(Sys.time()), "\n")

# --- Pakotnes ----------------------------------------------------------------
required_packages <- c("haven", "data.table", "ggplot2", "patchwork")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Instalēju pakotni:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
cat("Visas pakotnes ielādētas.\n\n")
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
dir.create("data/Allrounds_SPSS/processed", showWarnings = FALSE)
PL <- "output/pisa2025/plots"
VAL <- c("LVA", "EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU"); SAL <- setdiff(VAL, "LVA")
KODI <- c(LVA = 428000, EST = 233000, LTU = 440000, FIN = 246000, CZE = 203000, HUN = 348000, POL = 616000, DEU = 276000)
FAILI <- c(`2002` = "data/Allrounds_SPSS/HBSC2001OAed1.0_F4.sav", `2006` = "data/Allrounds_SPSS/HBSC2006OAed1.0_F1.sav",
           `2010` = "data/Allrounds_SPSS/HBSC2010OAed1.0_F4.sav", `2014` = "data/Allrounds_SPSS/HBSC2014OAed1.1_F1.sav",
           `2018` = "data/Allrounds_SPSS/2018/HBSC2018OAed1.1.sav")
KARTAS <- c(`2002` = "2001/02", `2006` = "2005/06", `2010` = "2009/10", `2014` = "2013/14", `2018` = "2017/18", `2022` = "2021/22")
PAAUDZE <- c(`2002` = 1986, `2006` = 1990, `2010` = 1994, `2014` = 1998, `2018` = 2002, `2022` = 2006)
# Pārlūka valstu nosaukumi tiem, kam SAV etiķete atšķiras
NOS_PARL <- c(`Czech Republic` = "Czechia", Macedonia = "North Macedonia", Russia = "Russian Federation", Moldova = "Republic of Moldova",
              `Republic of Moldova` = "Republic of Moldova", USA = "United States", Turkey = "Türkiye")
VAL_NOS <- c(LVA = "Latvia", EST = "Estonia", LTU = "Lithuania", POL = "Poland", FIN = "Finland", CZE = "Czechia", HUN = "Hungary", DEU = "Germany")
RAD <- c(dzimumsakari = "bijuši dzimumsakari, % no 15 gadīgajiem", agrini = "pirmie dzimumsakari 13 gados vai agrāk, % no 15 gadīgajiem",
         prezervativs = "prezervatīvs pēdējos dzimumsakaros, % no tiem, kam bijis", tablete = "tablete pēdējos dzimumsakaros, % no tiem, kam bijis",
         neviens = "ne prezervatīvs, ne tablete pēdējos dzimumsakaros, % no tiem, kam bijis",
         prezervativs_k = "prezervatīvs no kontracepcijas saraksta (tikai 2001 līdz 2010), % no tiem, kam bijis")
MIN_N <- c(dzimumsakari = 100, agrini = 100, prezervativs = 30, tablete = 30, neviens = 30, prezervativs_k = 30)
num <- function(x) as.numeric(x)
DZIM <- c(`1` = "zēni", `2` = "meitenes")

# ===========================================================================
cat(strrep("=", 100), "\n1. Mikrodati: 15 gadīgie, visas valstis, harmonizēti rādītāji\n", strrep("=", 100), "\n", sep = "")
# ===========================================================================
KESS <- "data/Allrounds_SPSS/processed/hbsc_seks_15.rds"
if (file.exists(KESS)) {
  cat("Kešs ir; SAV failus nelasa (dzēst", KESS, "lai pārlasītu).\n"); M <- readRDS(KESS)
} else {
  VARS <- c("countryno", "COUNTRYno", "agecat", "AGECAT", "sex", "hadsex", "agesex", "condomuse", "contraceptcondom", "contraceptpill", "contraceptnever",
            "contraceptnomet", "contraceptother", "contraceptwithdraw", "contraceptnotsure", "sampleweights", "M137", "weight", "schoolno", "id2", "classno",
            "cluster", "ClassID", "yearbirth", "monthbirth")
  M <- list()
  for (g in names(FAILI)) {
    cat("\n---", KARTAS[[g]], FAILI[[g]], "\n")
    d <- read_sav(FAILI[[g]], col_select = any_of(VARS))
    cn <- grep("^countryno$", names(d), ignore.case = TRUE, value = TRUE); ac <- grep("^agecat$", names(d), ignore.case = TRUE, value = TRUE)
    etik <- attr(d[[cn]], "labels"); nos <- setNames(names(etik), as.character(unname(etik)))
    d <- as.data.table(d)
    d <- d[num(get(ac)) == 3]                                      # tikai 15 gadīgie
    d[, kods := num(get(cn))][, valsts := nos[as.character(kods)]][, cnt := names(KODI)[match(kods, KODI)]]
    d[!is.na(cnt), valsts := VAL_NOS[cnt]]
    d[, valsts := fifelse(valsts %in% names(NOS_PARL), NOS_PARL[valsts], valsts)]
    d[, sex := num(sex)][!sex %in% 1:2, sex := NA]
    wv <- intersect(c("sampleweights", "M137", "weight"), names(d))[1]; d[, w := num(get(wv))]; d[is.na(w) | w <= 0, w := 1]
    sk <- intersect(c("schoolno", "id2"), names(d))[1]; kl <- intersect(c("cluster", "ClassID", "classno"), names(d))[1]
    d[, skola := fifelse(is.na(get(sk)), NA_character_, paste(kods, num(get(sk))))][, klase := fifelse(is.na(get(kl)), NA_character_, paste(kods, num(get(kl))))]
    d[is.na(skola), skola := klase][is.na(skola), skola := paste(kods, "ind", seq_len(.N))]
    d[, skola_ok := uniqueN(skola) > 1, by = kods][skola_ok == FALSE, skola := paste(kods, "ind", seq_len(.N))]
    V <- function(v) if (v %in% names(d)) num(d[[v]]) else rep(NA_real_, nrow(d))
    hs <- V("hadsex"); hs[!hs %in% 1:2] <- NA                          # 0 = INAP (2001, 2006) izslēgts
    hadsex01 <- fifelse(hs == 1, 100, 0)
    as_ <- V("agesex")
    if (g %in% c("2002", "2006", "2010")) {
      as_[!as_ %in% 1:8] <- NA                                        # 1 = nekad, 2 = ≤ 11, 3 = 12, 4 = 13, ..., 8 = 17+ (tikai 2010)
      agrini01 <- fifelse(as_ %in% 2:4, 100, fifelse(as_ %in% c(1, 5:8) | hs == 2, 0, NA_real_))   # NA, ja bijis un vecums nav atbildēts
      cu <- V("condomuse"); cu[!cu %in% 2:3] <- NA                      # 1 = nekad nav bijis izslēgts; 2 = jā, 3 = nē
      prez01 <- fifelse(cu == 2, 100, 0)
      # Kontracepcijas saraksts: 2001 un 2006 atzīmējamais saraksts (neatzīmēts lielākoties tukšs), 2010 jā/nē katram vienumam.
      # Saucējs: bijuši dzimumsakari un vismaz viens saraksta vienums atbildēts (1 vai 2).
      it <- cbind(V("contraceptcondom"), V("contraceptpill"), V("contraceptwithdraw"), V("contraceptother"), V("contraceptnomet"), V("contraceptnotsure"))
      it[!(it %in% 1:2)] <- NA; atb <- rowSums(!is.na(it)) > 0 & hs == 1 & !is.na(hs)
      kc <- it[, 1] == 1 & !is.na(it[, 1]); kp <- it[, 2] == 1 & !is.na(it[, 2])
      prezk01 <- fifelse(atb, 100 * kc, NA_real_); tab01 <- fifelse(atb, 100 * kp, NA_real_); nev01 <- fifelse(atb, 100 * (!kc & !kp), NA_real_)
      tab01b <- tab01; nev01b <- nev01; prez01b <- prez01
    } else {
      as_[!as_ %in% 1:6] <- NA                                        # 1 = ≤ 11, 2 = 12, 3 = 13, ..., 6 = 16+; uzdots tikai tiem, kam bijis
      agrini01 <- fifelse(hs == 2, 0, fifelse(hs == 1 & !is.na(as_), 100 * (as_ <= 3), NA_real_))
      cc <- V("contraceptcondom"); cc[!cc %in% 1:3] <- NA; cp <- V("contraceptpill"); cp[!cp %in% 1:3] <- NA
      cc[hs != 1 | is.na(hs)] <- NA; cp[hs != 1 | is.na(hs)] <- NA   # tikai tiem, kam bijis
      # a variants: "nezinu" saucējā (= ne jā); b variants: "nezinu" izslēgts
      prez01 <- fifelse(!is.na(cc), 100 * (cc == 1), NA_real_); prez01b <- fifelse(cc %in% 1:2, 100 * (cc == 1), NA_real_)
      tab01 <- fifelse(!is.na(cp), 100 * (cp == 1), NA_real_); tab01b <- fifelse(cp %in% 1:2, 100 * (cp == 1), NA_real_)
      nev01 <- fifelse(!is.na(cc) & !is.na(cp), 100 * (cc != 1 & cp != 1), NA_real_); nev01b <- fifelse(cc %in% 1:2 & cp %in% 1:2, 100 * (cc == 2 & cp == 2), NA_real_)
      prezk01 <- rep(NA_real_, nrow(d))
    }
    dz <- if (all(c("yearbirth", "monthbirth") %in% names(d))) num(d$yearbirth) + (num(d$monthbirth) - 0.5) / 12 else rep(NA_real_, nrow(d))
    M[[g]] <- data.table(gads = as.integer(g), kods = d$kods, valsts = d$valsts, cnt = d$cnt, sex = d$sex, w = d$w, skola = d$skola, dz = dz, svars = wv,
                         dzimumsakari = hadsex01, agrini = agrini01, prezervativs = prez01, tablete = tab01, neviens = nev01, prezervativs_k = prezk01,
                         prezervativs_b = prez01b, tablete_b = tab01b, neviens_b = nev01b)
    cat("15 gadīgie:", nrow(d), "; valstis:", uniqueN(d$valsts), "; svars:", wv, "; skola:", sk, "; klase:", kl, "\n")
    cat("Latvija 15 g.: n =", sum(d$cnt == "LVA", na.rm = TRUE), "; hadsex derīgi:", sum(!is.na(hadsex01[d$cnt == "LVA"])), "; bijuši:", sum(hadsex01[d$cnt == "LVA"] == 100, na.rm = TRUE),
        "; prezervatīva saucējs:", sum(!is.na(prez01[d$cnt == "LVA"])), "; tabletes saucējs:", sum(!is.na(tab01[d$cnt == "LVA"])), "\n")
    rm(d); invisible(gc(verbose = FALSE))
  }
  M <- rbindlist(M); saveRDS(M, KESS); cat("\nKešs (mikrodati, tikai lokāli, nenodot):", KESS, nrow(M), "rindas\n")
}
cat("\nLatvijas 15 gadīgo n pa kārtām un dzimumiem (derīgs hadsex):\n")
print(dcast(M[cnt == "LVA" & !is.na(dzimumsakari), .N, by = .(gads, dzimums = DZIM[as.character(sex)])], dzimums ~ gads, value.var = "N"), row.names = FALSE)
cat("Latvijas 15 gadīgo vidējais dzimšanas gads:", paste(M[cnt == "LVA", .(dz = round(mean(dz, na.rm = TRUE), 1)), by = gads][order(gads), paste0(gads, ": ", dz)], collapse = "; "), "\n")

# Harmonizācijas tabula
HARM <- rbindlist(list(
  data.table(karta = c("2001/02", "2005/06"), mainigais = "hadsex", kodejums = "0 = INAP; 1 = jā; 2 = nē", harmonizacija = "0 izslēgts; dzimumsakari = (1); saucējs 1 vai 2"),
  data.table(karta = c("2009/10", "2013/14", "2017/18"), mainigais = "hadsex", kodejums = "1 = jā; 2 = nē", harmonizacija = "dzimumsakari = (1); saucējs 1 vai 2"),
  data.table(karta = c("2001/02", "2005/06"), mainigais = "agesex", kodejums = "0 = INAP; 1 = nekad; 2 = 11 vai jaunāks; 3 = 12; 4 = 13; 5 = 14; 6 = 15; 7 = 16",
             harmonizacija = "0 izslēgts; agrini = (2, 3, 4); 0 ja 1 vai hadsex = 2; saucējs visi ar derīgu atbildi"),
  data.table(karta = "2009/10", mainigais = "agesex", kodejums = "1 = nekad; 2 = 11 vai jaunāks; ...; 7 = 16; 8 = 17 vai vecāks", harmonizacija = "agrini = (2, 3, 4); kā 2001 un 2006"),
  data.table(karta = c("2013/14", "2017/18"), mainigais = "agesex", kodejums = "1 = 11 vai jaunāks; 2 = 12; 3 = 13; 4 = 14; 5 = 15; 6 = 16 vai vecāks (uzdots tikai tiem, kam bijis)",
             harmonizacija = "agrini = hadsex = 1 un agesex (1, 2, 3); 0 ja hadsex = 2; NA ja hadsex = 1 un agesex tukšs"),
  data.table(karta = c("2001/02", "2005/06"), mainigais = "condomuse", kodejums = "0 = INAP; 1 = nekad nav bijis; 2 = jā; 3 = nē", harmonizacija = "prezervativs = (2); saucējs 2 vai 3"),
  data.table(karta = "2009/10", mainigais = "condomuse", kodejums = "1 = nekad; 2 = jā; 3 = nē", harmonizacija = "prezervativs = (2); saucējs 2 vai 3"),
  data.table(karta = c("2013/14", "2017/18"), mainigais = "contraceptcondom", kodejums = "1 = jā; 2 = nē; 3 = nezinu (tikai tiem, kam bijis)", harmonizacija = "prezervativs = (1); saucējs pēc pārbaudes pret pārlūku (a: 1, 2, 3; b: 1, 2)"),
  data.table(karta = c("2001/02", "2005/06"), mainigais = "contraceptcondom, contraceptpill, contraceptwithdraw, contraceptother, contraceptnomet, contraceptnotsure",
             kodejums = "0 = INAP; 1 = jā; 2 = nē (atzīmējamais saraksts, neatzīmētais pārsvarā tukšs)",
             harmonizacija = "saucējs: hadsex = 1 un vismaz viens vienums atbildēts; tablete = pill (1); neviens = ne condom (1), ne pill (1); prezervativs_k = condom (1)"),
  data.table(karta = "2009/10", mainigais = "contraceptcondom, contraceptpill, ...", kodejums = "1 = jā; 2 = nē katram vienumam", harmonizacija = "kā 2001 un 2006"),
  data.table(karta = c("2013/14", "2017/18"), mainigais = "contraceptpill", kodejums = "1 = jā; 2 = nē; 3 = nezinu", harmonizacija = "tablete = (1); neviens = condom ≠ 1 un pill ≠ 1 (a) vai condom = 2 un pill = 2 (b); saucējs pēc pārbaudes"),
  data.table(karta = "2013/14", mainigais = "contraceptnat", kodejums = "1 = jā; 2 = nē; 3 = nezinu", harmonizacija = "nelieto (nacionālā izvēle, Latvijai tukšs)"),
  data.table(karta = "2021/22", mainigais = "pārlūks d_sex, d_condom, d_nocontracept", kodejums = "procenti meitenēm un zēniem; kopā = pārlūka sort_gender", harmonizacija = "dzimumsakari, prezervativs, neviens; agrini un tablete 2022 nav"),
  data.table(karta = c("2001/02", "2005/06", "2009/10"), mainigais = "sampleweights", kodejums = "svars", harmonizacija = "lietots; = 1 lielākajai daļai valstu"),
  data.table(karta = "2013/14", mainigais = "M137", kodejums = "svars", harmonizacija = "lietots; = 1 visām, izņemot DEU (faktiski nesvērts)"),
  data.table(karta = "2017/18", mainigais = "weight", kodejums = "svars", harmonizacija = "lietots"),
  data.table(karta = c("2001/02", "2009/10"), mainigais = "schoolno, classno", kodejums = "klasteri", harmonizacija = "SE ar skolu klasteriem; DEU 2001/02 bez skolas un klases (indivīds)"),
  data.table(karta = "2005/06", mainigais = "schoolno, cluster", kodejums = "klasteri", harmonizacija = "SE ar skolu klasteriem; kur skolas nav, cluster"),
  data.table(karta = "2013/14", mainigais = "id2, ClassID", kodejums = "klasteri", harmonizacija = "SE ar skolu (id2) klasteriem; kur nav, ClassID"),
  data.table(karta = "2017/18", mainigais = "id2, cluster", kodejums = "klasteri", harmonizacija = "SE ar skolu (id2) klasteriem")))
fwrite(HARM, "output/pisa2025/hbsc_seks_harmonizacija.csv")
cat("\nHarmonizācijas tabula: output/pisa2025/hbsc_seks_harmonizacija.csv,", nrow(HARM), "rindas\n")

# ===========================================================================
cat("\n", strrep("=", 100), "\n2. Apkopojumi: valsts × kārta × dzimums × rādītājs (svērts, skolu klasteru SE)\n", strrep("=", 100), "\n", sep = "")
# ===========================================================================
wmean_se <- function(x, w, cl, min_n = 100) {
  i <- !is.na(x) & !is.na(w) & w > 0; x <- x[i]; w <- w[i]; cl <- cl[i]
  if (length(x) < min_n) return(c(NA_real_, NA_real_, length(x)))
  m <- sum(w * x) / sum(w); us <- tapply(w * (x - m), cl, sum); G <- length(us)
  if (G < 2) return(c(m, sqrt(sum(w^2 * (x - m)^2)) / sum(w), length(x)))
  c(m, sqrt(G / (G - 1) * sum(us^2)) / sum(w), length(x))
}
RADI <- c(names(RAD), "prezervativs_b", "tablete_b", "neviens_b")
MINN <- c(MIN_N, prezervativs_b = 30, tablete_b = 30, neviens_b = 30)
agg <- function(D) D[, { r <- rbindlist(lapply(RADI, function(v) { e <- wmean_se(get(v), w, skola, MINN[[v]]); data.table(raditajs = v, est = e[1], se = e[2], n = as.integer(e[3])) }))
  r[, `:=`(skolas = uniqueN(skola), svari = fifelse(uniqueN(w) > 1, "svērts", "nesvērts (svars = 1)"))] }, by = .(gads, kods, valsts, cnt, dzimums)]
AG <- rbind(agg(M[!is.na(sex)][, dzimums := DZIM[as.character(sex)]]), agg(copy(M)[, dzimums := "kopā"]))
AG <- AG[!is.na(est)][, avots := "HBSC mikrodati"]
cat("Mikrodatu apkopojumu (ar n ≥ minimumu):", nrow(AG), "; valstis × kārtas:", uniqueN(AG[, paste(gads, valsts)]), "\n")
cat("Minimālais n: dzimumsakari un agrini 100; prezervatīvs, tablete, neviens 30 (starp tiem, kam bijis).\n")
cat("\nValstu skaits ar dzimumsakaru rādītāju (kopā) pa kārtām:", paste(AG[raditajs == "dzimumsakari" & dzimums == "kopā", .N, by = gads][order(gads), paste0(gads, ": ", N)], collapse = "; "), "\n")

# ===========================================================================
cat("\n", strrep("=", 100), "\n3. Pārlūks 2014, 2018, 2022 un pārbaude pret mikrodatiem 2014 un 2018\n", strrep("=", 100), "\n", sep = "")
# ===========================================================================
HB <- fread("data/hbsc/hbsc_browser_data.csv", header = TRUE, encoding = "UTF-8", fill = TRUE)   # otrā rinda tukša, tāpēc header = TRUE jāpasaka
HB <- HB[HBSC_id %in% c("d_sex", "d_condom", "d_nocontracept") & Age_group == "15-year-olds" & !grepl("AVERAGE", Country)]   # HBSC vidējā rinda nav valsts
HB <- HB[, .(gads = as.integer(Survey_year), valsts = Country, raditajs = c(d_sex = "dzimumsakari", d_condom = "prezervativs", d_nocontracept = "neviens")[HBSC_id],
             meitenes = num(Girl), zēni = num(Boy), kopā = num(sort_gender))]
HB <- melt(HB, id.vars = c("gads", "valsts", "raditajs"), variable.name = "dzimums", value.name = "est")[!is.na(est)][, dzimums := as.character(dzimums)]
HB[, cnt := names(VAL_NOS)[match(valsts, VAL_NOS)]]
cat("Pārlūka rindas:", nrow(HB), "; valstis 2022:", uniqueN(HB[gads == 2022, valsts]), "\n")
cat("Pārlūka valstis, kuru nav mikrodatu nosaukumos (2014, 2018):", paste(setdiff(HB[gads < 2022, unique(valsts)], AG$valsts), collapse = ", "), "\n")
cat("Mikrodatu valstis, kuru nav pārlūkā (2014, 2018):", paste(setdiff(AG[gads >= 2014, unique(valsts)], HB$valsts), collapse = ", "), "\n")

# Pārbaude: a (nezinu saucējā) pret b (nezinu izslēgts) 2014 un 2018; visas valstis
PB <- merge(AG[gads %in% c(2014, 2018), .(gads, valsts, cnt, dzimums, raditajs, mikro = est, n)], HB[, .(gads, valsts, dzimums, raditajs, parluks = est)],
            by.x = c("gads", "valsts", "dzimums", "raditajs"), by.y = c("gads", "valsts", "dzimums", "raditajs"))
PBb <- merge(AG[gads %in% c(2014, 2018) & raditajs %in% c("prezervativs_b", "tablete_b", "neviens_b"), .(gads, valsts, cnt, dzimums, raditajs = sub("_b$", "", raditajs), mikro_b = est)],
             HB[, .(gads, valsts, dzimums, raditajs, parluks = est)], by = c("gads", "valsts", "dzimums", "raditajs"))
PB <- merge(PB, PBb[, .(gads, valsts, dzimums, raditajs, mikro_b)], by = c("gads", "valsts", "dzimums", "raditajs"), all.x = TRUE)
PB[, `:=`(starp_a = mikro - parluks, starp_b = mikro_b - parluks)]
cat("\nMikrodati pret pārlūku 2014 un 2018 (visas valstis ar abiem avotiem; a: nezinu saucējā; b: nezinu izslēgts):\n")
print(PB[, .(punkti = .N, r_a = round(cor(mikro, parluks), 3), vid_starp_a = round(mean(starp_a), 2), abs_starp_a = round(mean(abs(starp_a)), 2), maks_abs_a = round(max(abs(starp_a)), 1),
             abs_starp_b = round(mean(abs(starp_b), na.rm = TRUE), 2), maks_abs_b = round(max(abs(starp_b), na.rm = TRUE), 1)), by = .(raditajs, gads)][order(raditajs, gads)], row.names = FALSE)
IZV <- PB[raditajs != "dzimumsakari", .(a = mean(abs(starp_a)), b = mean(abs(starp_b), na.rm = TRUE)), by = raditajs][, variants := fifelse(b < a, "b", "a")]
IZV <- rbind(IZV, data.table(raditajs = "tablete", a = NA_real_, b = NA_real_, variants = IZV[raditajs == "neviens", variants]))   # tabletes pārlūkā nav; tas pats jautājums kā neviens
cat("\nIzvēlētais variants 2014 un 2018 (mazākā vidējā absolūtā starpība pret pārlūku; tabletei kā neviens):\n"); print(IZV[, .(raditajs, abs_a = round(a, 2), abs_b = round(b, 2), variants)], row.names = FALSE)
SAUC <- setNames(fifelse(IZV$variants == "a", "jā, nē, nezinu", "jā, nē (nezinu izslēgts)"), IZV$raditajs)
cat("\nLatvija un 7 valstis: mikrodati (a / b) pret pārlūku, 2014 un 2018:\n")
print(PB[!is.na(cnt)][order(raditajs, gads, cnt, dzimums), .(raditajs, gads, cnt, dzimums, mikro = round(mikro, 1), mikro_b = round(mikro_b, 1), parluks, n)], row.names = FALSE, nrows = 200)
fwrite(PB[order(raditajs, gads, valsts, dzimums)], "output/pisa2025/hbsc_seks_parbaude.csv")

# Galvenā rinda: izvēlētais variants 2014 un 2018 (a vai b); 2001 līdz 2010 nemainās
for (v in IZV[variants == "b", raditajs]) AG[gads >= 2014 & raditajs == paste0(v, "_b"), raditajs := paste0(v, "_izv")]
for (v in IZV[variants == "a", raditajs]) AG[gads >= 2014 & raditajs == v, raditajs := paste0(v, "_izv")]
AG <- AG[!raditajs %in% c("prezervativs", "tablete", "neviens", "prezervativs_b", "tablete_b", "neviens_b") | gads < 2014]
AG[, raditajs := sub("_izv$", "", raditajs)]
AG <- AG[!raditajs %in% c("prezervativs_b", "tablete_b", "neviens_b")]
AG[, piezime := ""]
AG[gads >= 2014 & raditajs %in% c("prezervativs", "tablete", "neviens"), piezime := paste0("saucējs: ", SAUC[raditajs])]
AG[gads == 2014, piezime := paste0(fifelse(piezime == "", "", paste0(piezime, "; ")), "M137 = 1 (faktiski nesvērts)")]

# Pārlūka rindas: 2022 visām valstīm; 2014 LTU (mikrodatos ed. 1.1 Lietuvas nav). SE tuvinājums: binomiāls ar n no 2018 mikrodatiem tai pašai grupai
N18 <- AG[gads == 2018, .(valsts, dzimums, raditajs, n18 = n)]
HBp <- HB[gads == 2022 | (gads == 2014 & cnt == "LTU")]
HBp <- merge(HBp, N18, by = c("valsts", "dzimums", "raditajs"), all.x = TRUE)
HBp[is.na(n18), n18 := as.integer(median(N18$n18[N18$raditajs == .BY$raditajs])), by = raditajs]
HBp[, se := 100 * sqrt(pmin(pmax(est, 1), 99) / 100 * (1 - pmin(pmax(est, 1), 99) / 100) / n18)]
HBp <- HBp[, .(gads, kods = NA_real_, valsts, cnt, dzimums, raditajs, est, se, n = NA_integer_, skolas = NA_integer_, svari = "pārlūks (svērts HBSC)", avots = "HBSC Data Browser",
               piezime = paste0("SE tuvinājums ar n ≈ ", n18, " no 2018 mikrodatiem"))]
cat("\nPārlūka rindas rindai: 2022:", nrow(HBp[gads == 2022]), "; 2014 LTU:", nrow(HBp[gads == 2014]), "\n")

A <- rbind(AG, HBp)[, karta := KARTAS[as.character(gads)]][, paaudze := PAAUDZE[as.character(gads)]][, nosaukums := RAD[raditajs]]
A <- A[order(raditajs, gads, valsts, dzimums)]
fwrite(A[, .(valsts, cnt, gads, karta, paaudze, dzimums, raditajs, nosaukums, est = round(est, 2), se = round(se, 3), n, skolas, svari, avots, piezime)], "output/pisa2025/hbsc_seks_rinda.csv")
cat("hbsc_seks_rinda.csv:", nrow(A), "rindas;", uniqueN(A$valsts), "valstis\n")

# ===========================================================================
cat("\n", strrep("=", 100), "\n4. Latvija: rinda, rangs, 7 valstu vidējais, visu valstu mediāna, dzimumu starpība\n", strrep("=", 100), "\n", sep = "")
# ===========================================================================
A[, `:=`(n_valstis = .N, rangs = frank(-est, ties.method = "min"), mediana_visas = median(est)), by = .(gads, dzimums, raditajs)]
V7 <- A[cnt %in% SAL, .(vid7 = mean(est), n7 = .N, valstis7 = paste(sort(cnt), collapse = " ")), by = .(gads, dzimums, raditajs)]
LV <- merge(A[cnt == "LVA"], V7, by = c("gads", "dzimums", "raditajs"), all.x = TRUE)
LV[, lva_minus_vid7 := est - vid7]
ZM <- dcast(LV[dzimums != "kopā", .(gads, raditajs, dz = fifelse(dzimums == "zēni", "z", "m"), est, se)], gads + raditajs ~ dz, value.var = c("est", "se"))
ZM[, `:=`(zeni_minus_meitenes = est_z - est_m, se_starp = sqrt(se_z^2 + se_m^2))]
LV <- merge(LV, ZM[, .(gads, raditajs, zeni_minus_meitenes, se_starp)], by = c("gads", "raditajs"), all.x = TRUE)
LV[dzimums != "kopā", `:=`(zeni_minus_meitenes = NA_real_, se_starp = NA_real_)]
LV <- LV[order(raditajs, gads, factor(dzimums, c("kopā", "zēni", "meitenes")))]
fwrite(LV[, .(gads, karta, paaudze, dzimums, raditajs, nosaukums, est = round(est, 2), se = round(se, 3), n, avots, rangs, n_valstis, vid7 = round(vid7, 2), n7, valstis7,
              lva_minus_vid7 = round(lva_minus_vid7, 2), mediana_visas = round(mediana_visas, 2), zeni_minus_meitenes = round(zeni_minus_meitenes, 2), se_starp = round(se_starp, 3), piezime)],
       "output/pisa2025/hbsc_seks_latvija.csv")
cat("hbsc_seks_latvija.csv:", nrow(LV), "rindas\n")
f1 <- function(x) ifelse(is.na(x), "", sprintf("%.1f", x))
for (v in names(RAD)) {
  cat("\n--- ", RAD[[v]], " ---\n", sep = "")
  x <- LV[raditajs == v]
  if (!nrow(x)) { cat("nav datu\n"); next }
  cat("Latvija (est ± SE; n):\n")
  print(dcast(x[, .(gads, dzimums, v = sprintf("%.1f ± %.1f (n %s)", est, se, ifelse(is.na(n), "pārlūks", n)))], dzimums ~ gads, value.var = "v"), row.names = FALSE)
  cat("Rangs starp visām HBSC valstīm (1 = augstākā vērtība) / valstu skaits:\n")
  print(dcast(x[, .(gads, dzimums, v = paste0(rangs, " / ", n_valstis))], dzimums ~ gads, value.var = "v"), row.names = FALSE)
  cat("7 valstu vidējais (n7) un visu valstu mediāna, kopā:\n")
  print(x[dzimums == "kopā", .(gads, lva = round(est, 1), vid7 = round(vid7, 1), n7, lva_minus_vid7 = round(lva_minus_vid7, 1), mediana_visas = round(mediana_visas, 1), n_valstis)], row.names = FALSE)
  cat("Dzimumu starpība Latvijā (zēni mīnus meitenes, pp ± SE):\n")
  print(x[dzimums == "kopā", .(gads, zeni_minus_meitenes = round(zeni_minus_meitenes, 1), se = round(se_starp, 1))], row.names = FALSE)
}
cat("\n7 valstis pa kārtām (dzimumsakari, kopā, %):\n")
print(dcast(A[cnt %in% VAL & raditajs == "dzimumsakari" & dzimums == "kopā", .(gads, cnt, v = round(est, 1))], cnt ~ gads, value.var = "v"), row.names = FALSE)
cat("\n7 valstis pa kārtām (prezervatīvs, kopā, %):\n")
print(dcast(A[cnt %in% VAL & raditajs == "prezervativs" & dzimums == "kopā", .(gads, cnt, v = round(est, 1))], cnt ~ gads, value.var = "v"), row.names = FALSE)
cat("\n7 valstis pa kārtām (agrīnā pieredze, kopā, %):\n")
print(dcast(A[cnt %in% VAL & raditajs == "agrini" & dzimums == "kopā", .(gads, cnt, v = round(est, 1))], cnt ~ gads, value.var = "v"), row.names = FALSE)
cat("\nPrezervatīvs 2001 līdz 2010 Latvijā: atsevišķais jautājums (condomuse) pret kontracepcijas sarakstu (contraceptcondom), kopā:\n")
print(dcast(LV[raditajs %in% c("prezervativs", "prezervativs_k") & dzimums == "kopā" & gads <= 2010, .(gads, raditajs, v = sprintf("%.1f (n %d)", est, n))], raditajs ~ gads, value.var = "v"), row.names = FALSE)

# Lineārais trends Latvijā (pp kārtā, svērts 1/SE²), dzimumsakari un prezervatīvs
cat("\nLineārais trends Latvijā 2002 līdz 2022 (pp gadā ± SE, svērts 1/SE²):\n")
TR <- LV[raditajs %in% c("dzimumsakari", "agrini", "prezervativs", "neviens") & !is.na(se), if (.N < 3) NULL else { f <- summary(lm(est ~ gads, weights = 1 / se^2))$coefficients
  .(punkti = .N, slipums = round(f[2, 1], 2), se = round(f[2, 2], 2), p = signif(f[2, 4], 2)) }, by = .(raditajs, dzimums)]
print(TR[order(raditajs, dzimums)], row.names = FALSE)

# ===========================================================================
cat("\n", strrep("=", 100), "\n5. Grafiks\n", strrep("=", 100), "\n", sep = "")
# ===========================================================================
TEMA <- theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), plot.title.position = "plot", plot.title = element_text(face = "bold"),
                                              plot.caption = element_text(colour = "grey40", hjust = 0), legend.position = "bottom")
KR <- c("Latvija, zēni" = "#2a78d6", "Latvija, meitenes" = "#c2410c", "7 valstu vidējais (kopā)" = "#3f3f46", "Visu HBSC valstu mediāna (kopā)" = "#8a8f98")
G <- rbind(LV[dzimums != "kopā", .(gads, raditajs, grupa = paste0("Latvija, ", dzimums), est, se, avots)],
           LV[dzimums == "kopā", .(gads, raditajs, grupa = "7 valstu vidējais (kopā)", est = vid7, se = NA_real_, avots = "HBSC mikrodati")],
           LV[dzimums == "kopā", .(gads, raditajs, grupa = "Visu HBSC valstu mediāna (kopā)", est = mediana_visas, se = NA_real_, avots = "HBSC mikrodati")])
G[gads == 2022, avots := "HBSC Data Browser"]; G[, grupa := factor(grupa, names(KR))]
pan <- function(v, tit, yl) {
  g <- G[raditajs == v & !is.na(est)]
  ggplot(g, aes(gads, est, colour = grupa)) +
    geom_ribbon(data = g[!is.na(se)], aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se, fill = grupa), alpha = 0.12, colour = NA) +
    geom_line(aes(linetype = grupa), linewidth = 0.8) + geom_point(aes(shape = avots), size = 2.6, fill = "white", stroke = 0.9) +
    geom_text(data = g[grupa %in% c("Latvija, zēni", "Latvija, meitenes")], aes(label = sprintf("%.0f", est)), size = 2.7, vjust = -1.1, show.legend = FALSE) +
    scale_colour_manual(values = KR, name = NULL) + scale_fill_manual(values = KR, guide = "none") +
    scale_linetype_manual(values = c("solid", "solid", "solid", "dashed"), name = NULL) +
    scale_shape_manual(values = c("HBSC mikrodati" = 16, "HBSC Data Browser" = 21), name = NULL) +
    scale_x_continuous(breaks = c(2002, 2006, 2010, 2014, 2018, 2022), labels = KARTAS, name = NULL) +
    labs(y = yl, title = tit) + TEMA + theme(axis.text.x = element_text(size = 9))
}
p1 <- pan("dzimumsakari", "Bijuši dzimumsakari, % no 15 gadīgajiem", "%") + coord_cartesian(ylim = c(0, 45))
p2 <- pan("prezervativs", "Prezervatīvs pēdējos dzimumsakaros, % no tiem, kam bijis", "%") + coord_cartesian(ylim = c(40, 95))
p <- (p1 | p2) + plot_layout(guides = "collect") +
  plot_annotation(title = "HBSC: Latvijas 15 gadīgo seksuālā uzvedība 2001/02 līdz 2021/22 pret 7 valstīm un visām HBSC valstīm",
                  subtitle = "Josla = 95 % intervāls (skolu klasteru SE; 2021/22 tuvinājums). 7 valstis: EST, LTU, POL, FIN, CZE, HUN, DEU. Prezervatīva jautājums 2001 līdz 2010 un 2014 līdz 2022 atšķiras.",
                  caption = "Avots: HBSC Open Access data files HBSC2001OAed1.0, HBSC2006OAed1.0, HBSC2010OAed1.0, HBSC2014OAed1.1, HBSC2018OAed1.1, HBSC Data Management Centre, University of Bergen;\n2021/22 un LTU 2013/14: HBSC Data Browser. Skripts 85, docs/88.",
                  theme = TEMA)
f <- file.path(PL, "85_hbsc_seks_rinda.png"); ggsave(f, p, width = 14, height = 7, dpi = 200, bg = "white"); cat("Saglabāts:", f, "\n")

# ===========================================================================
cat("\n", strrep("=", 100), "\n6. Kopsavilkums un izvades faili\n", strrep("=", 100), "\n", sep = "")
# ===========================================================================
cat("Izvade:\n  output/pisa2025/hbsc_seks_rinda.csv, hbsc_seks_latvija.csv, hbsc_seks_harmonizacija.csv, hbsc_seks_parbaude.csv\n  ", f, "\n  kešs (privāts): ", KESS, "\n", sep = "")
cat("\nPabeigts:", format(Sys.time()), "\n")

# --- Noslēgums ---------------------------------------------------------------
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
