# ============================================================================
# 22. Ex ante prognoze: ko varēja pateikt 2023. gada decembrī
# ============================================================================
#
# Jautājums. Vai izglītības politikas veidotāji varēja paredzēt 2025. gada
# lasītprasmes kritumu. Tas nav jautājums par mūsu modeļa precizitāti, tas ir
# jautājums par to, vai informācija bija pieejama un vai tā bija pietiekama.
#
# Kas bija zināms 2023. gada decembrī, kad publicēja PISA 2022 rezultātus:
#   1. PISA 2022 mikrodati, ieskaitot testa valodu katram skolēnam
#   2. Ka 2022. gadā 416 krievu ģimeņu bērni jau kārtoja testu latviski,
#      bet 1436 kārtoja krieviski
#   3. Pārejas uz latviešu mācību valodu grafiks, tātad ka līdz 2025. gadam
#      visi kārtos testu latviski
#
# Tātad 2023. gadā bija iespējams uzdot jautājumu: cik zemāk lasa krievu
# ģimenes bērns, kas jau tagad darbojas latviski, un kas notiks, kad visa
# grupa būs tādā stāvoklī. Šis skripts to izrēķina tā, kā to varēja izrēķināt
# tolaik, un tikai pēc tam salīdzina ar to, kas patiešām notika.
#
# SVARĪGI PAR NOVIRZI. 2022. gada latviski testētie krievu ģimeņu bērni nav
# nejauša izlase, jo ģimene skolu izvēlējās pati. Tāpēc rēķinām divas versijas,
# proti, neapstrādātu un ar ESCS un demogrāfijas kontroli, un abas rādām.
#
# Ievade:  data/pisa2022/processed/pisa_lva_2022_stu.rds
#          data/pisa2025/processed/pisa_lva_2025_stu.rds  (tikai pārbaudei)
# Izvade:  output/pisa2025/ex_ante_prognoze.csv
#          output/pisa2025/logs/22_ex_ante_prognoze_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/22_ex_ante_prognoze_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

required_packages <- c("haven")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}

d22 <- readRDS("data/pisa2022/processed/pisa_lva_2022_stu.rds")
d25 <- readRDS("data/pisa2025/processed/pisa_lva_2025_stu.rds")

FAY_DALITAJS <- 20
PV <- paste0("PV", 1:10, "READ")

grupa_no_langn <- function(d) {
  lab <- as.character(as_factor(d$LANGN))
  g <- rep("Cita", length(lab))
  g[grepl("^Latvian", lab)] <- "Latviesu"
  g[grepl("^Russian", lab)] <- "Krievu"
  factor(g, levels = c("Latviesu", "Krievu", "Cita"))
}
d22$grupa <- grupa_no_langn(d22)
d25$grupa <- grupa_no_langn(d25)
d22$tests_lv <- as.numeric(as.character(as_factor(d22$LANGTEST_COG)) == "Latvian")
d22$read <- rowMeans(sapply(PV, function(p) d22[[p]]))
d25$read <- rowMeans(sapply(PV, function(p) d25[[p]]))

sv <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)])

pisa_videjais <- function(d, i) {
  rw <- grep("^W_FSTURWT", names(d), value = TRUE)
  est_pv <- vapply(PV, function(p) sv(d[[p]][i], d$W_FSTUWT[i]), numeric(1))
  est <- mean(est_pv)
  rep_est <- vapply(rw, function(r) sv(d[[PV[1]]][i], d[[r]][i]), numeric(1))
  var_izl <- sum((rep_est - est_pv[1])^2) / FAY_DALITAJS
  var_imp <- (1 + 1 / length(PV)) * stats::var(est_pv)
  c(est = est, se = sqrt(var_izl + var_imp), n = sum(i),
    dala = sum(d$W_FSTUWT[i]) / sum(d$W_FSTUWT))
}


# === 1. KAS BIJA REDZAMS 2022. GADA DATOS ===================================
cat(strrep("=", 70), "\n")
cat("1. Signāls, kas bija 2022. gada datos\n")
cat(strrep("=", 70), "\n\n")

kr <- d22$grupa == "Krievu"
a_kr_lv <- pisa_videjais(d22, kr & d22$tests_lv == 1)
a_kr_ru <- pisa_videjais(d22, kr & d22$tests_lv == 0)
a_lv    <- pisa_videjais(d22, d22$grupa == "Latviesu")

cat("Krievu ģimeņu bērni 2022. gadā, pēc testa valodas:\n")
cat(sprintf("  tests latviski   %6.1f (SE %4.1f)  n = %4d   %4.1f %% izlases\n",
            a_kr_lv["est"], a_kr_lv["se"], a_kr_lv["n"], 100 * a_kr_lv["dala"]))
cat(sprintf("  tests krieviski  %6.1f (SE %4.1f)  n = %4d   %4.1f %% izlases\n",
            a_kr_ru["est"], a_kr_ru["se"], a_kr_ru["n"], 100 * a_kr_ru["dala"]))
cat(sprintf("  starpība         %6.1f\n\n", a_kr_lv["est"] - a_kr_ru["est"]))
cat(sprintf("Latviešu ģimenes  %6.1f (SE %4.1f)  n = %4d\n\n",
            a_lv["est"], a_lv["se"], a_lv["n"]))

cat("Šī starpība bija redzama publiskajos 2022. gada mikrodatos no 2023. gada\n")
cat("decembra. Tā ir tiešā liecība par to, cik zemāk lasa krievu ģimenes\n")
cat("bērns, kas jau darbojas latviski.\n\n")


# === 2. SODS PAR LATVISKU TESTU, AR KONTROLĒM ===============================
cat(strrep("=", 70), "\n")
cat("2. Cik no starpības paliek pēc kontrolēm\n")
cat(strrep("=", 70), "\n\n")

cat("Latviski testētie nav nejauša izlase, jo skolu izvēlējās ģimene.\n")
cat("Regresija tikai krievu ģimeņu bērnu vidū, 2022. gads.\n\n")

kr22 <- d22[kr, ]
kr22$sieviete <- as.numeric(as.character(as_factor(kr22$ST004D01T)) == "Female")
mod <- list(
  M0 = read ~ tests_lv,
  M1 = read ~ tests_lv + ESCS,
  M2 = read ~ tests_lv + ESCS + sieviete
)
sodi <- numeric(0)
for (nos in names(mod)) {
  f <- lm(mod[[nos]], data = kr22, weights = W_FSTUWT)
  b <- coef(f)["tests_lv"]
  sodi[nos] <- b
  cat(sprintf("  %-4s beta(tests latviski) = %6.1f   n = %d\n",
              nos, b, nobs(f)))
}
cat("\nSods, ko lietosim prognozē:", round(sodi["M2"], 1), "punkti\n\n")


# === 3. PROGNOZE, KO VARĒJA UZTAISĪT 2023. GADĀ =============================
cat(strrep("=", 70), "\n")
cat("3. Prognoze 2025. gadam ar 2023. gadā pieejamo informāciju\n")
cat(strrep("=", 70), "\n\n")

cat("Pieņēmumi, katrs ir tāds, ko 2023. gadā varēja pamatot:\n")
cat("  A. Līdz 2025. gadam visi kārto testu latviski, tas bija zināms grafiks\n")
cat("  B. Latviešu ģimeņu rezultāts turpina 2018 līdz 2022 trendu\n")
cat("  C. Krievu ģimeņu bērniem pievienojas sods par latvisku testu\n")
cat("  D. Populācijas sastāvs paliek 2022. gada līmenī\n\n")

# B. Latviešu grupas trends. 2018. gada Latvijas vidējais lasīšanā ir 478.7,
# 2022. gadā 474.6, tātad aptuveni -1 punkts gadā. Trīs gadi ir -3.1.
TRENDS_3G <- -3.1

dalas22 <- c(
  Latviesu = unname(a_lv["dala"]),
  Krievu   = sum(d22$W_FSTUWT[kr]) / sum(d22$W_FSTUWT),
  Cita     = sum(d22$W_FSTUWT[d22$grupa == "Cita"]) / sum(d22$W_FSTUWT)
)
a_cita <- pisa_videjais(d22, d22$grupa == "Cita")

# Krievu grupas 2022. gada vidējais, visi kopā.
a_kr_visi <- pisa_videjais(d22, kr)

prognoze <- function(sods, nos) {
  lv_p   <- unname(a_lv["est"]) + TRENDS_3G
  # Krievu grupā sodu saņem tie, kas 2022. gadā vēl testu kārtoja krieviski.
  dala_ru <- unname(a_kr_ru["dala"] / (a_kr_lv["dala"] + a_kr_ru["dala"]))
  kr_p   <- unname(a_kr_visi["est"]) + TRENDS_3G + dala_ru * unname(sods)
  cita_p <- unname(a_cita["est"]) + TRENDS_3G + dala_ru * unname(sods)
  kopa   <- dalas22["Latviesu"] * lv_p + dalas22["Krievu"] * kr_p +
    dalas22["Cita"] * cita_p
  cat(sprintf("  %-22s latviešu %6.1f   krievu %6.1f   valsts %6.1f\n",
              nos, lv_p, kr_p, kopa))
  c(latviesu = lv_p, krievu = kr_p, valsts = unname(kopa))
}

cat("Prognozētie 2025. gada rezultāti:\n")
p_neapstr <- prognoze(a_kr_lv["est"] - a_kr_ru["est"], "neapstrādāts sods")
p_kontrol <- prognoze(sodi["M2"], "sods ar kontrolēm")
p_bez     <- prognoze(0, "bez soda, tikai trends")


# === 4. SALĪDZINĀJUMS AR TO, KAS NOTIKA =====================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("4. Ko rādīja 2025. gads\n")
cat(strrep("=", 70), "\n\n")

f_lv   <- pisa_videjais(d25, d25$grupa == "Latviesu")
f_kr   <- pisa_videjais(d25, d25$grupa == "Krievu")
f_kopa <- pisa_videjais(d25, rep(TRUE, nrow(d25)))

cat(sprintf("  %-22s %8s %10s %10s\n", "", "prognoze", "faktiskais", "kļūda"))
rindas <- list(
  c("Latviešu ģimenes", p_kontrol["latviesu"], f_lv["est"]),
  c("Krievu ģimenes",   p_kontrol["krievu"],   f_kr["est"]),
  c("Valsts vidējais",  p_kontrol["valsts"],   f_kopa["est"])
)
for (r in rindas) {
  pr <- as.numeric(r[2]); fk <- as.numeric(r[3])
  cat(sprintf("  %-22s %8.1f %10.1f %10.1f\n", r[1], pr, fk, fk - pr))
}

cat("\nValsts vidējais pēc trim pieņēmumiem par sodu:\n")
for (p in list(c("bez soda", p_bez["valsts"]), c("ar kontrolēm", p_kontrol["valsts"]),
               c("neapstrādāts", p_neapstr["valsts"]))) {
  cat(sprintf("  %-14s %6.1f   kļūda %6.1f\n", p[1], as.numeric(p[2]),
              f_kopa["est"] - as.numeric(p[2])))
}


# === 5. CIK LIELA DAĻA BIJA PAREDZAMA ========================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("5. Cik liela krituma daļa bija paredzama\n")
cat(strrep("=", 70), "\n\n")

k22 <- pisa_videjais(d22, rep(TRUE, nrow(d22)))
kopejais <- f_kopa["est"] - k22["est"]
paredzets_trends <- p_bez["valsts"] - k22["est"]
paredzets_sods   <- p_kontrol["valsts"] - k22["est"]

cat(sprintf("  Faktiskais kritums 2022 līdz 2025            %6.1f\n", kopejais))
cat(sprintf("  Paredzams tikai no trenda                    %6.1f  (%2.0f %%)\n",
            paredzets_trends, 100 * paredzets_trends / kopejais))
cat(sprintf("  Paredzams no trenda un testa valodas soda     %6.1f  (%2.0f %%)\n",
            paredzets_sods, 100 * paredzets_sods / kopejais))
cat(sprintf("  Neparedzētais atlikums                       %6.1f  (%2.0f %%)\n",
            kopejais - paredzets_sods,
            100 * (kopejais - paredzets_sods) / kopejais))

rez <- data.frame(
  raditajs = c("faktiskais_2022", "faktiskais_2025", "kritums",
               "prognoze_bez_soda", "prognoze_ar_kontrolem", "prognoze_neapstradats",
               "sods_neapstradats", "sods_ar_kontrolem"),
  vertiba = c(k22["est"], f_kopa["est"], kopejais,
              p_bez["valsts"], p_kontrol["valsts"], p_neapstr["valsts"],
              a_kr_lv["est"] - a_kr_ru["est"], sodi["M2"])
)
write.csv(rez, "output/pisa2025/ex_ante_prognoze.csv", row.names = FALSE)

cat("\nIzveidotie faili:\n")
cat("  output/pisa2025/ex_ante_prognoze.csv\n")
cat(" ", output_file, "\n")
cat("\nPabeigts:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
