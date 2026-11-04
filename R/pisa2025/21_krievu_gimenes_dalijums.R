# ============================================================================
# 21. Lasītprasmes kritums pa ģimenes valodas un skolas vides grupām
# ============================================================================
#
# Jautājums. Cik liels ir 2022 līdz 2025 lasītprasmes kritums atsevišķi
# latviešu ģimenēs, krievu ģimenēs, kuru bērni darbojas latviskā vidē, un
# krievu ģimenēs, kuru bērni darbojas krieviskā vidē.
#
# UZMANĪBU AR INTERPRETĀCIJU. PISA nav mainīgā, kas abos gados pateiktu, vai
# krievu ģimenes bērns runā arī latviski. LANGN dod vienu mājās runāto valodu,
# ST333 par valodu skaitu mājās ir tikai 2025. gadā, vecāku anketa Latvijā
# 2025. gadā netika administrēta, etniskās piederības mainīgā PISA nav.
# Tāpēc šeit dalām nevis pēc bērna valodu prasmes, bet pēc skolas valodas
# vides, proti, cik liela daļa skolasbiedru mājās runā krieviski. Tas ir
# aizstājējs, un 4. sadaļā ir pārbaude, cik labi tas sakrīt ar 2022. gada
# testa valodu, kas ir asākais pieejamais rādītājs, bet tikai vienā gadā.
#
# Ievade:  data/pisa2022/processed/pisa_lva_2022_stu.rds
#          data/pisa2025/processed/pisa_lva_2025_stu.rds
# Izvade:  output/pisa2025/kritums_gimenes_skolas_vide.csv
#          output/pisa2025/logs/21_krievu_gimenes_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/21_krievu_gimenes_output.txt"
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

FAY_DALITAJS <- 20   # 80 replikāti, Fay 0.5
SLIEKSNIS <- 0.5     # skola ir krieviska, ja puse vai vairāk runā krieviski


# === 1. GRUPU VEIDOŠANA =====================================================
cat(strrep("=", 70), "\n")
cat("1. Grupu veidošana\n")
cat(strrep("=", 70), "\n\n")

sagatavo <- function(d) {
  lab <- as.character(as_factor(d$LANGN))
  d$krievu_gimene <- grepl("^Russian", lab)
  d$latv_gimene   <- grepl("^Latvian", lab)

  # Skolas vide. Krieviski runājošo svērtā daļa skolā, rēķināta no izlases.
  sk <- tapply(seq_len(nrow(d)), d$CNTSCHID, function(i)
    sum(d$W_FSTUWT[i] * d$krievu_gimene[i]) / sum(d$W_FSTUWT[i]))
  d$skolas_kr_dala <- as.numeric(sk[as.character(d$CNTSCHID)])
  d$krieviska_vide <- d$skolas_kr_dala >= SLIEKSNIS

  d$grupa <- NA_character_
  d$grupa[d$latv_gimene]                        <- "Latviešu ģimene"
  d$grupa[d$krievu_gimene & !d$krieviska_vide]  <- "Krievu ģimene, latviska skola"
  d$grupa[d$krievu_gimene &  d$krieviska_vide]  <- "Krievu ģimene, krieviska skola"
  d$grupa <- factor(d$grupa, levels = c("Latviešu ģimene",
                                        "Krievu ģimene, latviska skola",
                                        "Krievu ģimene, krieviska skola"))
  d
}

d22 <- sagatavo(d22)
d25 <- sagatavo(d25)

for (nos in c("2022", "2025")) {
  d <- if (nos == "2022") d22 else d25
  cat(nos, "\n")
  for (g in levels(d$grupa)) {
    i <- !is.na(d$grupa) & d$grupa == g
    cat(sprintf("  %-32s n = %4d   %5.1f %% skolēnu\n", g, sum(i),
                100 * sum(d$W_FSTUWT[i]) / sum(d$W_FSTUWT)))
  }
  cat(sprintf("  %-32s n = %4d\n\n", "bez grupas (cita valoda)",
              sum(is.na(d$grupa))))
}


# === 2. LASĪŠANAS VIDĒJIE UN KRITUMS ========================================
cat(strrep("=", 70), "\n")
cat("2. Lasīšana, vidējie un kritums\n")
cat(strrep("=", 70), "\n\n")

# PISA aplēse ar 10 plausible values un 80 BRR replikātsvariem.
pisa_videjais <- function(d, i, domens = "READ") {
  pvs <- paste0("PV", 1:10, domens)
  rw  <- grep("^W_FSTURWT", names(d), value = TRUE)
  sv  <- function(x, wt) sum(x * wt, na.rm = TRUE) / sum(wt[!is.na(x)])

  est_pv <- vapply(pvs, function(p) sv(d[[p]][i], d$W_FSTUWT[i]), numeric(1))
  est <- mean(est_pv)
  rep_est <- vapply(rw, function(r) sv(d[[pvs[1]]][i], d[[r]][i]), numeric(1))
  var_izl <- sum((rep_est - est_pv[1])^2) / FAY_DALITAJS
  var_imp <- (1 + 1 / length(pvs)) * stats::var(est_pv)
  c(est = est, se = sqrt(var_izl + var_imp), n = sum(i),
    dala = sum(d$W_FSTUWT[i]) / sum(d$W_FSTUWT))
}

rez <- list()
for (domens in c("READ", "MATH")) {
  cat(if (domens == "READ") "Lasīšana\n" else "\nMatemātika\n")
  cat(sprintf("  %-32s %8s %8s %10s\n", "", "2022", "2025", "starpība"))
  for (g in levels(d22$grupa)) {
    a <- pisa_videjais(d22, !is.na(d22$grupa) & d22$grupa == g, domens)
    b <- pisa_videjais(d25, !is.na(d25$grupa) & d25$grupa == g, domens)
    # Gadi ir neatkarīgas izlases, tāpēc dispersijas summējas.
    st <- b["est"] - a["est"]
    st_se <- sqrt(a["se"]^2 + b["se"]^2)
    zvg <- if (abs(st / st_se) > 1.96) "*" else " "
    cat(sprintf("  %-32s %8.1f %8.1f %8.1f %s (SE %.1f)\n",
                g, a["est"], b["est"], st, zvg, st_se))
    rez[[length(rez) + 1]] <- data.frame(
      domens = domens, grupa = g,
      est_2022 = a["est"], se_2022 = a["se"], n_2022 = a["n"], dala_2022 = a["dala"],
      est_2025 = b["est"], se_2025 = b["se"], n_2025 = b["n"], dala_2025 = b["dala"],
      starpiba = st, starpiba_se = st_se)
  }
}


# === 3. JUTĪGUMS PRET SLIEKSNI ==============================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("3. Jutīgums pret skolas sliekšņa izvēli\n")
cat(strrep("=", 70), "\n\n")

for (sl in c(0.25, 0.50, 0.75)) {
  cat("Slieksnis", sl, "\n")
  for (kr_vide in c(FALSE, TRUE)) {
    a <- pisa_videjais(d22, d22$krievu_gimene & (d22$skolas_kr_dala >= sl) == kr_vide)
    b <- pisa_videjais(d25, d25$krievu_gimene & (d25$skolas_kr_dala >= sl) == kr_vide)
    cat(sprintf("  krievu ģimene, %-9s skola  %6.1f -> %6.1f  starpība %6.1f  n %d/%d\n",
                if (kr_vide) "krieviska" else "latviska",
                a["est"], b["est"], b["est"] - a["est"], a["n"], b["n"]))
  }
  cat("\n")
}


# === 4. CIK LABS AIZSTĀJĒJS IR SKOLAS VIDE ==================================
cat(strrep("=", 70), "\n")
cat("4. Pārbaude uz 2022. gada testa valodu\n")
cat(strrep("=", 70), "\n\n")

cat("2022. gadā daļa krievu ģimeņu bērnu testu kārtoja latviski. Testa valoda\n")
cat("ir asākais rādītājs par to, kurā plūsmā bērns mācās, bet 2025. gadā tā\n")
cat("vairs nepastāv, tāpēc kritumu pēc tās aprēķināt nevar.\n\n")

kr22 <- d22[d22$krievu_gimene, ]
tv <- as.character(as_factor(kr22$LANGTEST_COG))
cat("Krievu ģimeņu bērni 2022. gadā, skolas vide pret testa valodu:\n")
print(table(vide = ifelse(kr22$krieviska_vide, "krieviska skola", "latviska skola"),
            tests = tv))
sakritiba <- mean((tv == "Russian") == kr22$krieviska_vide, na.rm = TRUE)
cat(sprintf("\nSakritība: %.1f %%\n\n", 100 * sakritiba))

cat("2022. gada lasīšana pēc testa valodas, tikai atsaucei:\n")
for (v in c("Latvian", "Russian")) {
  r <- pisa_videjais(kr22, tv == v & !is.na(tv))
  cat(sprintf("  tests %-8s  %6.1f (SE %.1f)  n = %d\n", v, r["est"], r["se"], r["n"]))
}


# === 5. SAGLABĀŠANA =========================================================
rez_df <- do.call(rbind, rez)
rownames(rez_df) <- NULL
write.csv(rez_df, "output/pisa2025/kritums_gimenes_skolas_vide.csv", row.names = FALSE)

cat("\nIzveidotie faili:\n")
cat("  output/pisa2025/kritums_gimenes_skolas_vide.csv\n")
cat(" ", output_file, "\n")
cat("\nPabeigts:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
