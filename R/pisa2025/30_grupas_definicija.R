# ============================================================================
# 30. Vai krievvalodīgo grupas vispār ir salīdzināmas
# ============================================================================
#
# Iebildums. Grupa "mājās runā krieviski" ir PAŠDEKLARĒTA, un tās īpatsvars
# Latvijā saruka no 33.7 uz 28.8 procentiem, kamēr latviski runājošo daļa
# auga no 61.0 uz 65.2. Igaunijā tajā pašā laikā sarukums bija tikai no 26.6
# uz 24.9. Tas izskatās pēc ziņošanas pārbīdes, ne pēc demogrāfijas.
#
# Ja tie, kas pārgāja uz "latviski", bija grupas stiprākie, tad 2025. gada
# krievvalodīgo grupa ir negatīvi atlasīta, un daļa no 15 punktu krituma ir
# atlase, ne politikas efekts.
#
# Ukrainas bēgļi to neizskaidro: 0.2 % pret 0.6 % no izlases.
#
# Divas pieejas.
#
# 1. ADMINISTRATĪVĀ DEFINĪCIJA. PROGN rāda izglītības programmu, un tā nav
#    pašdeklarēta. "Mazākumtautību pamatizglītība" 2022. gadā ir 1436
#    skolēni, 2025. gadā 1208. Bet arī tā nav tīra, jo programma pati tika
#    likvidēta, tātad izstāšanās no tās IR apstrādes daļa.
#
# 2. LĪ TIPA ROBEŽAS. Ja grupa saruka par p procentiem, tad sliktākais
#    gadījums ir, ka aizgāja p procenti stiprāko. Nogriež 2022. gada grupai
#    p procentus no augšas un pārrēķina. Otra robeža ir p procenti no apakšas.
#    Patiesais efekts ir starp abām, ja vien aizgājēji nav vēl selektīvāki.
#
# Ievade:  data/pisa2025/processed/uzdevumu_limenis.rds
# Izvade:  output/pisa2025/grupas_definicija.csv
#          output/pisa2025/logs/30_grupas_definicija_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/30_grupas_definicija_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

library(haven)
source("R/pisa2025/00_kopigas_funkcijas.R")

U <- readRDS("data/pisa2025/processed/uzdevumu_limenis.rds"); uzd <- U$uzd
RW <- paste0("W_FSTURWT", 1:80)

sagatavo <- function(cog, gads) {
  i <- as.character(cog$CNT) == "LVA"
  S <- vapply(cog[paste0(uzd, "S")], klasificet, character(nrow(cog)))[i, ]
  colnames(S) <- uzd
  P <- punkti_matrica(S)
  s <- readRDS(sprintf("data/pisa%d/processed/pisa_lva_%d_stu.rds", gads, gads))
  j <- match(as.numeric(cog$CNTSTUID[i]), as.numeric(s$CNTSTUID))
  vert <- !is.na(S) & S %in% VERTETAS_AR_NESASNIEGTIEM
  n_v <- rowSums(vert)
  # Skolēna paša rezultāts, vajadzīgs nogriešanai.
  pasu <- ifelse(n_v >= 10, rowSums(P * vert, na.rm = TRUE) / n_v, NA)
  prog <- as.character(as_factor(s$PROGN))[j]
  list(gads = gads, S = S, P = P, pasu = pasu,
       w = as.numeric(s$W_FSTUWT)[j],
       rw = as.data.frame(lapply(s[RW], function(z) as.numeric(z)[j])),
       lang = as.character(as_factor(s$LANGN))[j],
       prog = prog)
}
A <- sagatavo(U$d22, 2022); B <- sagatavo(U$d25, 2025)

pct <- function(z, wt, keep) {
  S <- z$S[keep, ]; P <- z$P[keep, ]; wt <- wt[keep]; wt[is.na(wt)] <- 0
  W <- matrix(wt, nrow = nrow(S), ncol = ncol(S))
  sel <- !is.na(S) & S %in% VERTETAS_AR_NESASNIEGTIEM
  100 * sum(W[sel] * P[sel], na.rm = TRUE) / sum(W[sel])
}
aplese <- function(z, keep) {
  keep[is.na(keep)] <- FALSE
  est <- pct(z, z$w, keep)
  rp <- vapply(RW, function(r) pct(z, z$rw[[r]], keep), numeric(1))
  c(est = est, var = sum((rp - est)^2) / 20, n = sum(keep))
}
starpiba <- function(a, b, nos) {
  d <- b["est"] - a["est"]; s <- sqrt(a["var"] + b["var"])
  zv <- if (abs(d / s) > 1.96) "*" else " "
  cat(sprintf("  %-42s %7.2f -> %7.2f   %+7.2f (SE %.2f)%s  n %d/%d\n",
              nos, a["est"], b["est"], d, s, zv, a["n"], b["n"]))
  data.frame(definicija = nos, y2022 = unname(a["est"]), y2025 = unname(b["est"]),
             parmaina = unname(d), se = unname(s),
             n2022 = unname(a["n"]), n2025 = unname(b["n"]))
}

rez <- list()

# === 1. DIVAS DEFINĪCIJAS ===================================================
cat(strrep("=", 70), "\n1. Pašdeklarētā pret administratīvo definīciju\n",
    strrep("=", 70), "\n\n")

rez[[1]] <- starpiba(aplese(A, grepl("^Russian", A$lang)),
                     aplese(B, grepl("^Russian", B$lang)),
                     "Mājās runā krieviski (pašdeklarēts)")
rez[[2]] <- starpiba(aplese(A, grepl("[Mm]inority", A$prog)),
                     aplese(B, grepl("[Mm]inority", B$prog)),
                     "Mazākumtautību programma (administratīvs)")
rez[[3]] <- starpiba(aplese(A, grepl("^Latvian", A$lang)),
                     aplese(B, grepl("^Latvian", B$lang)),
                     "Mājās runā latviski (salīdzinājumam)")

cat("\nGrupu lielums svērtajā populācijā:\n")
for (z in list(A, B)) {
  w <- z$w; w[is.na(w)] <- 0
  cat(sprintf("  %d  krieviski %5.1f %%   mazākumtautību programma %5.1f %%\n",
              z$gads,
              100 * sum(w[grepl("^Russian", z$lang)], na.rm = TRUE) / sum(w),
              100 * sum(w[grepl("[Mm]inority", z$prog)], na.rm = TRUE) / sum(w)))
}


# === 2. LĪ TIPA ROBEŽAS =====================================================
cat("\n", strrep("=", 70), "\n2. Robežas pašatlases scenārijam\n",
    strrep("=", 70), "\n\n")

w22 <- A$w; w22[is.na(w22)] <- 0
w25 <- B$w; w25[is.na(w25)] <- 0
d22 <- sum(w22[grepl("^Russian", A$lang)]) / sum(w22)
d25 <- sum(w25[grepl("^Russian", B$lang)]) / sum(w25)
p <- (d22 - d25) / d22
cat(sprintf("Grupa saruka no %.1f %% uz %.1f %% populācijas, tātad zaudēja %.1f %%\n",
            100 * d22, 100 * d25, 100 * p))
cat("no saviem locekļiem. Ja aizgāja stiprākie, 2022. gada bāze ir par augstu.\n\n")

# Nogriešanai vajag skolēna paša rezultātu, kas prasa vismaz 10 uzdevumus.
# Lai salīdzinājums būtu godīgs, TO PAŠU ierobežojumu piemēro arī bāzei un
# 2025. gadam. Bez tā nogrieztā un nenogrieztā aplēse nāktu no dažādām
# izlasēm, un starpība rādītu ierobežojumu, ne nogriešanu.
kr22 <- grepl("^Russian", A$lang) & !is.na(A$pasu)
kr25 <- grepl("^Russian", B$lang) & !is.na(B$pasu)
cat(sprintf("Analīzes izlase ar vismaz 10 uzdevumiem: 2022 %d no %d, 2025 %d no %d\n\n",
            sum(kr22), sum(grepl("^Russian", A$lang)),
            sum(kr25), sum(grepl("^Russian", B$lang))))

nogriez <- function(z, baze, dala, no_augsas) {
  x <- z$pasu; w <- z$w; w[is.na(w)] <- 0
  idx <- which(baze)[order(x[baze], decreasing = no_augsas)]
  cw <- cumsum(w[idx]) / sum(w[idx])
  keep <- baze; keep[idx[cw <= dala]] <- FALSE
  keep
}

rez[[length(rez) + 1]] <- starpiba(aplese(A, kr22), aplese(B, kr25),
                                   "bez nogriešanas, tā pati izlase")
for (nos in c("augšas", "apakšas")) {
  keep <- nogriez(A, kr22, p, nos == "augšas")
  rez[[length(rez) + 1]] <- starpiba(
    aplese(A, keep), aplese(B, kr25),
    sprintf("2022 nogriezti %.0f %% no %s", 100 * p, nos))
}


# === 3. INTERPRETĀCIJA ======================================================
cat("\n", strrep("=", 70), "\n3. Ko no tā secināt\n", strrep("=", 70), "\n\n")

df <- do.call(rbind, rez); rownames(df) <- NULL
augs <- df$parmaina[grepl("no augšas", df$definicija)]
apak <- df$parmaina[grepl("no apakšas", df$definicija)]
adm  <- df$parmaina[grepl("administratīvs", df$definicija)]
pasd <- df$parmaina[grepl("pašdeklarēts", df$definicija)]

cat(sprintf("  Pašdeklarētā definīcija        %+7.2f\n", pasd))
cat(sprintf("  Administratīvā definīcija      %+7.2f\n", adm))
cat(sprintf("  Sliktākais gadījums atlasei    %+7.2f\n", augs))
cat(sprintf("  Labākais gadījums atlasei      %+7.2f\n", apak))
cat(sprintf("\n  Robežas: no %.2f līdz %.2f\n", min(augs, apak), max(augs, apak)))

write.csv(df, "output/pisa2025/grupas_definicija.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/grupas_definicija.csv\n ", output_file,
    "\n\nPabeigts:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
