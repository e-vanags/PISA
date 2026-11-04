# ============================================================================
# 76b. Raituma korelācija ar pārējo lasīšanu: vai docs/59 vērtība 0,66 "Latvijai
#      2025" ir A un B plūsmas maisījuma sekas (docs/70 papildpārbaude)
# ============================================================================
#
# JAUTĀJUMS. 62b (visa Latvija kopā, bez svariem, MHRM) 2025. gadā deva raituma
# un pārējās lasīšanas latento korelāciju 0,66 (Igaunijā 0,55). 76 (pa plūsmām,
# svērti, QMCEM) dod A plūsmai 0,54 un B plūsmai 0,67. Šeit tas pats F2 modelis
# A un B plūsmai kopā: ja kopējā korelācija ir augstāka par abu plūsmu
# korelācijām atsevišķi, pieaugumu rada plūsmu vidējo atšķirība (maisījums),
# nevis A plūsmas atbilžu uzvedība.
#
# Tie paši dati, sagatavošana un iestatījumi kā 76_dimensionalitate.R.
# Izvade: output/pisa2025/logs/76b_raitums_plusmu_maisijums_output.txt
#         output/pisa2025/dimensionalitate_raitums_maisijums.csv
# ============================================================================

suppressPackageStartupMessages({ library(mirt) })
source("R/pisa2025/00_ab_dati.R")

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/76b_raitums_plusmu_maisijums_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n")
cat("Sākts:", format(Sys.time()), "\n\n")

GADI <- c("2022", "2025")
MIN_N <- 150; MIN_KAT <- 10; MIN_SK <- 5; N_MAX <- 3000
TOL_Q <- 5e-4; NCYC <- 1500; QMC_PTS <- 5000; SEED <- 20260913
mirtCluster(4)

sagatavo <- function(X, min_n) {
  X <- X[, colSums(!is.na(X)) >= min_n, drop = FALSE]
  for (j in seq_len(ncol(X))) {
    x <- X[, j]; tb <- table(x)
    while (length(tb) > 2 && any(tb < MIN_KAT)) {
      k <- as.numeric(names(tb))[which.min(tb)]; lv <- as.numeric(names(tb))
      nb <- if (k == max(lv)) max(lv[lv < k]) else min(lv[lv > k])
      x[x %in% k] <- nb; tb <- table(x)
    }
    X[, j] <- match(x, sort(unique(x[!is.na(x)]))) - 1L
  }
  X[, apply(X, 2, function(x) length(unique(x[!is.na(x)])) >= 2 && min(table(x)) >= MIN_KAT), drop = FALSE]
}
itemtype <- function(X) ifelse(apply(X, 2, max, na.rm = TRUE) > 1, "gpcm", "2PL")
f2_modelis <- function(raitums) mirt.model(paste0("raitums = ", paste(which(raitums), collapse = ","), "\nparejie = ",
                                                  paste(which(!raitums), collapse = ","), "\nCOV = raitums*parejie"))
pielago <- function(X, w, model, nosaukums) {
  D <- as.data.frame(X); t0 <- Sys.time()
  fit <- mirt(D, model, itemtype = itemtype(X), method = "QMCEM", survey.weights = w, quadpts = QMC_PTS, TOL = TOL_Q,
              verbose = FALSE, technical = list(NCYCLES = NCYC, set.seed = SEED))
  cat(sprintf("    %-28s %3d uzd. %5d sk.  logLik %10.1f  BIC %9.1f  %4.1f min%s\n", nosaukums, ncol(X), nrow(X),
              extract.mirt(fit, "logLik"), extract.mirt(fit, "BIC"), as.numeric(difftime(Sys.time(), t0, units = "mins")),
              if (!extract.mirt(fit, "converged")) "  NAV KONVERĢĒJIS" else ""))
  fit
}

REZ <- list()
for (g in GADI) {
  cat("\n", strrep("=", 90), "\n", g, "\n", strrep("=", 90), "\n", sep = "")
  P <- readRDS(sprintf("data/pisa2025/processed/psihometrija_%s.rds", g))
  X0 <- P$K; X0[X0 == -1L] <- 0L; X0[X0 %in% c(-2L, -3L)] <- NA
  S <- readRDS(sprintf("data/pisa2025/processed/simptomi_%s.rds", g))$stu
  S <- data.frame(CNT = as.character(haven::zap_labels(S$CNT)), CNTSTUID = as.numeric(haven::zap_labels(S$CNTSTUID)),
                  w = as.numeric(haven::zap_labels(S$W_FSTUWT)), LANGN = as.character(as.numeric(haven::zap_labels(S$LANGN))), stringsAsFactors = FALSE)
  S <- S[S$CNT == "LVA", ]
  m <- match(paste(P$sk$CNT, P$sk$CNTSTUID), paste(S$CNT, S$CNTSTUID))
  w_all <- S$w[m]; pl <- ifelse(P$sk$CNT == "LVA", ab_grupa(S$LANGN[m], g), NA)
  sel <- which(pl %in% c("A", "B") & !is.na(w_all) & rowSums(!is.na(X0)) >= MIN_SK)
  set.seed(SEED); if (length(sel) > N_MAX) sel <- sort(sample(sel, N_MAX))
  X <- sagatavo(X0[sel, , drop = FALSE], MIN_N); ok <- rowSums(!is.na(X)) >= MIN_SK
  X <- X[ok, , drop = FALSE]; w <- w_all[sel][ok]; plusma <- pl[sel][ok]
  raitums <- grepl("^590", colnames(X))
  cat(sprintf("  A un B plūsma kopā: %d skolēni (A %d, B %d), %d uzdevumi (raitums %d)\n", nrow(X), sum(plusma == "A"), sum(plusma == "B"), ncol(X), sum(raitums)))
  for (sv in c("svērti", "bez svariem")) {
    ww <- if (sv == "svērti") w / mean(w) else rep(1, nrow(X))
    fit <- pielago(X, ww, f2_modelis(raitums), paste("F2 A+B", sv))
    r <- cov2cor(coef(fit, simplify = TRUE)$cov)[1, 2]
    cat(sprintf("    F2 korelācija raitums–pārējie (A+B, %s): %.3f\n", sv, r))
    REZ[[length(REZ) + 1]] <- data.frame(gads = g, grupa = "LVA_A+B", svari = sv, skoleni = nrow(X), A = sum(plusma == "A"), B = sum(plusma == "B"),
                                         uzdevumi = ncol(X), kor_raitums = r, konverg = extract.mirt(fit, "converged"))
  }
}
REZ <- do.call(rbind, REZ)
utils::write.csv(REZ, "output/pisa2025/dimensionalitate_raitums_maisijums.csv", row.names = FALSE)
cat("\nSalīdzinājumam no 76 (svērti, pa plūsmām): 2022 A 0,544, B 0,577, EST 0,562; 2025 A 0,538, B 0,669, EST 0,569.\n",
    "No 62b (visa Latvija, bez svariem, MHRM): 2022 0,54; 2025 0,66.\n")
print(REZ, row.names = FALSE, digits = 3)
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
