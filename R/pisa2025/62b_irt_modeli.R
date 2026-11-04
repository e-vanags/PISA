# ============================================================================
# 62b. IRT modeļi lasīšanas uzdevumiem 2015, 2018, 2022, 2025 (docs/59)
# ============================================================================
#
# Katram ciklam, uz 62a izvilkuma (Latvija, Igaunija), ar R paketi mirt:
#   M1_LVA, M1_EST  viendimensiju modelis katrai valstij: 2PL vienpunkta,
#                   GPCM daļēja kredīta uzdevumiem (tā pati modeļu saime kā PISA)
#   M1_KOP          viendimensiju modelis abām valstīm kopā; tā EAP ir
#                   izlīdzināšanas mainīgais DIF analīzei (62c)
#   D_PROC          trīs dimensijas pēc kognitīvā procesa (atrast informāciju /
#                   saprast / novērtēt un reflektēt; 2015: piekļūt un atrast /
#                   integrēt un interpretēt / reflektēt un novērtēt), bez fluences
#   D_FLU           divas dimensijas: fluences uzdevumi pret pārējiem (2018+)
#   D1 salīdzinājumam uz tiem pašiem uzdevumiem kā D_PROC
#
# Uzdevumi modelī: uzdoti vismaz 150 reizes attiecīgajā grupā; kategorijas
# pārkodētas secīgi no nulles; kategorija ar < 10 atbildēm apvienota ar blakus
# esošo. Neatbildēts = 0 (kā PISA), nav sasniegts = trūkst (kā PISA).
# Modeļi bez svariem (psihometriskās īpašības, ne populācijas aplēses).
#
# No katra modeļa saglabā: parametrus (a, b), EAP un tā SE, empīrisko un
# marginālo ticamību, infit un outfit (aprēķināti tieši no atlikumiem, jo
# datos ir trūkstošās vērtības pēc dizaina), Q3 lokālo atkarību, AIC/BIC un
# latento korelāciju matricu.
#
# Izvade: data/pisa2025/processed/irt/<gads>_<modelis>.rds
#         output/pisa2025/logs/62b_irt_modeli_output.txt
# ============================================================================

suppressPackageStartupMessages(library(mirt))

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/62b_irt_modeli_output.txt"
log_con <- file(output_file, open = "at", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("\n===== Sākts:", format(Sys.time()), "=====\n")

DIR <- "data/pisa2025/processed/irt"; dir.create(DIR, showWarnings = FALSE, recursive = TRUE)
GADI <- c("2015", "2018", "2022", "2025")
MIN_N <- 150; MIN_KAT <- 10
mirtCluster(max(1, parallel::detectCores() - 2))

K22 <- utils::read.csv("output/pisa2025/uzdevumu_klasifikacija_2022.csv", stringsAsFactors = FALSE)
K15 <- utils::read.csv("output/pisa2025/uzdevumu_klasifikacija_2015.csv", stringsAsFactors = FALSE)
PROC22 <- c("Locate information" = "atrast", "Understand" = "saprast", "Evaluate and reflect" = "novertet")
PROC15 <- c("Access and retrieve" = "atrast", "Integrate and interpret" = "saprast", "Reflect and evaluate" = "novertet")
process_of <- function(uzd, g) {
  k <- paste0("CR", uzd)
  p22 <- PROC22[K22$process_skala[match(k, K22$uzd)]]
  p15 <- PROC15[K15$aspekts[match(k, K15$uzd)]]
  out <- if (g == "2015") ifelse(is.na(p15), p22, p15) else ifelse(is.na(p22), p15, p22)
  unname(out)
}

# Pārkodē katru kolonnu uz secīgām kategorijām; apvieno retās
sagatavo <- function(X) {
  keep <- colSums(!is.na(X)) >= MIN_N
  X <- X[, keep, drop = FALSE]
  for (j in seq_len(ncol(X))) {
    x <- X[, j]; tb <- table(x)
    while (length(tb) > 2 && any(tb < MIN_KAT)) {
      k <- as.numeric(names(tb))[which.min(tb)]; lv <- as.numeric(names(tb))
      nb <- if (k == max(lv)) max(lv[lv < k]) else min(lv[lv > k])
      x[x %in% k] <- nb; tb <- table(x)
    }
    X[, j] <- match(x, sort(unique(x[!is.na(x)]))) - 1L
  }
  X[, apply(X, 2, function(x) length(unique(x[!is.na(x)])) >= 2 & min(table(x)) >= MIN_KAT), drop = FALSE]
}

# Daudzdimensiju modeļi ar MHRM: EM ar pietiekamu integrācijas precizitāti trīs
# dimensijās (21^3 mezgli) adaptīvā testa izlasēm prasītu stundas. MHRM logLik ir
# Montekarlo novērtējums (draws), 1D salīdzinājuma modelis paliek ar precīzu EM.
pielago <- function(X, model = 1, quadpts = NULL, nosaukums = "", method = "EM") {
  it <- ifelse(apply(X, 2, max, na.rm = TRUE) > 1, "gpcm", "2PL")
  t0 <- Sys.time()
  fit <- if (method == "EM") mirt(as.data.frame(X), model, itemtype = it, method = "EM", verbose = FALSE,
              quadpts = quadpts, TOL = 1e-4, technical = list(NCYCLES = 3000)) else
         mirt(as.data.frame(X), model, itemtype = it, method = "MHRM", verbose = FALSE, draws = 3000,
              technical = list(NCYCLES = 3000, set.seed = 20260911))
  cat(sprintf("    %-22s %4d uzd., %5d skolēni, %s, logLik %.1f, BIC %.1f, %.1f min%s\n", nosaukums, ncol(X), nrow(X),
      if (is.numeric(model)) paste0(model, "D") else "konfirm.", extract.mirt(fit, "logLik"), extract.mirt(fit, "BIC"),
      as.numeric(difftime(Sys.time(), t0, units = "mins")), if (!extract.mirt(fit, "converged")) " NAV KONVERĢĒJIS" else ""))
  fit
}

# Viendimensiju modeļa diagnostika
diagnostika <- function(fit, X, uzd) {
  fs <- fscores(fit, method = "EAP", full.scores.SE = TRUE)
  th <- fs[, 1]; n <- nrow(X); J <- ncol(X)
  E <- V <- matrix(NA_real_, n, J)
  for (j in seq_len(J)) {
    pt <- probtrace(extract.item(fit, j), matrix(th)); k <- 0:(ncol(pt) - 1)
    E[, j] <- pt %*% k; V[, j] <- pt %*% k^2 - E[, j]^2
  }
  Rz <- X - E; ok <- !is.na(X)
  outfit <- colMeans(ifelse(ok, Rz^2 / V, NA), na.rm = TRUE)
  infit <- colSums(ifelse(ok, Rz^2, 0)) / colSums(ifelse(ok, V, 0))
  cp <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
  bcol <- grep("^b", colnames(cp))
  par <- data.frame(uzd = uzd, a = cp[, "a"], b = rowMeans(cp[, bcol, drop = FALSE], na.rm = TRUE),
                    kat = apply(X, 2, max, na.rm = TRUE) + 1, n = colSums(ok), outfit = outfit, infit = infit,
                    stringsAsFactors = FALSE)
  # Q3: atlikumu korelācija pāriem, kas uzdoti kopā vismaz 100 reizes
  Rr <- ifelse(ok, Rz, NA)
  Q3 <- suppressWarnings(cor(Rr, use = "pairwise.complete.obs")); nn <- crossprod(ok * 1)
  Q3[nn < 100] <- NA; diag(Q3) <- NA
  vien <- sub("^([0-9]{3}).*", "\\1", uzd)
  ut <- which(upper.tri(Q3) & !is.na(Q3), arr.ind = TRUE)
  q3 <- data.frame(u1 = uzd[ut[, 1]], u2 = uzd[ut[, 2]], q3 = Q3[ut], n = nn[ut],
                   viena_vieniba = vien[ut[, 1]] == vien[ut[, 2]], stringsAsFactors = FALSE)
  list(par = par, eap = th, eap_se = fs[, 2], rxx_emp = empirical_rxx(fs), rxx_marg = marginal_rxx(fit),
       q3 = q3, logLik = extract.mirt(fit, "logLik"), AIC = extract.mirt(fit, "AIC"), BIC = extract.mirt(fit, "BIC"))
}

kor_matrica <- function(fit) {
  S <- coef(fit, simplify = TRUE)$cov; D <- diag(1 / sqrt(diag(S))); R <- D %*% S %*% D
  dimnames(R) <- dimnames(S); R
}

konf_modelis <- function(grupa) {
  lv <- unique(grupa[!is.na(grupa)])
  paste(c(vapply(lv, function(l) paste0(l, " = ", paste(which(grupa %in% l), collapse = ",")), ""),
          paste0("COV = ", paste(lv, collapse = "*"))), collapse = "\n")
}

for (g in GADI) {
  P <- readRDS(sprintf("data/pisa2025/processed/psihometrija_%s.rds", g))
  X0 <- P$K; X0[X0 == -1L] <- 0L; X0[X0 %in% c(-2L, -3L)] <- NA
  cat(sprintf("\n%s: %d skolēni (%s), %d uzdevumi\n", g, nrow(X0), paste(names(table(P$sk$CNT)), table(P$sk$CNT), collapse = ", "), ncol(X0)))
  grupas <- list(LVA = P$sk$CNT == "LVA", EST = P$sk$CNT == "EST", KOP = rep(TRUE, nrow(X0)))
  for (gr in names(grupas)) {
    f <- file.path(DIR, sprintf("%s_M1_%s.rds", g, gr))
    if (file.exists(f)) { cat("    ", basename(f), "jau ir\n"); next }
    sel <- which(grupas[[gr]])
    X <- sagatavo(X0[sel, , drop = FALSE])            # sagatavo atmet tikai kolonnas
    rk <- rowSums(!is.na(X)) >= 5
    X <- X[rk, , drop = FALSE]; idx <- sel[rk]
    fit <- pielago(X, 1, 61, paste("M1", gr))
    dg <- diagnostika(fit, X, colnames(X))
    dg$rindas <- idx; dg$CNTSTUID <- P$sk$CNTSTUID[idx]
    saveRDS(dg, f); rm(fit); gc(verbose = FALSE)
  }
  # Dimensionalitāte: tikai valstu atsevišķie modeļi
  for (gr in c("LVA", "EST")) {
    f <- file.path(DIR, sprintf("%s_DIM_%s.rds", g, gr))
    if (file.exists(f)) { cat("    ", basename(f), "jau ir\n"); next }
    X <- sagatavo(X0[grupas[[gr]], , drop = FALSE]); X <- X[rowSums(!is.na(X)) >= 5, , drop = FALSE]
    rez <- list()
    pr <- process_of(colnames(X), g); flu <- grepl("^590", colnames(X))
    j <- which(!is.na(pr) & !flu)
    if (length(unique(pr[j])) == 3) {
      Xp <- X[, j]; Xp <- Xp[rowSums(!is.na(Xp)) >= 3, ]
      # Ar EM un 11 punktiem dimensijā 3D modeļa logLik iznāca zemāks par 1D (neiespējami
      # ligzdotiem modeļiem, integrācijas kļūda), tāpēc MHRM.
      f1 <- pielago(Xp, 1, 61, paste("D1 (process)", gr))
      f3 <- pielago(Xp, mirt.model(konf_modelis(pr[j])), NULL, paste("D_PROC", gr), method = "MHRM")
      rez$proc <- data.frame(modelis = c("1D", "3D process"), logLik = c(extract.mirt(f1, "logLik"), extract.mirt(f3, "logLik")),
        AIC = c(extract.mirt(f1, "AIC"), extract.mirt(f3, "AIC")), BIC = c(extract.mirt(f1, "BIC"), extract.mirt(f3, "BIC")),
        df = c(extract.mirt(f1, "nest"), extract.mirt(f3, "nest")))
      rez$proc_kor <- kor_matrica(f3); rez$proc_n <- table(pr[j])
      rm(f1, f3); gc(verbose = FALSE)
    }
    if (sum(flu) >= 10) {
      grp <- ifelse(flu, "fluence", "parejie")
      f1 <- pielago(X, 1, 61, paste("D1 (fluence)", gr))
      f2 <- pielago(X, mirt.model(konf_modelis(grp)), NULL, paste("D_FLU", gr), method = "MHRM")
      rez$flu <- data.frame(modelis = c("1D", "2D fluence"), logLik = c(extract.mirt(f1, "logLik"), extract.mirt(f2, "logLik")),
        AIC = c(extract.mirt(f1, "AIC"), extract.mirt(f2, "AIC")), BIC = c(extract.mirt(f1, "BIC"), extract.mirt(f2, "BIC")),
        df = c(extract.mirt(f1, "nest"), extract.mirt(f2, "nest")))
      rez$flu_kor <- kor_matrica(f2)
      rm(f1, f2); gc(verbose = FALSE)
    }
    saveRDS(rez, f)
  }
}
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
