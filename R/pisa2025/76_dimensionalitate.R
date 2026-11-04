# ============================================================================
# 76. Lasīšanas testa dimensionalitāte un uzdevumu atbilstība modelim Latvijā,
#     2015–2025 (docs/46 Slānis 6, atlikusī daļa; docs/70)
# ============================================================================
#
# JAUTĀJUMS. Vai PISA lasīšanas tests Latvijā 2025. gadā mērīja to pašu
# konstruktu, ko iepriekšējos ciklos un ko Igaunijā: vai uzdevumi veido vienu
# dimensiju, vai raitums ir atsevišķa dimensija, vai dimensionalitāte laikā
# mainās, un kuri uzdevumi Latvijā modelim neatbilst.
#
# DATI. 62a izvilkums (visi vērtētie lasīšanas uzdevumi, Latvija un Igaunija),
# simptomu izvilkums 64a (svari W_FSTUWT, LANGN plūsmas noteikšanai),
# uzdevumu klasifikācija: PISA 2022 TR A pielikums (54a) un PISA 2015 TR A.3
# tabula (53a).
#
# GRUPAS. LVA A (latviešu valodas plūsma), LVA B (krievu valodas plūsma, tikai
# aprakstoši), EST (visa Igaunija). Katrā ciklā 2015, 2018, 2022, 2025.
#
# MODEĻI (mirt 1.47; 2PL vienpunkta un GPCM daļēja kredīta uzdevumiem; svērti
# ar W_FSTUWT, normalizētiem uz vidējo 1):
#   M1   viena dimensija, visi uzdevumi (EM, 61 kvadratūras punkti). Diagnostika:
#        PV-Q1 (Chalmers un Ng 2017; S-X² ar trūkstošajiem datiem pēc dizaina nav
#        aprēķināms), infit un outfit, Q3 atlikumu korelācijas pa vienībām un
#        testletēm, empīriskā ticamība.
#   P3   trīs korelētas dimensijas pēc kognitīvā procesa (atrast / saprast /
#        izvērtēt un reflektēt), bez raituma; QMCEM (5000 kvazi-Montekarlo punkti;
#        pārbaudīts, ka 20 000 punkti dod to pašu logLik līdz 0,3). Salīdzinājumam
#        viena dimensija uz tiem pašiem uzdevumiem ar to pašu QMCEM (P1q).
#   PB   bifaktors: vispārīgais faktors + trīs procesu specifiskie (EM ar
#        dimensiju redukciju, precīza kvadratūra); ECV, ω_h, LR tests pret P1.
#   T3   trīs (vai divas) korelētas dimensijas pēc teksta formāta (nepārtraukts /
#        nepārtraukts ar tabulām u. c. jaukts / nenepārtraukts), bez raituma; QMCEM.
#   F2   divas korelētas dimensijas: raitums pret pārējiem, visi uzdevumi (2018+),
#        QMCEM; salīdzinājumam F1q.
#   FB   bifaktors visiem uzdevumiem: vispārīgais + specifiskie procesi un raitums
#        (neklasificētie uzdevumi tikai uz vispārīgā faktora); ECV raitumam.
# M2 / RMSEA / SRMSR nav aprēķināmi: adaptīvajā un bukletu dizainā nevienam
# skolēnam nav pilnas atbilžu rindas (mirt M2 prasa pilnus datus).
#
# Ja grupā ir vairāk par N_MAX skolēnu, ņem nejaušu apakšizlasi (svari paliek).
#
# Izvade:  output/pisa2025/dimensionalitate_modeli.csv
#          output/pisa2025/dimensionalitate_uzdevumi.csv
#          output/pisa2025/dimensionalitate_q3.csv
#          output/pisa2025/plots/76_*.png
#          output/pisa2025/logs/76_dimensionalitate_output.txt
#          kešs: data/pisa2025/processed/irt/76/<gads>_<grupa>_<modelis>.rds
# ============================================================================

suppressPackageStartupMessages({ library(mirt); library(ggplot2) })
source("R/pisa2025/00_ab_dati.R")          # ab_grupa()

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
SMOKE <- nzchar(Sys.getenv("PISA76_SMOKE"))
output_file <- if (SMOKE) "output/pisa2025/logs/76_dimensionalitate_smoke.txt" else "output/pisa2025/logs/76_dimensionalitate_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n")
cat("Sākts:", format(Sys.time()), "\n\n")

# --- Iestatījumi -------------------------------------------------------------
GADI   <- strsplit(Sys.getenv("PISA76_GADI", "2015,2018,2022,2025"), ",")[[1]]
GRUPAS <- c("LVA_A", "LVA_B", "EST")
DIR    <- if (SMOKE) "data/pisa2025/processed/irt/76_smoke" else "data/pisa2025/processed/irt/76"
OUT    <- if (SMOKE) "data/pisa2025/processed/irt/76_smoke/" else "output/pisa2025/"
PLOT   <- if (SMOKE) OUT else "output/pisa2025/plots/"
dir.create(DIR, showWarnings = FALSE, recursive = TRUE)
MIN_N   <- c(LVA_A = 150, LVA_B = 100, EST = 150)   # uzdevums modelī, ja uzdots vismaz tik reižu
MIN_KAT <- 10                                        # kategorija ar mazāk atbildēm apvienota ar blakus esošo
MIN_SK  <- 5                                         # skolēns modelī, ja atbildējis vismaz tik uzdevumus
N_MAX   <- if (SMOKE) 400 else 3000                  # lielākais skolēnu skaits grupā (nejauša apakšizlase)
TOL     <- if (SMOKE) 1e-2 else 1e-4                 # EM viendimensiju modeļiem
TOL_Q   <- if (SMOKE) 1e-2 else 5e-4                 # QMCEM daudzdimensiju modeļiem
TOL_B   <- if (SMOKE) 1e-2 else 1e-3                 # bifaktoram: pārbaudīts, ka ECV pret 1e-4 atšķiras par < 0,001, bet 10 reizes ātrāk
NCYC    <- if (SMOKE) 200 else 1500
QMC_PTS <- if (SMOKE) 1000 else 5000
SEED    <- 20260913
mirtCluster(4)
cat(sprintf("Cikli: %s | N_MAX %d | TOL EM %g, QMCEM %g, bifaktors %g | QMCEM punkti %d | pavedieni 4%s\n\n", paste(GADI, collapse = " "), N_MAX, TOL, TOL_Q, TOL_B, QMC_PTS,
            if (SMOKE) " | DŪMU TESTS" else ""))

K22 <- utils::read.csv("output/pisa2025/uzdevumu_klasifikacija_2022.csv", stringsAsFactors = FALSE)
K15 <- utils::read.csv("output/pisa2025/uzdevumu_klasifikacija_2015.csv", stringsAsFactors = FALSE)
PROC22 <- c("Locate information" = "atrast", "Understand" = "saprast", "Evaluate and reflect" = "izvertet")
PROC15 <- c("Access and retrieve" = "atrast", "Integrate and interpret" = "saprast", "Reflect and evaluate" = "izvertet")
FMT    <- c("Continuous" = "nepartraukts", "Non-continuous" = "nenepartraukts", "Mixed" = "jaukts", "Multiple" = "jaukts")
PROC_LAB <- c(atrast = "atrast informāciju", saprast = "saprast", izvertet = "izvērtēt un reflektēt", raitums = "raitums")
GR_LAB   <- c(LVA_A = "Latvija, A plūsma", LVA_B = "Latvija, B plūsma", EST = "Igaunija")
KR       <- c(LVA_A = "#2a78d6", EST = "#eb6834", LVA_B = "#1baf7a")

# Klasifikācija: 2015. gadā primāri PISA 2015 TR (aspekti), citos ciklos PISA 2022 TR
klasif <- function(uzd, g) {
  k <- paste0("CR", uzd)
  p22 <- unname(PROC22[K22$process_skala[match(k, K22$uzd)]]); p15 <- unname(PROC15[K15$aspekts[match(k, K15$uzd)]])
  f22 <- unname(FMT[K22$teksta_formats[match(k, K22$uzd)]]);   f15 <- unname(FMT[K15$teksta_formats[match(k, K15$uzd)]])
  raitums <- grepl("^590", uzd)
  proc <- if (g == "2015") ifelse(is.na(p15), p22, p15) else ifelse(is.na(p22), p15, p22)
  fmt  <- if (g == "2015") ifelse(is.na(f15), f22, f15) else ifelse(is.na(f22), f15, f22)
  proc[raitums] <- "raitums"; fmt[raitums] <- NA
  data.frame(uzd = uzd, vieniba = substr(uzd, 1, 3), process = proc, formats = fmt, raitums = raitums, stringsAsFactors = FALSE)
}

# Pārkodē uz secīgām kategorijām, apvieno retās, atmet uzdevumus ar par maz atbildēm (kā 62b)
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

# Testletes: uzdevumu grupas, kas gandrīz vienmēr uzdotas kopā (Žakāra līdzība ≥ 0,95), kā 62c
testletes <- function(X) {
  A <- (!is.na(X)) * 1; C <- crossprod(A); n <- diag(C); jac <- C / (outer(n, n, "+") - C)
  stats::cutree(stats::hclust(stats::as.dist(1 - jac), method = "complete"), h = 0.05)
}

# Kompakts modeļa kopsavilkums (pilni mirt objekti netiek glabāti)
kopsavilkums <- function(fit, nosaukums, metode, X) {
  S <- tryCatch(coef(fit, simplify = TRUE)$cov, error = function(e) NULL)
  R <- if (!is.null(S) && nrow(S) > 1) { D <- diag(1 / sqrt(diag(S))); R <- D %*% S %*% D; dimnames(R) <- dimnames(S); R } else NULL
  L <- tryCatch(summary(fit, verbose = FALSE)$rotF, error = function(e) NULL)
  list(modelis = nosaukums, metode = metode, uzd = ncol(X), n = nrow(X), logLik = extract.mirt(fit, "logLik"),
       AIC = extract.mirt(fit, "AIC"), BIC = extract.mirt(fit, "BIC"), nest = extract.mirt(fit, "nest"),
       konverg = extract.mirt(fit, "converged"), kor = R, slodzes = L)
}
pielago <- function(X, w, model, nosaukums, metode = c("EM", "QMCEM", "bifaktors"), spec = NULL) {
  metode <- match.arg(metode); D <- as.data.frame(X); it <- itemtype(X); t0 <- Sys.time()
  fit <- switch(metode,
    EM = mirt(D, model, itemtype = it, method = "EM", survey.weights = w, quadpts = 61, TOL = TOL, verbose = FALSE,
              technical = list(NCYCLES = NCYC)),
    QMCEM = mirt(D, model, itemtype = it, method = "QMCEM", survey.weights = w, quadpts = QMC_PTS, TOL = TOL_Q, verbose = FALSE,
                 technical = list(NCYCLES = NCYC, set.seed = SEED)),
    bifaktors = bfactor(D, spec, itemtype = it, survey.weights = w, TOL = TOL_B, verbose = FALSE, technical = list(NCYCLES = NCYC)))
  cat(sprintf("    %-6s %-9s %3d uzd. %5d sk.  logLik %10.1f  BIC %9.1f  %5.1f min%s\n", nosaukums, metode, ncol(X), nrow(X),
              extract.mirt(fit, "logLik"), extract.mirt(fit, "BIC"), as.numeric(difftime(Sys.time(), t0, units = "mins")),
              if (!extract.mirt(fit, "converged")) "  NAV KONVERĢĒJIS" else ""))
  fit
}
konf <- function(grupa) {                      # mirt.model teksts korelētu faktoru modelim
  lv <- unique(grupa[!is.na(grupa)])
  mirt.model(paste(c(vapply(lv, function(l) paste0(l, " = ", paste(which(grupa %in% l), collapse = ",")), ""),
                     paste0("COV = ", paste(lv, collapse = "*"))), collapse = "\n"))
}
kesots <- function(f, expr) if (file.exists(f)) { cat("    ", basename(f), "no keša\n"); readRDS(f) } else { r <- expr; saveRDS(r, f); r }
kesa_fails <- function(g, gr, m) file.path(DIR, sprintf("%s_%s_%s.rds", g, gr, m))

# Bifaktora rādītāji no standartizētajām slodzēm: ECV vispārīgajam faktoram, ω_h, katra
# specifiskā faktora daļa savu uzdevumu kopējā dispersijā (I-ECV specifiskajam)
bif_raditaji <- function(L) {
  G <- L[, 1]; S <- L[, -1, drop = FALSE]; h2 <- rowSums(L^2)
  ecv <- sum(G^2) / sum(L^2)
  omega_h <- sum(G)^2 / (sum(G)^2 + sum(colSums(S)^2) + sum(1 - h2))
  omega_t <- (sum(G)^2 + sum(colSums(S)^2)) / (sum(G)^2 + sum(colSums(S)^2) + sum(1 - h2))
  spec <- vapply(seq_len(ncol(S)), function(k) { i <- S[, k] != 0; sum(S[i, k]^2) / sum(L[i, ]^2) }, 1)
  names(spec) <- colnames(S)
  list(ecv = ecv, omega_h = omega_h, omega_t = omega_t, spec_ecv = spec, ecv_vid_uzd = mean(G^2 / h2, na.rm = TRUE))
}

MOD <- list(); UZD <- list(); Q3 <- list()

for (g in GADI) {
  cat("\n", strrep("=", 100), "\n", g, "\n", strrep("=", 100), "\n", sep = "")
  P <- readRDS(sprintf("data/pisa2025/processed/psihometrija_%s.rds", g))
  X0 <- P$K; X0[X0 == -1L] <- 0L; X0[X0 %in% c(-2L, -3L)] <- NA   # neatbildēts = 0, nesasniegts = trūkst (kā PISA)
  S <- readRDS(sprintf("data/pisa2025/processed/simptomi_%s.rds", g))$stu
  S <- data.frame(CNT = as.character(haven::zap_labels(S$CNT)), CNTSTUID = as.numeric(haven::zap_labels(S$CNTSTUID)),
                  w = as.numeric(haven::zap_labels(S$W_FSTUWT)), LANGN = as.character(as.numeric(haven::zap_labels(S$LANGN))), stringsAsFactors = FALSE)
  S <- S[S$CNT %in% c("LVA", "EST"), ]
  m <- match(paste(P$sk$CNT, P$sk$CNTSTUID), paste(S$CNT, S$CNTSTUID))
  w_all <- S$w[m]
  grupa <- ifelse(P$sk$CNT == "EST", "EST", paste0("LVA_", ab_grupa(S$LANGN[m], g)))
  cat(sprintf("Skolēni: %s; bez svara %d\n", paste(names(table(grupa)), table(grupa), collapse = ", "), sum(is.na(w_all))))
  KL <- klasif(colnames(X0), g)

  for (gr in GRUPAS) {
    sel <- which(grupa %in% gr & !is.na(w_all) & rowSums(!is.na(X0)) >= MIN_SK)
    if (length(sel) < 300) { cat(sprintf("\n  %s: tikai %d skolēnu, izlaists\n", gr, length(sel))); next }
    set.seed(SEED); if (length(sel) > N_MAX) sel <- sort(sample(sel, N_MAX))
    X <- sagatavo(X0[sel, , drop = FALSE], MIN_N[[gr]]); ok <- rowSums(!is.na(X)) >= MIN_SK
    X <- X[ok, , drop = FALSE]; w <- w_all[sel][ok]; w <- w / mean(w)
    if (ncol(X) < 10) { cat(sprintf("\n  %s: tikai %d uzdevumi ar pietiekamu atbilžu skaitu, izlaists\n", gr, ncol(X))); next }
    kl <- KL[match(colnames(X), KL$uzd), ]
    cat(sprintf("\n  %s (%s): %d skolēni, %d uzdevumi (raitums %d; process: %s; formāts: %s)\n", gr, GR_LAB[[gr]], nrow(X), ncol(X), sum(kl$raitums),
                paste(names(table(kl$process[!kl$raitums])), table(kl$process[!kl$raitums]), collapse = " "),
                paste(names(table(kl$formats)), table(kl$formats), collapse = " ")))
    rez <- list()

    # ---- M1: viena dimensija, visi uzdevumi, diagnostika ----------------------------------
    rez$M1 <- kesots(kesa_fails(g, gr, "M1"), {
      fit <- pielago(X, w, 1, "M1", "EM")
      ks <- kopsavilkums(fit, "M1", "EM", X)
      fs <- fscores(fit, method = "EAP", full.scores.SE = TRUE); th <- fs[, 1]
      ks$rxx <- unname(empirical_rxx(fs))
      fi <- itemfit(fit, fit_stats = c("PV_Q1", "infit"))
      cp <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items; bcol <- grep("^b", colnames(cp))
      ks$par <- data.frame(uzd = colnames(X), a = cp[, "a"], b = rowMeans(cp[, bcol, drop = FALSE], na.rm = TRUE),
                           kat = apply(X, 2, max, na.rm = TRUE) + 1, n = colSums(!is.na(X)), izpilde = colMeans(X, na.rm = TRUE) / (apply(X, 2, max, na.rm = TRUE)),
                           infit = fi$infit, outfit = fi$outfit, pvq1 = fi$PV_Q1, pvq1_df = fi$df.PV_Q1, pvq1_rmsea = fi$RMSEA.PV_Q1,
                           pvq1_p = fi$p.PV_Q1, stringsAsFactors = FALSE)
      # Q3: atlikumu korelācijas pāriem, kas uzdoti kopā vismaz 100 reizes
      n <- nrow(X); J <- ncol(X); E <- matrix(NA_real_, n, J)
      for (j in seq_len(J)) { pt <- probtrace(extract.item(fit, j), matrix(th)); E[, j] <- pt %*% (0:(ncol(pt) - 1)) }
      okm <- !is.na(X); Rr <- ifelse(okm, X - E, NA)
      Q <- suppressWarnings(stats::cor(Rr, use = "pairwise.complete.obs")); nn <- crossprod(okm * 1); Q[nn < 100] <- NA; diag(Q) <- NA
      tl <- testletes(X); vien <- kl$vieniba
      ut <- which(upper.tri(Q) & !is.na(Q), arr.ind = TRUE)
      ks$q3 <- data.frame(u1 = colnames(X)[ut[, 1]], u2 = colnames(X)[ut[, 2]], q3 = Q[ut], n = nn[ut],
                          pari = ifelse(vien[ut[, 1]] == vien[ut[, 2]], "viena vienība",
                                 ifelse(tl[ut[, 1]] == tl[ut[, 2]], "viena testlete, citas vienības", "dažādas testletes")), stringsAsFactors = FALSE)
      ks$testletes <- length(unique(tl)); ks
    })
    m1 <- rez$M1
    q <- m1$q3; q3v <- mean(q$q3); q$q3s <- q$q3 - q3v
    qs <- do.call(rbind, lapply(split(q, q$pari), function(x) data.frame(gads = g, grupa = gr, pari = x$pari[1], n_pari = nrow(x),
             q3_vid = mean(x$q3), q3s_vid = mean(x$q3s), q3s_virs_020 = mean(x$q3s > 0.20), q3s_p90 = unname(stats::quantile(x$q3s, 0.9)))))
    Q3[[length(Q3) + 1]] <- qs
    cat(sprintf("    M1: ticamība (EAP) %.3f; testletes %d; Q3 vidējais %.3f; PV-Q1 p < 0,01: %d uzd., outfit ārpus 0,7–1,3: %d uzd.\n",
                m1$rxx, m1$testletes, q3v, sum(m1$par$pvq1_p < 0.01, na.rm = TRUE), sum(m1$par$outfit < 0.7 | m1$par$outfit > 1.3)))
    print(format(qs[, -(1:2)], digits = 2), row.names = FALSE)

    # ---- P: procesi (bez raituma) ----------------------------------------------------------
    jp <- which(!is.na(kl$process) & !kl$raitums)
    tb <- table(kl$process[jp]); jp <- jp[kl$process[jp] %in% names(tb)[tb >= 8]]
    if (length(unique(kl$process[jp])) >= 2) {
      Xp <- X[, jp, drop = FALSE]; rp <- rowSums(!is.na(Xp)) >= 3; Xp <- Xp[rp, , drop = FALSE]; wp <- w[rp]
      pres <- c("atrast", "saprast", "izvertet"); pres <- pres[pres %in% kl$process[jp]]; spec <- match(kl$process[jp], pres)
      rez$P1  <- kesots(kesa_fails(g, gr, "P1"),  kopsavilkums(pielago(Xp, wp, 1, "P1", "EM"), "P1", "EM", Xp))
      rez$P1q <- kesots(kesa_fails(g, gr, "P1q"), kopsavilkums(pielago(Xp, wp, 1, "P1q", "QMCEM"), "P1q", "QMCEM", Xp))
      rez$P3  <- kesots(kesa_fails(g, gr, "P3"),  kopsavilkums(pielago(Xp, wp, konf(kl$process[jp]), "P3", "QMCEM"), "P3", "QMCEM", Xp))
      rez$PB  <- kesots(kesa_fails(g, gr, "PB"), {
        fb <- pielago(Xp, wp, NULL, "PB", "bifaktors", spec = spec); ks <- kopsavilkums(fb, "PB", "bifaktors", Xp)
        colnames(ks$slodzes) <- c("G", paste0("S_", pres))
        ks$rad <- bif_raditaji(ks$slodzes); ks })
      R <- rez$P3$kor; r <- R[upper.tri(R)]
      cat(sprintf("    P3 korelācijas: %s | PB: ECV %.3f, ω_h %.3f, specifiskie: %s | ΔBIC(P3−P1q) %.1f, ΔBIC(PB−P1) %.1f, LR χ² %.1f (df %d)\n",
                  paste(sprintf("%s–%s %.3f", rownames(R)[row(R)[upper.tri(R)]], colnames(R)[col(R)[upper.tri(R)]], r), collapse = ", "),
                  rez$PB$rad$ecv, rez$PB$rad$omega_h, paste(sprintf("%s %.2f", names(rez$PB$rad$spec_ecv), rez$PB$rad$spec_ecv), collapse = " "),
                  rez$P3$BIC - rez$P1q$BIC, rez$PB$BIC - rez$P1$BIC, 2 * (rez$PB$logLik - rez$P1$logLik), rez$PB$nest - rez$P1$nest))
    }

    # ---- T: teksta formāts (bez raituma) ---------------------------------------------------
    jt <- which(!is.na(kl$formats) & !kl$raitums); tb <- table(kl$formats[jt]); jt <- jt[kl$formats[jt] %in% names(tb)[tb >= 8]]
    if (length(unique(kl$formats[jt])) >= 2) {
      Xt <- X[, jt, drop = FALSE]; rt <- rowSums(!is.na(Xt)) >= 3; Xt <- Xt[rt, , drop = FALSE]; wt <- w[rt]
      rez$T1q <- kesots(kesa_fails(g, gr, "T1q"), kopsavilkums(pielago(Xt, wt, 1, "T1q", "QMCEM"), "T1q", "QMCEM", Xt))
      rez$T3  <- kesots(kesa_fails(g, gr, "T3"),  kopsavilkums(pielago(Xt, wt, konf(kl$formats[jt]), "T3", "QMCEM"), "T3", "QMCEM", Xt))
      R <- rez$T3$kor; r <- R[upper.tri(R)]
      cat(sprintf("    T3 korelācijas: %s | ΔBIC(T3−T1q) %.1f\n",
                  paste(sprintf("%s–%s %.3f", rownames(R)[row(R)[upper.tri(R)]], colnames(R)[col(R)[upper.tri(R)]], r), collapse = ", "), rez$T3$BIC - rez$T1q$BIC))
    }

    # ---- F: raitums pret pārējiem, visi uzdevumi (2018+) ----------------------------------
    if (sum(kl$raitums) >= 10) {
      grp <- ifelse(kl$raitums, "raitums", "parejie")
      rez$F1q <- kesots(kesa_fails(g, gr, "F1q"), kopsavilkums(pielago(X, w, 1, "F1q", "QMCEM"), "F1q", "QMCEM", X))
      rez$F2  <- kesots(kesa_fails(g, gr, "F2"),  kopsavilkums(pielago(X, w, konf(grp), "F2", "QMCEM"), "F2", "QMCEM", X))
      pres <- c("atrast", "saprast", "izvertet", "raitums"); pres <- pres[pres %in% kl$process]
      spec <- match(kl$process, pres)                              # NA: uzdevums tikai uz vispārīgā faktora
      rez$FB <- kesots(kesa_fails(g, gr, "FB"), {
        fb <- pielago(X, w, NULL, "FB", "bifaktors", spec = spec); ks <- kopsavilkums(fb, "FB", "bifaktors", X)
        colnames(ks$slodzes) <- c("G", paste0("S_", pres))
        ks$rad <- bif_raditaji(ks$slodzes); ks })
      cat(sprintf("    F2 korelācija raitums–pārējie %.3f | ΔBIC(F2−F1q) %.1f | FB: ECV %.3f, ω_h %.3f, specifiskie: %s | ΔBIC(FB−M1) %.1f\n",
                  rez$F2$kor[1, 2], rez$F2$BIC - rez$F1q$BIC, rez$FB$rad$ecv, rez$FB$rad$omega_h,
                  paste(sprintf("%s %.2f", names(rez$FB$rad$spec_ecv), rez$FB$rad$spec_ecv), collapse = " "), rez$FB$BIC - m1$BIC))
    }

    # ---- apkopo ----------------------------------------------------------------------------
    for (nm in names(rez)) {
      x <- rez[[nm]]; R <- x$kor
      MOD[[length(MOD) + 1]] <- data.frame(gads = g, grupa = gr, modelis = nm, metode = x$metode, uzdevumi = x$uzd, skoleni = x$n,
        parametri = x$nest, logLik = x$logLik, AIC = x$AIC, BIC = x$BIC, konverg = x$konverg,
        ticamiba = if (!is.null(x$rxx)) x$rxx else NA,
        kor_min = if (!is.null(R)) min(R[upper.tri(R)]) else NA, kor_vid = if (!is.null(R)) mean(R[upper.tri(R)]) else NA,
        korelacijas = if (!is.null(R)) paste(sprintf("%s-%s %.3f", rownames(R)[row(R)[upper.tri(R)]], colnames(R)[col(R)[upper.tri(R)]], R[upper.tri(R)]), collapse = "; ") else "",
        ecv = if (!is.null(x$rad)) x$rad$ecv else NA, omega_h = if (!is.null(x$rad)) x$rad$omega_h else NA,
        omega_t = if (!is.null(x$rad)) x$rad$omega_t else NA, ecv_uzd_vid = if (!is.null(x$rad)) x$rad$ecv_vid_uzd else NA,
        spec_ecv = if (!is.null(x$rad)) paste(sprintf("%s %.3f", names(x$rad$spec_ecv), x$rad$spec_ecv), collapse = "; ") else "",
        stringsAsFactors = FALSE)
    }
    u <- m1$par; u$gads <- g; u$grupa <- gr; u <- cbind(u, kl[, c("vieniba", "process", "formats", "raitums")])
    slod <- function(L, uzd) if (is.null(L)) rep(NA_real_, length(uzd)) else { i <- match(uzd, rownames(L)); ifelse(is.na(i), NA, L[i, 1]) }
    slod_s <- function(L, uzd) if (is.null(L)) rep(NA_real_, length(uzd)) else { i <- match(uzd, rownames(L)); s <- L[, -1, drop = FALSE]
      v <- apply(s, 1, function(z) if (all(z == 0)) NA else z[z != 0][1]); ifelse(is.na(i), NA, v[i]) }
    u$PB_G <- slod(rez$PB$slodzes, u$uzd); u$PB_S <- slod_s(rez$PB$slodzes, u$uzd)
    u$FB_G <- slod(rez$FB$slodzes, u$uzd); u$FB_S <- slod_s(rez$FB$slodzes, u$uzd)
    UZD[[length(UZD) + 1]] <- u
    gc(verbose = FALSE)
  }
}

MOD <- do.call(rbind, MOD); UZD <- do.call(rbind, UZD); Q3 <- do.call(rbind, Q3)
utils::write.csv(MOD, paste0(OUT, "dimensionalitate_modeli.csv"), row.names = FALSE)
utils::write.csv(UZD, paste0(OUT, "dimensionalitate_uzdevumi.csv"), row.names = FALSE)
utils::write.csv(Q3, paste0(OUT, "dimensionalitate_q3.csv"), row.names = FALSE)

# === 1. MODEĻU SALĪDZINĀJUMS ================================================================
hdr <- function(t) cat("\n", strrep("=", 100), "\n", t, "\n", strrep("=", 100), "\n\n", sep = "")
hdr("1. Modeļu salīdzinājums pa cikliem un grupām")
f3 <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = 3))
f1 <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = 1))
del <- function(g, gr, a, b, k = "BIC") { x <- MOD[MOD$gads == g & MOD$grupa == gr, ]; if (all(c(a, b) %in% x$modelis)) x[[k]][x$modelis == a] - x[[k]][x$modelis == b] else NA }
get <- function(g, gr, m, k) { x <- MOD[MOD$gads == g & MOD$grupa == gr & MOD$modelis == m, ]; if (nrow(x)) x[[k]] else NA }
SAL <- do.call(rbind, lapply(split(MOD, list(MOD$gads, MOD$grupa), drop = TRUE), function(x) { g <- x$gads[1]; gr <- x$grupa[1]
  data.frame(gads = g, grupa = gr, skoleni = get(g, gr, "M1", "skoleni"), uzdevumi = get(g, gr, "M1", "uzdevumi"), ticamiba = get(g, gr, "M1", "ticamiba"),
    P3_kor_min = get(g, gr, "P3", "kor_min"), P3_kor_vid = get(g, gr, "P3", "kor_vid"), dBIC_P3 = del(g, gr, "P3", "P1q"), dAIC_P3 = del(g, gr, "P3", "P1q", "AIC"),
    PB_ECV = get(g, gr, "PB", "ecv"), PB_omega_h = get(g, gr, "PB", "omega_h"), dBIC_PB = del(g, gr, "PB", "P1"),
    T3_kor_min = get(g, gr, "T3", "kor_min"), dBIC_T3 = del(g, gr, "T3", "T1q"),
    F2_kor = get(g, gr, "F2", "kor_min"), dBIC_F2 = del(g, gr, "F2", "F1q"), FB_ECV = get(g, gr, "FB", "ecv"), FB_omega_h = get(g, gr, "FB", "omega_h"),
    FB_raitums_spec = { s <- get(g, gr, "FB", "spec_ecv"); if (is.na(s) || !grepl("raitums", s)) NA else as.numeric(sub(".*S_raitums ([0-9.]+).*", "\\1", s)) },
    stringsAsFactors = FALSE) }))
SAL <- SAL[order(SAL$grupa != "LVA_A", SAL$grupa != "EST", SAL$gads), ]
print(format(SAL, digits = 3), row.names = FALSE)
cat("\n  dBIC < 0: daudzdimensiju modelis labāks (BIC sods par papildu parametriem pārsniegts). P3 salīdzināts ar 1D uz tiem pašiem\n",
    " uzdevumiem un to pašu QMCEM (P1q); PB un FB (bifaktors, EM) ar precīzo EM 1D (P1, M1). ECV: vispārīgā faktora daļa kopējā\n",
    " izskaidrotajā dispersijā; ω_h: vispārīgā faktora ticamība. FB_raitums_spec: raituma specifiskā faktora daļa raituma uzdevumu dispersijā.\n")
cat("\n  Bifaktora specifisko faktoru daļas (I-ECV) un neklasificētie uzdevumi:\n")
for (i in which(MOD$modelis %in% c("PB", "FB"))) cat(sprintf("    %s %-6s %s: %s%s\n", MOD$gads[i], MOD$grupa[i], MOD$modelis[i], MOD$spec_ecv[i], if (!MOD$konverg[i]) "  (nav konverģējis)" else ""))
cat("\n  Salīdzinājums ar 62b (bez svariem, MHRM), docs/59 8. sadaļa: procesu korelācijas 0,83–0,95, raitums–pārējie 0,54–0,56, LVA 2025 0,66.\n")

# === 2. LOKĀLĀ ATKARĪBA ====================================================================
hdr("2. Lokālā atkarība (Q3, centrēts pret vidējo, Q3*)")
Qw <- stats::reshape(Q3[, c("gads", "grupa", "pari", "q3s_vid", "q3s_virs_020")], idvar = c("gads", "grupa"), timevar = "pari", direction = "wide")
print(format(Qw[order(Qw$grupa != "LVA_A", Qw$grupa != "EST", Qw$gads), ], digits = 2), row.names = FALSE)
cat("\n  q3s_vid: vidējais Q3* pāru kategorijā; q3s_virs_020: pāru daļa ar Q3* > 0,20 (Yen: lokāla atkarība).\n")

# === 3. UZDEVUMU ATBILSTĪBA ================================================================
hdr("3. Uzdevumu atbilstība modelim: PV-Q1 un outfit, Latvija (A plūsma) pret Igauniju")
UZD$neatbilst <- (UZD$pvq1_p < 0.01 & !is.na(UZD$pvq1_p)) | UZD$outfit < 0.7 | UZD$outfit > 1.3
As <- do.call(rbind, lapply(split(UZD, list(UZD$gads, UZD$grupa), drop = TRUE), function(x) data.frame(gads = x$gads[1], grupa = x$grupa[1],
  uzdevumi = nrow(x), pvq1_p_001 = sum(x$pvq1_p < 0.01, na.rm = TRUE), pvq1_rmsea_med = stats::median(x$pvq1_rmsea, na.rm = TRUE),
  pvq1_rmsea_virs_005 = sum(x$pvq1_rmsea > 0.05, na.rm = TRUE), outfit_med = stats::median(x$outfit), outfit_ara = sum(x$outfit < 0.7 | x$outfit > 1.3),
  infit_ara = sum(x$infit < 0.8 | x$infit > 1.2), neatbilst = sum(x$neatbilst), PB_G_med = stats::median(x$PB_G, na.rm = TRUE), FB_G_med = stats::median(x$FB_G, na.rm = TRUE))))
print(format(As[order(As$grupa != "LVA_A", As$grupa != "EST", As$gads), ], digits = 3), row.names = FALSE)
cat("\n  neatbilst: PV-Q1 p < 0,01 vai outfit ārpus 0,7–1,3.\n")

DIF <- utils::read.csv("output/pisa2025/psihometrija_dif.csv", stringsAsFactors = FALSE)
NOS <- c("568Q14", "543Q04", "466Q03", "564Q04", "556Q01")
cat("\n  Uzdevumi, kas Latvijā (A plūsma) neatbilst, bet Igaunijā atbilst; sakritība ar DIF (docs/59 E: C klase) un docs/62 9. sadaļas uzdevumiem:\n")
SL <- list()
for (g in GADI) {
  l <- UZD[UZD$gads == g & UZD$grupa == "LVA_A", ]; e <- UZD[UZD$gads == g & UZD$grupa == "EST", ]
  if (!nrow(l) || !nrow(e)) next
  m <- merge(l, e, by = "uzd", suffixes = c("_lv", "_ee"))
  dif_valsts <- DIF$uzd[DIF$gads == g & DIF$salidzinajums == "valsts: LVA latviešu pret EST igauņu versiju" & DIF$klase == "C"]
  dif_valoda <- DIF$uzd[DIF$gads == g & DIF$salidzinajums == "valoda LVA: krievu pret latviešu versiju" & DIF$klase == "C"]
  m$dif_valsts_C <- m$uzd %in% dif_valsts; m$dif_valoda_C <- m$uzd %in% dif_valoda; m$docs62 <- m$uzd %in% NOS
  sl <- m[m$neatbilst_lv & !m$neatbilst_ee, ]; sl <- sl[order(sl$pvq1_p_lv), ]
  cat(sprintf("    %s: LVA neatbilst %d no %d, EST %d; tikai LVA %d; no tiem valsts DIF C %d (visu uzd. C daļa %.0f %%), valodas DIF C %d, docs/62 %d: %s\n",
              g, sum(m$neatbilst_lv), nrow(m), sum(m$neatbilst_ee), nrow(sl), sum(sl$dif_valsts_C), 100 * mean(m$dif_valsts_C), sum(sl$dif_valoda_C), sum(sl$docs62),
              paste(sprintf("%s%s", sl$uzd, ifelse(sl$docs62, "*", "")), collapse = " ")))
  if (nrow(sl)) SL[[g]] <- data.frame(gads = g, sl[, c("uzd", "process_lv", "raitums_lv", "a_lv", "a_ee", "b_lv", "b_ee", "outfit_lv", "outfit_ee",
                                                        "pvq1_rmsea_lv", "pvq1_rmsea_ee", "pvq1_p_lv", "pvq1_p_ee", "PB_G_lv", "PB_G_ee", "dif_valsts_C", "dif_valoda_C", "docs62")])
  ft <- tryCatch(stats::fisher.test(table(factor(m$neatbilst_lv & !m$neatbilst_ee, c(FALSE, TRUE)), factor(m$dif_valsts_C, c(FALSE, TRUE)))), error = function(e) NULL)
  if (!is.null(ft)) cat(sprintf("       Fišera tests neatbilstība (tikai LVA) × valsts DIF C: OR %.2f, p = %.3f\n", ft$estimate, ft$p.value))
}
SL <- do.call(rbind, SL)
cat("\n  Sliktāk atbilstošie uzdevumi Latvijā (A plūsma), kas Igaunijā atbilst (* = docs/62 9. sadaļa):\n")
if (!is.null(SL)) { print(format(SL, digits = 2), row.names = FALSE); utils::write.csv(SL, paste0(OUT, "dimensionalitate_neatbilstosie.csv"), row.names = FALSE) } else cat("    nav\n")
cat("\n  docs/62 9. sadaļas uzdevumi visos ciklos (LVA A / EST):\n")
for (u in NOS) { x <- UZD[UZD$uzd == u & UZD$grupa %in% c("LVA_A", "EST"), ]
  cat(sprintf("    %s: %s\n", u, paste(vapply(split(x, x$gads), function(z) { l <- z[z$grupa == "LVA_A", ]; e <- z[z$grupa == "EST", ]
    sprintf("%s outfit %s/%s, PV-Q1 p %s/%s, G %s/%s", z$gads[1], f3(l$outfit)[1], f3(e$outfit)[1], f3(l$pvq1_p)[1], f3(e$pvq1_p)[1], f3(l$PB_G)[1], f3(e$PB_G)[1]) }, ""), collapse = "; "))) }
cat("\n  Vai neatbilstība Latvijā atkārtojas ciklos (A plūsma; uzdevumi vismaz divos ciklos):\n")
ta <- tapply(UZD$neatbilst[UZD$grupa == "LVA_A"], UZD$uzd[UZD$grupa == "LVA_A"], function(z) c(sum(z), length(z)))
ta <- do.call(rbind, ta); ta <- ta[ta[, 2] >= 2, ]
cat(sprintf("    %d uzdevumi vismaz divos ciklos; neatbilst visos savos ciklos: %s\n", nrow(ta),
            paste(rownames(ta)[ta[, 1] == ta[, 2] & ta[, 1] >= 2], collapse = " ")))
cat("\n  Vispārīgā faktora slodzes (bifaktors PB) Latvijā pret Igauniju pa uzdevumiem:\n")
for (g in GADI) { l <- UZD[UZD$gads == g & UZD$grupa == "LVA_A", ]; e <- UZD[UZD$gads == g & UZD$grupa == "EST", ]
  m <- merge(l, e, by = "uzd", suffixes = c("_lv", "_ee")); ok <- !is.na(m$PB_G_lv) & !is.na(m$PB_G_ee)
  okf <- !is.na(m$FB_G_lv) & !is.na(m$FB_G_ee)
  cat(sprintf("    %s: PB G slodzes r = %.3f (n %d), vidējā LVA %.3f, EST %.3f%s\n", g, if (sum(ok) > 5) stats::cor(m$PB_G_lv[ok], m$PB_G_ee[ok]) else NA, sum(ok),
              mean(m$PB_G_lv[ok]), mean(m$PB_G_ee[ok]), if (sum(okf) > 5) sprintf("; FB G slodzes r = %.3f, vidējā LVA %.3f, EST %.3f (raitums LVA %.3f, EST %.3f)",
              stats::cor(m$FB_G_lv[okf], m$FB_G_ee[okf]), mean(m$FB_G_lv[okf]), mean(m$FB_G_ee[okf]), mean(m$FB_G_lv[okf & m$raitums_lv]), mean(m$FB_G_ee[okf & m$raitums_lv])) else "")) }

# === 4. GRAFIKI ============================================================================
hdr("4. Grafiki")
TEMA <- theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), legend.position = "bottom", plot.caption = element_text(colour = "grey40", hjust = 0))
SAL$grupa_lab <- factor(GR_LAB[SAL$grupa], levels = GR_LAB)
KRl <- stats::setNames(KR, GR_LAB[names(KR)])
G1 <- rbind(data.frame(SAL[, c("gads", "grupa_lab")], raditajs = "ECV, procesi (bifaktors PB, bez raituma)", y = SAL$PB_ECV),
            data.frame(SAL[, c("gads", "grupa_lab")], raditajs = "ECV, visi uzdevumi (bifaktors FB, ar raitumu)", y = SAL$FB_ECV),
            data.frame(SAL[, c("gads", "grupa_lab")], raditajs = "Raituma specifiskā faktora daļa (FB)", y = SAL$FB_raitums_spec))
G1 <- G1[!is.na(G1$y), ]; G1$gads <- as.numeric(G1$gads)
p <- ggplot(G1, aes(gads, y, colour = grupa_lab, shape = grupa_lab)) + geom_line(linewidth = 0.8) + geom_point(size = 2.6, fill = "white", stroke = 0.9) +
  facet_wrap(~raditajs, ncol = 3) + scale_colour_manual(values = KRl, name = NULL) + scale_shape_manual(values = c(21, 22, 24), name = NULL) +
  scale_x_continuous(breaks = c(2015, 2018, 2022, 2025)) + scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Cik lielu daļu lasīšanas uzdevumu dispersijas skaidro viens vispārīgais faktors", x = NULL, y = "daļa",
       caption = "Bifaktora modeļi ar mirt, svērti ar W_FSTUWT. ECV: vispārīgā faktora daļa kopējā izskaidrotajā dispersijā. Raituma uzdevumi tikai no 2018. gada.") + TEMA
ggsave(paste0(PLOT, "76_ecv.png"), p, width = 11, height = 4.6, dpi = 150, bg = "white"); cat("Saglabāts:", paste0(PLOT, "76_ecv.png"), "\n")

G2 <- rbind(data.frame(SAL[, c("gads", "grupa_lab")], raditajs = "Procesi: mazākā korelācija (P3)", y = SAL$P3_kor_min),
            data.frame(SAL[, c("gads", "grupa_lab")], raditajs = "Teksta formāti: mazākā korelācija (T3)", y = SAL$T3_kor_min),
            data.frame(SAL[, c("gads", "grupa_lab")], raditajs = "Raitums pret pārējiem (F2)", y = SAL$F2_kor))
G2 <- G2[!is.na(G2$y), ]; G2$gads <- as.numeric(G2$gads)
p <- ggplot(G2, aes(gads, y, colour = grupa_lab, shape = grupa_lab)) + geom_hline(yintercept = 0.9, linetype = 3, colour = "grey60") +
  geom_line(linewidth = 0.8) + geom_point(size = 2.6, fill = "white", stroke = 0.9) +
  facet_wrap(~raditajs, ncol = 3) + scale_colour_manual(values = KRl, name = NULL) + scale_shape_manual(values = c(21, 22, 24), name = NULL) +
  scale_x_continuous(breaks = c(2015, 2018, 2022, 2025)) + scale_y_continuous(limits = c(0.4, 1)) +
  labs(title = "Latentās korelācijas starp lasīšanas apakšdimensijām", x = NULL, y = "korelācija",
       caption = "Korelētu faktoru modeļi ar mirt (QMCEM), svērti ar W_FSTUWT. Punktētā līnija 0,90: praktiski viena dimensija.") + TEMA
ggsave(paste0(PLOT, "76_korelacijas.png"), p, width = 11, height = 4.6, dpi = 150, bg = "white"); cat("Saglabāts:", paste0(PLOT, "76_korelacijas.png"), "\n")

G3 <- do.call(rbind, lapply(GADI, function(g) { l <- UZD[UZD$gads == g & UZD$grupa == "LVA_A", ]; e <- UZD[UZD$gads == g & UZD$grupa == "EST", ]
  if (!nrow(l) || !nrow(e)) return(NULL); m <- merge(l, e, by = "uzd", suffixes = c("_lv", "_ee")); m$gads <- g; m }))
G3$kopa <- ifelse(G3$uzd %in% NOS, "docs/62 uzdevums", ifelse(G3$raitums_lv, "raitums", "cits"))
p <- ggplot(G3, aes(outfit_ee, outfit_lv)) + geom_abline(linetype = 3, colour = "grey60") +
  annotate("rect", xmin = 0.7, xmax = 1.3, ymin = 0.7, ymax = 1.3, fill = NA, colour = "grey75", linetype = 2) +
  geom_point(aes(colour = kopa, shape = kopa), size = 1.8, alpha = 0.75) +
  geom_text(data = G3[G3$kopa == "docs/62 uzdevums" | G3$outfit_lv > 1.3 | G3$outfit_lv < 0.65, ], aes(label = uzd), size = 2.3, vjust = -0.7, colour = "grey20") +
  facet_wrap(~gads, ncol = 4) + scale_colour_manual(values = c(cits = "grey55", raitums = "#1baf7a", "docs/62 uzdevums" = "#e34948"), name = NULL) +
  scale_shape_manual(values = c(cits = 16, raitums = 17, "docs/62 uzdevums" = 15), name = NULL) +
  labs(title = "Uzdevumu atbilstība modelim (outfit): Latvija, A plūsma, pret Igauniju", x = "outfit, Igaunija", y = "outfit, Latvija (A plūsma)",
       caption = "Viendimensiju 2PL/GPCM modeļi, svērti. Pārtrauktais kvadrāts: pieņemamā josla 0,7–1,3. Nosaukti docs/62 9. sadaļas uzdevumi un uzdevumi ārpus joslas Latvijā.") + TEMA
ggsave(paste0(PLOT, "76_atbilstiba.png"), p, width = 12, height = 4.8, dpi = 150, bg = "white"); cat("Saglabāts:", paste0(PLOT, "76_atbilstiba.png"), "\n")

Q3$grupa_lab <- factor(GR_LAB[Q3$grupa], levels = GR_LAB); Q3$gads_n <- as.numeric(Q3$gads)
p <- ggplot(Q3, aes(gads_n, q3s_virs_020, colour = grupa_lab, shape = grupa_lab)) + geom_line(linewidth = 0.8) + geom_point(size = 2.6, fill = "white", stroke = 0.9) +
  facet_wrap(~pari, ncol = 3) + scale_colour_manual(values = KRl, name = NULL) + scale_shape_manual(values = c(21, 22, 24), name = NULL) +
  scale_x_continuous(breaks = c(2015, 2018, 2022, 2025)) + scale_y_continuous(labels = function(x) paste0(round(100 * x), " %")) +
  labs(title = "Lokālā atkarība: uzdevumu pāru daļa ar Q3* > 0,20", x = NULL, y = "pāru daļa",
       caption = "Q3 no viendimensiju modeļa atlikumiem, centrēts pret vidējo (Q3*). Testlete: uzdevumu grupa, kas gandrīz vienmēr uzdota kopā.") + TEMA
ggsave(paste0(PLOT, "76_q3.png"), p, width = 11, height = 4.6, dpi = 150, bg = "white"); cat("Saglabāts:", paste0(PLOT, "76_q3.png"), "\n")

# --- Noslēgums ---------------------------------------------------------------
cat("\nIzvades faili:\n ", paste0(OUT, "dimensionalitate_modeli.csv"), "\n ", paste0(OUT, "dimensionalitate_uzdevumi.csv"), "\n ",
    paste0(OUT, "dimensionalitate_q3.csv"), "\n ", paste0(OUT, "dimensionalitate_neatbilstosie.csv"), "\n ", paste0(PLOT, "76_*.png"), "\n ", output_file, "\n")
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
