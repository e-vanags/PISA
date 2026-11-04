# ============================================================================
# 67a. Paaudžu modelis (docs/61 C līmenis), 1. solis: paaudžu panelis
# ============================================================================
#
# Vienība: valsts (Latvijā arī latviešu valodas plūsma) × pētījums × cikls → dzimšanas paaudze.
# Pētījumi un vecums:
#   PIRLS 4. kl. lasīšana (2001, 2006, 2016, 2021)        ~10,5 g.
#   TIMSS 4. kl. matemātika, dabaszinātnes (2003, 2007, 2019, 2023)  ~10,5 g.
#   ICCS 8. kl. pilsoniskās zināšanas (2009, 2016, 2022)  ~14,3 g.
#   ICILS 8. kl. datorpratība (2023)                      ~14,3 g.
#   PISA lasīšana (2000–2025)                            15,3–16,2 g.
# Rādītāji katram: vidējais, 10. procentile (P10), zemo sniegumu daļa:
#   PISA < 407,47 (zem 2. līmeņa); PIRLS/TIMSS < 400 (zem zemā etalona);
#   ICCS < 395 (zem 1./B līmeņa); ICILS < 407 (zem 1. līmeņa).
# SE: IEA ar jackknife (JKZONE/JKREP, abas puses, V = ½Σ) un 5 PV;
#     PISA ar skolu grupu jackknife (50 nejaušas skolu grupas, JK1) un PV (Rubins),
#     vienādi visiem cikliem (BRR svari ir tikai 2015+; stratifikācija netiek ņemta vērā,
#     tāpēc SE ir nedaudz konservatīvas).
# Publicēšanas datums (aptuveni) ex ante noteikumam: pētījums izmantojams prognozē, ja
#   publicēts pirms mērķa PISA testa (marts–maijs).
#
# Ievade: data/iea/processed/iea_skoleni.rds (66), data/pisa2025/processed/simptomi_apvienots.rds
# Izvade: data/pisa2025/processed/paaudzu_panelis.csv
# ============================================================================

suppressPackageStartupMessages(library(data.table))
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/67a_paaudzu_panelis_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8"); sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
set.seed(20260912)

VAL <- c("LVA", "EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")
wq <- function(x, w, p) { i <- !is.na(x) & w > 0; x <- x[i]; w <- w[i]; o <- order(x); cw <- cumsum(w[o]) / sum(w); x[o][which(cw >= p)[1]] }
wm <- function(x, w) { i <- !is.na(x) & w > 0; sum(w[i] * x[i]) / sum(w[i]) }
stat <- function(x, w, r, slieksnis) switch(r, vid = wm(x, w), p10 = wq(x, w, 0.10), zems = 100 * wm(as.numeric(x < slieksnis), w))
RAD <- c("vid", "p10", "zems")

# ---------------------------------------------------------------------------
# IEA: jackknife ar abām pusēm
# ---------------------------------------------------------------------------
S <- readRDS("data/iea/processed/iea_skoleni.rds")
S <- S[!(cnt == "DNW" & paste(st, g) %in% S[cnt == "DEU", unique(paste(st, g))])][, grupa := cnt][cnt == "DNW", grupa := "DEU"]
S[cnt == "LVA", lat := is.na(test_val) | test_val == "" | grepl("latv|<test language>", test_val, ignore.case = TRUE)]
iea_novert <- function(d, dm, sl) {
  pc <- paste0("pv_", dm, 1:5); if (!all(pc %in% names(d)) || all(is.na(d[[pc[1]]]))) return(NULL)
  zones <- unique(d$jz[!is.na(d$jz)])
  rbindlist(lapply(RAD, function(r) {
    e <- sapply(pc, function(p) {
      x <- d[[p]]; th <- stat(x, d$w, r, sl); rep <- c()
      for (h in zones) { i <- which(d$jz == h); w1 <- d$w; w2 <- d$w
        w1[i] <- d$w[i] * 2 * (d$jr[i] == 1); w2[i] <- d$w[i] * 2 * (d$jr[i] == 0)
        rep <- c(rep, stat(x, w1, r, sl), stat(x, w2, r, sl)) }
      c(th, 0.5 * sum((rep - th)^2))
    })
    data.table(radītājs = r, est = mean(e[1, ]), se = sqrt(mean(e[2, ]) + 1.2 * var(e[1, ])))
  }))
}
IEA_SPEC <- list(list(st = "PIRLS", dm = "las", sl = 400, pet = "PIRLS lasīšana"), list(st = "TIMSS4", dm = "mat", sl = 400, pet = "TIMSS4 matemātika"),
                 list(st = "TIMSS4", dm = "dab", sl = 400, pet = "TIMSS4 dabaszinātnes"), list(st = "ICCS", dm = "civ", sl = 395, pet = "ICCS pilsoniskās zināšanas"),
                 list(st = "ICILS", dm = "cil", sl = 407, pet = "ICILS datorpratība"))
t0 <- Sys.time(); IEA <- list()
for (sp in IEA_SPEC) for (gg in sort(unique(S[st == sp$st]$g))) {
  for (gr in c(VAL, "LVA A")) {
    d <- if (gr == "LVA A") S[st == sp$st & g == gg & cnt == "LVA" & lat & vm_ja %in% 1] else S[st == sp$st & g == gg & grupa == gr]
    if (nrow(d) < 200) next
    x <- iea_novert(d, sp$dm, sp$sl); if (is.null(x)) next
    IEA[[paste(sp$pet, gg, gr)]] <- x[, `:=`(petijums = sp$pet, gads = gg, grupa = gr, n = nrow(d), vecums = if (sp$st %in% c("PIRLS", "TIMSS4")) 10.5 else 14.3)]
  }
  cat(sprintf("  %s %d: %.1f min\n", sp$pet, gg, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
IEA <- rbindlist(IEA); rm(S); gc(verbose = FALSE)

# ---------------------------------------------------------------------------
# PISA: skolu grupu jackknife (JK1, 50 grupas) un PV
# ---------------------------------------------------------------------------
A <- readRDS("data/pisa2025/processed/simptomi_apvienots.rds")[, c("gads", "cnt", "plusma", "skola", "w", paste0("PV", 1:10, "READ")), with = FALSE]
A <- A[!is.na(w) & w > 0 & cnt %in% VAL]
G <- 50
pisa_novert <- function(d) {
  pvs <- paste0("PV", 1:10, "READ"); pvs <- pvs[sapply(pvs, function(p) any(!is.na(d[[p]])))]
  sk <- unique(d$skola); gr <- sample(rep_len(1:G, length(sk)))[match(d$skola, sk)]
  rbindlist(lapply(RAD, function(r) {
    e <- sapply(pvs, function(p) {
      x <- d[[p]]; th <- stat(x, d$w, r, 407.47)
      rep <- sapply(1:G, function(k) { ww <- d$w * (gr != k) * G / (G - 1); stat(x, ww, r, 407.47) })
      c(th, (G - 1) / G * sum((rep - mean(rep))^2))
    })
    m <- length(pvs)
    data.table(radītājs = r, est = mean(e[1, ]), se = sqrt(mean(e[2, 1:min(5, m)]) + (1 + 1 / m) * var(e[1, ])))
  }))
}
t0 <- Sys.time(); PI <- list()
for (gg in sort(unique(A$gads))) {
  for (gr in c(VAL, "LVA A")) {
    d <- if (gr == "LVA A") A[gads == gg & cnt == "LVA" & plusma %in% "A"] else A[gads == gg & cnt == gr]
    if (nrow(d) < 200) next
    PI[[paste(gg, gr)]] <- pisa_novert(d)[, `:=`(petijums = "PISA lasīšana", gads = as.integer(gg), grupa = gr, n = nrow(d), vecums = 15.8)]
  }
  cat(sprintf("  PISA %s: %.1f min\n", gg, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
PI <- rbindlist(PI); rm(A); gc(verbose = FALSE)

# ---------------------------------------------------------------------------
# Panelis
# ---------------------------------------------------------------------------
P <- rbind(IEA, PI)
P[, dz_gads := gads - vecums]
PUBL <- c("PIRLS lasīšana 2001" = "2003-04", "PIRLS lasīšana 2006" = "2007-11", "PIRLS lasīšana 2016" = "2017-12", "PIRLS lasīšana 2021" = "2023-05",
          "TIMSS4 2003" = "2004-12", "TIMSS4 2007" = "2008-12", "TIMSS4 2019" = "2020-12", "TIMSS4 2023" = "2024-12",
          "ICCS pilsoniskās zināšanas 2009" = "2010-06", "ICCS pilsoniskās zināšanas 2016" = "2017-11", "ICCS pilsoniskās zināšanas 2022" = "2023-11",
          "ICILS datorpratība 2023" = "2024-11")
P[, publ := ifelse(petijums == "PISA lasīšana", sprintf("%d-12", gads + 1),
                   PUBL[ifelse(grepl("^TIMSS4", petijums), paste("TIMSS4", gads), paste(petijums, gads))])]
setcolorder(P, c("petijums", "gads", "grupa", "radītājs", "est", "se", "n", "vecums", "dz_gads", "publ"))
fwrite(P, "data/pisa2025/processed/paaudzu_panelis.csv")
cat("\nPanelis:", nrow(P), "rindas\n")
print(dcast(P[radītājs == "vid"], petijums + gads ~ grupa, value.var = "est", fun.aggregate = function(x) round(mean(x))), row.names = FALSE)
cat("\nZemo sniegumu daļa (%):\n")
print(dcast(P[radītājs == "zems"], petijums + gads ~ grupa, value.var = "est", fun.aggregate = function(x) round(mean(x), 1)), row.names = FALSE)
cat("\nSE piemērs (Latvija, vidējais):\n"); print(P[grupa %in% c("LVA", "LVA A") & radītājs == "vid", .(petijums, gads, grupa, est = round(est, 1), se = round(se, 1))], row.names = FALSE)
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink(); close(log_con)
