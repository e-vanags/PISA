# ============================================================================
# 57. Apakšskalas, lasīšanas paradumi un grāmatas mājās (docs/57, B un C)
# ============================================================================
#
# B. APAKŠSKALAS. 2000 (tikai visa Latvija, jo plūsmu nevar nodalīt): atrast,
#    interpretēt, reflektēt. 2009: tie paši trīs aspekti un nepārtraukts /
#    nepārtraukts teksts. 2018: atrast, saprast, izvērtēt un reflektēt; viens /
#    vairāki avoti. Ietvari atšķiras, tāpēc apakšskalas netiek salīdzinātas
#    absolūti starp cikliem; rādīts relatīvais profils: apakšskala mīnus kopējā
#    lasīšana tajā pašā ciklā (pāros pa PV), ar Rubina un BRR SE.
# C. PARADUMI. Kopīgie jautājumi 2000, 2009, 2018: pieci attieksmes apgalvojumi
#    (% piekrīt), laiks lasīšanai priekam (% nelasa priekam; % vairāk par 30 min
#    dienā), pieci lasāmvielu veidi (% vairākas reizes mēnesī vai biežāk).
#    Grāmatas mājās 2000-2025: % ne vairāk kā 10; % vairāk nekā 200 (2000. gadā
#    kategorijas citas: vairāk nekā 250). 2022. un 2025. gadā ST255Q01JA ar 7
#    kategorijām ("nav grāmatu" un "1–10" atsevišķi), dati no 64a izvilkuma.
#
# Tikai A plūsma 2009 un 2018; salīdzinājumam arī visa Latvija, jo 2000. gadā
# plūsmu nav.
#
# Izvade:  output/pisa2025/apakskalas.csv, paradumi.csv
#          output/pisa2025/logs/57_apakskalas_paradumi_output.txt
# ============================================================================

source("R/pisa2025/00_ab_dati.R")

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/57_apakskalas_paradumi_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

D <- ab_dati()
F <- c("2015" = "data/pisa2015/raw/CY6_MS_CMB_STU_QQQ.sav",
       "2018" = "data/pisa2018/raw/STU/CY07_MSU_STU_QQQ.sav",
       "2022" = "data/pisa2022/raw/CY08MSP_STU_QQQ.SAV")
K <- "data/pisa2025/processed/ab_paradumi.rds"
if (!file.exists(K)) {
  SUB18 <- unlist(lapply(c("RCLI", "RCUN", "RCER", "RTSN", "RTML"), function(s) paste0("PV", 1:10, s)))
  V18 <- c("CNT", "CNTSTUID", "ST013Q01TA", paste0("ST160Q0", 1:5, "IA"), "ST175Q01IA",
           paste0("ST167Q0", 1:5, "IA"), SUB18)
  rd <- function(f, v) { d <- haven::read_sav(f, col_select = tidyselect::all_of(v))
    d <- d[as.character(d$CNT) == "LVA", ]; as.data.frame(lapply(d[, -1], as.numeric)) }
  x22 <- haven::read_sav(F[["2022"]], n_max = 1)
  lab22 <- vapply(x22, function(c) { a <- attr(c, "label"); if (is.null(a)) "" else a }, "")
  k22 <- names(lab22)[grepl("books", lab22, ignore.case = TRUE) & grepl("home", lab22, ignore.case = TRUE)]
  cat("2022 grāmatu mainīgie:", paste(k22, lab22[k22], sep = " = ", collapse = " | "), "\n")
  kat22 <- if (length(k22)) attr(x22[[k22[1]]], "labels") else NULL
  saveRDS(list(d15 = rd(F[["2015"]], c("CNT", "CNTSTUID", "ST013Q01TA")),
               d18 = rd(F[["2018"]], V18),
               d22 = if (length(k22)) rd(F[["2022"]], c("CNT", "CNTSTUID", k22[1])) else NULL,
               k22 = k22[1], kat22 = kat22), K)
}
PD <- readRDS(K)

vec <- function(g) utils::read.csv(sprintf("data/pisa_veci/lva_%s.csv", g), colClasses = "character")
num <- function(d, k) if (k %in% names(d)) suppressWarnings(as.numeric(d[[k]])) else rep(NA_real_, nrow(d))
# 2000. gadam svari un replikātsvari no izvilkuma (ielādētājā 2000. gada nav).
d00 <- vec("2000")
S00 <- list(w = num(d00, "W_FSTUWT"), rw = lapply(paste0("W_FSTR", 1:80), function(k) num(d00, k)),
            pv = lapply(paste0("PV", 1:5, "READ"), function(k) num(d00, k)))
pct <- function(ok, ev, w, rw) {
  f <- function(w) 100 * sum(w[ok & ev]) / sum(w[ok])
  e <- f(w); c(est = e, se = sqrt(sum((vapply(rw, f, 1) - e)^2) / 20), n = sum(ok))
}

# ---------------------------------------------------------------------------
cat(strrep("=", 90), "\nB. Apakšskalu relatīvais profils: apakšskala mīnus kopējā lasīšana\n",
    strrep("=", 90), "\n\n")
prof <- function(pvs, pvr, w, rw) rubin_brr(Map(`-`, pvs, pvr), w, rw, VID)
aps <- list()
add_ap <- function(gads, grupa, nos, r) aps[[length(aps) + 1]] <<- data.frame(
  gads = gads, grupa = grupa, apakskala = nos, starpiba = r[["est"]], se = r[["se"]])
for (k in 1:3) add_ap("2000", "Latvija", c("atrast (retrieving)", "interpretēt", "reflektēt")[k],
  prof(lapply(paste0("PV", 1:5, "READ", k), function(v) num(d00, v)), S00$pv, S00$w, S00$rw))
d09 <- vec("2009")
for (gr in c("A", "Latvija")) {
  a <- if (gr == "A") D[["2009"]]$sk$grupa %in% "A" else rep(TRUE, nrow(d09))
  for (k in 1:5) add_ap("2009", gr, c("atrast (access and retrieve)", "interpretēt (integrate)", "reflektēt",
                                      "nepārtraukts teksts", "nepārtraukts (non-continuous)")[k],
    prof(lapply(paste0("PV", 1:5, "READ", k), function(v) num(d09, v)[a]), lapply(D[["2009"]]$pv, `[`, a),
         D[["2009"]]$sk$w[a], lapply(D[["2009"]]$rw, `[`, a)))
}
p18 <- PD$d18[match(as.numeric(D[["2018"]]$sk$id), PD$d18$CNTSTUID), ]
for (gr in c("A", "Latvija")) {
  a <- if (gr == "A") D[["2018"]]$sk$grupa %in% "A" else rep(TRUE, nrow(p18))
  for (s in c("RCLI", "RCUN", "RCER", "RTSN", "RTML"))
    add_ap("2018", gr, c(RCLI = "atrast (locate)", RCUN = "saprast", RCER = "izvērtēt un reflektēt",
                         RTSN = "viens avots", RTML = "vairāki avoti")[[s]],
      prof(lapply(paste0("PV", 1:10, s), function(v) p18[[v]][a]), lapply(D[["2018"]]$pv, `[`, a),
           D[["2018"]]$sk$w[a], lapply(D[["2018"]]$rw, `[`, a)))
}
aps <- do.call(rbind, aps)
for (i in seq_len(nrow(aps))) cat(sprintf("  %s %-8s %-30s %+6.1f (%.1f)%s\n", aps$gads[i], aps$grupa[i],
  aps$apakskala[i], aps$starpiba[i], aps$se[i], if (abs(aps$starpiba[i] / aps$se[i]) > 1.96) "*" else " "))
utils::write.csv(aps, "output/pisa2025/apakskalas.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 90), "\nC. Lasīšanas paradumi\n", strrep("=", 90), "\n\n")
JAUT <- list(
  "lasu tikai tad, ja jālasa"       = c("ST35Q01", "ST24Q01", "ST160Q01IA"),
  "lasīšana ir mans hobijs"         = c("ST35Q02", "ST24Q02", "ST160Q02IA"),
  "patīk runāt par grāmatām"        = c("ST35Q03", "ST24Q03", "ST160Q03IA"),
  "lasīšana ir laika izšķiešana"    = c("ST35Q06", "ST24Q06", "ST160Q04IA"),
  "lasu tikai informācijai"         = c("ST35Q08", "ST24Q08", "ST160Q05IA"))
LAIKS <- c("ST34Q01", "ST23Q01", "ST175Q01IA")
VIELAS <- list("žurnāli" = c("ST36Q01", "ST25Q01", "ST167Q01IA"), "komiksi" = c("ST36Q02", "ST25Q02", "ST167Q02IA"),
               "daiļliteratūra" = c("ST36Q03", "ST25Q03", "ST167Q03IA"),
               "populārzinātniskas grāmatas" = c("ST36Q04", "ST25Q04", "ST167Q04IA"),
               "avīzes" = c("ST36Q06", "ST25Q05", "ST167Q05IA"))
avots <- function(g) switch(g, "2000" = d00, "2009" = d09, "2018" = p18)
kopas <- function(g) {
  if (g == "2000") return(list(Latvija = list(a = rep(TRUE, nrow(d00)), w = S00$w, rw = S00$rw)))
  sk <- D[[g]]; list(A = list(a = sk$sk$grupa %in% "A", w = sk$sk$w, rw = sk$rw),
                     Latvija = list(a = rep(TRUE, nrow(sk$sk)), w = sk$sk$w, rw = sk$rw))
}
par <- list()
add <- function(nos, g, gr, r) par[[length(par) + 1]] <<- data.frame(
  radītājs = nos, gads = g, grupa = gr, pct = r[["est"]], se = r[["se"]], n = r[["n"]])
for (gi in 1:3) {
  g <- c("2000", "2009", "2018")[gi]; src <- avots(g)
  for (gr in names(kopas(g))) {
    K2 <- kopas(g)[[gr]]; rwa <- lapply(K2$rw, function(r) r * K2$a); wa <- K2$w * K2$a
    for (nos in names(JAUT)) { x <- num(src, JAUT[[nos]][gi])
      add(nos, g, gr, pct(x %in% 1:4, x %in% 3:4, wa, rwa)) }
    x <- num(src, LAIKS[gi])
    add("nelasa priekam", g, gr, pct(x %in% 1:5, x %in% 1, wa, rwa))
    add("lasa priekam > 30 min dienā", g, gr, pct(x %in% 1:5, x %in% 3:5, wa, rwa))
    for (nos in names(VIELAS)) { x <- num(src, VIELAS[[nos]][gi])
      add(paste0("lasa ", nos, " vismaz vairākas reizes mēnesī"), g, gr, pct(x %in% 1:5, x %in% 4:5, wa, rwa)) }
  }
}
# Kategoriju pārbaude: sadalījums vienam jautājumam katrā gadā
for (gi in 1:3) { g <- c("2000", "2009", "2018")[gi]
  cat(sprintf("  kodu pārbaude %s %s: %s\n", g, LAIKS[gi], paste(names(table(num(avots(g), LAIKS[gi]))),
      table(num(avots(g), LAIKS[gi])), sep = ":", collapse = " "))) }

# Grāmatas mājās
GRAM <- list("2000" = list(k = "ST37Q01", maz = 1:2, daudz = 6:7, ok = 1:7),
             "2003" = list(k = "ST19Q01", maz = 1, daudz = 5:6, ok = 1:6),
             "2006" = list(k = "ST15Q01", maz = 1, daudz = 5:6, ok = 1:6),
             "2009" = list(k = "ST22Q01", maz = 1, daudz = 5:6, ok = 1:6),
             "2012" = list(k = "ST28Q01", maz = 1, daudz = 5:6, ok = 1:6),
             "2015" = list(k = "ST013Q01TA", maz = 1, daudz = 5:6, ok = 1:6),
             "2018" = list(k = "ST013Q01TA", maz = 1, daudz = 5:6, ok = 1:6),
             "2022" = list(k = "ST255Q01JA", maz = 1:2, daudz = 6:7, ok = 1:7),
             "2025" = list(k = "ST255Q01JA", maz = 1:2, daudz = 6:7, ok = 1:7))
# Labojums 2026-09-11: 2022. un 2025. gada skolēnu anketā grāmatu jautājums IR
# (ST255Q01JA, Latvijā derīgs ~98 %); agrāk tas netika atrasts, jo meklēja pēc
# etiķetes un atrada vecāku anketas mainīgo.
for (g in names(GRAM)) {
  G2 <- GRAM[[g]]
  x <- if (g == "2000") num(d00, G2$k) else if (g %in% VECIE) num(vec(g), G2$k) else if (g %in% c("2022", "2025")) {
    s <- readRDS(sprintf("data/pisa2025/processed/simptomi_%s.rds", g))$stu
    as.numeric(s[[G2$k]])[match(as.numeric(D[[g]]$sk$id), as.numeric(s$CNTSTUID))] } else {
    src <- switch(g, "2015" = PD$d15, "2018" = PD$d18)
    src[[G2$k]][match(as.numeric(D[[g]]$sk$id), src$CNTSTUID)] }
  cat(sprintf("  kodu pārbaude grāmatas %s: %s\n", g, paste(names(table(x)), table(x), sep = ":", collapse = " ")))
  for (gr in names(kopas(g))) {
    K2 <- kopas(g)[[gr]]; rwa <- lapply(K2$rw, function(r) r * K2$a); wa <- K2$w * K2$a
    add(if (g == "2000") "grāmatas: ne vairāk kā 10" else "grāmatas: ne vairāk kā 10", g, gr, pct(x %in% G2$ok, x %in% G2$maz, wa, rwa))
    add(if (g == "2000") "grāmatas: vairāk nekā 200 (2000: >250)" else "grāmatas: vairāk nekā 200 (2000: >250)", g, gr,
        pct(x %in% G2$ok, x %in% G2$daudz, wa, rwa))
  }
}
par <- do.call(rbind, par)
utils::write.csv(par, "output/pisa2025/paradumi.csv", row.names = FALSE)

cat("\n")
for (gr in c("A", "Latvija")) {
  cat(sprintf("\n  %s\n  %-50s %s\n", gr, "", paste(sprintf("%14s", c("2000", "2003", "2006", "2009", "2012", "2015", "2018", "2022")), collapse = "")))
  for (nos in unique(par$radītājs)) {
    x <- par[par$radītājs == nos & par$grupa == gr, ]
    if (!nrow(x)) next
    cel <- vapply(c("2000", "2003", "2006", "2009", "2012", "2015", "2018", "2022"), function(g) {
      r <- x[x$gads == g, ]; if (!nrow(r)) sprintf("%14s", "") else sprintf("%14s", sprintf("%.1f (%.1f)", r$pct, r$se)) }, "")
    cat(sprintf("  %-50s %s\n", substr(nos, 1, 50), paste(cel, collapse = "")))
  }
}
cat("\nIzveidotie faili:\n  output/pisa2025/apakskalas.csv\n  output/pisa2025/paradumi.csv\n ", output_file,
    "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
