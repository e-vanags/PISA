# ============================================================================
# 56. Citas jomas: matemātika un dabaszinātnes, A plūsma 2003-2025 (docs/57, D)
# ============================================================================
#
# JAUTĀJUMS. Vai A plūsmā vispārējs kritums (arī matemātikā un dabaszinātnēs)
# sākās agrāk nekā lasīšanā? Ja citās jomās kritums redzams jau pirms 2018. gada,
# tas ir agrīns signāls, ko lasīšanas vidējais neparāda.
#
# Matemātikas skala salīdzināma no 2003. gada, dabaszinātņu no 2006. gada.
# Saites kļūdas visām trim jomām no vienotās tabulas output/pisa2025/saites_kludas.csv
# (78. skripts: oficiālās līdz 2018 un OECD 2025 Vol. I atvasinātās pret 2025, sk.
# data/pisa_tehniskie/PISA2025_TR_AVOTS.md). Matemātikai un dabaszinātnēm 2018-2022
# vērtības nav: LE = 3 un rinda atzīmēta. Līdz 2026-09-13 pagaidu 3 bija arī visiem
# 2022-2025 pāriem (tagad lasīšana 1,094, matemātika 1,220, dabaszinātnes 3,116).
#
# Izvade:  output/pisa2025/citas_jomas.csv
#          output/pisa2025/logs/56_citas_jomas_output.txt
# ============================================================================

source("R/pisa2025/00_ab_dati.R")

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/56_citas_jomas_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

LE_TAB <- utils::read.csv("output/pisa2025/saites_kludas.csv", stringsAsFactors = FALSE)   # viens avots, 78. skripts
le_of <- function(joma, a, b) {
  i <- which(LE_TAB$joma == joma & LE_TAB$no == as.numeric(a) & LE_TAB$uz == as.numeric(b))
  if (length(i)) c(LE_TAB$LE[i[1]], 1) else c(3, 0)
}
L2 <- c(READ = 407.47, MATH = 420.07, SCIE = 409.54)
D <- ab_dati()

K <- "data/pisa2025/processed/ab_citas_jomas.rds"
if (!file.exists(K)) {
  F <- c("2015" = "data/pisa2015/raw/CY6_MS_CMB_STU_QQQ.sav",
         "2018" = "data/pisa2018/raw/STU/CY07_MSU_STU_QQQ.sav",
         "2022" = "data/pisa2022/raw/CY08MSP_STU_QQQ.SAV")
  V <- c("CNT", "CNTSTUID", paste0("PV", 1:10, "MATH"), paste0("PV", 1:10, "SCIE"))
  out <- list()
  for (g in names(F)) {
    d <- haven::read_sav(F[[g]], col_select = tidyselect::all_of(V))
    d <- d[as.character(d$CNT) == "LVA", ]
    out[[g]] <- as.data.frame(lapply(d[, -1], function(x) as.numeric(x)))
  }
  s <- readRDS("data/pisa2025/processed/pisa_lva_2025_stu.rds"); names(s) <- toupper(names(s))
  out[["2025"]] <- as.data.frame(lapply(s[, V[-1]], function(x) as.numeric(x)))
  saveRDS(out, K)
}
KJ <- readRDS(K)

pv_joma <- function(g, joma) {
  if (joma == "READ") return(D[[g]]$pv)
  if (g %in% VECIE) {
    d <- utils::read.csv(sprintf("data/pisa_veci/lva_%s.csv", g), colClasses = "character")
    return(lapply(paste0("PV", 1:5, joma), function(k) as.numeric(d[[k]])))
  }
  x <- KJ[[g]]; r <- match(as.numeric(D[[g]]$sk$id), x$CNTSTUID)
  lapply(paste0("PV", 1:10, joma), function(k) x[[k]][r])
}
wq1 <- function(x, w, p) { o <- order(x); x <- x[o]; w <- w[o]
  stats::approx((cumsum(w) - 0.5 * w) / sum(w), x, xout = p, rule = 2, ties = "ordered")$y }

JOMAS <- c(READ = "lasīšana", MATH = "matemātika", SCIE = "dabaszinātnes")
GADI_J <- list(READ = names(D), MATH = names(D), SCIE = setdiff(names(D), "2003"))
lim <- list()
for (j in names(JOMAS)) for (g in GADI_J[[j]]) {
  a <- D[[g]]$sk$grupa %in% "A"
  pv <- lapply(pv_joma(g, j), `[`, a)
  if (any(vapply(pv, function(p) all(is.na(p)), TRUE))) { cat("  trūkst PV:", j, g, "\n"); next }
  w <- D[[g]]$sk$w[a]; rw <- lapply(D[[g]]$rw, `[`, a)
  ST <- list(vid = VID, P10 = function(x, w) wq1(x, w, .10), P90 = function(x, w) wq1(x, w, .90),
             zem_L2 = function(x, w) 100 * sum(w * (x < L2[[j]])) / sum(w))
  for (k in names(ST)) {
    r <- rubin_brr(pv, w, rw, ST[[k]])
    jut <- if (k == "zem_L2") abs(ST[[k]](pv[[1]] + 1, w) - ST[[k]](pv[[1]], w)) else 1
    lim[[length(lim) + 1]] <- data.frame(joma = j, gads = g, radītājs = k, vertiba = r[["est"]],
                                         se = r[["se"]], le_uz_punktu = jut)
  }
}
lim <- do.call(rbind, lim)

PARI <- list(c("2003", "2006"), c("2006", "2009"), c("2009", "2012"), c("2012", "2015"),
             c("2015", "2018"), c("2018", "2022"), c("2022", "2025"),
             c("2003", "2012"), c("2006", "2015"), c("2009", "2015"), c("2003", "2015"), c("2009", "2018"))
izm <- list()
for (j in names(JOMAS)) for (k in unique(lim$radītājs)) for (p in PARI) {
  a <- lim[lim$joma == j & lim$radītājs == k & lim$gads == p[1], ]
  b <- lim[lim$joma == j & lim$radītājs == k & lim$gads == p[2], ]
  if (!nrow(a) || !nrow(b)) next
  le <- le_of(j, p[1], p[2]); lek <- le[1] * mean(c(a$le_uz_punktu, b$le_uz_punktu))
  d <- b$vertiba - a$vertiba; s <- sqrt(a$se^2 + b$se^2 + lek^2)
  izm[[length(izm) + 1]] <- data.frame(joma = j, radītājs = k, no = p[1], lidz = p[2], izm = d, se = s,
                                       z = d / s, le = le[1], le_oficiala = le[2])
}
izm <- do.call(rbind, izm)
utils::write.csv(lim, "output/pisa2025/citas_jomas_limeni.csv", row.names = FALSE)
utils::write.csv(izm, "output/pisa2025/citas_jomas.csv", row.names = FALSE)

cat(strrep("=", 100), "\nLīmeņi, A plūsma\n", strrep("=", 100), "\n\n")
G <- names(D)
cat(sprintf("%-14s %-7s %s\n", "joma", "", paste(sprintf("%8s", G), collapse = "")))
for (j in names(JOMAS)) for (k in c("vid", "P10", "P90", "zem_L2")) {
  x <- lim[lim$joma == j & lim$radītājs == k, ]
  cat(sprintf("%-14s %-7s %s\n", JOMAS[[j]], k, paste(ifelse(G %in% x$gads,
      sprintf("%8.1f", x$vertiba[match(G, x$gads)]), sprintf("%8s", "")), collapse = "")))
}
cat("\n", strrep("=", 100), "\nPārmaiņas (zvaigznīte: |z| > 1,96; ¹ LE nav tabulā, pieņemts 3)\n", strrep("=", 100), "\n\n")
lab <- vapply(PARI, function(p) paste0(substr(p[1], 3, 4), "→", substr(p[2], 3, 4)), "")
cat(sprintf("%-14s %-7s %s\n", "joma", "", paste(sprintf("%10s", lab), collapse = "")))
for (j in names(JOMAS)) for (k in c("vid", "P10", "P90", "zem_L2")) {
  x <- izm[izm$joma == j & izm$radītājs == k, ]
  cel <- vapply(PARI, function(p) { r <- x[x$no == p[1] & x$lidz == p[2], ]
    if (!nrow(r)) return(sprintf("%10s", ""))
    sprintf("%10s", paste0(sprintf("%+.1f", r$izm), if (abs(r$z) > 1.96) "*" else " ", if (r$le_oficiala == 0) "¹" else " ")) }, "")
  cat(sprintf("%-14s %-7s %s\n", JOMAS[[j]], k, paste(cel, collapse = "")))
}
cat("\nIzveidotie faili:\n  output/pisa2025/citas_jomas.csv\n  output/pisa2025/citas_jomas_limeni.csv\n ",
    output_file, "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
