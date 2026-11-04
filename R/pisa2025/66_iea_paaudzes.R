# ============================================================================
# 66. Ārējie simptomi: IEA pētījumi tai pašai paaudzei agrākā vecumā (paaudžu ķēdes)
# ============================================================================
#
# JAUTĀJUMS: vai paaudzes relatīvā pozīcija agrākā vecumā (PIRLS, TIMSS 4. klase, ICCS
# un ICILS 8. klase) un tās pārmaiņa starp paaudzēm paredz, kā mainīsies PISA
# lasītprasme, kad šī paaudze būs 15 gadus veca?
#
# DATI: IEA starptautiskās datubāzes (data/iea): PIRLS 2001, 2006, 2016, 2021; TIMSS 4. kl.
#   2003, 2007, 2019, 2023; ICCS 2009, 2016, 2022; ICILS 2023. Valstis: LVA, EST, LTU, POL,
#   FIN, CZE, HUN, DEU (ICCS 2016/2022 Vācijai tikai Ziemeļreina-Vestfālene, DNW).
# NOVĒRTĒJUMS: vidējie ar 5 PV (Rubins) un IEA jackknife (JKZONE/JKREP, abas puses,
#   V = ½ Σ); daļas (grāmatas > 100, testa valoda mājās, lasītprieks) ar jackknife.
# LATVIJA: "LVA A" = tests latviski (ja mainīgais ir) un mājās vienmēr vai gandrīz vienmēr
#   runā testa valodā; tuvinājums PISA latviešu valodas plūsmai (mājas valoda).
# PAAUDZES: dzimšanas gads ≈ testa gads − 10,5 (4. kl.) vai − 14,3 (8. kl.);
#   PISA cikls = dzimšanas gads + 16 (PISA 2025 = dzimuši 2009).
#   PIRLS 2001 → PISA 2006; PIRLS 2006 → 2012; PIRLS 2016 → 2022; PIRLS 2021 → 2027
#   TIMSS 2003 → 2009; 2007 → 2012; 2019 → 2025; 2023 → 2029
#   ICCS 2009 → ~2012 (2011); 2016 → 2018; 2022 → ~2025 (2024); ICILS 2023 → 2025
# PĀRI (nepārklājas): PIRLS 01→06 ↔ PISA 06→12; PIRLS 06→16 ↔ 12→22; TIMSS 03→07 ↔ 09→12;
#   TIMSS 07→19 ↔ 12→25; ICCS 09→16 ↔ 12→18; ICCS 16→22 ↔ 18→25.
#   Katrā pārī tikai valstis, kas ir abos IEA ciklos un abos PISA ciklos; relatīvi = valsts
#   mīnus pārējo pāra valstu vidējais. Kopējā korelācija, permutāciju p (pāra ietvaros).
# NĀKOTNES SIGNĀLI: PIRLS 16→21 (PISA 22→27), TIMSS 19→23 (PISA 25→29).
#
# Izvade: data/iea/processed/iea_panelis.csv, output/pisa2025/iea_paaudzes_pari.csv
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(haven) })
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/66_iea_paaudzes_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8"); sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
set.seed(20260912)
dir.create("data/iea/processed", showWarnings = FALSE)

VAL <- c("LVA", "EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU", "DNW")
SPEC <- list(
  list(st = "PIRLS", g = 2001, dir = "pirls2001", suf = "R1", pv = c(las = "ASRREA0"), lang = "ITLANG", v = c(gram = "ASBGBOOK", vm = "ASBGLANH", prieks = "ASBGRST6")),
  list(st = "PIRLS", g = 2006, dir = "pirls2006", suf = "R2", pv = c(las = "ASRREA0"), lang = "ITLANG", v = c(gram = "ASBGBOOK", vm = "ASBGLNGH", prieks = "ASBGRST6")),
  list(st = "PIRLS", g = 2016, dir = "pirls2016", suf = "R4", pv = c(las = "ASRREA0"), lang = "ITLANG", v = c(gram = "ASBG04", vm = "ASBG03", prieks = "ASBR06E")),
  list(st = "PIRLS", g = 2021, dir = "pirls2021", suf = "R5", pv = c(las = "ASRREA0"), lang = "ITLANG_SA", v = c(gram = "ASBG04", vm = "ASBG03", prieks = "ASBR07E")),
  list(st = "TIMSS4", g = 2003, dir = "timss2003", suf = "M3", pv = c(mat = "ASMMAT0", dab = "ASSSCI0"), lang = "ITLANG", v = c(gram = "ASBGBOOK", vm = "ASBGOLAN")),
  list(st = "TIMSS4", g = 2007, dir = "timss2007", suf = "M4", pv = c(mat = "ASMMAT0", dab = "ASSSCI0"), lang = "ITLANG", v = c(gram = "AS4GBOOK", vm = "AS4GOLAN")),
  list(st = "TIMSS4", g = 2019, dir = "timss2019", suf = "M7", pv = c(mat = "ASMMAT0", dab = "ASSSCI0"), lang = "ITLANG", v = c(gram = "ASBG04", vm = "ASBG03")),
  list(st = "TIMSS4", g = 2023, dir = "timss2023", suf = "M8", pv = c(mat = "ASMMAT0", dab = "ASSSCI0"), lang = "ITLANG_SQ", v = c(gram = "ASBG04", vm = "ASBG03")),
  list(st = "ICCS", g = 2009, dir = "iccs2009", pref = "ISG", suf = "C2", pv = c(civ = "PV%dCIV"), w = "TOTWGTS", jz = "JKZONES", jr = "JKREPS", lang = "ITLANG", v = c(gram = "IS2G11", vm = "TESTLANG")),
  list(st = "ICCS", g = 2016, dir = "iccs2016", pref = "ISG", suf = "C3", pv = c(civ = "PV%dCIV"), w = "TOTWGTS", jz = "JKZONES", jr = "JKREPS", lang = "ITLANG", v = c(gram = "IS3G11", vm = "S_TLANG")),
  list(st = "ICCS", g = 2022, dir = "iccs2022", pref = "ISG", suf = "C4", pv = c(civ = "PV%dCIV"), w = "TOTWGTS", jz = "JKZONES", jr = "JKREPS", lang = "ITLANG", v = c(gram = "IS4G10", vm = "S_TLANG")),
  list(st = "ICILS", g = 2023, dir = "icils2023", pref = "BSG", suf = "I3", pv = c(cil = "PV%dCIL"), w = "TOTWGTS", jz = "JKZONES", jr = "JKREPS", lang = "ITLANG_U", v = c(gram = "IS3G14", vm = "S_TLANG")))
pv_nos <- function(x) if (grepl("%d", x)) sprintf(x, 1:5) else paste0(x, 1:5)
etik <- function(x) { l <- attr(x, "labels"); if (is.null(l)) setNames(numeric(), character()) else l }
kodi <- function(x, re, ne = "^$") { l <- etik(x); as.numeric(l[grepl(re, names(l), ignore.case = TRUE) & !grepl(ne, names(l), ignore.case = TRUE)]) }

# ---------------------------------------------------------------------------
# 1. Izvilkums (skolēnu līmenis)
# ---------------------------------------------------------------------------
lasit <- function(s, cnt) {
  pref <- if (is.null(s$pref)) "ASG" else s$pref
  f <- list.files(file.path("data/iea", s$dir), pattern = paste0("^", pref, cnt, s$suf, "\\.sav$"), recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
  if (!length(f)) return(NULL)
  nm <- names(read_sav(f[1], n_max = 0)); up <- toupper(nm)
  w <- if (is.null(s$w)) "TOTWGT" else s$w; jz <- if (is.null(s$jz)) "JKZONE" else s$jz; jr <- if (is.null(s$jr)) "JKREP" else s$jr
  pvs <- unlist(lapply(s$pv, pv_nos))
  gribu <- c("IDSCHOOL", w, jz, jr, s$lang, pvs, s$v)
  ok <- gribu[toupper(gribu) %in% up]
  X <- read_sav(f[1], col_select = all_of(nm[match(toupper(ok), up)])); names(X) <- toupper(names(X))
  num <- function(v) if (toupper(v) %in% names(X)) as.numeric(zap_labels(X[[toupper(v)]])) else rep(NA_real_, nrow(X))
  d <- data.table(st = s$st, g = s$g, cnt = cnt, skola = num("IDSCHOOL"), w = num(w), jz = num(jz), jr = num(jr),
                  test_val = if (toupper(s$lang) %in% names(X)) as.character(as_factor(X[[toupper(s$lang)]])) else NA_character_)
  for (dm in names(s$pv)) for (p in 1:5) d[[paste0("pv_", dm, p)]] <- num(pv_nos(s$pv[[dm]])[p])
  gv <- toupper(s$v[["gram"]]); vv <- toupper(s$v[["vm"]]); pr <- if ("prieks" %in% names(s$v)) toupper(s$v[["prieks"]]) else NA
  # (kv, nevis g: d jau ir kolonna g = gads)
  if (gv %in% names(X)) { x <- X[[gv]]; kv <- num(gv)
    set(d, j = "gram100", value = fifelse(is.na(kv), NA_real_, as.numeric(kv %in% kodi(x, "101|201|200|500"))))
    set(d, j = "gram200", value = fifelse(is.na(kv), NA_real_, as.numeric(kv %in% kodi(x, "more than 200|over 200|201|500")))) }
  if (vv %in% names(X)) { x <- X[[vv]]; kv <- num(vv)
    set(d, j = "vm_ja", value = fifelse(is.na(kv), NA_real_, as.numeric(kv %in% kodi(x, "always|test lang|language of test", "other|never|sometimes|not ")))) }
  if (!is.na(pr) && pr %in% names(X)) { x <- X[[pr]]; kv <- num(pr)
    set(d, j = "prieks_ja", value = fifelse(is.na(kv), NA_real_, as.numeric(kv %in% kodi(x, "agree a lot|strongly agree")))) }
  attr(d, "etiketes") <- list(gram = if (gv %in% names(X)) etik(X[[gv]]) else NULL, vm = if (vv %in% names(X)) etik(X[[vv]]) else NULL,
                              prieks = if (!is.na(pr) && pr %in% names(X)) etik(X[[pr]]) else NULL)
  d[!is.na(w) & w > 0]
}
kesh <- "data/iea/processed/iea_skoleni.rds"
if (file.exists(kesh)) S <- readRDS(kesh) else {
  S <- list()
  for (s in SPEC) for (cnt in VAL) {
    d <- lasit(s, cnt); if (is.null(d)) next
    if (cnt == "LVA") {
      e <- attr(d, "etiketes")
      cat(sprintf("\n%s %d LVA: n = %d, testa valodas: %s\n", s$st, s$g, nrow(d), paste(names(table(d$test_val)), table(d$test_val), collapse = "; ")))
      for (k in names(e)) if (length(e[[k]])) cat(sprintf("   %s: %s\n", k, paste(paste0(e[[k]], "=", names(e[[k]])), collapse = " | ")))
      cat(sprintf("   daļas: grāmatas>100 %.2f, >200 %.2f, testa valoda mājās %.2f, lasītprieks %s\n", mean(d$gram100, na.rm = TRUE),
                  mean(d$gram200, na.rm = TRUE), mean(d$vm_ja, na.rm = TRUE), if ("prieks_ja" %in% names(d)) round(mean(d$prieks_ja, na.rm = TRUE), 2) else "—"))
    }
    S[[paste(s$st, s$g, cnt)]] <- d
  }
  S <- rbindlist(S, fill = TRUE); saveRDS(S, kesh)
}
cat("\nSkolēni pa pētījumiem un valstīm:\n"); print(dcast(S[, .N, by = .(pc = paste(st, g), cnt)], pc ~ cnt, value.var = "N"), row.names = FALSE)

# ---------------------------------------------------------------------------
# 2. Novērtējumi ar jackknife un PV
# ---------------------------------------------------------------------------
jk <- function(x, w, jz, jr) {
  f <- function(ww) { i <- !is.na(x) & ww > 0; sum(ww[i] * x[i]) / sum(ww[i]) }
  th <- f(w); r <- c()
  for (h in unique(jz[!is.na(jz)])) {
    i <- which(jz == h); w1 <- w; w2 <- w
    w1[i] <- w[i] * 2 * (jr[i] == 1); w2[i] <- w[i] * 2 * (jr[i] == 0); r <- c(r, f(w1), f(w2))
  }
  c(th, 0.5 * sum((r - th)^2, na.rm = TRUE))
}
novert <- function(d) {
  out <- list()
  for (dm in c("las", "mat", "dab", "civ", "cil")) {
    pc <- paste0("pv_", dm, 1:5); if (!all(pc %in% names(d)) || all(is.na(d[[pc[1]]]))) next
    e <- sapply(pc, function(p) jk(d[[p]], d$w, d$jz, d$jr))
    out[[dm]] <- c(est = mean(e[1, ]), se = sqrt(mean(e[2, ]) + 1.2 * var(e[1, ])))
  }
  for (k in c("gram100", "gram200", "vm_ja", "prieks_ja")) {
    if (!k %in% names(d) || all(is.na(d[[k]]))) next
    e <- jk(d[[k]], d$w, d$jz, d$jr); out[[k]] <- c(est = 100 * e[1], se = 100 * sqrt(e[2]))
  }
  if (!length(out)) return(NULL)
  data.table(raditajs = names(out), est = sapply(out, `[`, "est"), se = sapply(out, `[`, "se"), n = nrow(d))
}
# Vācija: DNW (Ziemeļreina-Vestfālene) tikai tur, kur nav visas valsts faila
S <- S[!(cnt == "DNW" & paste(st, g) %in% S[cnt == "DEU", unique(paste(st, g))])]
S[, grupa := cnt][cnt == "DNW", grupa := "DEU"]
# Latvijā testa valoda: "Latvian" vai vecajos failos "<TEST LANGUAGE>" (pamata valoda; "<COUNTRY SPECIFIC>" = krievu)
S[cnt == "LVA", lat := is.na(test_val) | test_val == "" | grepl("latv|<test language>", test_val, ignore.case = TRUE)]
PAN <- rbind(S[, novert(.SD), by = .(st, g, grupa)],
             S[cnt == "LVA", novert(.SD[lat & vm_ja %in% 1]), by = .(st, g)][, grupa := "LVA A"],
             S[cnt == "LVA", novert(.SD[!(lat & vm_ja %in% 1)]), by = .(st, g)][, grupa := "LVA cita"], fill = TRUE)
PAN[, dz_gads := g - fifelse(st %in% c("PIRLS", "TIMSS4"), 10.5, 14.3)][, pisa_paaudze := round(dz_gads) + 16]
fwrite(PAN, "data/iea/processed/iea_panelis.csv")
cat("\nSasniegumi (PV vidējais) pa pētījumiem un grupām:\n")
print(dcast(PAN[raditajs %in% c("las", "mat", "civ", "cil")], st + g + raditajs ~ grupa, value.var = "est", fun.aggregate = function(x) round(mean(x), 0)), row.names = FALSE)
cat("\nGrāmatas > 100 (%):\n")
print(dcast(PAN[raditajs == "gram100"], st + g ~ grupa, value.var = "est", fun.aggregate = function(x) round(mean(x), 0)), row.names = FALSE)

# ---------------------------------------------------------------------------
# 3. PISA puse: valstu vidējie, latviešu valodas plūsma, grāmatas > 100
# ---------------------------------------------------------------------------
P <- fread("output/pisa2025/pisa_valstu_videjie.csv", colClasses = c(gads = "character"))[cnt %in% VAL, .(gads, grupa = cnt, y = lasisana)]
RC <- fread("output/pisa2025/simptomi_modelis_C_parbaude.csv", colClasses = c(merkis = "character", iepr = "character"))
LA <- unique(rbind(RC[grupa == "LVA A", .(gads = merkis, y = fakt)], RC[grupa == "LVA A", .(gads = iepr, y = fakt_iepr)]))
P <- rbind(P, LA[, .(gads, grupa = "LVA A", y)])
A <- readRDS("data/pisa2025/processed/simptomi_apvienots.rds")[, .(gads, cnt, plusma, w, L_GRAMATAS_MAJAS)]
PG <- rbind(A[!is.na(L_GRAMATAS_MAJAS), .(gram100 = 100 * sum(w * (L_GRAMATAS_MAJAS >= 4)) / sum(w)), by = .(gads, grupa = cnt)],
            A[cnt == "LVA" & plusma == "A" & !is.na(L_GRAMATAS_MAJAS), .(gram100 = 100 * sum(w * (L_GRAMATAS_MAJAS >= 4)) / sum(w)), by = .(gads)][, grupa := "LVA A"])
rm(A); gc(verbose = FALSE)

# ---------------------------------------------------------------------------
# 4. Paaudžu pāri
# ---------------------------------------------------------------------------
PARI <- list(
  list(st = "PIRLS", r = "las", g1 = 2001, g2 = 2006, p1 = "2006", p2 = "2012"),
  list(st = "PIRLS", r = "las", g1 = 2006, g2 = 2016, p1 = "2012", p2 = "2022"),
  list(st = "TIMSS4", r = "mat", g1 = 2003, g2 = 2007, p1 = "2009", p2 = "2012"),
  list(st = "TIMSS4", r = "mat", g1 = 2007, g2 = 2019, p1 = "2012", p2 = "2025"),
  list(st = "ICCS", r = "civ", g1 = 2009, g2 = 2016, p1 = "2012", p2 = "2018"),
  list(st = "ICCS", r = "civ", g1 = 2016, g2 = 2022, p1 = "2018", p2 = "2025"))
loo <- function(x) x - (sum(x) - x) / (length(x) - 1)
PR <- rbindlist(lapply(PARI, function(p) {
  a <- PAN[st == p$st & raditajs == p$r & g == p$g1 & grupa %in% VAL, .(grupa, x1 = est, s1 = se)]
  b <- PAN[st == p$st & raditajs == p$r & g == p$g2 & grupa %in% VAL, .(grupa, x2 = est, s2 = se)]
  q1 <- P[gads == p$p1 & grupa %in% VAL, .(grupa, y1 = y)]; q2 <- P[gads == p$p2 & grupa %in% VAL, .(grupa, y2 = y)]
  x <- Reduce(function(u, v) merge(u, v, by = "grupa"), list(a, b, q1, q2))
  if (nrow(x) < 3) return(NULL)
  x[, `:=`(iea_drel = loo(x2) - loo(x1), pisa_drel = loo(y2) - loo(y1))]
  k <- nrow(x); x[, iea_drel_se := sqrt(s1^2 + s2^2 + (sum(s1^2 + s2^2) - (s1^2 + s2^2)) / (k - 1)^2)]
  la <- PAN[st == p$st & raditajs == p$r & grupa == "LVA A"]; pa <- P[grupa == "LVA A"]
  if (nrow(la[g %in% c(p$g1, p$g2)]) == 2 && all(c(p$p1, p$p2) %in% pa$gads)) {
    o <- x[grupa != "LVA"]
    x <- rbind(x, data.table(grupa = "LVA A", x1 = la[g == p$g1, est], x2 = la[g == p$g2, est],
                             iea_drel = (la[g == p$g2, est] - mean(o$x2)) - (la[g == p$g1, est] - mean(o$x1)),
                             pisa_drel = (pa[gads == p$p2, y] - mean(o$y2)) - (pa[gads == p$p1, y] - mean(o$y1))), fill = TRUE)
  }
  x[, `:=`(paris = sprintf("%s %d→%d ↔ PISA %s→%s", p$st, p$g1, p$g2, p$p1, p$p2), valstis = k)]
}))
fwrite(PR, "output/pisa2025/iea_paaudzes_pari.csv")
cat("\n", strrep("=", 100), "\nPaaudžu pāri: IEA relatīvā pārmaiņa pret vēlāko PISA relatīvo pārmaiņu (tā pati paaudze)\n", strrep("=", 100), "\n", sep = "")
print(PR[, .(paris, grupa, iea_drel = round(iea_drel, 1), iea_se = round(iea_drel_se, 1), pisa_drel = round(pisa_drel, 1))], row.names = FALSE)
V <- PR[grupa != "LVA A"]
perm <- function(x, y, g, B = 10000) { r0 <- cor(x, y); rb <- replicate(B, { yy <- y; for (k in unique(g)) { i <- which(g == k); yy[i] <- y[i][sample(length(i))] }; cor(x, yy) }); c(r0, mean(abs(rb) >= abs(r0) - 1e-12)) }
pk <- perm(V$iea_drel, V$pisa_drel, V$paris)
cat(sprintf("\n  Visi pāri kopā (valstis, %d punkti): r = %.2f, permutāciju p = %.3f; virziens sakrīt %d/%d\n", nrow(V), pk[1], pk[2],
            V[, sum(sign(iea_drel) == sign(pisa_drel))], nrow(V)))
for (s in unique(V$paris)) { x <- V[paris == s]; cat(sprintf("    %s: r = %.2f (n = %d)\n", s, cor(x$iea_drel, x$pisa_drel), nrow(x))) }
cat("\n  Latvija (valsts un LVA A):\n")
print(PR[grupa %in% c("LVA", "LVA A"), .(paris, grupa, iea_drel = round(iea_drel, 1), iea_se = round(iea_drel_se, 1), pisa_drel = round(pisa_drel, 1),
                                        virziens = ifelse(sign(iea_drel) == sign(pisa_drel), "sakrīt", "pretējs"))], row.names = FALSE)

cat("\n", strrep("=", 100), "\nNākotnes paaudžu signāli (Latvija relatīvi pret valstīm abos ciklos)\n", strrep("=", 100), "\n", sep = "")
for (p in list(list(st = "PIRLS", r = "las", g1 = 2016, g2 = 2021, t = "PISA 2022 → 2027"), list(st = "TIMSS4", r = "mat", g1 = 2019, g2 = 2023, t = "PISA 2025 → 2029"),
               list(st = "TIMSS4", r = "dab", g1 = 2019, g2 = 2023, t = "PISA 2025 → 2029"))) {
  a <- PAN[st == p$st & raditajs == p$r & g == p$g1, .(grupa, x1 = est, s1 = se)]; b <- PAN[st == p$st & raditajs == p$r & g == p$g2, .(grupa, x2 = est, s2 = se)]
  x <- merge(a, b, by = "grupa"); o <- x[grupa %in% setdiff(VAL, "LVA")]
  for (gg in c("LVA", "LVA A")) if (gg %in% x$grupa) {
    v <- x[grupa == gg]; d <- (v$x2 - mean(o$x2)) - (v$x1 - mean(o$x1))
    cat(sprintf("  %s %s %d→%d (%s; salīdzinājumā %s): %s relatīvā pārmaiņa %+.1f (SE %.1f); absolūtā %+.1f, citām %+.1f\n", p$st, p$r, p$g1, p$g2, p$t,
                paste(o$grupa, collapse = ","), gg, d, sqrt(v$s1^2 + v$s2^2 + sum(o$s1^2 + o$s2^2) / nrow(o)^2), v$x2 - v$x1, mean(o$x2 - o$x1)))
  }
}

cat("\n", strrep("=", 100), "\nGrāmatas mājās > 100 (%) pa paaudzēm: IEA agrākā vecumā un PISA 15 gados\n", strrep("=", 100), "\n", sep = "")
GB <- rbind(PAN[raditajs == "gram100", .(avots = paste(st, g), pisa_paaudze = as.character(pisa_paaudze), grupa, est)],
            PG[, .(avots = paste("PISA", gads), pisa_paaudze = gads, grupa, est = gram100)])
print(dcast(GB[grupa %in% c("LVA", "LVA A", "EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")], pisa_paaudze + avots ~ grupa, value.var = "est",
            fun.aggregate = function(x) round(x[1], 0))[order(pisa_paaudze, avots)], row.names = FALSE)

cat("\n", strrep("=", 100), "\nICILS 2023 (8. kl., PISA 2025 paaudze): datorpratība relatīvi un PISA lasītprasme relatīvi\n", strrep("=", 100), "\n", sep = "")
ic <- PAN[st == "ICILS" & raditajs == "cil" & grupa %in% VAL, .(grupa, cil = est)]
x <- merge(merge(ic, P[gads == "2025", .(grupa, p25 = y)], by = "grupa"), P[gads == "2022", .(grupa, p22 = y)], by = "grupa")
x[, `:=`(cil_rel = loo(cil), p25_rel = loo(p25), p22_rel = loo(p22))]
print(x[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)], row.names = FALSE)
cat(sprintf("  Korelācija (n = %d): ICILS rel ↔ PISA 2025 rel %.2f; ICILS rel ↔ PISA 2022 rel %.2f\n", nrow(x), cor(x$cil_rel, x$p25_rel), cor(x$cil_rel, x$p22_rel)))
la <- PAN[st == "ICILS" & grupa == "LVA A" & raditajs == "cil", est]
if (length(la)) cat(sprintf("  LVA A ICILS: %.0f (valsts %.0f)\n", la, ic[grupa == "LVA", cil]))
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink(); close(log_con)
