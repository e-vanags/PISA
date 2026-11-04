# ============================================================================
# 70. Vizualizācijas V1–V5 (docs/63 4. sadaļa)
# ============================================================================
#   V1 paaudžu (Lexis) diagramma: x dzimšanas gads, y vecums; mērījumu relatīvā vieta;
#      latviešu valodas plūsmas latentā trajektorija (KFAS, kā 67b); ziņojumu datumi kā
#      kalendāra diagonāles
#   V2 efekta lielumu mežs pa logiem ar Kraft joslām un Bloom gada pieaugumu
#   V3 brīdinājuma panelis: signāli kalendāra laikā ar ziņojumu formulējumiem
#   V4 sadalījuma grēdas: A plūsmas lasīšanas blīvumi 2003–2025 ar līmeņu robežām
#   V5 interaktīvs HTML: trajektorija pa variantiem, mērījumi, ex ante prognozes
# Izvade: output/pisa2025/vizualizacijas/V1..V4 (.png, .pdf; SVG nav: cairo nav pieejams), V5_trajektorija.html,
#         ilgtermina_signali.html (galerija), dati JSON
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(patchwork); library(KFAS); library(jsonlite) })
VZ <- "output/pisa2025/vizualizacijas"; dir.create(VZ, showWarnings = FALSE)
SALID <- c("CZE", "DEU", "EST", "FIN", "HUN", "LTU", "POL")
zim <- function(p, nos, w = 11, h = 7) { ggsave(file.path(VZ, paste0(nos, ".png")), p, width = w, height = h, dpi = 200, bg = "white")
  ggsave(file.path(VZ, paste0(nos, ".pdf")), p, width = w, height = h, device = grDevices::pdf, bg = "white"); cat("  ", nos, "\n") }
TEMA <- theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), plot.title.position = "plot", plot.title = element_text(face = "bold"),
                                              plot.caption = element_text(colour = "grey40", hjust = 0), legend.position = "bottom")
ZIN <- data.table(datums = c(2007.9, 2017.9, 2019.95, 2023.35, 2023.92, 2025.3), etikete = c("2007-11: PIRLS 2006\n\"nopietns brīdinājums\"", "2017-11: \"ievērojami\nuzlabojušies\"",
  "2019-12: PISA 2018\n\"kopumā labi, stabils\"", "2023-05: PIRLS 2021\n\"drauds sistēmai\"", "2023-12: PISA 2022\n\"labu un stabilu\"", "2025-03: PISA 2025\ntests"), tips = c("brīdina", "nomierina", "nomierina", "brīdina", "nomierina", "mērījums"))

# ---------------------------------------------------------------------------
# Dati: paaudžu panelis, relatīvā vieta, latentā trajektorija
# ---------------------------------------------------------------------------
P <- fread("data/pisa2025/processed/paaudzu_panelis.csv")[radītājs == "vid"]
P[, b := floor(dz_gads + 0.5)][, serija := fifelse(petijums == "PISA lasīšana", "PISA", petijums)]
REL <- P[, { o <- .SD[grupa %in% SALID]; l <- .SD[grupa == "LVA A"]; if (nrow(l) && nrow(o) >= 3) .(rel = l$est - mean(o$est), se = l$se, valstis = nrow(o), vecums = vecums[1], dz_gads = dz_gads[1], b = b[1]) else NULL }, by = .(petijums, gads, serija)]
# Skolēnu SD pa pētījumiem tuvinājumam (PISA A ~88, PIRLS ~68, TIMSS ~72, ICCS/ICILS ~80)
SDS <- c("PISA" = 88, "PIRLS lasīšana" = 68, "TIMSS4 matemātika" = 72, "TIMSS4 dabaszinātnes" = 72, "ICCS pilsoniskās zināšanas" = 80, "ICILS datorpratība" = 80)
REL[, rel_sd := rel / SDS[serija]]
# PIAAC kā vēlākā stadija (LVA A pret 7 valstīm, jau SD)
PA <- fread("output/pisa2025/piaac_paaudzes_pisa.csv")[joma == "LIT" & kopa == "7 valstis" & lv_grupa == "A"]
PA[, `:=`(vec = c("16-19" = 17.5, "20-24" = 22, "25-29" = 27, "30-34" = 32, "35-39" = 37)[grupa])][, dz_gads := 2023 - vec]
# Latentā trajektorija (67b modelis, "PISA + PIRLS" un "tikai PISA", kopa B)
D <- fread("data/pisa2025/processed/paaudzu_panelis.csv")[radītājs == "vid" & is.finite(est) & is.finite(se)]
D[, b := floor(dz_gads + 0.5)][, serija := fifelse(petijums == "PISA lasīšana", "PISA", petijums)]
VIEN <- c(SALID, "LVA A"); YRS <- 1980:2015
fe_resid <- function(X) { X <- copy(X)[, `:=`(sc = paste(serija, gads), ks = paste(grupa, serija))]; f <- lm(est ~ 0 + factor(sc) + factor(ks), data = X, weights = 1 / (se^2 + 1)); X[, r := est - fitted(f)] }
buvet <- function(Xk, ser, q, s2) { p <- length(ser); Y <- matrix(NA_real_, length(YRS), p); H <- array(0, c(p, p, length(YRS))); for (j in 1:p) for (t in seq_along(YRS)) H[j, j, t] <- 1
  a <- Xk[, .(r = mean(r), se2 = mean(se^2)), by = .(b, serija)]; for (i in seq_len(nrow(a))) { t <- match(a$b[i], YRS); j <- match(a$serija[i], ser); if (!is.na(t) && !is.na(j)) { Y[t, j] <- a$r[i]; H[j, j, t] <- a$se2[i] + s2[j] } }
  SSModel(Y ~ -1 + SSMcustom(Z = matrix(1, p, 1), T = matrix(1), R = matrix(1), Q = matrix(q), a1 = matrix(0), P1inf = matrix(1)), H = H) }
traj <- function(ser_kopa, nos) {
  X <- fe_resid(D[grupa %in% VIEN & serija %in% ser_kopa]); ser <- intersect(unique(X$serija), ser_kopa)
  nll <- function(par) { q <- exp(par[1]); s2 <- exp(par[-1]); -sum(sapply(VIEN, function(k) { ll <- tryCatch(logLik(buvet(X[grupa == k], ser, q, s2)), error = function(e) -1e10); if (is.finite(ll)) ll else -1e10 })) }
  o <- optim(c(log(4), rep(log(9), length(ser))), nll, method = "Nelder-Mead", control = list(maxit = 2000))
  q <- exp(o$par[1]); s2 <- setNames(exp(o$par[-1]), ser)
  # Trajektorija tikai vienības mērījumu dzimšanas gadu diapazonā (ārpus tā izlīdzinātājs ekstrapolē bez datiem)
  rbindlist(lapply(VIEN, function(k) { ks <- KFS(buvet(X[grupa == k], ser, q, s2), smoothing = "state"); rb <- range(X[grupa == k, b])
    data.table(variants = nos, grupa = k, b = YRS, theta = ifelse(YRS >= rb[1] & YRS <= rb[2], as.numeric(ks$alphahat[, 1]), NA_real_), se = as.numeric(sqrt(ks$V[1, 1, ]))) }))
}
cat("Latentās trajektorijas...\n")
TR <- rbind(traj("PISA", "tikai PISA"), traj(c("PISA", "PIRLS lasīšana"), "PISA + PIRLS"), traj(c("PISA", "PIRLS lasīšana", "TIMSS4 matemātika", "TIMSS4 dabaszinātnes", "ICCS pilsoniskās zināšanas", "ICILS datorpratība"), "visi pētījumi"))
TR[, `:=`(rel = theta - mean(theta[grupa %in% SALID], na.rm = TRUE)), by = .(variants, b)]   # relatīvi pret 7 valstīm (ar datiem)
# Fiksētie efekti izņem katras valsts pastāvīgo relatīvo līmeni; pievieno to atpakaļ (vidējā PISA relatīvā vieta pret 7 valstīm),
# lai trajektorija un mērījumu punkti būtu vienā skalā
OFF <- P[serija == "PISA", { o <- .SD[grupa %in% SALID]; .SD[, .(rel = est - mean(o$est)), by = grupa] }, by = gads][, .(off = mean(rel)), by = grupa]
TR[, rel := rel + OFF$off[match(grupa, OFF$grupa)]]
TRL <- TR[grupa == "LVA A" & !is.na(rel)]
fwrite(REL, file.path(VZ, "V1_relativa_vieta.csv")); fwrite(TR, file.path(VZ, "V1_trajektorija.csv"))

# ---------------------------------------------------------------------------
# V1. Paaudžu (Lexis) diagramma
# ---------------------------------------------------------------------------
cat("V1\n")
pts <- rbind(REL[, .(petijums, dz_gads, vecums, rel_sd, rel, tips = "mērījums")], PA[, .(petijums = "PIAAC lasītprasme", dz_gads, vecums = vec, rel_sd = piaac_sd, rel = piaac_starp, tips = "mērījums")])
pts[, forma := fcase(grepl("PISA", petijums), "PISA 15 g.", grepl("PIRLS", petijums), "PIRLS 4. kl.", grepl("TIMSS", petijums), "TIMSS 4. kl.", grepl("ICCS|ICILS", petijums), "ICCS / ICILS 8. kl.", default = "PIAAC 16–39 g.")]
pts[, paaudze := round(dz_gads)]
diag <- ZIN[, .(x = c(datums - 39, datums - 9), y = c(39, 9)), by = .(datums, etikete, tips)]
ETIK <- data.table(datums = c(2007.9, 2017.9, 2019.95, 2023.6, 2025.3), y = c(24, 24, 20, 30, 20), etikete = c("2007-11: PIRLS 2006\n\"nopietns brīdinājums\"", "2017-11:\n\"ievērojami uzlabojušies\"", "2019-12: PISA 2018\n\"kopumā labi, stabils\"", "2023-05: PIRLS 2021 \"drauds sistēmai\"\n2023-12: PISA 2022 \"labu un stabilu\"", "2025-03:\nPISA 2025 tests"), tips = c("brīdina", "nomierina", "nomierina", "brīdina", "mērījums"))
KAL <- data.table(gads = seq(2000, 2025, 5))[, .(x = c(gads - 39, gads - 9), y = c(39, 9)), by = gads]
KALL <- data.table(gads = seq(2000, 2025, 5))[, `:=`(y = pmax(9.6, gads - 2014), x = pmin(gads - 9.6, 2014))]
v1 <- ggplot() +
  geom_line(data = KAL, aes(x, y, group = gads), colour = "grey80", linewidth = 0.3) +
  geom_text(data = KALL, aes(x, y, label = paste("mērīts", gads)), colour = "grey55", size = 2.5, hjust = 1.05, vjust = -0.3, angle = -37) +
  geom_line(data = diag, aes(x, y, group = datums, linetype = tips, colour = tips), linewidth = 0.55) +
  scale_colour_manual(values = c(brīdina = "#b3261e", nomierina = "#9a5b00", mērījums = "grey30"), name = "Ziņojums") +
  geom_tile(data = TRL[variants == "PISA + PIRLS"], aes(x = b, y = 15.8, fill = rel / 88), height = 2.4, alpha = 0.9) +
  geom_line(data = pts[vecums <= 16][order(dz_gads)][, .SD[.N >= 2], by = paaudze], aes(dz_gads, vecums, group = paaudze), colour = "grey35", linewidth = 0.35, alpha = 0.7) +
  geom_point(data = pts, aes(dz_gads, vecums, fill = rel_sd, shape = forma), size = 5, colour = "black", stroke = 0.4) +
  scale_shape_manual(values = c("PISA 15 g." = 21, "PIRLS 4. kl." = 24, "TIMSS 4. kl." = 25, "ICCS / ICILS 8. kl." = 22, "PIAAC 16–39 g." = 23), name = NULL) +
  scale_fill_gradient2(low = "#b3261e", mid = "white", high = "#2a78d6", midpoint = 0, name = "Latviešu valodas plūsma pret\n7 valstu vidējo, skolēnu SD", limits = c(-0.5, 0.5), oob = scales::squish) +
  scale_linetype_manual(values = c(brīdina = "solid", nomierina = "dotted", mērījums = "dashed"), name = "Ziņojums") +
  geom_label(data = ETIK[, .(x = datums - y, y, etikete, tips)], aes(x, y, label = etikete, colour = tips), hjust = 0, vjust = 0.5, nudge_x = 0.3, size = 2.4, lineheight = 0.85, label.size = 0, fill = "white", alpha = 0.9, show.legend = FALSE) +
  scale_x_continuous("Dzimšanas gads (paaudze)", breaks = seq(1984, 2014, 2), sec.axis = sec_axis(~ . + 16, name = "PISA mērījuma gads (dzimšanas gads + 16); citiem pētījumiem mērījuma gads ir pelēkās diagonāles", breaks = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025))) +
  scale_y_continuous("Vecums mērījumā", breaks = c(10.5, 14.3, 15.8, 17.5, 22, 27, 32, 37), labels = c("4. kl. (10,5)", "8. kl. (14)", "PISA (15,8)", "17,5", "22", "27", "32", "37")) + coord_cartesian(xlim = c(1984, 2014.5), ylim = c(9, 39)) +
  labs(title = "Paaudžu diagramma: latviešu valodas plūsmas relatīvā vieta katrā mērījumā",
       subtitle = "Punkts = paaudze × vecums; krāsa = novirze no 7 salīdzinājuma valstu vidējā (skolēnu SD); horizontālā josla 15,8 g. = latentā trajektorija (KFAS, PISA + PIRLS);\ndiagonāles = ziņojumu datumi (kalendāra laiks = dzimšanas gads + vecums)",
       caption = "Avoti: PISA 2003–2025, PIRLS 2001–2021, TIMSS 4. kl. 2003–2023, ICCS 2009–2022, ICILS 2023, PIAAC 2023 (5 gadu vecuma joslas, paaudze aptuvena); docs/61, 63. Skolēnu SD tuvinājumi: PISA 88, PIRLS 68, TIMSS 72, ICCS/ICILS 80.") + TEMA + theme(legend.position = "right", legend.box = "vertical")
zim(v1, "V1_paaudzu_diagramma", 14, 8.5)

# ---------------------------------------------------------------------------
# V2. Efekta lielumu mežs
# ---------------------------------------------------------------------------
cat("V2\n")
E <- fread("output/pisa2025/efekta_lielumi.csv", encoding = "UTF-8")
E2 <- rbind(E[bloks == "A. PISA A plūsma" & raditajs %in% c("vid", "P95", "P10", "L5_un_virs", "zem_L2")][, logs := paste(no, "→", lidz)][, paz := fcase(raditajs == "vid", "Vidējais (d)", raditajs == "P95", "P95 augšgals (d)", raditajs == "P10", "P10 apakšgals (d)", raditajs == "L5_un_virs", "5.+ līmenis (h)", raditajs == "zem_L2", "Zem 2. līmeņa (h)")],
            E[bloks == "B. Grāmatas" & grepl("200", raditajs)][, logs := paste(no, "→", lidz)][, paz := "Grāmatas > 200 (h)"],
            E[bloks == "B. Paradumi" & grepl("nelasa priekam \\[Latvija\\]", raditajs)][, logs := paste(no, "→", lidz)][, paz := "Nelasa priekam, visa LV (h)"],
            E[bloks == "C. PIRLS A plūsma" & raditajs %in% c("vid (4. kl.)", "P10 (4. kl.)")][, logs := "PIRLS 2016 → 2021"][, paz := fifelse(raditajs == "vid (4. kl.)", "PIRLS 4. kl. vidējais (d)", "PIRLS 4. kl. P10 (d)")],
            E[bloks == "D. PIAAC A plūsma pret 7 valstīm" & grepl("16-19|25-29", raditajs)][, logs := "PIAAC 2023 līmenis"][, paz := paste("PIAAC", sub(".*vecums ", "", raditajs), "(d)")], fill = TRUE)
E2[, logs := factor(logs, levels = c("2003 → 2015", "2003 → 2018", "2003 → 2022", "2003 → 2025", "2018 → 2022", "2022 → 2025", "2000 → 2009", "2000 → 2018", "PIRLS 2016 → 2021", "PIAAC 2023 līmenis"))]
E2 <- E2[logs %in% c("2003 → 2015", "2003 → 2018", "2003 → 2025", "2018 → 2022", "2022 → 2025", "2000 → 2018", "PIRLS 2016 → 2021", "PIAAC 2023 līmenis")]
E2[, paz := factor(paz, levels = rev(c("Vidējais (d)", "P95 augšgals (d)", "P10 apakšgals (d)", "5.+ līmenis (h)", "Zem 2. līmeņa (h)", "Grāmatas > 200 (h)", "Nelasa priekam, visa LV (h)", "PIRLS 4. kl. vidējais (d)", "PIRLS 4. kl. P10 (d)", "PIAAC 16-19 (d)", "PIAAC 25-29 (d)")))]
E2[paz %in% c("Zem 2. līmeņa (h)", "Nelasa priekam, visa LV (h)"), `:=`(es = -es, es_ti_zem = -es_ti_augs, es_ti_augs = -es_ti_zem)]
jos <- data.table(xmin = c(-0.05, -0.2, -3), xmax = c(0.05, -0.05, -0.2), josla = factor(c("Kraft: mazs (<0,05)", "Kraft: vidējs (0,05–0,20)", "Kraft: liels (≥0,20)"), levels = c("Kraft: mazs (<0,05)", "Kraft: vidējs (0,05–0,20)", "Kraft: liels (≥0,20)")))
v2 <- ggplot(E2) +
  geom_rect(data = jos, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = josla), alpha = 0.18) +
  geom_rect(data = jos[, .(xmin = -xmax, xmax = -xmin, josla)], aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = josla), alpha = 0.18) +
  geom_vline(xintercept = 0, colour = "grey30") + geom_vline(xintercept = c(-0.24, 0.24), linetype = "dashed", colour = "#0f8a6e") +
  geom_errorbar(aes(y = paz, xmin = es_ti_zem, xmax = es_ti_augs), width = 0.25, colour = "grey40", orientation = "y") +
  geom_point(aes(x = es, y = paz, colour = veids), size = 2.8) +
  facet_wrap(~ logs, nrow = 2, scales = "free_y") +
  scale_fill_manual(values = c("#f2f2f2", "#fbe3c9", "#f6b9ac"), name = NULL) + scale_colour_manual(values = c("d (SD)" = "#0b0b0b", "h (Koena)" = "#7a57c7"), name = "Mērs") +
  scale_x_continuous("Efekta lielums (d = SD; h = Koena h daļām); 95 % intervāls ar saites kļūdu", limits = c(-1.0, 0.5), breaks = seq(-1, 0.5, 0.25)) + ylab(NULL) +
  labs(title = "Ilgtermiņa pazīmes kā efekta lielumi: latviešu valodas plūsma", subtitle = "Krāsu joslas = Kraft (2020) etaloni izglītības intervenciju efektiem; zaļā punktētā līnija (±0,24) = viena mācību gada pieaugums lasīšanā 8.–9. klasē (Bloom et al. 2008)",
       caption = "Avots: R/pisa2025/68_efekta_lielumi.R; docs/63. Negatīvs = pasliktinājums (zem 2. līmeņa un nelasīšana zīme apgriezta, lai negatīvs būtu sliktāk).") + TEMA
zim(v2, "V2_efekta_lielumi", 13, 8)

# ---------------------------------------------------------------------------
# V3. Brīdinājuma panelis
# ---------------------------------------------------------------------------
cat("V3\n")
L <- fread("output/pisa2025/agrie_signali_limeni.csv", colClasses = c(gads = "character"), encoding = "UTF-8")[grupa == "A visi"]
SD <- fread("output/pisa2025/a_plusmas_sd.csv")
h_fun <- function(p1, p2) 2 * asin(sqrt(p2)) - 2 * asin(sqrt(p1))
sig <- function(k, nos, veids) { x <- L[radītājs == k][order(gads)]; b <- x[1]
  if (veids == "d") x[, .(signals = nos, gads = as.numeric(gads), es = (vertiba - b$vertiba) / 88, se = sqrt(se^2 + b$se^2 + 5^2 * (gads != b$gads)) / 88)]
  else x[, .(signals = nos, gads = as.numeric(gads), es = h_fun(b$vertiba / 100, vertiba / 100), se = sqrt((se / 100)^2 / (vertiba / 100 * (1 - vertiba / 100)) + (b$se / 100)^2 / (b$vertiba / 100 * (1 - b$vertiba / 100))))] }
PR <- fread("output/pisa2025/paradumi.csv", colClasses = c(gads = "character"), encoding = "UTF-8")
gr <- PR[radītājs == "grāmatas: vairāk nekā 200 (2000: >250)" & grupa == "A"][order(gads)]; b <- gr[1]
nl <- PR[radītājs == "nelasa priekam" & grupa == "Latvija"][order(gads)]; b2 <- nl[1]
PI <- fread("output/pisa2025/latviesu_plusma_pirls.csv", colClasses = c(gads = "character")); b3 <- PI[1]
S3 <- rbind(sig("vid", "PISA vidējais (d pret 2003)", "d"), sig("P95", "PISA P95 augšgals (d)", "d"), sig("L5_un_virs", "PISA 5.+ līmenis (h)", "h"), sig("zem_L2", "PISA zem 2. līmeņa (h, apgriezts)", "h")[, es := -es],
            gr[, .(signals = "Grāmatas > 200 mājās (h pret 2003)", gads = as.numeric(gads), es = h_fun(b$pct / 100, pct / 100), se = sqrt((se / 100)^2 / (pct / 100 * (1 - pct / 100)) + (b$se / 100)^2 / (b$pct / 100 * (1 - b$pct / 100))))],
            nl[, .(signals = "Nelasa priekam, visa Latvija (h, apgriezts)", gads = as.numeric(gads), es = -h_fun(b2$pct / 100, pct / 100), se = sqrt((se / 100)^2 / (pct / 100 * (1 - pct / 100)) + (b2$pct / 100 * 0 + (b2$se / 100)^2 / (b2$pct / 100 * (1 - b2$pct / 100)))))],
            PI[, .(signals = "PIRLS 4. kl. vidējais (d pret 2001)", gads = as.numeric(gads), es = (pamat - b3$pamat) / 68, se = sqrt(pamat_se^2 + b3$pamat_se^2) / 68)],
            PA[, .(signals = "PIAAC 2023: paaudze pret 7 valstīm (d)", gads = pisa_gads, es = piaac_sd, se = piaac_se * abs(piaac_sd / piaac_starp))])
# Vienreizējs 2018. gada signāls (docs/58 papildinājums 2026-09-21): skolēnu daļa, kam dzimtās valodas
# stundās gadā bija jālasa teksts virs 100 lpp: LVA A 21,5 %, 7 valstis 47,4 %; Koena h pret 7 valstīm,
# SE ar dizaina efektu 3 (tuvinājums). Jautājums tikai 2018. gadā (ST154Q01HA), tāpēc viens punkts.
S3 <- rbind(S3, data.table(signals = "Skolā lasa tekstus virs 100 lpp, LVA A pret 7 valstīm (h, tikai 2018)", gads = 2018,
                           es = 2 * asin(sqrt(0.215)) - 2 * asin(sqrt(0.474)), se = sqrt(3 / 3304 + 3 / 39461)))
S3[, signals := factor(signals, levels = unique(signals))]
v3 <- ggplot(S3, aes(gads, es)) + geom_hline(yintercept = 0, colour = "grey40") + annotate("rect", xmin = -Inf, xmax = Inf, ymin = -0.2, ymax = 0.2, fill = "grey92", alpha = 0.6) +
  geom_vline(data = ZIN, aes(xintercept = datums, linetype = tips, colour = tips), linewidth = 0.5) +
  geom_ribbon(aes(ymin = es - 1.96 * se, ymax = es + 1.96 * se), fill = "#2a78d6", alpha = 0.18) + geom_line(colour = "#2a78d6") + geom_point(colour = "#2a78d6", size = 1.8) +
  facet_wrap(~ signals, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = c(brīdina = "#b3261e", nomierina = "#9a5b00", mērījums = "grey30"), name = "Ziņojums") + scale_linetype_manual(values = c(brīdina = "solid", nomierina = "dotted", mērījums = "dashed"), name = "Ziņojums") +
  scale_x_continuous(NULL, breaks = seq(2000, 2026, 3), limits = c(2000, 2026)) + ylab("Efekta lielums pret sākumu (SD vai h); negatīvs = sliktāk") +
  labs(title = "Brīdinājuma panelis: signāli krājas, ziņojumi paliek nemainīgi", subtitle = "Pelēkā josla = |0,20| (Kraft \"liels\"); sarkanas līnijas = brīdinoši ziņojumi, oranžas punktētas = nomierinoši; joslas = 95 % intervāls",
       caption = "Avoti: docs/58, 63; ziņojumu citāti docs/41, 45.") + TEMA
zim(v3, "V3_bridinajuma_panelis", 12, 9)

# ---------------------------------------------------------------------------
# V4. Sadalījuma grēdas
# ---------------------------------------------------------------------------
cat("V4\n")
# V4 uzlabota 2026-09-18 (lietotāja lūgums pēc metodisko atrunu apspriešanas):
#   (1) blīvums no VISĀM ticamajām vērtībām (katra ar svaru w / PV skaits), ne no to vidējā,
#       lai sadalījums nebūtu mākslīgi sašaurināts;
#   (2) divi paneļi: oficiālā skala un koriģētā skala (2018. gada instrumenta nobīde +9,6 no
#       2018. gada, docs/55, docs/67) — lai lasītājs redz, ka nobīde pa kreisi paliek;
#   (3) daļas zem 2. līmeņa un 5.+ līmenī rēķinātas no tām pašām PV abās skalās;
#   (4) parakstā nosaukta saites kļūda un 2025. gada aptvērums.
A0 <- readRDS("data/pisa2025/processed/simptomi_apvienots.rds")[cnt == "LVA" & plusma == "A" & gads != "2000"]
pvk <- grep("^PV[0-9]+READ$", names(A0), value = TRUE)
A4 <- melt(A0[, c("gads", "w", pvk), with = FALSE], id.vars = c("gads", "w"), variable.name = "pv", value.name = "x")[!is.na(x)]
A4[, w2 := w / .N, by = .(gads, w, x)]           # pagaidu; pārrēķins zemāk pa skolēnu
A4[, npv := uniqueN(pv), by = gads][, w2 := w / npv]
MSAT <- fread("output/pisa2025/korigetie_raditaji_slani.csv")[korekcija == "2018. gada instrumenta nobīde" & raditajs == "vid", .(gads = as.character(gads), nob = nobide)]
nob18 <- MSAT[gads == "2018", nob]
A4 <- rbind(A4[, .(skala = "oficiālā PISA skala", gads, w2, x)],
            A4[, .(skala = "koriģētā skala (2018. gada instrumenta nobīde +9,6 no 2018. gada)", gads, w2, x = x + fifelse(as.integer(gads) >= 2018, nob18, 0))])
A4[, skala := factor(skala, levels = unique(skala))]
DEN <- A4[, { d <- density(x, weights = w2 / sum(w2), from = 150, to = 800, n = 400); .(x = d$x, y = d$y) }, by = .(skala, gads)]
DEN[, gads := factor(gads, levels = rev(sort(unique(gads))))][, off := as.numeric(gads) - 1][, y2 := y / max(DEN$y) * 1.6]
LIM <- data.table(x = c(407.47, 552.89, 625.61), lab = c("2. līmeņa robeža (407)", "4. līmeņa robeža (553)", "5. līmeņa robeža (626)"))
DAL <- A4[, .(zem_L2 = 100 * sum(w2 * (x < 407.47)) / sum(w2), L5 = 100 * sum(w2 * (x >= 625.61)) / sum(w2), vid = sum(w2 * x) / sum(w2)), by = .(skala, gads)]
DAL[, gads := factor(gads, levels = levels(DEN$gads))][, off := as.numeric(gads) - 1]
DAL[, lab := gsub("\\.", ",", sprintf("zem 2. līm. %.0f %% · 5.+ līm. %.1f %%", zem_L2, L5))]
OE <- fread("output/pisa2025/oecd_videjais_lasisana.csv", colClasses = c(gads = "character"))[gads %in% levels(DEN$gads)]
OE[, gads := factor(gads, levels = levels(DEN$gads))][, off := as.numeric(gads) - 1]
OE <- rbind(OE[, .(skala = levels(A4$skala)[1], gads, off, oecd_23)], OE[, .(skala = levels(A4$skala)[2], gads, off, oecd_23)])[, skala := factor(skala, levels = levels(A4$skala))]
v4 <- ggplot() + geom_vline(data = LIM, aes(xintercept = x), linetype = "dashed", colour = "grey45") +
  geom_ribbon(data = DEN, aes(x = x, ymin = off, ymax = off + y2, group = gads, fill = as.numeric(as.character(gads))), colour = "grey20", linewidth = 0.3, alpha = 0.85) +
  geom_segment(data = OE, aes(x = oecd_23, xend = oecd_23, y = off, yend = off + 1.05), colour = "#e0641f", linewidth = 1.1, linetype = "22") +
  geom_segment(data = DAL, aes(x = vid, xend = vid, y = off, yend = off + 0.9), colour = "black", linewidth = 0.8) +
  geom_text(data = OE[gads == "2003"], aes(x = oecd_23, y = off + 1.15, label = "OECD vidējais (23 valstis)"), colour = "#e0641f", size = 2.7, hjust = 0.5, vjust = 0) +
  geom_text(data = LIM, aes(x = x, y = 8.75, label = lab), size = 2.5, colour = "grey30", angle = 90, hjust = 0, nudge_x = 6) +
  geom_text(data = DAL, aes(x = 800, y = off + 0.55, label = lab), size = 2.6, colour = "grey25", hjust = 1) +
  facet_wrap(~skala, nrow = 1) +
  scale_y_continuous(NULL, breaks = seq_along(levels(DEN$gads)) - 1, labels = levels(DEN$gads), limits = c(0, 11.2)) + scale_x_continuous("Lasītprasme (PISA punkti)", breaks = seq(200, 800, 100)) +
  scale_fill_gradient(low = "#dbe7f6", high = "#2a78d6", guide = "none") +
  labs(title = "Latviešu valodas plūsmas lasītprasmes sadalījums pa PISA cikliem 2003–2025",
       subtitle = "Svērtie blīvumi no visām ticamajām vērtībām (skolēna svars dalīts ar PV skaitu); melnā svītra = latviešu valodas plūsmas svērtais vidējais;\noranžā svītra = OECD vidējais tajā ciklā (23 valstis ar datiem visos ciklos; 2003: 497, 2025: 466); vertikālās līnijas = PISA prasmju līmeņu robežas.\n2003 → 2006 kritums, 2006–2018 vidējais stabils (483–489) un abu galu daļas svārstās bez skaidra trenda, no 2018. gada viss sadalījums slīd pa kreisi: 2025. gadā zem 2. līmeņa 28 %, 5.+ līmenis 2,7 %.\nLabajā panelī 2018.–2025. gada sadalījumi pārbīdīti par +9,6 punktiem (2018. gada adaptīvā testa nobīde, docs/55) — nobīde pa kreisi paliek arī pēc korekcijas.",
       caption = "Avots: OECD PISA mikrodati (LANGN = latviešu; svars W_FSTUWT; 2003–2012 pieci, no 2015 desmit PV). Cikli salīdzināmi OECD saites ietvaros (saites kļūda 1,1–8,6 punkti). OECD vidējais: PISA 2025 Vol. I, Table I.B1.2a.37.\n2025. gada aptvērums (izslēgšana 6,6 %) nav koriģēts. docs/58, docs/63, docs/67.") +
  TEMA + theme(strip.text = element_text(face = "bold", size = 10))
zim(v4, "V4_sadalijuma_gredas", 14, 8)
fwrite(DAL[, .(skala, gads, vid = round(vid, 1), zem_L2 = round(zem_L2, 1), L5 = round(L5, 1))], file.path(VZ, "V4_dalas.csv"))

# ---------------------------------------------------------------------------
# V5. Interaktīvs HTML: trajektorijas pa variantiem, mērījumi, ex ante prognozes
# ---------------------------------------------------------------------------
cat("V5\n")
SKV <- fread("output/pisa2025/salidzinajuma_kopa_valodas_plusmas.csv", colClasses = c(gads = "character"))
SK7 <- SKV[kopa == "galvenā valoda", .(vid = mean(vid), se = sqrt(sum(se^2)) / .N), by = gads]
PM <- fread("output/pisa2025/paaudzu_modelis_petijumi.csv")[radītājs == "vid" & grupa == "LVA A" & kopa == "B: LVA A", .(variants, merkis, fakt = round(fakt, 1), prog_rel = round(prog_rel, 1))]
d5 <- list(trajektorija = TR[!is.na(rel) & b >= 1984 & b <= 2014, .(variants, grupa, b, rel = round(rel, 1), se = round(se, 1))], merijumi = rbind(REL[, .(petijums, gads, dz_gads = round(dz_gads, 1), vecums, rel = round(rel, 1), se = round(se, 1))], PA[, .(petijums = "PIAAC", gads = 2023, dz_gads = round(dz_gads, 1), vecums = vec, rel = round(piaac_starp, 1), se = round(piaac_se, 1))]),
           prognozes = PM, valstis = c(SALID, "LVA A"),
           absol = P[serija == "PISA" & grupa %in% c(SALID, "LVA A"), .(grupa, gads, b = round(dz_gads, 1), est = round(est, 1), se = round(se, 1))],
           absol_iea = P[serija != "PISA" & grupa == "LVA A", .(petijums, gads, b = round(dz_gads, 1), est = round(est, 1), se = round(se, 1))],
           kopa7 = rbind(P[serija == "PISA" & grupa %in% SALID, .(kopa = "visi skolēni", est = round(mean(est), 1), se = round(sqrt(sum(se^2)) / .N, 1), b = round(dz_gads[1], 1)), by = gads],
                         SK7[, .(kopa = "galvenā valoda", est = round(vid, 1), se = round(se, 1), b = round(as.numeric(gads) - 15.8, 1)), by = gads][, .(gads, kopa, est, se, b)]))
js <- toJSON(d5, dataframe = "rows", auto_unbox = TRUE)
# V5 veidne glabājas atsevišķā failā (pārtaisīta 2026-09-16 pēc lietotāja aizrādījuma, ka saturs nav saprotams)
V5T <- paste(readLines("R/pisa2025/70_V5_veidne.html", encoding = "UTF-8", warn = FALSE), collapse = "\n")
html <- sub("__JSDATA__", js, V5T, fixed = TRUE)
writeLines(html, file.path(VZ, "V5_trajektorija.html"))

# Galerija
gal <- sprintf('<!doctype html><html lang="lv"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Ilgtermiņa signāli: vizualizācijas</title>
<style>body{font-family:system-ui,sans-serif;margin:0;background:#fcfcfb;color:#111}.wrap{max-width:1300px;margin:0 auto;padding:24px}h1{font-size:22px}h2{font-size:17px;margin:34px 0 6px}img{max-width:100%%;border:1px solid #e3e3dd;background:#fff}p{color:#444;font-size:14px;max-width:90ch}a{color:#2a78d6}</style></head><body><div class="wrap">
<h1>Vai to varēja redzēt? Ilgtermiņa signāli piecos skatos</h1><p>Ģenerēts %s ar R/pisa2025/70_vizualizacijas.R. Pamatojums un skaitļi: docs/63.</p>
<h2>V1. Paaudžu (Lexis) diagramma</h2><img src="V1_paaudzu_diagramma.png"><h2>V2. Efekta lielumu mežs</h2><img src="V2_efekta_lielumi.png"><h2>V3. Brīdinājuma panelis</h2><img src="V3_bridinajuma_panelis.png">
<h2>V4. Sadalījuma grēdas</h2><img src="V4_sadalijuma_gredas.png"><h2>V5. Interaktīvā trajektorija</h2><p><a href="V5_trajektorija.html">Atvērt V5 (interaktīvs)</a></p><iframe src="V5_trajektorija.html" style="width:100%%;height:760px;border:1px solid #e3e3dd;background:#fff"></iframe></div></body></html>', format(Sys.Date()))
writeLines(gal, file.path(VZ, "ilgtermina_signali.html"))
cat("Pabeigts:", format(Sys.time()), "\n")
