# ============================================================================
# 75. Slānis 10, virzītais variants: Bayesa tīkls simptomu konstruktiem un lasīšanai
# PISA datu analīze 2025
# ============================================================================
#
# Turpina 69c EBICglasso tīklu (docs/64, 3. sadaļa) ar virzītu modeli (bnlearn).
#
# Mainīgie: tā pati skolēnu konstruktu kopa kā 69c (aizpildītība ≥ 70 % 2015–2025 abās
#   grupās), ar vienu izmaiņu: no sešiem piederības vienumiem (ST034) paliek tikai
#   "skolā viegli iegūstu draugus" (69c vienīgais vienums ar tiešu malu uz lasīšanu
#   visos ciklos) un WLE indekss BELONG. Iemesls: 2022. gadā ST034 vienumi tika
#   administrēti ar plānoto izlaidumu (katrs skolēns saņēma 5 no 6), tāpēc pilnu gadījumu
#   ar visiem sešiem nav neviena, un hc ar Gausa BIC prasa pilnus datus. Blakus ieguvums:
#   pazūd determinētā saikne vienumi → BELONG, kas virzītā grafā rada mākslīgu centru.
# Modelis: Gausa Bayesa tīkls (visi mezgli standartizēti, ordinālie vienumi kā skaitļi).
#   Pamatojums: 69c lietoja Spīrmena korelācijas tiem pašiem mainīgajiem, tātad tas pats
#   lineārais tuvinājums; diskretizējot 22 mezglus terciļos, mezglam ar 4 vecākiem būtu
#   81 nosacītā sadalījuma šūnas pie n ≈ 2500, un nepārtrauktie mainīgie (rezultāts,
#   ESCS, HOMEPOS) zaudētu informāciju. Diskrētais (terciļu) variants tiek rēķināts kā
#   jutīguma pārbaude LVA A 2022 un 2025.
# Laika ierobežojumi (melnais saraksts, tiers2blacklist): ģimenes fons → skolas gaita →
#   skolas pieredze (klimats, kavējumi, labbūtība) → [lasīšanas paradumi, tikai
#   paplašinātajā 2009/2018 tīklā] → lasīšanas rezultāts. Rezultāts nevar būt vecāks.
#   Papildus: klase → atkārtošana un atkārtoja 5.–9. → atkārtoja 1.–4. aizliegti.
# Struktūra: hc ar bic-g, 200 sāknēšanas atkārtojumi ar svērtu izlasi (prob ∝ w; 7 valstu
#   kopā svari normēti, lai katra valsts × cikls sver vienādi), izlases lielums m = 2500
#   visām šūnām (vienāds BIC sods, lai LVA A un 7 valstu struktūras būtu salīdzināmas,
#   nevis atkarīgas no n). Vidējotais tīkls: loka stiprums ≥ 0,5, virziena varbūtība
#   ≥ 0,5, pievienojot lokus stipruma secībā bez cikliem. Jutīgums: tabu, pc.stable,
#   mmhc, BGe, diskrētais, bez skolu anketas mainīgajiem.
# Grupas un cikli: LVA A un 7 valstis (CZE, DEU, EST, FIN, HUN, LTU, POL); 2015, 2018,
#   2022, 2025 pamatkopā; 2012 ar samazinātu kopu; 2009 un 2018 ar lasīšanas paradumu
#   paplašinājumu (lasīšana bija galvenā joma, tāpēc anketā ir lasītprieks, lasāmviela,
#   metakognīcija, DISCLIMA, ICTRES).
# Jautājumi: (a) vai loku struktūra Latvijā laikā mainās (Hamminga un SHD attālumi ar
#   sāknēšanas intervāliem; loki, kas parādās vai pazūd 2022 → 2025); (b) vai Latvija
#   atšķiras no 7 valstīm; (c) lasīšanas Markova sega katrā ciklā; (d) parametri pie
#   fiksētas struktūras; (e) intervences ilustrācija (cpdist, mutilated), tikai aprakstoši.
#
# Izvade: output/pisa2025/bn_loki_<grupa>_<gads>.csv, bn_kopsavilkums.csv,
#   bn_salidzinajums.csv, bn_markova_sega.csv, bn_koeficienti_fiksets.csv,
#   bn_intervences.csv, bn_jutigums.csv, bn_69c_salidzinajums.csv,
#   output/pisa2025/plots/75_bn_<grupa>_<gads>.png, logs/75_bayesa_tikls_output.txt
# ============================================================================

# --- Darba direktorija -------------------------------------------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path))))
}
if (!file.exists("data/pisa2025/processed/simptomi_apvienots.rds")) stop("Skripts jāpalaiž no projekta saknes.")

# --- Izvades saglabāšana failā ---------------------------------------------
while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/plots", showWarnings = FALSE)
output_file <- "output/pisa2025/logs/75_bayesa_tikls_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n")
cat("Darba direktorija:", getwd(), "\n")
cat("Sākums:", format(Sys.time()), "\n\n")

# --- Pakotnes ----------------------------------------------------------------
required_packages <- c("data.table", "bnlearn", "igraph", "parallel")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Trūkst pakotnes: ", pkg, " (netiek instalēta).")
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
cat("Pakotnes: bnlearn", as.character(packageVersion("bnlearn")), ", igraph", as.character(packageVersion("igraph")), "\n")
setDTthreads(2); KODOLI <- 4     # fonā rēķinās cits darbs, ne vairāk par 4 pavedieniem
RNGkind("L'Ecuyer-CMRG"); set.seed(20260913)
B_SAKN <- 200; M_IZL <- 2500

# === 1. DATI UN MAINĪGIE =====================================================
cat(strrep("=", 90), "\n1. Dati un mainīgie\n", strrep("=", 90), "\n\n", sep = "")
A <- readRDS("data/pisa2025/processed/simptomi_apvienots.rds")
A <- A[!is.na(PVREAD) & !is.na(w) & w > 0]
A[, grupa := ifelse(cnt == "LVA", paste("LVA", fifelse(is.na(plusma), "?", plusma)), cnt)]
SALID <- c("CZE", "DEU", "EST", "FIN", "HUN", "LTU", "POL")
A <- A[grupa %in% c("LVA A", SALID)][, kopa := ifelse(grupa == "LVA A", "LVA A", "7 valstis")]
A[, wn := w * 1000 / sum(w), by = .(gads, cnt)]    # katra valsts × cikls sver vienādi

# Mezgli un līmeņi (laika kārtība)
LIM <- c(
  AUTO_ESCS = 1, AUTO_HOMEPOS = 1, AUTO_HISEI = 1, AUTO_IMMIG = 1, L_GRAMATAS_MAJAS = 1, D_TELEVIZORI_SKAITS = 1,
  D_PLANSETES_SKAITS = 1, L_MAJAS_EGRAMATU_LASITAJS = 1, S_PIRMSSKOLA_SAKUMA_VECUMS = 1, AUTO_ICTRES = 1,
  S_ATKARTOJA_ISCED1 = 2, S_ATKARTOJA_ISCED2 = 2, S_KLASE = 2, AUTO_CLSIZE = 2, AUTO_CREACTIV = 2,
  AUTO_TEACHSUP = 3, AUTO_BELONG = 3, K_PIED_DRAUGI = 3, S_KAVE_DIENU = 3, S_KAVE_STUNDAS = 3, S_NOKAVE = 3,
  W_DZIVES_APMIERINATIBA = 3, AUTO_DISCLIMA = 3, L_IDX_STIMREAD = 3,
  L_IDX_LASITPRIEKS = 4, L_LAIKS_PRIEKAM = 4, L_MAT_DAILLITERATURA = 4, L_MAT_NEDAILLITERATURA = 4, L_MAT_ZURNALI = 4,
  L_MAT_AVIZES = 4, L_MAT_KOMIKSI = 4, L_IDX_METASUM = 4, L_IDX_UNDREM = 4,
  PVREAD = 5)
LIM_NOS <- c("ģimenes fons", "skolas gaita", "skolas pieredze", "lasīšanas paradumi", "iznākums")
LAB <- c(
  AUTO_ESCS = "ESCS", AUTO_HOMEPOS = "HOMEPOS", AUTO_HISEI = "HISEI", AUTO_IMMIG = "Imigrācijas fons", L_GRAMATAS_MAJAS = "Grāmatas mājās",
  D_TELEVIZORI_SKAITS = "Televizori", D_PLANSETES_SKAITS = "Planšetes", L_MAJAS_EGRAMATU_LASITAJS = "E-lasītājs mājās",
  S_PIRMSSKOLA_SAKUMA_VECUMS = "Pirmsskolas sākums", AUTO_ICTRES = "IKT resursi mājās",
  S_ATKARTOJA_ISCED1 = "Atkārtoja 1.–4. kl.", S_ATKARTOJA_ISCED2 = "Atkārtoja 5.–9. kl.", S_KLASE = "Klase", AUTO_CLSIZE = "Klases lielums",
  AUTO_CREACTIV = "Radošās aktivitātes", AUTO_TEACHSUP = "Skolotāju atbalsts", AUTO_BELONG = "Piederība (BELONG)",
  K_PIED_DRAUGI = "Viegli iegūst draugus", S_KAVE_DIENU = "Kavē dienas", S_KAVE_STUNDAS = "Kavē stundas", S_NOKAVE = "Nokavē",
  W_DZIVES_APMIERINATIBA = "Dzīves apmierinātība", AUTO_DISCLIMA = "Disciplīna stundās", L_IDX_STIMREAD = "Skolotājs rosina lasīt",
  L_IDX_LASITPRIEKS = "Lasītprieks", L_LAIKS_PRIEKAM = "Laiks lasīšanai priekam", L_MAT_DAILLITERATURA = "Lasa daiļliteratūru",
  L_MAT_NEDAILLITERATURA = "Lasa nedaiļliteratūru", L_MAT_ZURNALI = "Lasa žurnālus", L_MAT_AVIZES = "Lasa avīzes", L_MAT_KOMIKSI = "Lasa komiksus",
  L_IDX_METASUM = "Metakognīcija: kopsavilkums", L_IDX_UNDREM = "Metakognīcija: izpratne", PVREAD = "Lasīšanas rezultāts")
PAMATA <- c("AUTO_ESCS", "AUTO_HOMEPOS", "AUTO_HISEI", "AUTO_IMMIG", "L_GRAMATAS_MAJAS", "D_TELEVIZORI_SKAITS", "D_PLANSETES_SKAITS",
            "L_MAJAS_EGRAMATU_LASITAJS", "S_PIRMSSKOLA_SAKUMA_VECUMS", "S_ATKARTOJA_ISCED1", "S_ATKARTOJA_ISCED2", "S_KLASE", "AUTO_CLSIZE",
            "AUTO_CREACTIV", "AUTO_TEACHSUP", "AUTO_BELONG", "K_PIED_DRAUGI", "S_KAVE_DIENU", "S_KAVE_STUNDAS", "S_NOKAVE",
            "W_DZIVES_APMIERINATIBA", "PVREAD")
K2012 <- setdiff(PAMATA, c("K_PIED_DRAUGI", "AUTO_TEACHSUP", "AUTO_BELONG", "D_PLANSETES_SKAITS", "W_DZIVES_APMIERINATIBA", "S_PIRMSSKOLA_SAKUMA_VECUMS"))
KLASA <- c("AUTO_ESCS", "AUTO_HOMEPOS", "AUTO_HISEI", "AUTO_IMMIG", "L_GRAMATAS_MAJAS", "D_TELEVIZORI_SKAITS", "AUTO_ICTRES",
           "S_ATKARTOJA_ISCED1", "S_ATKARTOJA_ISCED2", "S_KLASE", "AUTO_DISCLIMA", "L_IDX_STIMREAD",
           "L_IDX_LASITPRIEKS", "L_LAIKS_PRIEKAM", "L_MAT_DAILLITERATURA", "L_MAT_NEDAILLITERATURA", "L_MAT_ZURNALI", "L_MAT_AVIZES",
           "L_MAT_KOMIKSI", "L_IDX_METASUM", "L_IDX_UNDREM", "PVREAD")
cat("Pamatkopa:", length(PAMATA), "mezgli\n")
print(data.table(mezgls = PAMATA, limenis = LIM_NOS[LIM[PAMATA]], etikete = LAB[PAMATA]), row.names = FALSE)
cat("\n2012. gada samazinātā kopa:", length(K2012), "mezgli (bez piederības, skolotāju atbalsta, planšetēm, dzīves apmierinātības, pirmsskolas)\n")
cat("2009/2018 lasīšanas paplašinātā kopa:", length(KLASA), "mezgli:", paste(LAB[setdiff(KLASA, PAMATA)], collapse = ", "), "\n\n")

melnais <- function(mezgli) {
  lim <- LIM[mezgli]; tiers <- lapply(sort(unique(lim)), function(l) mezgli[lim == l])
  bl <- tiers2blacklist(tiers)
  extra <- rbind(if (all(c("S_KLASE", "S_ATKARTOJA_ISCED1") %in% mezgli)) c("S_KLASE", "S_ATKARTOJA_ISCED1"),
                 if (all(c("S_KLASE", "S_ATKARTOJA_ISCED2") %in% mezgli)) c("S_KLASE", "S_ATKARTOJA_ISCED2"),
                 if (all(c("S_ATKARTOJA_ISCED1", "S_ATKARTOJA_ISCED2") %in% mezgli)) c("S_ATKARTOJA_ISCED2", "S_ATKARTOJA_ISCED1"))
  bl <- unique(rbind(bl, extra)); colnames(bl) <- c("from", "to"); bl
}

# Šūnas dati: pilnie gadījumi, standartizēti (−999 = neatbildēts arī kā trūkstošs)
suna <- function(g, k, mezgli) {
  d <- A[gads %in% g & kopa == k]; M <- as.matrix(d[, ..mezgli]); M[M == -999] <- NA
  ok <- complete.cases(M); M <- M[ok, ]
  list(d = as.data.frame(scale(M)), w = d$wn[ok], n = nrow(d), n_pilni = sum(ok), vid = mean(M[, "PVREAD"]), sd = sd(M[, "PVREAD"]),
       gads = d$gads[ok])
}

# Sāknēšana ar svērtu izlasi, m gadījumi ar atkārtojumu, 4 kodoli
saknet <- function(d, w, bl, B = B_SAKN, m = M_IZL, metode = "hc", score = "bic-g", seed = 1) {
  set.seed(seed)
  mclapply(seq_len(B), function(b) {
    i <- sample.int(nrow(d), m, replace = TRUE, prob = w); x <- d[i, ]
    switch(metode,
           hc = hc(x, blacklist = bl, score = score),
           tabu = tabu(x, blacklist = bl, score = score),
           pc = pc.stable(x, blacklist = bl, alpha = 0.01),
           mmhc = mmhc(x, blacklist = bl, restrict.args = list(alpha = 0.01), maximize.args = list(score = score)))
  }, mc.cores = KODOLI, mc.set.seed = TRUE)
}

# Loku indikatoru matrica (B × p²) un stiprumi
indik <- function(tikli, mezgli) {
  p <- length(mezgli); P <- matrix(0, length(tikli), p * p, dimnames = list(NULL, paste(rep(mezgli, p), rep(mezgli, each = p), sep = "->")))
  for (b in seq_along(tikli)) {
    a <- arcs(tikli[[b]]); if (nrow(a)) P[b, paste(a[, 1], a[, 2], sep = "->")] <- 1
    u <- undirected.arcs(tikli[[b]]); if (nrow(u)) P[b, paste(u[, 1], u[, 2], sep = "->")] <- 0.5   # pc: nevirzīta mala = 0,5 katrā virzienā
  }
  P
}
stiprumi <- function(P, mezgli) {
  s <- colMeans(P); p <- length(mezgli); S <- matrix(s, p, p, dimnames = list(mezgli, mezgli))   # S[a, b] = P(a → b)
  ij <- which(upper.tri(S), arr.ind = TRUE)
  r <- data.table(a = mezgli[ij[, 1]], b = mezgli[ij[, 2]], p_ab = S[ij], p_ba = t(S)[ij])
  r[, stiprums := p_ab + p_ba][, `:=`(no = ifelse(p_ab >= p_ba, a, b), uz = ifelse(p_ab >= p_ba, b, a))]
  r[, virziens := pmax(p_ab, p_ba) / pmax(stiprums, 1e-9)][stiprums > 0][order(-stiprums)]
}
# Vidējotais tīkls: stiprums ≥ slieksnis, virziens ≥ 0,5, loki pievienoti stipruma secībā bez cikliem
konsenss <- function(st, mezgli, slieksnis = 0.5) {
  g <- empty.graph(mezgli); k <- st[stiprums >= slieksnis]
  for (i in seq_len(nrow(k))) g <- tryCatch(set.arc(g, k$no[i], k$uz[i], check.cycles = TRUE), error = function(e) g)
  g
}
# Attālumi starp divām stiprumu tabulām (skelets: Hamminga; SHD: + virziena atšķirības kopīgajās malās)
attalums <- function(s1, s2, slieksnis = 0.5) {
  k1 <- s1[stiprums >= slieksnis][, pāris := paste(pmin(a, b), pmax(a, b))]; k2 <- s2[stiprums >= slieksnis][, pāris := paste(pmin(a, b), pmax(a, b))]
  ham <- length(setdiff(k1$pāris, k2$pāris)) + length(setdiff(k2$pāris, k1$pāris))
  kop <- intersect(k1$pāris, k2$pāris)
  virz <- sum(k1$no[match(kop, k1$pāris)] != k2$no[match(kop, k2$pāris)])
  c(hamming = ham, shd = ham + virz, kopigas = length(kop), n1 = nrow(k1), n2 = nrow(k2))
}
# Standartizēti koeficienti no bn.fit pie dotās struktūras
koef <- function(g, d) {
  f <- bn.fit(g, d); rbindlist(lapply(nodes(g), function(v) {
    cf <- f[[v]]$coefficients; cf <- cf[names(cf) != "(Intercept)"]
    if (!length(cf)) return(NULL); data.table(no = names(cf), uz = v, koef = as.numeric(cf), r2 = 1 - f[[v]]$sd^2) }))
}

# Zīmējums: igraph, līmeņi no augšas uz leju, loka platums ∝ stiprums
KRASAS <- c("#4C72B0", "#55A868", "#DD8452", "#8172B3", "#C44E52")
zimet <- function(g, st, nos, fails, mezgli) {
  a <- arcs(g); if (!nrow(a)) return(invisible())
  gr <- graph_from_data_frame(as.data.frame(a), directed = TRUE, vertices = data.frame(name = mezgli))
  s <- st$stiprums[match(paste(pmin(a[, 1], a[, 2]), pmax(a[, 1], a[, 2])), paste(pmin(st$a, st$b), pmax(st$a, st$b)))]
  # Izkārtojums: līmeņi kā rindas no augšas (ģimenes fons) uz leju (rezultāts); kārtība rindā no Sugiyama (mazāk krustojumu),
  # bet atstarpes vienmērīgas; etiķetes pamīšus virs un zem mezgla, lai nepārklātos
  lim <- LIM[mezgli]; lay0 <- layout_with_sugiyama(gr, layers = lim)$layout; lay <- matrix(0, length(mezgli), 2); virs <- rep(-pi / 2, length(mezgli))
  for (l in unique(lim)) { i <- which(lim == l); k <- length(i); rk <- rank(lay0[i, 1], ties.method = "first")
    lay[i, 1] <- if (k == 1) 0 else seq(-1, 1, length.out = k)[rk]; lay[i, 2] <- -match(l, sort(unique(lim))); virs[i] <- if (k == 1) pi / 2 else ifelse(rk %% 2 == 0, pi / 2, -pi / 2) }
  same <- lim[match(a[, 1], mezgli)] == lim[match(a[, 2], mezgli)]
  png(fails, width = 2600, height = 1700, res = 200)
  par(mar = c(0.5, 0.5, 2.5, 0.5))
  plot(gr, layout = lay, rescale = TRUE, vertex.color = KRASAS[lim], vertex.frame.color = "grey30", vertex.size = 6, vertex.label = LAB[mezgli],
       vertex.label.cex = 0.62, vertex.label.color = "black", vertex.label.dist = 1.4, vertex.label.degree = virs, vertex.label.family = "sans",
       edge.width = 0.6 + 4 * (s - 0.5), edge.arrow.size = 0.35, edge.color = ifelse(s >= 0.8, "grey25", "grey60"), edge.curved = ifelse(same, 0.35, 0.05), main = nos)
  legend("bottomleft", legend = LIM_NOS[sort(unique(lim))], fill = KRASAS[sort(unique(lim))], bty = "n", cex = 0.7,
         title = "Līmenis (laika kārtība)")
  legend("bottomright", legend = c("stiprums ≥ 0,8", "stiprums 0,5–0,8"), col = c("grey25", "grey60"), lwd = c(2.5, 1), bty = "n", cex = 0.7)
  dev.off(); cat("Saglabāts:", fails, "\n")
}
drukat_lokus <- function(st, g, n = 40) {
  k <- st[stiprums >= 0.5]; k[, konsensa := paste(no, uz) %in% paste(arcs(g)[, 1], arcs(g)[, 2])]
  print(k[1:min(n, nrow(k)), .(no = LAB[no], uz = LAB[uz], stiprums = round(stiprums, 2), virziens = round(virziens, 2), konsensa)], row.names = FALSE)
}

# === 2. PAMATANALĪZE: 8 ŠŪNAS, 2012 UN LASĪŠANAS PAPLAŠINĀJUMS =================
cat(strrep("=", 90), "\n2. Struktūras mācīšanās (hc, bic-g, 200 sāknēšanas, m = 2500)\n", strrep("=", 90), "\n", sep = "")
cat("UZMANĪBU: šī sadaļa var aizņemt 10 līdz 20 minūtes.\n\n")
SUNAS <- rbind(CJ(gads = c("2015", "2018", "2022", "2025"), kopa = c("LVA A", "7 valstis"))[, `:=`(kopa_nos = "pamata")],
               data.table(gads = "2012", kopa = c("LVA A", "7 valstis"), kopa_nos = "2012"),
               CJ(gads = c("2009", "2018"), kopa = c("LVA A", "7 valstis"))[, `:=`(kopa_nos = "lasisana")])
REZ <- list(); KOPS <- list(); MS <- list()
for (i in seq_len(nrow(SUNAS))) {
  g <- SUNAS$gads[i]; k <- SUNAS$kopa[i]; kn <- SUNAS$kopa_nos[i]
  mezgli <- switch(kn, pamata = PAMATA, "2012" = K2012, lasisana = KLASA)
  kods <- paste0(gsub(" ", "", k), "_", g, if (kn == "lasisana") "_lasisana" else "")
  D <- suna(g, k, mezgli); bl <- melnais(mezgli); t0 <- Sys.time()
  tikli <- saknet(D$d, D$w, bl, seed = 100 + i)
  P <- indik(tikli, mezgli); st <- stiprumi(P, mezgli); G <- konsenss(st, mezgli)
  pilns <- hc(D$d, blacklist = bl, score = "bic-g")     # atsauce: viens tīkls uz visiem pilnajiem gadījumiem
  kf <- koef(G, D$d); vec <- kf[uz == "PVREAD"]
  st[, koef := kf$koef[match(paste(no, uz), paste(kf$no, kf$uz))]]
  st[, `:=`(gads = g, kopa = k, kopa_nos = kn, virziens_laika = LIM[no] != LIM[uz], konsensa = paste(no, uz) %in% paste(arcs(G)[, 1], arcs(G)[, 2]))]
  fwrite(st[, .(gads, kopa, kopa_nos, no, uz, no_etikete = LAB[no], uz_etikete = LAB[uz], stiprums = round(stiprums, 3), virziens = round(virziens, 3),
                virziens_laika, konsensa, koef = round(koef, 3))], sprintf("output/pisa2025/bn_loki_%s.csv", kods))
  REZ[[kods]] <- list(P = P, st = st, G = G, D = D, mezgli = mezgli, gads = g, kopa = k, kopa_nos = kn, tikli = tikli)
  MS[[kods]] <- data.table(gads = g, kopa = k, kopa_nos = kn, vecaks = vec$no, etikete = LAB[vec$no], koef = round(vec$koef, 3),
                           stiprums = round(st$stiprums[match(paste(vec$no, "PVREAD"), paste(st$no, st$uz))], 3))
  KOPS[[kods]] <- data.table(gads = g, kopa = k, kopa_nos = kn, mezgli = length(mezgli), n = D$n, n_pilni = D$n_pilni, pilnie_dala = round(D$n_pilni / D$n, 3),
                             loki_konsenss = narcs(G), loki_st08 = st[stiprums >= 0.8, .N], loki_pilns_hc = narcs(pilns),
                             vid_stiprums_konsenss = round(mean(st[konsensa == TRUE]$stiprums), 3),
                             pvread_vecaki = paste(vec$no, collapse = "; "), pvread_r2 = round(vec$r2[1], 3),
                             pvread_vecaki_pilns = paste(parents(pilns, "PVREAD"), collapse = "; "), minutes = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1))
  cat(sprintf("\n--- %s %s (%s): n = %d, pilnie %d (%.0f %%), konsensa loki %d (≥ 0,8: %d), pilna hc loki %d, %.1f min\n", k, g, kn, D$n, D$n_pilni,
              100 * D$n_pilni / D$n, narcs(G), st[stiprums >= 0.8, .N], narcs(pilns), KOPS[[kods]]$minutes))
  cat("Lasīšanas vecāki (Markova sega), koef. SD vienībās un loka stiprums:\n")
  print(MS[[kods]][, .(etikete, koef, stiprums)], row.names = FALSE)
  cat("Loki ar stiprumu ≥ 0,5:\n"); drukat_lokus(st, G)
  zimet(G, st, sprintf("Bayesa tīkls, %s, %s%s (n = %d, sāknēšana 200, loki ar stiprumu ≥ 0,5)", k, g, if (kn == "lasisana") ", lasīšanas paplašinājums" else if (kn == "2012") ", samazināta kopa" else "", D$n_pilni),
        sprintf("output/pisa2025/plots/75_bn_%s.png", kods), mezgli)
}
KOPS <- rbindlist(KOPS); MSEGA <- rbindlist(MS)
fwrite(KOPS, "output/pisa2025/bn_kopsavilkums.csv"); fwrite(MSEGA, "output/pisa2025/bn_markova_sega.csv")
cat("\nKopsavilkums pa šūnām:\n"); print(KOPS[, .(gads, kopa, kopa_nos, n_pilni, pilnie_dala, loki_konsenss, loki_st08, loki_pilns_hc, pvread_r2)], row.names = FALSE)

# === 3. LASĪŠANAS MARKOVA SEGA PA CIKLIEM ====================================
cat("\n", strrep("=", 90), "\n3. Lasīšanas Markova sega (vecāki) pa cikliem: koeficients SD vienībās (loka stiprums)\n", strrep("=", 90), "\n", sep = "")
cat("Piezīme: rezultāts ir pēdējā līmenī un nevar būt vecāks, tāpēc Markova sega = vecāku kopa.\n\n")
for (kn in c("pamata", "2012", "lasisana")) {
  m <- MSEGA[kopa_nos == kn]; if (!nrow(m)) next
  m[, v := sprintf("%+.2f (%.2f)", koef, stiprums)]
  cat(sprintf("Kopa: %s\n", kn)); print(dcast(m, etikete ~ kopa + gads, value.var = "v", fill = ""), row.names = FALSE); cat("\n")
}

# === 4. STRUKTŪRAS SALĪDZINĀJUMS: CIKLI UN GRUPAS =============================
cat(strrep("=", 90), "\n4. Struktūras attālumi ar sāknēšanas intervāliem (100 apakšizlases no 200 tīkliem)\n", strrep("=", 90), "\n\n", sep = "")
# Apakšizlase: 200 tīklu izlase ar atkārtojumu → stiprumi → konsenss; intervāls no 100 šādām izlasēm;
# trokšņa līmenis: divas neatkarīgas apakšizlases no tās pašas šūnas.
apaks <- function(r, j) { set.seed(1000 + j); stiprumi(r$P[sample.int(nrow(r$P), replace = TRUE), , drop = FALSE], r$mezgli) }
salidz_int <- function(k1, k2, nos, J = 100) {
  r1 <- REZ[[k1]]; r2 <- REZ[[k2]]; pk <- attalums(r1$st, r2$st)
  Dm <- t(sapply(seq_len(J), function(j) attalums(apaks(r1, j), apaks(r2, j))[1:2]))
  T1 <- t(sapply(seq_len(J), function(j) attalums(apaks(r1, j), apaks(r1, j + J))[1:2]))
  T2 <- t(sapply(seq_len(J), function(j) attalums(apaks(r2, j), apaks(r2, j + J))[1:2]))
  sk <- skersojumi(r1$st, r2$st)
  data.table(salidzinajums = nos, malas_1 = pk["n1"], malas_2 = pk["n2"], kopigas = pk["kopigas"], hamming = pk["hamming"],
             hamming_int = sprintf("%.0f–%.0f", quantile(Dm[, 1], 0.025), quantile(Dm[, 1], 0.975)),
             hamming_mk = sprintf("%.1f / %.1f", mean(T1[, 1]), mean(T2[, 1])), shd = pk["shd"],
             shd_int = sprintf("%.0f–%.0f", quantile(Dm[, 2], 0.025), quantile(Dm[, 2], 0.975)),
             skerso_05 = sk["skerso"], stipruma_kor = sk["kor"])
}
# Cik malu šķērso 0,5 starp divām stiprumu tabulām, un stiprumu korelācija pa visiem pāriem (slieksnim nejutīgs mērs)
skersojumi <- function(s1, s2) {
  m <- merge(s1[, .(a, b, st1 = stiprums)], s2[, .(a, b, st2 = stiprums)], by = c("a", "b"), all = TRUE)
  for (x in c("st1", "st2")) set(m, which(is.na(m[[x]])), x, 0)
  c(skerso = m[(st1 >= 0.5) != (st2 >= 0.5), .N], kor = round(cor(m$st1, m$st2), 3))
}
SAL <- list()
for (pr in list(c("2015", "2018"), c("2018", "2022"), c("2022", "2025"), c("2015", "2025")))
  for (k in c("LVAA", "7valstis")) SAL[[paste(k, pr[1], pr[2])]] <- salidz_int(paste0(k, "_", pr[1]), paste0(k, "_", pr[2]), sprintf("%s %s → %s", ifelse(k == "LVAA", "LVA A", "7 valstis"), pr[1], pr[2]))
for (g in c("2015", "2018", "2022", "2025")) SAL[[paste("grupas", g)]] <- salidz_int(paste0("LVAA_", g), paste0("7valstis_", g), sprintf("LVA A pret 7 valstīm %s", g))
SAL[["2012 grupas"]] <- salidz_int("LVAA_2012", "7valstis_2012", "LVA A pret 7 valstīm 2012 (samazināta kopa)")
SAL[["lasisana LVA"]] <- salidz_int("LVAA_2009_lasisana", "LVAA_2018_lasisana", "LVA A 2009 → 2018 (lasīšanas kopa)")
SAL[["lasisana 7"]] <- salidz_int("7valstis_2009_lasisana", "7valstis_2018_lasisana", "7 valstis 2009 → 2018 (lasīšanas kopa)")
for (g in c("2009", "2018")) SAL[[paste("lasisana grupas", g)]] <- salidz_int(paste0("LVAA_", g, "_lasisana"), paste0("7valstis_", g, "_lasisana"), sprintf("LVA A pret 7 valstīm %s (lasīšanas kopa)", g))
SALT <- rbindlist(SAL)
# Dalītās izlases atskaite: šūnu sadala divās nejaušās pusēs, katrai sava sāknēšana (200, m = 2500); attālums starp pusēm rāda,
# cik lielu attālumu dod divas neatkarīgas izlases no tās pašas populācijas. Tā ir īstā atskaite pārmaiņu vērtēšanai.
cat("Dalītās izlases atskaite (6 šūnas × 2 puses × 200 sāknēšanas), var aizņemt 3 minūtes.\n")
DAL <- list()
for (kods in c("LVAA_2015", "LVAA_2018", "LVAA_2022", "LVAA_2025", "7valstis_2022", "7valstis_2025")) {
  r <- REZ[[kods]]; set.seed(900); i <- sample(nrow(r$D$d)); h <- i[seq_len(floor(length(i) / 2))]; bl <- melnais(PAMATA)
  s1 <- stiprumi(indik(saknet(r$D$d[h, ], r$D$w[h], bl, seed = 901), PAMATA), PAMATA)
  s2 <- stiprumi(indik(saknet(r$D$d[-h, ], r$D$w[-h], bl, seed = 902), PAMATA), PAMATA)
  pk <- attalums(s1, s2); sk <- skersojumi(s1, s2)
  DAL[[kods]] <- data.table(salidzinajums = sprintf("%s %s: puse A pret pusi B (n = %d / %d)", r$kopa, r$gads, length(h), nrow(r$D$d) - length(h)),
                            malas_1 = pk["n1"], malas_2 = pk["n2"], kopigas = pk["kopigas"], hamming = pk["hamming"], hamming_int = "", hamming_mk = "",
                            shd = pk["shd"], shd_int = "", skerso_05 = sk["skerso"], stipruma_kor = sk["kor"])
}
DALT <- rbindlist(DAL); SALT <- rbind(SALT, DALT); fwrite(SALT, "output/pisa2025/bn_salidzinajums.csv")
cat("\nHamminga attālums (skelets) un SHD (ar virzieniem) starp vidējotajiem tīkliem; intervāls 2,5–97,5 % no 100 apakšizlasēm;\n")
cat("hamming_mk = Montekarlo kļūda (divas apakšizlases tās pašas šūnas 200 tīkliem, pirmā / otrā šūna); skerso_05 = malu skaits, kas\n")
cat("šķērso 0,5 slieksni; stipruma_kor = malu stiprumu korelācija pa visiem pāriem. Pēdējās rindas: dalītās izlases atskaite, t. i.,\n")
cat("cik atšķiras divas neatkarīgas puses no tās pašas šūnas. Attālums starp cikliem, kas nepārsniedz šo atskaiti, nav pārmaiņa.\n\n")
print(SALT, row.names = FALSE)

# Loki, kas parādās vai pazūd (stiprums šķērso 0,5) starp cikliem un starp grupām
maina <- function(k1, k2, nos) {
  s1 <- REZ[[k1]]$st; s2 <- REZ[[k2]]$st
  m <- merge(s1[, .(a, b, st1 = stiprums, no1 = no, uz1 = uz)], s2[, .(a, b, st2 = stiprums, no2 = no, uz2 = uz)], by = c("a", "b"), all = TRUE)
  for (x in c("st1", "st2")) set(m, which(is.na(m[[x]])), x, 0)
  m[, `:=`(salidzinajums = nos, starpiba = st2 - st1)]
  m[, statuss := fifelse(st1 < 0.5 & st2 >= 0.5, "parādās", fifelse(st1 >= 0.5 & st2 < 0.5, "pazūd", fifelse(abs(starpiba) >= 0.3, "mainās stiprums", "")))]
  m[statuss != ""][order(-abs(starpiba))]
}
MAI <- rbind(maina("LVAA_2022", "LVAA_2025", "LVA A 2022 → 2025"), maina("LVAA_2018", "LVAA_2022", "LVA A 2018 → 2022"),
             maina("LVAA_2015", "LVAA_2018", "LVA A 2015 → 2018"), maina("7valstis_2022", "7valstis_2025", "7 valstis 2022 → 2025"),
             maina("7valstis_2025", "LVAA_2025", "7 valstis pret LVA A 2025"), maina("7valstis_2022", "LVAA_2022", "7 valstis pret LVA A 2022"))
fwrite(MAI[, .(salidzinajums, no = fifelse(st2 >= st1, no2, no1), uz = fifelse(st2 >= st1, uz2, uz1), a_etikete = LAB[a], b_etikete = LAB[b],
               stiprums_1 = round(st1, 2), stiprums_2 = round(st2, 2), starpiba = round(starpiba, 2), statuss)], "output/pisa2025/bn_loku_mainas.csv")
cat("\nLoki, kas parādās vai pazūd (stiprums šķērso 0,5) vai maina stiprumu par ≥ 0,3. Atskaite: dalītās izlases šķērsojumu skaits (skerso_05) iepriekš.\n")
for (nos in unique(MAI$salidzinajums)) {
  cat("\n", nos, ": šķērso 0,5 kopā", MAI[salidzinajums == nos & statuss != "mainās stiprums", .N], "malas\n"); x <- MAI[salidzinajums == nos]
  print(x[1:min(15, nrow(x)), .(mala = paste(LAB[a], "–", LAB[b]), stiprums_1 = round(st1, 2), stiprums_2 = round(st2, 2), starpiba = round(starpiba, 2), statuss)], row.names = FALSE)
}

# Stabilie loki: konsensā visos četros ciklos LVA A; un tikai Latvijā / tikai valstīs 2025
cat("\nLoki, kas ir vidējotajā tīklā visos četros LVA A ciklos (skelets):\n")
sk <- function(k) { s <- REZ[[k]]$st[stiprums >= 0.5]; paste(pmin(s$a, s$b), pmax(s$a, s$b)) }
stab <- Reduce(intersect, lapply(paste0("LVAA_", c("2015", "2018", "2022", "2025")), sk))
cat(paste(sapply(strsplit(stab, " "), function(x) paste(LAB[x[1]], "–", LAB[x[2]])), collapse = "\n"), "\n")
cat("\nTikai LVA A 2025 (nav 7 valstu tīklā):", paste(sapply(strsplit(setdiff(sk("LVAA_2025"), sk("7valstis_2025")), " "), function(x) paste(LAB[x[1]], "–", LAB[x[2]])), collapse = "; "), "\n")
cat("Tikai 7 valstis 2025 (nav LVA A tīklā):", paste(sapply(strsplit(setdiff(sk("7valstis_2025"), sk("LVAA_2025")), " "), function(x) paste(LAB[x[1]], "–", LAB[x[2]])), collapse = "; "), "\n")

# === 5. FIKSĒTA STRUKTŪRA, PARAMETRI PA CIKLIEM ==============================
cat("\n", strrep("=", 90), "\n5. Fiksēta struktūra (LVA A 2015–2025 apvienoti, sāknēšana 200) un koeficienti pa cikliem\n", strrep("=", 90), "\n\n", sep = "")
DP <- suna(c("2015", "2018", "2022", "2025"), "LVA A", PAMATA)
# standartizē katrā ciklā atsevišķi, lai struktūru nenosaka līmeņu pārmaiņas starp cikliem
DP$d <- as.data.frame(do.call(rbind, lapply(split(DP$d, DP$gads), scale)))[order(order(DP$gads)), ]
tikli_f <- saknet(DP$d, DP$w, melnais(PAMATA), seed = 777)
st_f <- stiprumi(indik(tikli_f, PAMATA), PAMATA); GF <- konsenss(st_f, PAMATA)
cat("Fiksētā struktūra: loki", narcs(GF), "; lasīšanas vecāki:", paste(LAB[parents(GF, "PVREAD")], collapse = ", "), "\n")
fwrite(st_f[, .(no, uz, stiprums = round(stiprums, 3), virziens = round(virziens, 3))], "output/pisa2025/bn_loki_LVAA_fiksets.csv")
zimet(GF, st_f, "Fiksētā struktūra: LVA A 2015–2025 apvienoti (sāknēšana 200)", "output/pisa2025/plots/75_bn_LVAA_fiksets.png", PAMATA)
# koeficienti pa cikliem ar sāknēšanas SE (svērta izlase, 200), abām grupām
vec_f <- parents(GF, "PVREAD"); KF <- list()
for (k in c("LVA A", "7 valstis")) for (g in c("2015", "2018", "2022", "2025")) {
  D <- suna(g, k, PAMATA); kf <- koef(GF, D$d)
  set.seed(55); bs <- replicate(200, { i <- sample.int(nrow(D$d), replace = TRUE, prob = D$w); coef(lm(reformulate(vec_f, "PVREAD"), D$d[i, ]))[vec_f] })
  kf[, se := NA_real_]; kf[uz == "PVREAD", se := apply(bs, 1, sd)[match(no, vec_f)]]
  KF[[paste(k, g)]] <- kf[, `:=`(gads = g, kopa = k)]
}
KF <- rbindlist(KF); fwrite(KF[, .(kopa, gads, no, uz, koef = round(koef, 3), se = round(se, 3), r2 = round(r2, 3))], "output/pisa2025/bn_koeficienti_fiksets.csv")
cat("\nLasīšanas vecāku koeficienti (SD vienībās, sāknēšanas SE) pie fiksētās struktūras:\n")
x <- KF[uz == "PVREAD"][, `:=`(v = sprintf("%+.3f (%.3f)", koef, se), etikete = LAB[no])]
print(dcast(x, etikete ~ kopa + gads, value.var = "v"), row.names = FALSE)
cat("\nLasīšanas R² pie fiksētās struktūras:\n"); print(dcast(KF[uz == "PVREAD", .(r2 = round(r2[1], 3)), by = .(kopa, gads)], kopa ~ gads, value.var = "r2"), row.names = FALSE)
cat("\nKoeficientu pārmaiņa LVA A 2022 → 2025 (z = starpība / sqrt(SE² + SE²)):\n")
z <- merge(x[kopa == "LVA A" & gads == "2022", .(no, k22 = koef, s22 = se)], x[kopa == "LVA A" & gads == "2025", .(no, k25 = koef, s25 = se)], by = "no")
z[, z := (k25 - k22) / sqrt(s22^2 + s25^2)]; print(z[order(-abs(z)), .(etikete = LAB[no], k2022 = round(k22, 3), k2025 = round(k25, 3), z = round(z, 2))], row.names = FALSE)
cat("\nKoeficientu starpība LVA A pret 7 valstīm 2025:\n")
z2 <- merge(x[kopa == "LVA A" & gads == "2025", .(no, kl = koef, sl = se)], x[kopa == "7 valstis" & gads == "2025", .(no, k7 = koef, s7 = se)], by = "no")
z2[, z := (kl - k7) / sqrt(sl^2 + s7^2)]; print(z2[order(-abs(z)), .(etikete = LAB[no], LVA_A = round(kl, 3), valstis7 = round(k7, 3), z = round(z, 2))], row.names = FALSE)
# Visu loku koeficientu stabilitāte laikā: korelācija starp cikliem
cat("\nVisu fiksētās struktūras loku koeficientu korelācija starp cikliem (LVA A):\n")
KW <- dcast(KF[kopa == "LVA A"], no + uz ~ gads, value.var = "koef"); print(round(cor(KW[, .(`2015`, `2018`, `2022`, `2025`)]), 3))

# === 6. INTERVENČU ILUSTRĀCIJA ===============================================
cat("\n", strrep("=", 90), "\n6. Intervenču ilustrācija (cpdist): novērojumu nosacījums pret do() mutilētā tīklā\n", strrep("=", 90), "\n", sep = "")
cat("ATRUNA: Bayesa tīkls no šķērsgriezuma novērojumu datiem nav cēloņu pierādījums. Virzieni starp līmeņiem ir pieņemti (laika\n")
cat("kārtība), virzieni līmeņa iekšienē ir vāji identificēti, un neizmērīti kopīgi cēloņi nav izslēgti. Skaitļi ir modeļa\n")
cat("implicētie \"kas notiktu, ja\", ne prognoze par reālu intervenci.\n\n")
SCEN <- list(L_GRAMATAS_MAJAS = 1, AUTO_HOMEPOS = 1, AUTO_ESCS = 1, S_KAVE_DIENU = 1, AUTO_TEACHSUP = 1, AUTO_BELONG = 1, K_PIED_DRAUGI = 1, AUTO_CLSIZE = 1, S_KLASE = 1)
INT <- list()
for (g in c("2015", "2018", "2022", "2025")) for (k in c("LVA A", "7 valstis")) {
  r <- REZ[[paste0(gsub(" ", "", k), "_", g)]]; f <- bn.fit(r$G, r$D$d)
  for (v in names(SCEN)) {
    set.seed(7); ev <- setNames(list(SCEN[[v]]), v)
    ob <- cpdist(f, nodes = "PVREAD", evidence = ev, method = "lw", n = 50000)
    e_ob <- weighted.mean(ob$PVREAD, attr(ob, "weights"))
    mu <- mutilated(f, ev); dv <- cpdist(mu, nodes = "PVREAD", evidence = TRUE, method = "ls", n = 50000); e_do <- mean(dv$PVREAD)
    INT[[length(INT) + 1]] <- data.table(kopa = k, gads = g, mainigais = v, etikete = LAB[v], vertiba_sd = SCEN[[v]],
                                         E_noverots_punkti = round(e_ob * r$D$sd, 1), E_do_punkti = round(e_do * r$D$sd, 1),
                                         tiess_koef_punkti = round(r$D$sd * ifelse(v %in% parents(r$G, "PVREAD"), f$PVREAD$coefficients[v], 0), 1))
  }
}
INT <- rbindlist(INT); fwrite(INT, "output/pisa2025/bn_intervences.csv")
cat("Gaidāmā lasīšanas rezultāta nobīde (punkti) pret vidējo, ja mainīgais ir +1 SD: novērots (E[Y|X]) / do (E[Y|do(X)]) / tikai tiešais loks.\n")
cat("Starpība starp novēroto un do parāda, cik no saistības iet caur mainīgā vecākiem (kopīgiem cēloņiem), ko do() pārtrauc.\n\n")
INT[, v := sprintf("%+.0f / %+.0f / %+.0f", E_noverots_punkti, E_do_punkti, tiess_koef_punkti)]
print(dcast(INT, etikete ~ kopa + gads, value.var = "v"), row.names = FALSE)

# === 7. JUTĪGUMA PĀRBAUDES ===================================================
cat("\n", strrep("=", 90), "\n7. Jutīgums: tabu, pc.stable, mmhc, BGe, diskrētais (terciļi), bez skolu anketas mainīgajiem\n", strrep("=", 90), "\n", sep = "")
cat("UZMANĪBU: šī sadaļa var aizņemt 10 minūtes.\n\n")
jacc <- function(s1, s2) { k1 <- s1[stiprums >= 0.5][, paste(pmin(a, b), pmax(a, b))]; k2 <- s2[stiprums >= 0.5][, paste(pmin(a, b), pmax(a, b))]
  length(intersect(k1, k2)) / length(union(k1, k2)) }
diskret <- function(x) {
  u <- sort(unique(x))
  if (length(u) >= 6) { q <- quantile(x, c(1/3, 2/3)); return(factor(cut(x, unique(c(-Inf, q, Inf)), labels = FALSE))) }   # terciļi
  # ordinālais ar ≤ 5 vērtībām: retos līmeņus (< 5 %) apvieno ar kaimiņu, augšējos apvieno, līdz paliek ≤ 3 līmeņi
  g <- match(x, u)
  repeat {
    tb <- prop.table(table(g)); lv <- as.integer(names(tb))
    if (length(lv) <= 2 || (length(lv) <= 3 && all(tb >= 0.05))) break
    i <- if (any(tb < 0.05)) which.min(tb) else length(lv)
    j <- if (i == length(lv)) i - 1 else i + 1
    g[g == lv[i]] <- lv[j]
  }
  factor(g)
}
JUT <- list()
jut_rinda <- function(kods, metode, st_alt, G_alt, B) {
  r <- REZ[[kods]]; vp <- parents(r$G, "PVREAD"); va <- parents(G_alt, "PVREAD")
  data.table(kopa = r$kopa, gads = r$gads, metode = metode, B = B, loki = narcs(G_alt), loki_hc = narcs(r$G), skeleta_jaccard = round(jacc(r$st, st_alt), 3),
             pvread_vecaki = paste(LAB[va], collapse = "; "), vecaki_sakrit = length(intersect(vp, va)), vecaki_tikai_hc = paste(LAB[setdiff(vp, va)], collapse = "; "),
             vecaki_tikai_alt = paste(LAB[setdiff(va, vp)], collapse = "; "))
}
for (kods in c("LVAA_2025", "LVAA_2022", "LVAA_2018", "LVAA_2015", "7valstis_2025", "7valstis_2022")) {
  r <- REZ[[kods]]; bl <- melnais(PAMATA)
  for (mt in c("tabu", "pc", "mmhc")) {
    Bm <- if (mt == "tabu") 200 else 100
    tk <- saknet(r$D$d, r$D$w, bl, B = Bm, metode = mt, seed = 300); s <- stiprumi(indik(tk, PAMATA), PAMATA)
    JUT[[paste(kods, mt)]] <- jut_rinda(kods, mt, s, konsenss(s, PAMATA), Bm)
  }
  if (grepl("LVAA_202", kods)) {
    tk <- saknet(r$D$d, r$D$w, bl, B = 200, score = "bge", seed = 301); s <- stiprumi(indik(tk, PAMATA), PAMATA)
    JUT[[paste(kods, "bge")]] <- jut_rinda(kods, "hc, BGe", s, konsenss(s, PAMATA), 200)
    dd <- as.data.frame(lapply(r$D$d, diskret)); tk <- saknet(dd, r$D$w, bl, B = 200, score = "bic", seed = 302); s <- stiprumi(indik(tk, PAMATA), PAMATA)
    JUT[[paste(kods, "disk")]] <- jut_rinda(kods, "hc, diskrēts (terciļi, BIC)", s, konsenss(s, PAMATA), 200)
    cat(sprintf("Diskrētais variants %s: līmeņu skaits pa mezgliem: %s\n", kods, paste(sapply(dd, nlevels), collapse = " ")))
  }
  if (kods == "LVAA_2025") {
    bez <- setdiff(PAMATA, c("AUTO_HISEI", "AUTO_CLSIZE", "AUTO_CREACTIV")); D2 <- suna("2025", "LVA A", bez)
    tk <- saknet(D2$d, D2$w, melnais(bez), B = 200, seed = 303); s <- stiprumi(indik(tk, bez), bez); G2 <- konsenss(s, bez)
    JUT[["bez"]] <- data.table(kopa = "LVA A", gads = "2025", metode = "hc bez HISEI, klases lieluma, radošajām aktivitātēm", B = 200, loki = narcs(G2), loki_hc = narcs(r$G),
                               skeleta_jaccard = round(jacc(r$st[a %in% bez & b %in% bez], s), 3), pvread_vecaki = paste(LAB[parents(G2, "PVREAD")], collapse = "; "),
                               vecaki_sakrit = length(intersect(parents(r$G, "PVREAD"), parents(G2, "PVREAD"))), vecaki_tikai_hc = paste(LAB[setdiff(parents(r$G, "PVREAD"), parents(G2, "PVREAD"))], collapse = "; "),
                               vecaki_tikai_alt = paste(LAB[setdiff(parents(G2, "PVREAD"), parents(r$G, "PVREAD"))], collapse = "; "))
    cat(sprintf("Bez skolu anketas mainīgajiem un HISEI: pilnie gadījumi %d no %d (%.0f %%)\n", D2$n_pilni, D2$n, 100 * D2$n_pilni / D2$n))
  }
}
JUT <- rbindlist(JUT); fwrite(JUT, "output/pisa2025/bn_jutigums.csv")
cat("\nJutīgums: skeleta Žakāra sakritība ar hc konsensu un lasīšanas vecāki:\n"); print(JUT, row.names = FALSE)

# === 8. SALĪDZINĀJUMS AR 69c EBICglasso ======================================
cat("\n", strrep("=", 90), "\n8. Salīdzinājums ar 69c EBICglasso tīklu (kopīgie mezgli, skelets)\n", strrep("=", 90), "\n\n", sep = "")
if (file.exists("output/pisa2025/tikls_malas.csv")) {
  E <- fread("output/pisa2025/tikls_malas.csv", colClasses = list(character = "gads")); S69 <- list()
  for (g in c("2015", "2018", "2022", "2025")) for (k in c("LVA A", "7 valstis")) {
    r <- REZ[[paste0(gsub(" ", "", k), "_", g)]]; e <- E[gads == g & kopa == k & no %in% PAMATA & uz %in% PAMATA]
    e69 <- paste(pmin(e$no, e$uz), pmax(e$no, e$uz)); s <- r$st[stiprums >= 0.5]; ebn <- paste(pmin(s$a, s$b), pmax(s$a, s$b))
    pv69 <- e[no == "PVREAD" | uz == "PVREAD"][, ifelse(no == "PVREAD", uz, no)]; pvbn <- parents(r$G, "PVREAD")
    S69[[paste(g, k)]] <- data.table(gads = g, kopa = k, malas_69c = length(e69), malas_bn = length(ebn), kopigas = length(intersect(e69, ebn)),
                                     jaccard = round(length(intersect(e69, ebn)) / length(union(e69, ebn)), 3),
                                     pvread_69c = paste(LAB[pv69], collapse = "; "), pvread_bn = paste(LAB[pvbn], collapse = "; "),
                                     pvread_kopigi = length(intersect(pv69, pvbn)), pvread_tikai_69c = paste(LAB[setdiff(pv69, pvbn)], collapse = "; "),
                                     pvread_tikai_bn = paste(LAB[setdiff(pvbn, pv69)], collapse = "; "))
  }
  S69 <- rbindlist(S69); fwrite(S69, "output/pisa2025/bn_69c_salidzinajums.csv")
  print(S69[, .(gads, kopa, malas_69c, malas_bn, kopigas, jaccard, pvread_kopigi, pvread_tikai_69c, pvread_tikai_bn)], row.names = FALSE)
} else cat("69c malu fails nav atrasts, salīdzinājums izlaists.\n")

# --- Noslēgums ---------------------------------------------------------------
cat("\nIzvades faili: output/pisa2025/bn_loki_*.csv, bn_kopsavilkums.csv, bn_markova_sega.csv, bn_salidzinajums.csv, bn_loku_mainas.csv,\n")
cat("bn_koeficienti_fiksets.csv, bn_intervences.csv, bn_jutigums.csv, bn_69c_salidzinajums.csv, plots/75_bn_*.png\n")
cat("Pabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
cat("Pilna izvade saglabāta:", output_file, "\n")
