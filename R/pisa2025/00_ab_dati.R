# ============================================================================
# 00. Kopīgais A/B datu ielādētājs, PISA 2003-2025 (docs/51, 0. posms)
# ============================================================================
#
# Visi korekciju skripti (49. un tālāk) ņem datus tikai no šejienes, lai plūsmu
# definīcija, svari un trūkstošo vērtību kodi būtu noteikti vienā vietā. Grupas
# un novērtējums sakrīt ar 43. un 44. skriptu; to pārbauda ab_validacija().
#
# LIETOŠANA
#   source("R/pisa2025/00_ab_dati.R")
#   D <- ab_dati()          saraksts pa gadiem "2003" ... "2025"
#   D[["2012"]]$sk          skolēnu mainīgie (data.frame)
#   D[["2012"]]$pv          plausible values, 5 vai 10 vektori
#   D[["2012"]]$rw          80 replikātsvaru vektori
#
# SKOLĒNU MAINĪGIE sk
#   id         skolēna ID: CNTSTUID (2015-2025), "SCHOOLID STIDSTD" (2003-2012)
#   grupa      "A" latviešu valodas plūsma, "B" krievu valodas plūsma pēc LANGN
#   w          galīgais skolēna svars W_FSTUWT
#   skola      skolas ID
#   dzimums    1 meitene, 2 zēns
#   klase      absolūtā klase, ST01Q01 (2003-2012) vai ST001D01T (2015-2025)
#   escs       ESCS kā failā. Skalas cikliem atšķiras, nesalīdzināt starp gadiem!
#   escs_tr    ESCS 2022. gada trenda skalā: 2012-2018 no escs_trend.csv, 2022
#              no faila; 2003-2009 NA, jo trenda skalas nav. 2025 no faila, BET
#              tas nav apstiprināts kā trenda skala, sk. zemāk
#
# ESCS 2025 (00a skalas pārbaude, output/pisa2025/escs_skalas_parbaude.csv).
# 2022. gadā ESCS = -3,416 + 0,01888 HISEI + 0,1692 PAREDINT + 0,4235 HOMEPOS
# precīzi (R2 = 1). Trenda failā 2012-2018 koeficienti pie HISEI un PAREDINT
# ir tie paši, tātad tas ir 2022. gada skalā. 2025. gadā -3,383 + 0,01933 HISEI
# + 0,1670 PAREDINT + 0,4278 HOMEPOS: formula atšķiras līdz 2,4 %, tātad indekss
# ir standartizēts no jauna, nevis piesaistīts 2022. gadam. Turklāt HISEI vidējais
# 2022 -> 2025 visās trijās Baltijas valstīs pieaug par 3-7 punktiem, bet HOMEPOS
# Latvijā krīt par 0,25; komponentu salīdzināmība nav pārbaudīta. Tāpēc
# ESCS_TRENDA_APSTIPRINATS["2025"] ir FALSE un 2025 lieto tikai jutīguma variantos.
ESCS_TRENDA_APSTIPRINATS <- c("2012" = TRUE, "2015" = TRUE, "2018" = TRUE,
                              "2022" = TRUE, "2025" = FALSE)
#   imig       IMMIG: 1 vietējais, 2 otrā paaudze, 3 pirmā paaudze
#   testa_val  "latviešu" / "krievu" / NA; reģistrēta tikai no 2015. gada
#   piepule1   EFFORT1, pašnovērtēta piepūle šajā testā, 1-10 (no 2015. gada)
#   piepule2   EFFORT2, piepūle, ja tests ietekmētu atzīmes, 1-10 (no 2015. gada)
#
# TRŪKSTOŠO VĒRTĪBU KODI vecajos ciklos tiek pārkodēti šeit:
#   ESCS >= 997, klase 96-99, IMMIG 7-9.  Jaunajos ciklos haven lietotāja
#   trūkstošos jau dod kā NA, bet ST001D01T kods 96 "Ungraded" ir derīga vērtība
#   un tiek pārkodēts šeit (2015-2022: 20-44 skolēni ciklā).
#
# KOREKCIJU IZVADES STANDARTS (docs/51): katrs korekcijas skripts atgriež
# korekcijas_rinda() rindas ar kolonnām
#   gads, grupa, korekcija, nobīde, apakšējā, augšējā, pieņēmumi
# kur nobīde ir punktos (koriģētais mīnus nekoriģētais), bet apakšējā un
# augšējā ir robežas, ja korekcija ir intervāls, nevis punkts.
# ============================================================================

VECIE  <- c("2003", "2006", "2009", "2012")
JAUNIE <- c("2015", "2018", "2022", "2025")

ab_grupa <- function(lang, gads) {
  if (gads == "2003") {
    ifelse(lang == "428001", "A", ifelse(lang == "428002", "B", NA))
  } else {
    ifelse(lang == "325", "A", ifelse(lang == "495", "B", NA))
  }
}

.kods_na <- function(x, no) { x[!is.na(x) & x >= no] <- NA; x }

.trends <- function() {
  t <- utils::read.csv("data/pisa2022/escs_trend/escs_trend.csv")
  t <- t[t$cnt == "LVA", c("cycle", "schoolid", "studentid", "escs_trend")]
  t$gads <- c("5" = "2012", "6" = "2015", "7" = "2018")[as.character(t$cycle)]
  t
}

.vecais <- function(g, TR) {
  d <- utils::read.csv(sprintf("data/pisa_veci/lva_%s.csv", g), colClasses = "character")
  num <- function(k) if (k %in% names(d)) as.numeric(d[[k]]) else rep(NA_real_, nrow(d))
  sk <- data.frame(
    gads = g, id = paste(trimws(d$SCHOOLID), trimws(d$STIDSTD)),
    grupa = ab_grupa(trimws(d$LANGN), g), w = num("W_FSTUWT"),
    skola = paste(g, trimws(d$SCHOOLID)), dzimums = num("DZIMUMS"),
    klase = .kods_na(num("ST01Q01"), 96), escs = .kods_na(num("ESCS"), 997),
    escs_tr = NA_real_, imig = .kods_na(num("IMMIG"), 7), testa_val = NA_character_,
    piepule1 = NA_real_, piepule2 = NA_real_, stringsAsFactors = FALSE)
  if (g == "2012") {
    k <- match(paste(as.numeric(d$SCHOOLID), as.numeric(d$STIDSTD)),
               with(TR[TR$gads == g, ], paste(schoolid, studentid)))
    sk$escs_tr <- TR$escs_trend[TR$gads == g][k]
  }
  list(gads = g, sk = sk,
       pv = lapply(paste0("PV", 1:5, "READ"), num),
       rw = lapply(paste0("W_FSTR", 1:80), num))
}

.jaunais <- function(g, KESA, PAP, TR) {
  d <- KESA[[g]]; d <- d[as.character(d$CNT) == "LVA", ]
  id <- as.numeric(d$CNTSTUID)
  if (g == "2025") {
    p <- readRDS("data/pisa2025/processed/pisa_lva_2025_stu.rds")
    names(p) <- toupper(names(p))
    p <- data.frame(CNTSTUID = as.numeric(p$CNTSTUID), ST001D01T = as.numeric(p$ST001D01T),
                    IMMIG = as.numeric(p$IMMIG),
                    LANGTEST_LAB = as.character(haven::as_factor(p$LANGTEST_COG)),
                    EFFORT1 = as.numeric(p$EFFORT1), EFFORT2 = as.numeric(p$EFFORT2))
  } else {
    p <- PAP[PAP$gads == g, ]
  }
  p <- p[match(id, p$CNTSTUID), ]
  ef <- function(k) { x <- if (k %in% names(p)) as.numeric(p[[k]]) else rep(NA_real_, nrow(p))
    x[!is.na(x) & (x < 1 | x > 10)] <- NA; x }
  tl <- ifelse(grepl("Latvian", p$LANGTEST_LAB), "latviešu",
               ifelse(grepl("Russian", p$LANGTEST_LAB), "krievu", NA))
  escs <- as.numeric(d$ESCS)
  escs_tr <- switch(g,
    "2022" = escs,
    "2025" = escs,
    { k <- match(paste(as.numeric(d$CNTSCHID) - 42800000, id - 42800000),
                 with(TR[TR$gads == g, ], paste(schoolid, studentid)))
      TR$escs_trend[TR$gads == g][k] })
  sk <- data.frame(
    gads = g, id = id, grupa = ab_grupa(as.character(as.numeric(d$LANGN)), g),
    w = as.numeric(d$W_FSTUWT), skola = paste(g, as.numeric(d$CNTSCHID)),
    dzimums = as.numeric(d$ST004D01T), klase = .kods_na(p$ST001D01T, 96), escs = escs,
    escs_tr = escs_tr, imig = p$IMMIG, testa_val = tl,
    piepule1 = ef("EFFORT1"), piepule2 = ef("EFFORT2"), stringsAsFactors = FALSE)
  list(gads = g, sk = sk,
       pv = lapply(paste0("PV", 1:10, "READ"), function(k) as.numeric(d[[k]])),
       rw = lapply(paste0("W_FSTURWT", 1:80), function(k) as.numeric(d[[k]])))
}

ab_dati <- function() {
  TR   <- .trends()
  KESA <- readRDS("data/pisa2025/processed/a_grupa_pisa.rds")
  PAP  <- readRDS("data/pisa2025/processed/ab_papildu_2015_2022.rds")
  D <- list()
  for (g in VECIE)  D[[g]] <- .vecais(g, TR)
  for (g in JAUNIE) D[[g]] <- .jaunais(g, KESA, PAP, TR)
  D
}

# Apakškopa: pv, w un rw tikai izvēlētajiem skolēniem.
ab_apakskopa <- function(d, i) {
  i[is.na(i)] <- FALSE
  list(gads = d$gads, sk = d$sk[i, ], pv = lapply(d$pv, `[`, i),
       rw = lapply(d$rw, `[`, i))
}

# Pilnā Rubina dispersija, BRR katrai PV (kā 44. skriptā). Fay 0,5 ar G = 80
# dod dalītāju 20. stat(x, w) ir jebkura svērta statistika.
rubin_brr <- function(pv, w, rw, stat) {
  M <- length(pv)
  e <- vapply(pv, function(p) stat(p, w), numeric(1))
  U <- mean(vapply(seq_len(M), function(m)
    sum((vapply(rw, function(r) stat(pv[[m]], r), numeric(1)) - e[m])^2) / 20,
    numeric(1)))
  B <- stats::var(e)
  c(est = mean(e), se = sqrt(U + (1 + 1 / M) * B),
    se_izlase = sqrt(U), se_imputacija = sqrt((1 + 1 / M) * B))
}

VID <- function(x, w) sum(x * w) / sum(w)

# ---------------------------------------------------------------------------
# Sastāva pārsvēršana (DiNardo-Fortin-Lemieux), kopīga 49. un 50. skriptam.
# Cikla vai apakšgrupas izlase (y = 1) un atsauces izlase (y = 0) katra sver
# kopā 1, tāpēc (1 - p) / p ir blīvumu attiecība f_atsauce(x) / f_izlase(x).
# Trūkstošs ESCS tiek aizpildīts ar escs_aizp un atzīmēts ar escs_nav.
# ---------------------------------------------------------------------------
kl_kat <- function(k) ifelse(is.na(k), "nav", ifelse(k <= 7, "7-",
                      ifelse(k >= 10, "10+", as.character(k))))
dz_kat <- function(d) ifelse(is.na(d), "nav", as.character(d))
kovar <- function(sk, escs_aizp) {
  e <- sk$escs_tr; nav <- is.na(e); e[nav] <- escs_aizp
  data.frame(escs = e, escs2 = e^2, escs_nav = as.numeric(nav),
             dz = dz_kat(sk$dzimums), kl = kl_kat(sk$klase),
             im = ifelse(is.na(sk$imig), "nav", ifelse(sk$imig == 1, "vietējais", "imigrants")),
             stringsAsFactors = FALSE)
}
dfl_faktors <- function(Xt, w, Xref, wref, ar_escs = TRUE) {
  dat <- rbind(Xt, Xref)
  dat$y <- c(rep(1, nrow(Xt)), rep(0, nrow(Xref)))
  ww <- c(w / sum(w), wref / sum(wref)); ww <- ww * length(ww)
  t <- c(if (ar_escs) c("escs", "escs2", "escs_nav"), "dz", "kl", "im")
  t <- t[vapply(t, function(k) length(unique(dat[[k]])) > 1, logical(1))]
  m <- suppressWarnings(stats::glm(stats::reformulate(t, response = "y"),
                                   family = stats::quasibinomial(), data = dat, weights = ww))
  p <- stats::fitted(m)[seq_len(nrow(Xt))]
  (1 - p) / p
}
n_eff <- function(w) sum(w)^2 / sum(w^2)

korekcijas_rinda <- function(gads, grupa, korekcija, nobide,
                             apaksa = NA_real_, augsa = NA_real_, pienemumi = "") {
  x <- data.frame(gads = gads, grupa = grupa, korekcija = korekcija, nobide = nobide,
                  apaksa = apaksa, augsa = augsa, pienemumi = pienemumi,
                  stringsAsFactors = FALSE)
  names(x) <- c("gads", "grupa", "korekcija", "nobīde", "apakšējā", "augšējā", "pieņēmumi")
  x
}

# Pārbaude: vidējie jāsakrīt ar output/pisa2025/verifikacija.csv (44. skripts),
# un sastāva mainīgo trūkstošo daļai jābūt saprātīgai.
ab_validacija <- function(D) {
  ver <- utils::read.csv("output/pisa2025/verifikacija.csv")
  out <- list()
  for (g in names(D)) for (gr in c("A", "B")) {
    s <- ab_apakskopa(D[[g]], D[[g]]$sk$grupa == gr)
    r <- rubin_brr(s$pv, s$sk$w, s$rw, VID)
    v <- ver[as.character(ver$gads) == g & ver$grupa == gr, ]
    tr <- function(k) round(100 * mean(is.na(s$sk[[k]])), 1)
    out[[length(out) + 1]] <- data.frame(
      gads = g, grupa = gr, n = nrow(s$sk), vid = r[["est"]], se = r[["se"]],
      vid_44 = if (nrow(v)) v$aplese[1] else NA, se_44 = if (nrow(v)) v$se[1] else NA,
      trukst_escs = tr("escs"), trukst_escs_tr = tr("escs_tr"), trukst_klase = tr("klase"),
      trukst_dzim = tr("dzimums"), trukst_imig = tr("imig"), trukst_testa_val = tr("testa_val"))
  }
  do.call(rbind, out)
}
