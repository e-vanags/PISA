# ============================================================================
# Kopīgās funkcijas uzdevumu līmeņa analīzei
# ============================================================================
#
# Kāpēc atsevišķs fails. Skriptos 23 līdz 27 atbilžu klasifikācija bija
# atkārtota, un tajā bija kļūda, kas jālabo visur vienādi.
#
# KĻŪDA, KAS IZLABOTA 2026-09-09. Sākotnējā klasifikācija skatījās uz
# etiķetes pirmo rakstzīmi: "^0" nozīmēja nepareizi, "^1" pareizi. Tas
# nedarbojas trīs gadījumos:
#
#   1. Daļēja kredīta uzdevumi. "1 - Partial credit" tika skaitīts kā pilnīgi
#      pareizs, lai gan tas ir puse punkta.
#   2. Divu punktu uzdevumi. "2 - Full credit" nesakrita ne ar "^0", ne "^1",
#      tāpēc pilnīgi pareiza atbilde tika izmesta kā trūkstoša.
#   3. Divciparu kodi. PISA lieto kodus kā "13 - Partial credit" un
#      "21 - Full credit", kur pirmais cipars ir punktu skaits. "13" sakrita
#      ar "^1" un tika skaitīts kā pareizs.
#
# Skāra 7 no 196 uzdevumiem un 1.4 % derīgo atbilžu.
#
# Turklāt 2018. gada failā etiķetes ir bez cipariem, proti, "Full credit" un
# "No credit", tāpēc ciparu pieeja tur nestrādā vispār.
#
# RISINĀJUMS. Klasifikācija pēc etiķetes TEKSTA, ne cipariem. Punkti tiek
# normalizēti uz nulli līdz vienu: pilns kredīts 1, daļējs 0.5, bez kredīta 0.
# Daļējs 0.5 ir precīzs, jo šajos datos maksimums ir divi punkti.
# ============================================================================

# Atgriež kategoriju katrai atbildei.
#   "pilns"       pilns kredīts
#   "dalejs"      daļējs kredīts
#   "nulle"       bez kredīta
#   "neatbildets" izlaista atbilde, PISA to vērtē kā nepareizu
#   "nesasniegts" uzdevums nav sasniegts, PISA to nevērtē
#   "nederigs"    tehniska kļūda
#   NA            uzdevums nav uzdots
klasificet <- function(x) {
  lab <- as.character(haven::as_factor(x))
  o <- rep(NA_character_, length(lab))
  o[grepl("No credit",      lab, ignore.case = TRUE)] <- "nulle"
  o[grepl("Partial credit", lab, ignore.case = TRUE)] <- "dalejs"
  o[grepl("Full credit",    lab, ignore.case = TRUE)] <- "pilns"
  o[grepl("No Response",    lab, ignore.case = TRUE)] <- "neatbildets"
  o[grepl("Not Reached",    lab, ignore.case = TRUE)] <- "nesasniegts"
  o[grepl("Invalid|Technical", lab, ignore.case = TRUE)] <- "nederigs"
  o
}

# Punkti nulle līdz viens. Neatbildētais ir nulle, jo PISA to vērtē.
PUNKTI <- c(pilns = 1, dalejs = 0.5, nulle = 0, neatbildets = 0,
            nesasniegts = 0, nederigs = NA)

punkti_matrica <- function(S) {
  P <- matrix(PUNKTI[S], nrow = nrow(S), ncol = ncol(S), dimnames = dimnames(S))
  P
}

# Atbildes laiks sekundēs. PISA trūkstošie laika kodi ir 99999997 un
# 99999999, un ar user_na = TRUE tie nonāk datos kā skaitļi.
tirs_laiks <- function(x) {
  v <- as.numeric(x)
  v[v <= 0 | v >= 1e7] <- NA
  v
}

# Vai atbilde tiek vērtēta, proti, vai tā ieiet pareizo atbilžu īpatsvarā.
# Nesasniegtie pēc noklusējuma neieiet, kā to dara PISA.
VERTETAS <- c("pilns", "dalejs", "nulle", "neatbildets")
VERTETAS_AR_NESASNIEGTIEM <- c(VERTETAS, "nesasniegts")
