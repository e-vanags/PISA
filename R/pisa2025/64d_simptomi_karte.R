# ============================================================================
# 64d. Simptomu modelis, 1b. solis: saskaņotā konstruktu karte
# ============================================================================
#
# Ievade:
#   R/pisa2025/simptomi_karte.csv        ar roku saskaņotie konstrukti (arī vecie ↔
#                                         jaunie cikli ar atšķirīgiem nosaukumiem)
#   output/pisa2025/simptomi_inventars_*.csv
#
# Automātiski papildina: katrs skolēnu vai skolu anketas mainīgais, kam vismaz divos
# ciklos ir **vienāds nosaukums** un kas nav jau manuālajā kartē, kļūst par konstruktu
# "AUTO_<nosaukums>" (PISA 2015–2025 un 2000–2012 iekšienē nosaukumi atkārtojas).
# Mainīgie, kas ir tikai vienā ciklā, arī paliek (longitudinālajā modelī tie piedalās
# savā ciklā), bet konstrukts ir "VIENS_<gads>_<nosaukums>".
#
# Pārbaude: katra manuālās kartes rinda jāatrod inventārā; derīgo daļa Latvijā un
# atšķirīgo vērtību skaits, lai redzētu skalu atšķirības.
#
# Izvade: output/pisa2025/simptomi_karte_pilna.csv (konstrukts × gads × mainīgais)
#         output/pisa2025/logs/64d_simptomi_karte_output.txt
# ============================================================================

while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/64d_simptomi_karte_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

I <- rbind(
  utils::read.csv("output/pisa2025/simptomi_inventars_veci.csv", stringsAsFactors = FALSE)[,
    c("gads", "fails", "mainigais", "etikete", "lva_derigi_pct", "sal_derigi_pct", "lva_vertibu_skaits")],
  utils::read.csv("output/pisa2025/simptomi_inventars_jaunie.csv", stringsAsFactors = FALSE)[,
    c("gads", "fails", "mainigais", "etikete", "lva_derigi_pct", "sal_derigi_pct", "lva_vertibu_skaits")])
I$mainigais <- toupper(I$mainigais); I$gads <- as.character(I$gads)
NE <- "^(CNT|COUNTRY|CNTRYID|CNTSCHID|CNTSTUID|SCHOOLID|STIDSTD|SUBNATIO|STRATUM|OECD|NC|REGION|ADMINMODE|LANGTEST|BOOKID|CBASCI|OPTION|VER_|W_|PV[0-9]|SENWT|WVARSTR|UNIT$|STRATID|RDESIGN|RCORE|RS[12]_|CYC$|NATCEN|WLE|WLERR|WLEREAD)"
I$kandidats <- !grepl(NE, I$mainigais) & I$lva_derigi_pct >= 20 & I$lva_vertibu_skaits >= 2

M <- utils::read.csv("R/pisa2025/simptomi_karte.csv", stringsAsFactors = FALSE, colClasses = "character")
M$mainigais <- toupper(M$mainigais)
M$fails <- ifelse(grepl("^SK_", M$konstrukts), "SCH", "STU")   # SK_ konstrukti ir skolu anketā
m <- merge(M, I[, c("gads", "mainigais", "fails", "etikete", "lva_derigi_pct", "sal_derigi_pct", "lva_vertibu_skaits")],
           by = c("gads", "mainigais", "fails"), all.x = TRUE)
m$fails[is.na(m$etikete) & is.na(m$lva_derigi_pct)] <- NA        # neatrasts inventārā
nav <- m[is.na(m$fails), ]
cat("Manuālā karte:", length(unique(M$konstrukts)), "konstrukti,", nrow(M), "rindas\n")
cat("  neatrasti inventārā:", nrow(nav), "\n")
if (nrow(nav)) print(nav[, c("konstrukts", "gads", "mainigais")], row.names = FALSE)
maz <- m[!is.na(m$lva_derigi_pct) & m$lva_derigi_pct < 20, ]
if (nrow(maz)) { cat("  Latvijā derīgi < 20 %:\n"); print(maz[, c("konstrukts", "gads", "mainigais", "lva_derigi_pct")], row.names = FALSE) }
cat("\n  Manuālie konstrukti: gadi, derīgo daļa Latvijā, atšķirīgo vērtību skaits (skalas pārbaudei):\n")
for (k in unique(M$konstrukts)) { z <- m[m$konstrukts == k & !is.na(m$fails), ]; z <- z[order(z$gads), ]
  cat(sprintf("  %-28s %s\n", k, paste(sprintf("%s:%s(%s)", z$gads, round(z$lva_derigi_pct), z$lva_vertibu_skaits), collapse = " "))) }

# Automātiskā daļa
man <- paste(m$gads, m$mainigais)
A <- I[I$kandidats & !(paste(I$gads, I$mainigais) %in% man), ]
# Vienāds nosaukums vēl nenozīmē vienu jautājumu: 2000–2012 kodi (piem. ST24Q01)
# dažādos ciklos apzīmē dažādus jautājumus. Tāpēc vienā AUTO konstruktā paliek tikai
# cikli, kuru etiķete ir līdzīga pirmajai (vārdu Žakāra līdzība ≥ 0,4) vai kuru
# nosaukums ir 2015+ stila (…TA/NA/JA/HA/IA/DA/D01T u. c.) vai atvasināts indekss (bez "Q").
STOPV <- c("the", "a", "of", "to", "in", "and", "or", "your", "you", "do", "how", "what", "is", "are", "at", "with", "q", "wle")
tok <- function(s) { s <- tolower(gsub("<[^>]*>|\\[[^]]*\\]", " ", s)); s <- gsub("\\b(st|sc|ic|ec)[0-9]+[a-z0-9]*\\b|q[0-9]+[a-z]?", " ", s)
  w <- unique(strsplit(gsub("[^a-z ]", " ", s), " +")[[1]]); w[nchar(w) > 2 & !(w %in% STOPV)] }
jac <- function(a, b) if (!length(a) || !length(b)) 0 else length(intersect(a, b)) / length(union(a, b))
drošs_nos <- function(v) grepl("(TA|NA|JA|HA|IA|DA|JB|D0[0-9]T|WA)$", v) | !grepl("Q[0-9]", v)
A$grupa <- paste(A$fails, A$mainigais)
for (gr in unique(A$grupa[duplicated(A$grupa)])) {
  i <- which(A$grupa == gr); if (all(drošs_nos(A$mainigais[i]))) next
  i <- i[order(A$gads[i])]; t1 <- tok(A$etikete[i[1]])
  for (j in i[-1]) if (jac(t1, tok(A$etikete[j])) < 0.4) A$grupa[j] <- paste(gr, A$gads[j])
}
cnt <- table(A$grupa)
A$konstrukts <- ifelse(cnt[A$grupa] >= 2, paste0("AUTO_", sub("^(STU|SCH) ", "", A$grupa[match(A$grupa, A$grupa)])),
                       paste0("VIENS_", A$gads, "_", A$mainigais))
A$konstrukts <- gsub(" ", "_", A$konstrukts)
A$domens <- ifelse(A$fails == "SCH", "skola", "auto"); A$apraksts <- A$etikete; A$piezime <- ""
K <- rbind(
  data.frame(konstrukts = m$konstrukts, domens = m$domens, apraksts = m$apraksts, gads = m$gads, mainigais = m$mainigais,
             fails = ifelse(is.na(m$fails), "STU", m$fails), veids = "manuāls", lva_derigi_pct = m$lva_derigi_pct,
             piezime = m$piezime, stringsAsFactors = FALSE),
  data.frame(konstrukts = A$konstrukts, domens = A$domens, apraksts = A$apraksts, gads = A$gads, mainigais = A$mainigais,
             fails = A$fails, veids = ifelse(grepl("^AUTO_", A$konstrukts), "auto nosaukums", "viens cikls"),
             lva_derigi_pct = A$lva_derigi_pct, piezime = "", stringsAsFactors = FALSE))
K <- K[!is.na(K$lva_derigi_pct), ]
utils::write.csv(K, "output/pisa2025/simptomi_karte_pilna.csv", row.names = FALSE)

cat("\nPilnā karte:", nrow(K), "rindas,", length(unique(K$konstrukts)), "konstrukti\n")
kk <- stats::aggregate(gads ~ konstrukts + veids + fails, K, function(x) length(unique(x)))
print(table(veids = kk$veids, cikli = kk$gads))
cat("\n  Konstrukti ar vismaz 2 cikliem pa gadiem (cik no tiem ir katrā ciklā):\n")
vairak <- kk$konstrukts[kk$gads >= 2]
print(table(K$fails[K$konstrukts %in% vairak], K$gads[K$konstrukts %in% vairak]))
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
