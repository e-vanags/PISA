# ============================================================================
# 64b. Simptomu modelis, 1b. solis: mainīgo saskaņošana starp PISA cikliem
# ============================================================================
#
# Ievade: output/pisa2025/simptomi_inventars_veci.csv (2000–2012),
#         output/pisa2025/simptomi_inventars_jaunie.csv (2015–2025).
#
# Kandidāti: anketu mainīgie (ne identifikatori, svari, PV, bukleta un dizaina
# mainīgie), kas Latvijā derīgi vismaz 20 % skolēnu / skolu.
#
# Saskaņošana:
#   1. Vienāds nosaukums (atvasinātie indeksi: ESCS, HOMEPOS, JOYREAD, BELONG u. c.).
#   2. Etiķetes teksta līdzība: normalizēti vārdi (bez jautājuma kodiem, valsts
#      vietturiem <...>, stopvārdiem), Žakāra līdzība ≥ 0,55, tā pati faila grupa
#      (STU/SCH). Katram mainīgajam blakus ciklā tiek ņemts labākais pāris; ķēdes
#      caur cikliem veido konstruktu.
# Tas ir automātisks pirmais piegājiens; galvenie konstrukti pēc tam pārbaudāmi ar roku.
#
# Izvade: output/pisa2025/simptomi_katalogs.csv, simptomi_katalogs_kopsavilkums.csv
#         output/pisa2025/logs/64b_simptomi_katalogs_output.txt
# ============================================================================

while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/64b_simptomi_katalogs_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

GADI <- c("2000", "2003", "2006", "2009", "2012", "2015", "2018", "2022", "2025")
I <- rbind(
  utils::read.csv("output/pisa2025/simptomi_inventars_veci.csv", stringsAsFactors = FALSE, colClasses = c(gads = "character"))[,
    c("gads", "fails", "mainigais", "etikete", "lva_derigi_pct", "sal_derigi_pct", "lva_vertibu_skaits")],
  utils::read.csv("output/pisa2025/simptomi_inventars_jaunie.csv", stringsAsFactors = FALSE, colClasses = c(gads = "character"))[,
    c("gads", "fails", "mainigais", "etikete", "lva_derigi_pct", "sal_derigi_pct", "lva_vertibu_skaits")])
I$mainigais <- toupper(I$mainigais)
NE <- "^(CNT|COUNTRY|CNTRYID|CNTSCHID|CNTSTUID|SCHOOLID|STIDSTD|SUBNATIO|STRATUM|OECD|NC|REGION|ADMINMODE|LANGTEST|BOOKID|CBASCI|OPTION|VER_|W_|PV[0-9]|SENWT|WVARSTR|UNIT|STRATID|RDESIGN|RCORE|RS[12]_|EFFORT|TESTLANG|LANGN$|PRIVATESCH$|CYC$|NATCEN|SUBNAT|ISCEDO$)"
I$kandidats <- !grepl(NE, I$mainigais) & I$lva_derigi_pct >= 20 & I$lva_vertibu_skaits >= 2
cat("Mainīgie inventārā:", nrow(I), "| simptomu kandidāti:", sum(I$kandidats), "\n")
print(with(I[I$kandidats, ], table(gads, fails)))

STOP <- c("the", "a", "an", "of", "to", "in", "on", "for", "and", "or", "your", "you", "do", "does", "how", "what", "which",
          "is", "are", "this", "that", "at", "with", "by", "from", "as", "be", "have", "has", "q", "st", "sc", "i", "my",
          "me", "it", "about", "during", "last", "following", "these", "those", "derived", "index", "wle", "student", "school")
norm <- function(s) {
  s <- tolower(s); s <- gsub("<[^>]*>", " ", s); s <- gsub("\\b(st|sc|ec|ic|pa|q)[0-9]+[a-z0-9]*\\b", " ", s)
  s <- gsub("[^a-z ]", " ", s); w <- unique(strsplit(s, " +")[[1]]); w <- w[nchar(w) > 2 & !(w %in% STOP)]; w
}
# Etiķetes specifiskā daļa: vecajos cikos "Read Attitude - Only if I have to", jaunajos
# "... agree ...? - I read only if I have to." Salīdzina gan visu etiķeti, gan daļu aiz
# pēdējā " - ", ":" vai "?", un ņem lielāko līdzību.
aste <- function(s) { s2 <- sub(".*( - |: |\\? )", "", s); if (identical(s2, s)) "" else s2 }
K <- I[I$kandidats, ]; K$tok <- lapply(K$etikete, norm); K$tok_a <- lapply(vapply(K$etikete, aste, ""), norm)
jac <- function(a, b) if (!length(a) || !length(b)) 0 else length(intersect(a, b)) / length(union(a, b))
lidz <- function(a, b) {
  s1 <- jac(K$tok[[a]], K$tok[[b]])
  ta <- if (length(K$tok_a[[a]])) K$tok_a[[a]] else K$tok[[a]]; tb <- if (length(K$tok_a[[b]])) K$tok_a[[b]] else K$tok[[b]]
  s2 <- if (min(length(ta), length(tb)) >= 2) jac(ta, tb) else 0
  max(s1, s2)
}

# Pāri starp jebkuriem diviem cikliem (tā pati faila grupa). Katram mainīgajam ņem
# labāko pāri katrā citā ciklā; tuvākie cikli ir priekšroka pie vienādas līdzības.
pari <- list()
for (i in seq_len(length(GADI) - 1)) for (j in (i + 1):length(GADI)) for (ft in c("STU", "SCH")) {
  ia <- which(K$gads == GADI[i] & K$fails == ft); ib <- which(K$gads == GADI[j] & K$fails == ft)
  if (!length(ia) || !length(ib)) next
  for (a in ia) {
    if (K$mainigais[a] %in% K$mainigais[ib]) { b <- ib[match(K$mainigais[a], K$mainigais[ib])]; s <- 1; veids <- "nosaukums" } else {
      sm <- vapply(ib, function(b) lidz(a, b), 1); b <- ib[which.max(sm)]; s <- max(sm); veids <- "etikete" }
    if (s >= 0.6) pari[[length(pari) + 1]] <- data.frame(fails = ft, g1 = GADI[i], v1 = K$mainigais[a], g2 = GADI[j],
      v2 = K$mainigais[b], lidziba = s, attalums = j - i, veids = veids, stringsAsFactors = FALSE)
  }
}
pari <- do.call(rbind, pari)
# Katram mērķa mainīgajam katrā avota ciklā viens labākais pāris; tad globāli: katram
# mainīgajam ne vairāk kā viens pāris uz katru citu ciklu
pari <- pari[order(-pari$lidziba, pari$attalums), ]
pari <- pari[!duplicated(paste(pari$g1, pari$g2, pari$fails, pari$v2)), ]
pari <- pari[!duplicated(paste(pari$g1, pari$g2, pari$fails, pari$v1)), ]
utils::write.csv(pari, "output/pisa2025/simptomi_pari.csv", row.names = FALSE)
cat("\nPāri starp cikliem:", nrow(pari), " (pēc nosaukuma", sum(pari$veids == "nosaukums"), "; ne blakus cikliem",
    sum(pari$attalums > 1), ")\n")

# Ķēdes → konstrukti
K$id <- paste(K$gads, K$fails, K$mainigais)
sak <- stats::setNames(K$id, K$id)
atrod <- function(x) { while (sak[[x]] != x) x <- sak[[x]]; x }
for (r in seq_len(nrow(pari))) {
  a <- atrod(paste(pari$g1[r], pari$fails[r], pari$v1[r])); b <- atrod(paste(pari$g2[r], pari$fails[r], pari$v2[r]))
  if (a != b) sak[[b]] <- a
}
K$konstrukts <- vapply(K$id, atrod, "")
kat <- do.call(rbind, lapply(split(K, K$konstrukts), function(z) {
  z <- z[order(match(z$gads, GADI)), ]
  row <- data.frame(konstrukts = z$id[1], fails = z$fails[1], etikete = z$etikete[nrow(z)], cikli = length(unique(z$gads)),
                    no = z$gads[1], lidz = z$gads[nrow(z)], stringsAsFactors = FALSE)
  for (g in GADI) row[[paste0("v", g)]] <- paste(z$mainigais[z$gads == g], collapse = "|")
  row
}))
kat <- kat[order(-kat$cikli, kat$fails), ]
utils::write.csv(kat, "output/pisa2025/simptomi_katalogs.csv", row.names = FALSE)

cat("\nKonstrukti:", nrow(kat), "\n  pēc ciklu skaita:\n"); print(table(kat$fails, kat$cikli))
ks <- data.frame(
  periods = c("2018–2025 (3 cikli)", "2015–2025 (4)", "2009–2025", "2000–2025 (visi 9)"),
  STU = c(sum(kat$fails == "STU" & kat$v2018 != "" & kat$v2022 != "" & kat$v2025 != ""),
          sum(kat$fails == "STU" & kat$v2015 != "" & kat$v2018 != "" & kat$v2022 != "" & kat$v2025 != ""),
          sum(kat$fails == "STU" & kat$v2009 != "" & kat$v2025 != ""), sum(kat$fails == "STU" & kat$cikli == 9)),
  SCH = c(sum(kat$fails == "SCH" & kat$v2018 != "" & kat$v2022 != "" & kat$v2025 != ""),
          sum(kat$fails == "SCH" & kat$v2015 != "" & kat$v2018 != "" & kat$v2022 != "" & kat$v2025 != ""),
          sum(kat$fails == "SCH" & kat$v2009 != "" & kat$v2025 != ""), sum(kat$fails == "SCH" & kat$cikli == 9)))
cat("\n  Konstrukti, kas ir katrā norādītā perioda ciklā:\n"); print(ks, row.names = FALSE)
utils::write.csv(ks, "output/pisa2025/simptomi_katalogs_kopsavilkums.csv", row.names = FALSE)
cat("\n  Garākās ķēdes (skolēni), 40:\n")
print(utils::head(kat[kat$fails == "STU", c("cikli", "no", "lidz", "etikete")], 40), row.names = FALSE)
cat("\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
