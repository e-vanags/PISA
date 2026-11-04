# 53a. Lasīšanas uzdevumi 2015 un 2018: Latvija, Igaunija, Lietuva (docs/51, 7. solis)
#
# Kopīgie vērtētie lasīšanas uzdevumi 2015. un 2018. gadā, ar atbildes laiku,
# un 2018. gada adaptīvā dizaina mainīgajiem. Igaunija un Lietuva ir salīdzinājumam:
# ja formāta maiņa uzdevumus padarīja vieglākus vai grūtākus visām valstīm, tas
# nav Latvijas skalas artefakts.
#
# NOSAUKUMI. Abos gados automātiski vērtētie ir CRxxxQyyS, vērtētāju kodētie
# DRxxxQyyC. Uzdevuma atslēga ir xxxQyy (CR...S var būt ar burtu beigās).
# DR...C tiek ņemti tikai bez burta pirms C, jo 2015. gadā DR219Q01AC utt. ir
# daudzdaļīga uzdevuma apakšdaļas. Abos gados vērtējums tiek pārdēvēts par
# CRxxxQyyS. Laiks ir CRxxxQyyT, "Timing of Last Visit". user_na = TRUE.
#
# Izvade: data/pisa2025/processed/uzdevumi_2015_2018.rds

suppressPackageStartupMessages(library(haven))

VALSTIS <- c("LVA", "EST", "LTU")
F <- c("2015" = "data/pisa2015/raw/CY6_MS_CMB_STU_COG.sav",
       "2018" = "data/pisa2018/raw/COG/CY07_MSU_STU_COG.sav")
N <- lapply(F, function(f) names(read_sav(f, n_max = 0)))

vertetie <- function(n) {
  k1 <- sub("^CR(.*)S$", "\\1", grep("^CR[0-9]{3}Q[0-9]{2}[A-Z]?S$", n, value = TRUE))
  k2 <- sub("^DR(.*)C$", "\\1", grep("^DR[0-9]{3}Q[0-9]{2}C$", n, value = TRUE))
  c(stats::setNames(paste0("CR", k1, "S"), k1),
    stats::setNames(paste0("DR", setdiff(k2, k1), "C"), setdiff(k2, k1)))
}
v15 <- vertetie(N[["2015"]]); v18 <- vertetie(N[["2018"]])
kop <- sort(intersect(names(v15), names(v18)))
uzd <- paste0("CR", kop)
cat("Kopīgi vērtēti uzdevumi:", length(kop),
    " (2015 DR...C:", sum(grepl("C$", v15[kop])), ", 2018 DR...C:", sum(grepl("C$", v18[kop])), ")\n")

lasa <- function(g, v, papildu = character()) {
  tt <- intersect(paste0(uzd, "T"), N[[g]])
  d <- read_sav(F[[g]], user_na = TRUE,
                col_select = tidyselect::all_of(c("CNT", "CNTSTUID", papildu, unname(v[kop]), tt)))
  d <- d[as.character(d$CNT) %in% VALSTIS, ]
  names(d)[match(v[kop], names(d))] <- paste0(uzd, "S")
  cat(g, ": rindas", nrow(d), " ar laiku", length(tt), "uzdevumi\n")
  d
}
d15 <- lasa("2015", v15)
d18 <- lasa("2018", v18, c("RDESIGN", "RCORE_TEST", "RCORE_PERF"))

saveRDS(list(d15 = d15, d18 = d18, uzd = uzd, avots15 = v15[kop], avots18 = v18[kop]),
        "data/pisa2025/processed/uzdevumi_2015_2018.rds")
cat("Saglabāts: data/pisa2025/processed/uzdevumi_2015_2018.rds\n")
