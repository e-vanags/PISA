# 59a. PISA 2015 uzdevumi un PV salīdzināmajām valstīm ķēdes posmam 2012 -> 2015
#      (docs/57, E grupa)
#
# 2012. gada vērtētie lasīšanas uzdevumi (papīrs, PRxxxQyy) 2015. gadā ir
# datorā kā CRxxxQyyS vai DRxxxQyyC. 2012. gada kopējie vērtējumi ar "T" galotni
# (piem., PR404Q07T) 2015. gadā ir bez tās. Valstis kā py/kog_veci_izvilkums.py.
#
# Izvade: data/pisa2025/processed/uzdevumi_2015_salid.rds
#         list(kog = uzdevumi, stu = PV un svari, karte = 2012 atslēga -> 2015 mainīgais)

suppressPackageStartupMessages(library(haven))
VALSTIS <- c("LVA", "EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")
k12 <- names(utils::read.csv("data/pisa_veci/kog_2012.csv", nrows = 1, check.names = FALSE))
k12 <- sub("^P?R", "", grep("^P?R[0-9]{3}Q", k12, value = TRUE))
fc <- "data/pisa2015/raw/CY6_MS_CMB_STU_COG.sav"
nc <- names(read_sav(fc, n_max = 0))
karte <- vapply(k12, function(k) {
  kand <- c(paste0("CR", k, "S"), paste0("DR", k, "C"),
            paste0("CR", sub("T$", "", k), "S"), paste0("DR", sub("T$", "", k), "C"))
  kand <- kand[kand %in% nc]; if (length(kand)) kand[1] else NA_character_
}, "")
cat("2012 uzdevumi:", length(k12), " atrasti 2015:", sum(!is.na(karte)), "\n")
kog <- read_sav(fc, user_na = TRUE, col_select = tidyselect::all_of(c("CNT", "CNTSTUID", unname(karte[!is.na(karte)]))))
kog <- kog[as.character(kog$CNT) %in% VALSTIS, ]
fs <- "data/pisa2015/raw/CY6_MS_CMB_STU_QQQ.sav"
v <- c("CNT", "CNTSTUID", "LANGN", "W_FSTUWT", paste0("PV", 1:10, "READ"), paste0("W_FSTURWT", 1:80))
stu <- read_sav(fs, col_select = tidyselect::all_of(v))
stu <- stu[as.character(stu$CNT) %in% VALSTIS, ]
stu <- as.data.frame(lapply(stu, function(x) if (is.character(x)) x else as.numeric(x)))
saveRDS(list(kog = kog, stu = stu, karte = karte), "data/pisa2025/processed/uzdevumi_2015_salid.rds")
print(table(as.character(kog$CNT)))
cat("Saglabāts: data/pisa2025/processed/uzdevumi_2015_salid.rds\n")
