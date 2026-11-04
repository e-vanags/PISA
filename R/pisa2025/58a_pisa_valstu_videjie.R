# 58a. PISA valstu vidējie 2000-2025 kohortu ekstrapolācijai (docs/57, F grupa)
#
# 2000-2022: CRAN pakotne learningtower (skolēnu līmeņa PISA dati visām valstīm,
# kā 41. skriptā). 2025: pilnais PISA 2025 skolēnu fails, 10 PV.
# Punktu aplēses bez SE; tās vajag tikai valstu grupas atskaites līmenim.
#
# Izvade: output/pisa2025/pisa_valstu_videjie.csv (gads, cnt, lasīšana, matemātika,
#         dabaszinātnes, n)

suppressMessages(library(learningtower))
d <- as.data.frame(load_student(year = "all"))
d <- d[, c("year", "country", "read", "math", "science", "stu_wgt")]
d$year <- as.character(d$year); d$country <- as.character(d$country)
cat("learningtower: gadi", paste(sort(unique(d$year)), collapse = " "), "| valstu kodi, piemēri:",
    paste(head(sort(unique(d$country)), 8), collapse = " "), "\n")
wm <- function(x, w) { i <- !is.na(x) & !is.na(w); if (!any(i)) NA_real_ else sum(x[i] * w[i]) / sum(w[i]) }
sp <- split(d, list(d$year, d$country), drop = TRUE)
lt <- do.call(rbind, lapply(sp, function(x) data.frame(
  gads = x$year[1], cnt = x$country[1], lasisana = wm(x$read, x$stu_wgt),
  matematika = wm(x$math, x$stu_wgt), dabaszinatnes = wm(x$science, x$stu_wgt), n = nrow(x))))

f25 <- file.path(Sys.getenv("PISA2025_SAV_DIR", unset = path.expand("~/pisa2025_sav")), "CY09_MS_STU_PUF.sav")
v <- c("CNT", "W_FSTUWT", paste0("PV", 1:10, "READ"), paste0("PV", 1:10, "MATH"), paste0("PV", 1:10, "SCIE"))
p <- haven::read_sav(f25, col_select = tidyselect::all_of(v))
p <- as.data.frame(lapply(p, function(x) if (is.character(x)) x else as.numeric(x)))
pvm <- function(x, jom) rowMeans(x[, paste0("PV", 1:10, jom)])
s25 <- split(p, p$CNT)
p25 <- do.call(rbind, lapply(s25, function(x) data.frame(
  gads = "2025", cnt = x$CNT[1], lasisana = wm(pvm(x, "READ"), x$W_FSTUWT),
  matematika = wm(pvm(x, "MATH"), x$W_FSTUWT), dabaszinatnes = wm(pvm(x, "SCIE"), x$W_FSTUWT), n = nrow(x))))
out <- rbind(lt, p25); rownames(out) <- NULL
utils::write.csv(out, "output/pisa2025/pisa_valstu_videjie.csv", row.names = FALSE)
cat("Valstis pa gadiem:\n"); print(table(out$gads))
cat("Latvija:\n"); print(out[out$cnt == "LVA", ], row.names = FALSE)
cat("Saglabāts: output/pisa2025/pisa_valstu_videjie.csv\n")
