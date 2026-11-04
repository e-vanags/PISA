# ============================================================================
# 43. A un B grupas lasītprasmes rinda, PISA 2003-2025
# ============================================================================
#
# Pirmā reize, kad latviešu un krievu valodas plūsmu var izsekot PISA datos visā
# pieejamajā vēsturē. Līdz šim `docs/40` rinda sākās 2015. gadā, jo vecie
# mikrodati nebija. Tagad tie ir, sk. `data/pisa_veci/`.
#
# KĀPĒC NE NO 2000. GADA. PISA 2000 failā plūsmas mainīgā NAV VISPĀR:
#   nav LANGN, nav PROGN, nav testa valodas, SUBNATIO visiem ir "00",
#   stratifikācijas mainīgā nav, un skolu failā valodas nav.
# Vienīgais ir ST17Q01 ar kategorijām "<Test language>" un "<Other official
# languages>", kas pasaka, vai skolēns mājās runā testa valodā, bet NE kurā
# valodā viņš mācās. Tāpēc 2000. gadam ir tikai valsts rādītājs.
#
# GRUPU DEFINĪCIJA. Tikai pēc mājās runātās valodas `LANGN`, jo:
#   - `PROGN` vecajos ciklos mazākumtautību programmu NENOŠĶIR: 2003.-2012.
#     gadā gandrīz visi ir vienā kodā "pamatizglītība". Mazākumtautību kods
#     parādās tikai no PISA 2018, tāpat kā `docs/40` konstatēja par 2015. gadu
#   - `docs/40` pārbaudīja, ka 2018.-2025. gadā abas definīcijas atšķiras ne
#     vairāk kā par 1,2 punktiem
#
# KODU DEKODĒŠANA, pārbaudīta pret PISA 2015 failu, kur ir etiķetes:
#   325 = latviešu, 495 = krievu, no 2006. gada līdz 2025. gadam
#   2003. gadā formāts ir cits: 428001 un 428002, kur 001 ir valsts galvenā
#   valoda. Skripts to pārbauda pēc grupu lielumiem un nepārtrauktības ar
#   2006. gadu, un izdrukā pārbaudi 1. sadaļā.
#
# METODE. BRR ar Fay 0.5, dalītājs 20. Vecajos ciklos replikātsvari saucas
# W_FSTR1 līdz W_FSTR80, jaunajos W_FSTURWT1 līdz W_FSTURWT80. Vecajos ir
# 5 plausible values, jaunajos 10; Rubina korekcija to ņem vērā.
#
# Izvade:  output/pisa2025/ab_rinda_no_2003.csv
#          output/pisa2025/logs/43_ab_rinda_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/43_ab_rinda_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

library(haven)
L2 <- 407.47

# ---------------------------------------------------------------------------
# Vecie cikli no izvilkuma CSV
# ---------------------------------------------------------------------------
veca <- function(g) {
  d <- utils::read.csv(sprintf("data/pisa_veci/lva_%s.csv", g),
                       colClasses = "character")
  pv <- paste0("PV", 1:5, "READ")
  rw <- paste0("W_FSTR", 1:80)
  list(gads = g,
       pv = lapply(pv, function(p) as.numeric(d[[p]])),
       w  = as.numeric(d$W_FSTUWT),
       rw = lapply(rw, function(p) as.numeric(d[[p]])),
       lang = trimws(d$LANGN))
}

# ---------------------------------------------------------------------------
# Jaunie cikli no jau sagatavotā keša
# ---------------------------------------------------------------------------
JAUNIE <- readRDS("data/pisa2025/processed/a_grupa_pisa.rds")
jauna <- function(g) {
  d <- JAUNIE[[g]]; d <- d[as.character(d$CNT) == "LVA", ]
  pv <- paste0("PV", 1:10, "READ")
  rw <- paste0("W_FSTURWT", 1:80)
  list(gads = g,
       pv = lapply(pv, function(p) as.numeric(d[[p]])),
       w  = as.numeric(d$W_FSTUWT),
       rw = lapply(rw, function(p) as.numeric(d[[p]])),
       lang = as.character(as.numeric(d$LANGN)))
}

D <- list()
for (g in c("2003", "2006", "2009", "2012")) D[[g]] <- veca(g)
for (g in c("2015", "2018", "2022", "2025")) D[[g]] <- jauna(g)

# Latviešu un krievu kodi. 2003. gadā formāts ir valsts prefikss plus kārtas
# numurs, no 2006. gada tas ir starptautiskais valodas kods.
grupa <- function(lang, gads) {
  if (gads == "2003") {
    ifelse(lang == "428001", "A", ifelse(lang == "428002", "B", NA))
  } else {
    ifelse(lang == "325", "A", ifelse(lang == "495", "B", NA))
  }
}

cat(strrep("=", 78), "\n1. Grupu lielumi un kodu pārbaude\n", strrep("=", 78), "\n\n")
cat(sprintf("  %-6s %8s %10s %10s %10s %10s\n", "gads", "kopā",
            "A latviešu", "%", "B krievu", "%"))
for (g in names(D)) {
  gr <- grupa(D[[g]]$lang, g); n <- length(gr)
  cat(sprintf("  %-6s %8d %10d %9.1f %% %10d %9.1f %%\n", g, n,
              sum(gr == "A", na.rm = TRUE), 100 * mean(gr == "A", na.rm = TRUE),
              sum(gr == "B", na.rm = TRUE), 100 * mean(gr == "B", na.rm = TRUE)))
}
cat("\n  Krievu valodas plūsmas īpatsvaram jāsarūk pakāpeniski. Ja 2003. gada kodi būtu\n",
    " apmainīti vietām, tur būtu lēciens. Pārbaude: 2003 -> 2006 pāreja.\n")

# ---------------------------------------------------------------------------
brr <- function(x, gads, fun) {
  d <- D[[gads]]
  if (sum(x, na.rm = TRUE) < 30) return(c(est = NA, se = NA, n = 0))
  x[is.na(x)] <- FALSE
  e <- vapply(d$pv, function(p) fun(p[x], d$w[x]), numeric(1))
  rv <- vapply(d$rw, function(r) fun(d$pv[[1]][x], r[x]), numeric(1))
  M <- length(d$pv)
  c(est = mean(e),
    se = sqrt(sum((rv - e[1])^2) / 20 + (1 + 1 / M) * stats::var(e)),
    n = sum(x))
}
VID <- function(x, w) sum(x * w) / sum(w)
ZEM <- function(x, w) 100 * sum(w * (x < L2)) / sum(w)
P10 <- function(x, w) { o <- order(x); x <- x[o]; w <- w[o]
  stats::approx((cumsum(w) - 0.5 * w) / sum(w), x, xout = 0.10, rule = 2,
                ties = "ordered")$y }

rez <- list()
for (g in names(D)) {
  gr <- grupa(D[[g]]$lang, g)
  for (gn in c("A", "B", "visi")) {
    i <- if (gn == "visi") rep(TRUE, length(gr)) else (!is.na(gr) & gr == gn)
    a <- brr(i, g, VID); z <- brr(i, g, ZEM); p <- brr(i, g, P10)
    rez[[length(rez) + 1]] <- data.frame(
      gads = g, grupa = gn, vid = unname(a["est"]), vid_se = unname(a["se"]),
      zem = unname(z["est"]), zem_se = unname(z["se"]),
      p10 = unname(p["est"]), p10_se = unname(p["se"]), n = unname(a["n"]))
  }
}
df <- do.call(rbind, rez)

NOS <- c(A = "A. Latviešu valodas plūsma", B = "B. Krievu valodas plūsma", visi = "Visi")
cat("\n", strrep("=", 78), "\n2. Vidējie sasniegumi lasīšanā\n", strrep("=", 78), "\n\n")
cat(sprintf("  %-20s %s\n", "grupa",
            paste(sprintf("%14s", names(D)), collapse = "")))
for (gn in c("A", "B", "visi")) {
  x <- df[df$grupa == gn, ]
  cat(sprintf("  %-20s %s\n", NOS[[gn]],
              paste(sprintf("%14s", sprintf("%.1f (%.1f)", x$vid, x$vid_se)),
                    collapse = "")))
}

cat("\n", strrep("=", 78), "\n3. Pārmaiņas starp cikliem\n", strrep("=", 78), "\n\n")
for (gn in c("A", "B")) {
  x <- df[df$grupa == gn, ]
  cat(sprintf("  %-20s", NOS[[gn]]))
  for (i in 2:nrow(x)) {
    d <- x$vid[i] - x$vid[i - 1]; s <- sqrt(x$vid_se[i]^2 + x$vid_se[i - 1]^2)
    cat(sprintf(" %s%+6.1f%s", substr(x$gads[i], 3, 4), d,
                if (!is.na(d) && abs(d / s) > 1.96) "*" else " "))
  }
  cat("\n")
}
cat("\n  Uzkrātā pārmaiņa pret 2003. gadu\n\n")
for (gn in c("A", "B")) {
  x <- df[df$grupa == gn, ]
  cat(sprintf("  %-20s", NOS[[gn]]))
  for (i in 2:nrow(x)) {
    d <- x$vid[i] - x$vid[1]; s <- sqrt(x$vid_se[i]^2 + x$vid_se[1]^2)
    cat(sprintf(" %s%+7.1f%s", substr(x$gads[i], 3, 4), d,
                if (!is.na(d) && abs(d / s) > 1.96) "*" else " "))
  }
  cat("\n")
}

cat("\n", strrep("=", 78), "\n4. Starpība starp plūsmām\n", strrep("=", 78), "\n\n")
cat(sprintf("  %-8s %12s %12s %16s\n", "gads", "A", "B", "A mīnus B"))
for (g in names(D)) {
  a <- df[df$gads == g & df$grupa == "A", ]
  b <- df[df$gads == g & df$grupa == "B", ]
  d <- a$vid - b$vid; s <- sqrt(a$vid_se^2 + b$vid_se^2)
  cat(sprintf("  %-8s %12.1f %12.1f %+12.1f (%.1f)%s\n", g, a$vid, b$vid, d, s,
              if (!is.na(d) && abs(d / s) > 1.96) "*" else " "))
}

cat("\n", strrep("=", 78), "\n5. Apakšgals: zem 2. līmeņa un P10\n", strrep("=", 78), "\n\n")
for (gn in c("A", "B")) {
  x <- df[df$grupa == gn, ]
  cat(sprintf("  %-20s zem 2. līm. %s\n", NOS[[gn]],
              paste(sprintf("%12s", sprintf("%.1f %%", x$zem)), collapse = "")))
  cat(sprintf("  %-20s P10         %s\n", "",
              paste(sprintf("%12.1f", x$p10), collapse = "")))
}
cat(sprintf("\n  %-20s %s\n", "", paste(sprintf("%12s", names(D)), collapse = "")))

write.csv(df, "output/pisa2025/ab_rinda_no_2003.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/ab_rinda_no_2003.csv\n ", output_file,
    "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
