# ============================================================================
# 47. Kāpēc ticamības intervāli mainās pa gadiem
# ============================================================================
#
# JAUTĀJUMS. Grafikā josla 2003.-2012. gadā ir platāka nekā no 2015. gada.
# Vai tas nozīmē, ka rezultātu izkliede sarūk, vai kaut ko citu?
#
# Iekavās tabulā ir STANDARTKĻŪDA (SE), nevis standartnovirze (SD). Josla ir
# vidējais +/- 1,96 * SE. SE ir vidējā aplēses nenoteiktība, nevis skolēnu
# rezultātu izkliede. Tā ir atkarīga no:
#   n     skolēnu skaita grupā
#   SD    rezultātu izkliedes grupā
#   deff  izlases dizaina efekta: skolēni ir sagrupēti skolās, tāpēc viena
#         skola nes mazāk informācijas nekā tikpat daudz nejauši izvēlētu
#         skolēnu. deff = (SE_izlases / SE_vienkāršai_izlasei)^2
#   M     ticamo vērtību skaita (5 vai 10), kas nosaka imputācijas daļu
#
# Šis skripts katram gadam un grupai sadala SE šajās daļās, lai redzētu,
# kura no tām izskaidro pārmaiņu.
#
# Ievade:  data/pisa_veci/lva_<gads>.csv, data/pisa2025/processed/a_grupa_pisa.rds,
#          output/pisa2025/verifikacija.csv
# Izvade:  output/pisa2025/ti_platums.csv
#          output/pisa2025/logs/47_ti_platums_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/47_ti_platums_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)

veca <- function(g) {
  d <- utils::read.csv(sprintf("data/pisa_veci/lva_%s.csv", g), colClasses = "character")
  list(pv = lapply(paste0("PV", 1:5, "READ"), function(p) as.numeric(d[[p]])),
       w = as.numeric(d$W_FSTUWT), skola = d$SCHOOLID, lang = trimws(d$LANGN))
}
J <- readRDS("data/pisa2025/processed/a_grupa_pisa.rds")
jauna <- function(g) {
  d <- J[[g]]; d <- d[as.character(d$CNT) == "LVA", ]
  list(pv = lapply(paste0("PV", 1:10, "READ"), function(p) as.numeric(d[[p]])),
       w = as.numeric(d$W_FSTUWT), skola = as.character(d$CNTSCHID),
       lang = as.character(as.numeric(d$LANGN)))
}
D <- list()
for (g in c("2003", "2006", "2009", "2012")) D[[g]] <- veca(g)
for (g in c("2015", "2018", "2022", "2025")) D[[g]] <- jauna(g)
grupa <- function(l, g) if (g == "2003") ifelse(l == "428001", "A", ifelse(l == "428002", "B", NA)) else
  ifelse(l == "325", "A", ifelse(l == "495", "B", NA))
wsd <- function(x, w) { m <- sum(w * x) / sum(w); sqrt(sum(w * (x - m)^2) / sum(w)) }

v <- read.csv("output/pisa2025/verifikacija.csv")
rez <- list()
cat(sprintf("%-5s %-5s %6s %6s %6s %7s %8s %8s %6s %6s %3s\n", "gads", "grupa", "n", "skolas",
            "SD", "SE", "SE_izl.", "SE_VNI", "deff", "gamma", "M"))
for (g in names(D)) {
  d <- D[[g]]; gr <- grupa(d$lang, g)
  for (k in c("A", "B", "visi")) {
    i <- if (k == "visi") rep(TRUE, length(gr)) else (!is.na(gr) & gr == k)
    sd <- mean(vapply(d$pv, function(p) wsd(p[i], d$w[i]), numeric(1)))
    n <- sum(i); ns <- length(unique(d$skola[i]))
    r <- v[v$gads == as.integer(g) & v$grupa == k, ]
    se_vni <- sd / sqrt(n)                       # vienkārša nejauša izlase
    deff <- (r$se_izlase / se_vni)^2
    cat(sprintf("%-5s %-5s %6d %6d %6.1f %7.2f %8.2f %8.2f %6.2f %6.3f %3d\n", g, k, n, ns,
                sd, r$se, r$se_izlase, se_vni, deff, r$gamma, r$M))
    rez[[length(rez) + 1]] <- data.frame(gads = g, grupa = k, n = n, skolas = ns, sd = sd,
      se = r$se, se_izlase = r$se_izlase, se_vni = se_vni, deff = deff, gamma = r$gamma, M = r$M)
  }
}
write.csv(do.call(rbind, rez), "output/pisa2025/ti_platums.csv", row.names = FALSE)
cat("\nIzveidots: output/pisa2025/ti_platums.csv\n")
while (sink.number() > 0) sink()
close(log_con)
