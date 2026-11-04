# ============================================================================
# 69d. Slānis 11: daudzlīmeņu sadalījums — cik no līmeņa un pārmaiņas ir starp skolām
# ============================================================================
#
# Katrai valstij (Latvijā A plūsmai) un ciklam 2003–2025: lasīšanas PV dispersijas
# sadalījums starp skolām un skolu iekšienē (lme4, tukšais modelis, PV vidējais, bez
# svariem ICC; svērtais starp skolu SD ar skolu svērtajiem vidējiem). Pārmaiņa 2022 → 2025
# LVA A: vai kritums ir vienmērīgs pa skolām (skolu vidējo sadalījuma nobīde pret
# izplešanos): skolu vidējo P10/P50/P90; skolu daļa, kurās vidējais zem 2. līmeņa robežas.
# Skolu ID starp cikliem nav sasaistāmi, tāpēc paneļa nav.
#
# Izvade: output/pisa2025/daudzlimenu_icc.csv, daudzlimenu_skolu_sadalijums.csv
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(lme4) })
while (sink.number() > 0) sink()
output_file <- "output/pisa2025/logs/69d_daudzlimenu_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8"); sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
A <- readRDS("data/pisa2025/processed/simptomi_apvienots.rds")[!is.na(PVREAD) & !is.na(w) & w > 0 & gads != "2000"]
A[, grupa := ifelse(cnt == "LVA", paste("LVA", fifelse(is.na(plusma), "?", plusma)), cnt)]
G <- c("LVA A", "LVA B", "CZE", "DEU", "EST", "FIN", "HUN", "LTU", "POL")
wq <- function(x, w, p) { o <- order(x); cw <- cumsum(w[o]) / sum(w); x[o][which(cw >= p)[1]] }
ICC <- A[grupa %in% G, {
  m <- lmer(PVREAD ~ 1 + (1 | skola), REML = TRUE); v <- as.data.frame(VarCorr(m)); vb <- v$vcov[1]; vw <- v$vcov[2]
  sk <- .SD[, .(vid = sum(w * PVREAD) / sum(w), sw = sum(w), n = .N), by = skola][n >= 10]
  .(icc = vb / (vb + vw), sd_starp = sqrt(vb), sd_ieks = sqrt(vw), skolas = nrow(sk), skolu_vid_p10 = wq(sk$vid, sk$sw, .1), skolu_vid_p50 = wq(sk$vid, sk$sw, .5),
    skolu_vid_p90 = wq(sk$vid, sk$sw, .9), skolu_dala_zem_L2 = sum(sk$sw[sk$vid < 407.47]) / sum(sk$sw), n = .N)
}, by = .(gads, grupa)][order(grupa, gads)]
fwrite(ICC, "output/pisa2025/daudzlimenu_icc.csv")
cat("ICC (starp skolu dispersijas daļa) pa ciklu un grupu:\n"); print(dcast(ICC, grupa ~ gads, value.var = "icc", fun.aggregate = function(x) if (length(x)) round(x[1], 3) else NA_real_), row.names = FALSE)
cat("\nStarp skolu SD:\n"); print(dcast(ICC, grupa ~ gads, value.var = "sd_starp", fun.aggregate = function(x) if (length(x)) round(x[1], 1) else NA_real_), row.names = FALSE)
cat("\nLVA A: skolu svērto vidējo P10 / P50 / P90 un skolu daļa ar vidējo zem 2. līmeņa\n")
print(ICC[grupa == "LVA A", .(gads, skolas, icc = round(icc, 3), sd_starp = round(sd_starp, 1), sd_ieks = round(sd_ieks, 1), p10 = round(skolu_vid_p10), p50 = round(skolu_vid_p50), p90 = round(skolu_vid_p90), dala_zem_L2 = round(100 * skolu_dala_zem_L2, 1))], row.names = FALSE)
# Pārmaiņa 2022 → 2025 LVA A: skolu vidējo sadalījums
x <- ICC[grupa == "LVA A" & gads %in% c("2022", "2025")]
cat(sprintf("\nLVA A 2022 → 2025: skolu vidējo P10 %+.0f, P50 %+.0f, P90 %+.0f; skolu daļa zem 2. līmeņa %.0f → %.0f %%; starp skolu SD %.0f → %.0f; iekšējā SD %.0f → %.0f\n",
            diff(x$skolu_vid_p10), diff(x$skolu_vid_p50), diff(x$skolu_vid_p90), 100 * x$skolu_dala_zem_L2[1], 100 * x$skolu_dala_zem_L2[2], x$sd_starp[1], x$sd_starp[2], x$sd_ieks[1], x$sd_ieks[2]))
cat("  Interpretācija: ja P10, P50, P90 krīt līdzīgi, kritums ir vienmērīgs pa skolām; ja P10 vairāk, vājās skolas krīt vairāk.\n")
cat("\nPabeigts:", format(Sys.time()), "\n"); while (sink.number() > 0) sink(); close(log_con)
