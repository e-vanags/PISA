# ============================================================================
# 13. Sintētiskā kontrole: cik liels ir Latvijai specifiskais lasīšanas kritums
# ============================================================================
#
# Jautājums: cik daudz Latvija 2025. gadā nokrita vairāk, nekā prognozē tās
# pašas iepriekšējā trajektorija, salīdzinot ar citām valstīm?
#
# Metode: sintētiskā kontrole. Sintētiskā Latvija ir donorvalstu svērta
# kombinācija (svari >= 0, summa 1), kas vislabāk atkārto Latvijas lasīšanas
# rezultātus priekšperiodā. Starpība 2025. gadā ir efekta aplēse.
#
# Secinājumi balstās uz Abadie placebo permutācijām, nevis uz asimptotiku.
#
# Ievade:  data/pisa2025/processed/lasisana_trends_valstis.csv
# Izvade:  output/pisa2025/logs/13_sintetiska_kontrole_output.txt
#          output/pisa2025/plots/13_sintetiska_kontrole.png
#          output/pisa2025/sintetiska_kontrole_svari.csv
#
# Bez ārējām pakotnēm: simplekso ierobežojumu risina ar projicēto gradientu.
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs",  showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/plots", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/13_sintetiska_kontrole_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")


# === 1. KONFIGURĀCIJA ========================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("1. Konfigurācija\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

MERKA_VALSTS  <- "Latvia"
PRIEKSPERIODS <- c(2009, 2012, 2015, 2018, 2022)
PECPERIODS    <- 2025
IZSLEGT_KAROGOTAS <- FALSE   # TRUE = izmest valstis ar OECD atrunu (*)

cat("Mērķa valsts:  ", MERKA_VALSTS, "\n")
cat("Priekšperiods: ", paste(PRIEKSPERIODS, collapse = ", "), "\n")
cat("Pēcperiods:    ", PECPERIODS, "\n\n")


# === 2. DATI UN DONORU KOPA ==================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("2. Donoru kopa\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

d <- read.csv("data/pisa2025/processed/lasisana_trends_valstis.csv",
              encoding = "UTF-8", stringsAsFactors = FALSE)
d <- d[!is.na(d$lasisana) & d$lasisana != "", ]
d$lasisana <- as.numeric(d$lasisana)

vajadzigie <- c(PRIEKSPERIODS, PECPERIODS)
pilnas <- names(which(tapply(d$gads, d$valsts,
                            function(g) all(vajadzigie %in% g))))
if (IZSLEGT_KAROGOTAS) {
  karogotas <- unique(d$valsts[d$karogs == 1])
  pilnas <- setdiff(pilnas, setdiff(karogotas, MERKA_VALSTS))
}
stopifnot(MERKA_VALSTS %in% pilnas)
donori <- setdiff(pilnas, MERKA_VALSTS)

cat("Valstis ar pilnu sēriju:", length(pilnas), "\n")
cat("Donori (bez mērķa):     ", length(donori), "\n\n")

# Matrica: rindas = gadi, kolonnas = valstis
uz_matricu <- function(valstis, gadi) {
  m <- sapply(valstis, function(v)
    sapply(gadi, function(g) d$lasisana[d$valsts == v & d$gads == g][1]))
  matrix(m, nrow = length(gadi), dimnames = list(gadi, valstis))
}
Z0 <- uz_matricu(donori,      PRIEKSPERIODS)   # donori, priekšperiods
Z1 <- uz_matricu(MERKA_VALSTS, PRIEKSPERIODS)  # mērķis, priekšperiods


# === 3. SVARU ATRAŠANA =======================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("3. Svaru atrašana\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Projekcija uz simpleksu (Duchi et al. 2008) — eksakta, ne aproksimācija
proj_simplekss <- function(v) {
  u <- sort(v, decreasing = TRUE)
  cs <- cumsum(u) - 1
  rho <- max(which(u - cs / seq_along(u) > 0))
  pmax(v - cs[rho] / rho, 0)
}

# Projicētais gradients: min || z1 - Z0 w ||^2  pie  w >= 0, sum(w) = 1
sc_svari <- function(Z0, z1, iter = 20000) {
  n <- ncol(Z0)
  w <- rep(1 / n, n)
  L <- max(eigen(crossprod(Z0), only.values = TRUE)$values)  # Lipšica konstante
  step <- 1 / L
  for (i in seq_len(iter)) {
    grad <- crossprod(Z0, Z0 %*% w - z1)
    w_new <- proj_simplekss(w - step * grad)
    if (max(abs(w_new - w)) < 1e-12) { w <- w_new; break }
    w <- w_new
  }
  as.vector(w)
}

w <- sc_svari(Z0, as.vector(Z1))
names(w) <- donori

rmspe <- function(e) sqrt(mean(e^2))
fit_pirms <- rmspe(as.vector(Z1) - Z0 %*% w)

cat("Priekšperioda RMSPE:", round(fit_pirms, 3), "punkti\n\n")
cat("Svari virs 0.01:\n")
top <- sort(w[w > 0.01], decreasing = TRUE)
for (i in seq_along(top)) cat(sprintf("   %-26s %.3f\n", names(top)[i], top[i]))
cat("   (kopā", length(top), "valstis, svaru summa",
    round(sum(top), 3), ")\n\n")


# === 4. EFEKTS 2025. GADĀ ====================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("4. Efekts\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

Y0_pec <- uz_matricu(donori, PECPERIODS)
Y1_pec <- uz_matricu(MERKA_VALSTS, PECPERIODS)
sint_2025 <- as.vector(Y0_pec %*% w)
fakt_2025 <- as.vector(Y1_pec)
atstarpe  <- fakt_2025 - sint_2025

cat("Faktiskā Latvija 2025:  ", round(fakt_2025, 1), "\n")
cat("Sintētiskā Latvija 2025:", round(sint_2025, 1), "\n")
cat("ATSTARPE:               ", round(atstarpe, 1), "punkti\n\n")

visi_gadi <- c(PRIEKSPERIODS, PECPERIODS)
traj_f <- as.vector(uz_matricu(MERKA_VALSTS, visi_gadi))
traj_s <- as.vector(uz_matricu(donori, visi_gadi) %*% w)
cat("Trajektorijas:\n")
cat(sprintf("   %-8s %10s %10s %8s\n", "Gads", "Latvija", "Sintēt.", "Starp."))
for (i in seq_along(visi_gadi))
  cat(sprintf("   %-8s %10.1f %10.1f %8.1f\n",
              visi_gadi[i], traj_f[i], traj_s[i], traj_f[i] - traj_s[i]))
cat("\n")


# === 5. PLACEBO PERMUTĀCIJAS =================================================
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("5. Placebo permutācijas\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

placebo <- data.frame(valsts = character(), pirms = numeric(),
                      atstarpe = numeric(), attieciba = numeric(),
                      stringsAsFactors = FALSE)
for (v in donori) {
  citi <- setdiff(donori, v)
  Z0v <- uz_matricu(citi, PRIEKSPERIODS)
  z1v <- as.vector(uz_matricu(v, PRIEKSPERIODS))
  wv  <- sc_svari(Z0v, z1v)
  pv  <- rmspe(z1v - Z0v %*% wv)
  av  <- as.vector(uz_matricu(v, PECPERIODS)) -
         as.vector(uz_matricu(citi, PECPERIODS) %*% wv)
  placebo <- rbind(placebo, data.frame(valsts = v, pirms = pv,
                                       atstarpe = av, attieciba = abs(av) / pv))
}

lv_attieciba <- abs(atstarpe) / fit_pirms
cat("Latvijas pēc/pirms RMSPE attiecība:", round(lv_attieciba, 2), "\n\n")

# Standarta prakse: izmet placebo ar sliktu priekšperioda atbilstību
slieksnis <- 5 * fit_pirms
derigi <- placebo[placebo$pirms <= slieksnis, ]
cat("Placebo ar priekšfitu <= 5x Latvijas (", round(slieksnis, 2), "):",
    nrow(derigi), "no", nrow(placebo), "\n\n")

visi_att <- c(lv_attieciba, derigi$attieciba)
rangs <- sum(visi_att >= lv_attieciba)
p_vert <- rangs / length(visi_att)
cat("Latvijas rangs pēc attiecības:", rangs, "no", length(visi_att), "\n")
cat("Permutācijas p-vērtība:      ", round(p_vert, 4), "\n")
cat("(Mazākā sasniedzamā p ar šo donoru skaitu:",
    round(1 / length(visi_att), 4), ")\n\n")

cat("5 galējākās valstis pēc pēc/pirms attiecības:\n")
sal <- rbind(data.frame(valsts = MERKA_VALSTS, pirms = fit_pirms,
                        atstarpe = atstarpe, attieciba = lv_attieciba), derigi)
sal <- sal[order(-sal$attieciba), ]
for (i in 1:min(5, nrow(sal)))
  cat(sprintf("   %-26s atstarpe %7.1f   attiec. %5.2f\n",
              sal$valsts[i], sal$atstarpe[i], sal$attieciba[i]))


# === 6. SAGLABĀŠANA UN GRAFIKS ===============================================
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("6. Saglabāšana\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

write.csv(data.frame(valsts = names(w), svars = round(w, 5))[w > 0.001, ],
          "output/pisa2025/sintetiska_kontrole_svari.csv", row.names = FALSE)
write.csv(sal, "output/pisa2025/sintetiska_kontrole_placebo.csv", row.names = FALSE)

png("output/pisa2025/plots/13_sintetiska_kontrole.png",
    width = 1800, height = 900, res = 170)
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 3, 1))

plot(visi_gadi, traj_f, type = "b", pch = 19, lwd = 2, col = "#c2410c",
     ylim = range(c(traj_f, traj_s)) + c(-12, 12), xlab = "PISA cikls",
     ylab = "Lasīšanas vidējais punktu skaits",
     main = "Latvija pret sintētisko Latviju")
lines(visi_gadi, traj_s, type = "b", pch = 1, lwd = 2, lty = 2, col = "#1d4ed8")
abline(v = 2023.5, col = "grey60", lty = 3)
legend("bottomleft", c("Latvija", "Sintētiskā Latvija"), bty = "n",
       col = c("#c2410c", "#1d4ed8"), lty = c(1, 2), pch = c(19, 1), lwd = 2)

plot(NA, xlim = range(visi_gadi), ylim = c(-60, 60), xlab = "PISA cikls",
     ylab = "Atstarpe (valsts - sintētiskā)", main = "Placebo permutācijas")
for (v in derigi$valsts) {
  citi <- setdiff(donori, v)
  wv <- sc_svari(uz_matricu(citi, PRIEKSPERIODS),
                 as.vector(uz_matricu(v, PRIEKSPERIODS)))
  gv <- as.vector(uz_matricu(v, visi_gadi)) -
        as.vector(uz_matricu(citi, visi_gadi) %*% wv)
  lines(visi_gadi, gv, col = "#bbbbbb", lwd = 1)
}
lines(visi_gadi, traj_f - traj_s, col = "#c2410c", lwd = 3)
abline(h = 0, col = "grey40"); abline(v = 2023.5, col = "grey60", lty = 3)
legend("bottomleft", c("Latvija", "Placebo valstis"), bty = "n",
       col = c("#c2410c", "#bbbbbb"), lwd = c(3, 1))
dev.off()

cat("Grafiks: output/pisa2025/plots/13_sintetiska_kontrole.png\n")
cat("Svari:   output/pisa2025/sintetiska_kontrole_svari.csv\n")
cat("\nPabeigts:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
while (sink.number() > 0) sink()
