# ============================================================================
# 26. DIF: vai 2022. gada krievu testa versija bija vieglāka
# ============================================================================
#
# Jautājums. Galvenais rezultāts ir, ka Latvijas krieviski runājošo neapstrādāts
# pareizo atbilžu īpatsvars no 2022. līdz 2025. gadam krita par 13.9
# procentpunktiem, kamēr Igaunijas kontrolgrupa nokritās par 1.3. Šis
# secinājums balstās uz pieņēmumu, ka 2022. gada bāze ir godīga.
#
# Ja 2022. gada krievu valodas testa versija bija vieglāka par latviešu
# versiju, tad krieviski testēto 2022. gada rezultāts bija mākslīgi augsts un
# kritums ir pārvērtēts. Tas ir vienīgais atlikušais skaidrojums, kas var
# apgāzt stāstu, tāpēc tas jāpārbauda.
#
# Metode. Mantela un Hensela diferenciālā uzdevumu funkcionēšana. Katram
# uzdevumam salīdzina pareizo atbilžu varbūtību starp latviski un krieviski
# testētajiem, IZLĪDZINOT pēc kopējā snieguma. ETS delta klasifikācija:
#   |delta| < 1.0        A, nenozīmīga
#   1.0 <= |delta| < 1.5 B, vidēja
#   |delta| >= 1.5       C, liela
# Pozitīva delta nozīmē, ka uzdevums ir vieglāks krieviski testētajiem.
#
# FUNDAMENTĀLS IEROBEŽOJUMS. MH izlīdzina pēc kopējā snieguma, tāpēc tas
# atklāj tikai RELATĪVO DIF, proti, vai daži uzdevumi ir vieglāki vienā
# versijā salīdzinājumā ar pārējiem uzdevumiem. Ja visa versija būtu
# vienmērīgi vieglāka, MH to neredzētu. To sauc par DIF izlīdzināšanās
# problēmu, un tā nav atrisināma bez ārēja enkura.
#
# Ievade:  data/pisa2025/processed/uzdevumu_limenis.rds  (skripta 23 kešs)
#          data/pisa2022/processed/pisa_lva_2022_stu.rds (testa valoda)
# Izvade:  output/pisa2025/dif_2022.csv
#          output/pisa2025/logs/26_dif_2022_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/26_dif_2022_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

library(haven)

U <- readRDS("data/pisa2025/processed/uzdevumu_limenis.rds"); uzd <- U$uzd
klase <- function(x) {
  lab <- as.character(as_factor(x)); o <- rep(NA_character_, length(lab))
  o[grepl("^0", lab)] <- "nepareizi"; o[grepl("^1", lab)] <- "pareizi"
  o[grepl("No Response", lab)] <- "neatbildets"
  o[grepl("Not Reached", lab)] <- "nesasniegts"; o
}
d22 <- U$d22[as.character(U$d22$CNT) == "LVA", ]
S <- vapply(d22[paste0(uzd, "S")], klase, character(nrow(d22)))
colnames(S) <- uzd

# Vērtējams = 1 pareizi, 0 nepareizi vai neatbildēts. Nesasniegtie ārā, jo tie
# ir par testa garumu, ne par uzdevuma funkcionēšanu.
X <- matrix(NA_integer_, nrow(S), ncol(S), dimnames = dimnames(S))
X[S == "pareizi"] <- 1L
X[S %in% c("nepareizi", "neatbildets")] <- 0L

stu <- readRDS("data/pisa2022/processed/pisa_lva_2022_stu.rds")
tv <- as.character(as_factor(stu$LANGTEST_COG))[match(as.numeric(d22$CNTSTUID),
                                                      as.numeric(stu$CNTSTUID))]
lab <- as.character(as_factor(stu$LANGN))[match(as.numeric(d22$CNTSTUID),
                                                as.numeric(stu$CNTSTUID))]
grupa <- ifelse(tv == "Russian", "fokus", ifelse(tv == "Latvian", "atsauce", NA))

cat(strrep("=", 70), "\n1. Izlase\n", strrep("=", 70), "\n\n")
cat("Latvija 2022, uzdevumi:", ncol(X), "\n")
print(table(testa_valoda = tv, majas_valoda = substr(lab, 1, 8), useNA = "ifany"))


# === 2. IZLĪDZINĀŠANAS MAINĪGAIS ============================================
cat("\n", strrep("=", 70), "\n2. Izlīdzināšana pēc snieguma\n",
    strrep("=", 70), "\n\n")

# Katram skolēnam pareizo daļa no viņam uzdotajiem uzdevumiem, jo bukleti
# atšķiras. Pēc tam decilēs.
n_uzd <- rowSums(!is.na(X))
n_par <- rowSums(X == 1, na.rm = TRUE)
dala <- ifelse(n_uzd >= 10, n_par / n_uzd, NA)
der <- !is.na(grupa) & !is.na(dala)
cat("Derīgi skolēni:", sum(der), " no", nrow(X), "\n")

K <- 10
robezas <- quantile(dala[der], probs = seq(0, 1, length.out = K + 1), na.rm = TRUE)
robezas[1] <- -Inf; robezas[K + 1] <- Inf
strata <- cut(dala, robezas, labels = FALSE)
cat("Strata:", K, " skolēnu skaits katrā:\n")
print(table(strata[der], grupa[der]))


# === 3. MANTELA UN HENSELA DIF ==============================================
cat("\n", strrep("=", 70), "\n3. Mantela un Hensela DIF\n",
    strrep("=", 70), "\n\n")

mh <- function(x, g, s) {
  ok <- !is.na(x) & !is.na(g) & !is.na(s)
  x <- x[ok]; g <- g[ok]; s <- s[ok]
  num <- den <- 0; chi_num <- chi_den <- 0
  for (k in unique(s)) {
    i <- s == k
    A <- sum(x[i] == 1 & g[i] == "atsauce"); B <- sum(x[i] == 0 & g[i] == "atsauce")
    C <- sum(x[i] == 1 & g[i] == "fokus");   D <- sum(x[i] == 0 & g[i] == "fokus")
    N <- A + B + C + D
    if (N < 10 || (A + C) == 0 || (B + D) == 0) next
    num <- num + A * D / N
    den <- den + B * C / N
    nR <- A + B; nF <- C + D; n1 <- A + C
    chi_num <- chi_num + (A - nR * n1 / N)
    chi_den <- chi_den + nR * nF * n1 * (N - n1) / (N^2 * (N - 1))
  }
  if (den <= 0 || num <= 0) return(c(or = NA, delta = NA, chi = NA, n = length(x)))
  or <- num / den
  chi <- if (chi_den > 0) (abs(chi_num) - 0.5)^2 / chi_den else NA
  c(or = or, delta = -2.35 * log(or), chi = chi, n = length(x))
}

rez <- t(vapply(seq_len(ncol(X)),
                function(j) mh(X[, j], grupa, strata), numeric(4)))
rownames(rez) <- uzd
df <- data.frame(uzdevums = uzd, fluence = grepl("^CR590", uzd),
                 or = rez[, "or"], delta = rez[, "delta"],
                 chi = rez[, "chi"], n = rez[, "n"])
df$p <- pchisq(df$chi, 1, lower.tail = FALSE)
df$klase <- ifelse(is.na(df$delta), NA,
            ifelse(abs(df$delta) < 1.0, "A",
            ifelse(abs(df$delta) < 1.5, "B", "C")))
# ETS prasa arī nozīmību B un C klasei
df$klase[!is.na(df$p) & df$p > 0.05 & df$klase %in% c("B", "C")] <- "A"

cat("Uzdevumi ar aprēķinātu DIF:", sum(!is.na(df$delta)), "no", nrow(df), "\n\n")
cat("Pozitīva delta = uzdevums vieglāks krieviski testētajiem\n\n")
cat("ETS klasifikācija:\n")
print(table(df$klase, useNA = "ifany"))
cat("\nPa uzdevumu tipiem:\n")
print(table(ifelse(df$fluence, "fluence", "parastie"), df$klase))

cat("\nDelta sadalījums:\n")
print(round(summary(df$delta[!is.na(df$delta)]), 3))
cat("\nVidējā delta:", round(mean(df$delta, na.rm = TRUE), 3),
    " mediāna:", round(median(df$delta, na.rm = TRUE), 3), "\n")
poz <- sum(df$delta > 0, na.rm = TRUE); neg <- sum(df$delta < 0, na.rm = TRUE)
cat("Uzdevumi par labu krievu versijai:", poz,
    " par labu latviešu versijai:", neg, "\n")

cat("\n10 uzdevumi ar lielāko DIF par labu krievu versijai:\n")
o <- order(-df$delta); print(head(df[o, c("uzdevums","fluence","delta","p","klase")], 10),
                             row.names = FALSE, digits = 3)
cat("\n10 uzdevumi ar lielāko DIF par labu latviešu versijai:\n")
o <- order(df$delta); print(head(df[o, c("uzdevums","fluence","delta","p","klase")], 10),
                            row.names = FALSE, digits = 3)


# === 4. VAI DIF IR LĪDZSVAROTS ==============================================
cat("\n", strrep("=", 70), "\n4. Vai DIF ir līdzsvarots\n",
    strrep("=", 70), "\n\n")

cat("Ja DIF ir izkliedēts abos virzienos un vidējā delta ir tuvu nullei, tad\n")
cat("uzdevumu līmeņa nelīdzsvarotība lielā mērā izlīdzinās testa līmenī.\n")
cat("Ja tas sistemātiski sliecas uz vienu pusi, tas ir brīdinājums.\n\n")

t_test <- t.test(df$delta[!is.na(df$delta)])
cat(sprintf("Vidējā delta = %.3f, 95 %% TI [%.3f, %.3f], p = %.3f\n",
            mean(df$delta, na.rm = TRUE), t_test$conf.int[1], t_test$conf.int[2],
            t_test$p.value))
cat(sprintf("B vai C klases uzdevumi: %d no %d (%.1f %%)\n",
            sum(df$klase %in% c("B","C")), sum(!is.na(df$klase)),
            100 * mean(df$klase %in% c("B","C"), na.rm = TRUE)))

cat("\nATCERIETIES. Vienmērīga versijas atšķirība MH metodei nav redzama.\n")
cat("Šis tests atbild uz jautājumu, vai versijas ir NELĪDZSVAROTAS, nevis uz\n")
cat("jautājumu, vai viena no tām kopumā ir vieglāka.\n")

write.csv(df, "output/pisa2025/dif_2022.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/dif_2022.csv\n ", output_file,
    "\n\nPabeigts:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
