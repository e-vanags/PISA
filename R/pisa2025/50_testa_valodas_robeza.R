# ============================================================================
# 50. Testa valodas robeža B plūsmai, PISA 2015-2025 (docs/51, 4. solis)
# ============================================================================
#
# JAUTĀJUMS. Cik lielu daļu no krievu valodas plūsmas (B) pārmaiņām var radīt
# tas, ka mainījās testa valoda? 2015-2022 aptuveni ceturtā daļa B plūsmas
# pildīja testu latviski, 2025. gadā visi (46. skripts).
#
# KĀPĒC TIKAI ROBEŽA. Testa valodas "sods" skolēnam, kurš mācījies krieviski un
# pilda testu latviski, datos nav identificējams: 2015-2022 latviski testētie B
# skolēni pārsvarā mācās skolās, kur tests notika latviski, tātad tā ir cita,
# atlasīta grupa. Tāpēc skripts dod trīs lietas.
#   1. Starpību starp latviski un krieviski testētajiem B skolēniem katrā ciklā,
#      nekoriģētu un pie vienāda sastāva (latviski testētie pārsvērti uz krieviski
#      testēto sastāvu). Tas NAV sods, tā ir atlases un soda summa.
#   2. Testa valodu sajaukuma efektu 2015 -> 2018 -> 2022:
#      (latviski testēto daļas pārmaiņa) x starpība.
#   3. 2022 -> 2025 pārmaiņu kā funkciju no pieņemtā soda π:
#        pārmaiņa(π) = novērotā pārmaiņa + krieviski testēto daļa 2022 x π
#      un π*, pie kura pārmaiņa būtu nulle. Kā otru atskaiti: 2025 pret 2022. gada
#      latviski un krieviski testētajiem, abus pārsvērtus uz 2025. gada sastāvu.
#
# Sastāva mainīgie: dzimums, klase, IMMIG, 2015-2022 arī ESCS trenda skalā.
# Salīdzinājumā ar 2025 ESCS netiek lietots galvenajā variantā (sk.
# 00_ab_dati.R), tikai jutīgumā; turklāt 2025. gadā B plūsmai ESCS trūkst 17 %.
#
# Izvade:  output/pisa2025/testa_valodas_robeza.csv       1. un 2. daļa
#          output/pisa2025/testa_valodas_pi.csv           3. daļa
#          output/pisa2025/korekcijas/04_testa_valoda.csv standarta formāts
#          output/pisa2025/logs/50_testa_valodas_robeza_output.txt
# ============================================================================

source("R/pisa2025/00_ab_dati.R")

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/korekcijas", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/50_testa_valodas_robeza_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

LE_TAB <- utils::read.csv("output/pisa2025/saites_kludas.csv", stringsAsFactors = FALSE)   # viens avots, 78. skripts
le_tab <- function(no, uz, joma = "READ") {
  i <- which(LE_TAB$joma == joma & LE_TAB$no == as.numeric(no) & LE_TAB$uz == as.numeric(uz))
  if (!length(i)) stop("Nav saites kļūdas pārim ", no, " -> ", uz); LE_TAB$LE[i[1]] }
LE <- le_tab("2022", "2025")   # 1,094: OECD 2025 Vol. I, atvasināts, sk. data/pisa_tehniskie/PISA2025_TR_AVOTS.md (līdz 2026-09-13 pagaidu 3)
PI <- c(0, 10, 20, 30, 40, 50)
CIKLI <- c("2015", "2018", "2022")
D <- ab_dati()
B <- lapply(D[c(CIKLI, "2025")], function(d) ab_apakskopa(d, d$sk$grupa %in% "B"))
wvid <- function(x, w) { i <- !is.na(x); sum(x[i] * w[i]) / sum(w[i]) }
zv <- function(d, s) if (abs(d / s) > 1.96) "*" else " "

vid_B <- lapply(B, function(s) rubin_brr(s$pv, s$sk$w, s$rw, VID))

# ---------------------------------------------------------------------------
cat(strrep("=", 78), "\n1. Latviski un krieviski testētie B plūsmas skolēni\n",
    strrep("=", 78), "\n\n")
cikls <- list()
for (g in CIKLI) {
  s <- ab_apakskopa(B[[g]], !is.na(B[[g]]$sk$testa_val))
  lv <- s$sk$testa_val == "latviešu"; w <- s$sk$w
  a_lv <- ab_apakskopa(s, lv); a_ru <- ab_apakskopa(s, !lv)
  m_lv <- rubin_brr(a_lv$pv, a_lv$sk$w, a_lv$rw, VID)
  m_ru <- rubin_brr(a_ru$pv, a_ru$sk$w, a_ru$rw, VID)
  starp <- function(x, w) VID(x[lv], w[lv]) - VID(x[!lv], w[!lv])
  gap <- rubin_brr(s$pv, w, s$rw, starp)
  X <- kovar(s$sk, wvid(s$sk$escs_tr, w))
  f <- function(w) { w2 <- w
    w2[lv] <- w[lv] * dfl_faktors(X[lv, ], w[lv], X[!lv, ], w[!lv]); w2 }
  w1 <- f(w)
  gap_k <- rubin_brr(s$pv, w1, lapply(s$rw, f), starp)
  sk_lv <- tapply(lv, s$sk$skola, any); sk_ru <- tapply(!lv, s$sk$skola, any)
  jauktas <- names(sk_lv)[sk_lv & sk_ru]
  cikls[[g]] <- data.frame(
    gads = g, n_lv = sum(lv), n_ru = sum(!lv), dala_lv = 100 * sum(w[lv]) / sum(w),
    skolas_tikai_lv = sum(sk_lv & !sk_ru), skolas_tikai_ru = sum(sk_ru & !sk_lv),
    skolas_jauktas = length(jauktas),
    dala_jauktas_skolas = 100 * sum(w[s$sk$skola %in% jauktas]) / sum(w),
    vid_lv = m_lv[["est"]], se_lv = m_lv[["se"]], vid_ru = m_ru[["est"]], se_ru = m_ru[["se"]],
    starpiba = gap[["est"]], se_starpiba = gap[["se"]],
    starpiba_kor = gap_k[["est"]], se_starpiba_kor = gap_k[["se"]],
    escs_lv = wvid(s$sk$escs_tr[lv], w[lv]), escs_ru = wvid(s$sk$escs_tr[!lv], w[!lv]),
    klase9_lv = 100 * sum(w[lv & s$sk$klase %in% 9]) / sum(w[lv]),
    klase9_ru = 100 * sum(w[!lv & s$sk$klase %in% 9]) / sum(w[!lv]),
    n_eff_attieciba_lv = n_eff(w1[lv]) / n_eff(w[lv]))
  x <- cikls[[g]]
  cat(sprintf("  %s  latviski %4.1f %% (n %4d): %5.1f (%.1f)   krieviski (n %4d): %5.1f (%.1f)\n",
              g, x$dala_lv, x$n_lv, x$vid_lv, x$se_lv, x$n_ru, x$vid_ru, x$se_ru))
  cat(sprintf("        starpība lv - ru: %+6.1f (%.1f)%s   pie vienāda sastāva: %+6.1f (%.1f)%s\n",
              x$starpiba, x$se_starpiba, zv(x$starpiba, x$se_starpiba),
              x$starpiba_kor, x$se_starpiba_kor, zv(x$starpiba_kor, x$se_starpiba_kor)))
  cat(sprintf("        skolas: tikai lv %d, tikai ru %d, jauktas %d (%.1f %% B skolēnu)\n",
              x$skolas_tikai_lv, x$skolas_tikai_ru, x$skolas_jauktas, x$dala_jauktas_skolas))
  cat(sprintf("        ESCS lv %.2f / ru %.2f; 9. klasē lv %.1f %% / ru %.1f %%; n_eff pēc pārsv. %.2f\n\n",
              x$escs_lv, x$escs_ru, x$klase9_lv, x$klase9_ru, x$n_eff_attieciba_lv))
}
cikls <- do.call(rbind, cikls)

# ---------------------------------------------------------------------------
cat(strrep("=", 78), "\n2. Testa valodu sajaukuma efekts starp cikliem\n", strrep("=", 78), "\n\n")
cat("  efekts = (latviski testēto daļas pārmaiņa) x (abu ciklu vidējā starpība lv - ru)\n\n")
sajaukums <- list()
for (p in list(c("2015", "2018"), c("2018", "2022"), c("2015", "2022"))) {
  a <- cikls[cikls$gads == p[1], ]; b <- cikls[cikls$gads == p[2], ]
  dd <- (b$dala_lv - a$dala_lv) / 100
  e_n <- dd * mean(c(a$starpiba, b$starpiba)); e_k <- dd * mean(c(a$starpiba_kor, b$starpiba_kor))
  izm <- vid_B[[p[2]]][["est"]] - vid_B[[p[1]]][["est"]]
  le <- le_tab(p[1], p[2])
  se <- sqrt(vid_B[[p[2]]][["se"]]^2 + vid_B[[p[1]]][["se"]]^2 + le^2)
  sajaukums[[length(sajaukums) + 1]] <- data.frame(
    no = p[1], lidz = p[2], dalas_izm_pp = 100 * dd, efekts = e_n, efekts_kor = e_k,
    B_izm = izm, B_izm_se = se, le = le)
  cat(sprintf("  %s->%s  daļa %+5.1f pp  efekts %+5.2f (sastāvs koriģēts %+5.2f)  B pārmaiņa %+6.1f (%.1f)%s\n",
              p[1], p[2], 100 * dd, e_n, e_k, izm, se, zv(izm, se)))
}
sajaukums <- do.call(rbind, sajaukums)
utils::write.csv(merge(cikls, sajaukums, by.x = "gads", by.y = "lidz", all = TRUE),
                 "output/pisa2025/testa_valodas_robeza.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n3. B plūsma 2022 -> 2025 un testa valodas soda π\n",
    strrep("=", 78), "\n\n")
d_obs <- vid_B[["2025"]][["est"]] - vid_B[["2022"]][["est"]]
se_obs <- sqrt(vid_B[["2025"]][["se"]]^2 + vid_B[["2022"]][["se"]]^2 + LE^2)
dala_ru <- setNames(1 - cikls$dala_lv / 100, cikls$gads)
cat(sprintf("  Novērotā pārmaiņa 2022 -> 2025: %+.1f (SE %.1f ar LE = %g)\n", d_obs, se_obs, LE))
cat(sprintf("  Krieviski testēto daļa 2022: %.1f %%\n\n", 100 * dala_ru[["2022"]]))
cat("  π ir punkti, ko krieviski mācījies skolēns zaudētu, pildot testu latviski.\n",
    " Datos tas nav identificējams, tāpēc tabula dod pārmaiņu pie dažādiem π.\n\n")
pi_tab <- data.frame(pi = PI, izm = d_obs + dala_ru[["2022"]] * PI, se = se_obs)
pi_tab$zim <- ifelse(abs(pi_tab$izm / pi_tab$se) > 1.96, "*", "")
for (i in seq_len(nrow(pi_tab)))
  cat(sprintf("  π = %2d   pārmaiņa pie nemainīgas (latviešu) testa valodas %+6.1f%s\n",
              pi_tab$pi[i], pi_tab$izm[i], pi_tab$zim[i]))
pi_zv <- -d_obs / dala_ru[["2022"]]
pi_sig <- (-d_obs - 1.96 * se_obs) / dala_ru[["2022"]]
cat(sprintf("\n  π*, pie kura pārmaiņa būtu nulle: %.0f punkti\n", pi_zv))
cat(sprintf("  π, pie kura kritums vairs nebūtu statistiski nozīmīgs: %.0f punkti\n", pi_sig))

cat("\n  Otra atskaite: 2022. gada B apakšgrupas, pārsvērtas uz 2025. gada B sastāvu\n\n")
s25 <- B[["2025"]]; w25 <- s25$sk$w
atsk <- list()
for (ar_escs in c(FALSE, TRUE)) for (k in c("visi", "latviešu", "krievu")) {
  s22 <- B[["2022"]]
  if (k != "visi") s22 <- ab_apakskopa(s22, s22$sk$testa_val %in% k)
  aizp <- wvid(s22$sk$escs_tr, s22$sk$w)
  X22 <- kovar(s22$sk, aizp); X25 <- kovar(s25$sk, aizp)
  f <- function(w) w * dfl_faktors(X22, w, X25, w25, ar_escs = ar_escs)
  w1 <- f(s22$sk$w)
  m0 <- rubin_brr(s22$pv, s22$sk$w, s22$rw, VID)
  m1 <- rubin_brr(s22$pv, w1, lapply(s22$rw, f), VID)
  d1 <- vid_B[["2025"]][["est"]] - m1[["est"]]
  se1 <- sqrt(vid_B[["2025"]][["se"]]^2 + m1[["se"]]^2 + LE^2)
  atsk[[length(atsk) + 1]] <- data.frame(
    apakskopa_2022 = k, ar_escs = ar_escs, vid_2022 = m0[["est"]], vid_2022_parsv = m1[["est"]],
    se_2022_parsv = m1[["se"]], izm_lidz_2025 = d1, se = se1,
    n_eff_attieciba = n_eff(w1) / n_eff(s22$sk$w))
  cat(sprintf("  %-9s %-9s 2022: %5.1f -> pārsvērts %5.1f (%.1f)   2025 - 2022: %+6.1f (%.1f)%s  n_eff %.2f\n",
              k, if (ar_escs) "ar ESCS" else "bez ESCS", m0[["est"]], m1[["est"]], m1[["se"]],
              d1, se1, zv(d1, se1), n_eff(w1) / n_eff(s22$sk$w)))
}
atsk <- do.call(rbind, atsk)
utils::write.csv(pi_tab, "output/pisa2025/testa_valodas_pi.csv", row.names = FALSE)
utils::write.csv(atsk, "output/pisa2025/testa_valodas_atskaite_2025.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# Standarta formāts: katra cikla B līmeņa korekcija, ja visi būtu testēti
# latviski, pie π = 10, 20, 40. 2025. gadam korekcija ir 0 pēc definīcijas.
# ---------------------------------------------------------------------------
kor <- list()
for (g in CIKLI) for (p in c(10, 20, 40))
  kor[[length(kor) + 1]] <- korekcijas_rinda(
    g, "B", sprintf("testa valoda, π = %d", p), -dala_ru[[g]] * p,
    pienemumi = sprintf(paste("B līmenis, ja visi būtu testēti latviski; krieviski testētie",
                              "(%.1f %%) latviski iegūtu par π punktiem mazāk; π nav identificējams"),
                        100 * dala_ru[[g]]))
utils::write.csv(do.call(rbind, kor), "output/pisa2025/korekcijas/04_testa_valoda.csv",
                 row.names = FALSE)

cat("\nIzveidotie faili:\n  output/pisa2025/testa_valodas_robeza.csv\n  output/pisa2025/testa_valodas_pi.csv\n",
    " output/pisa2025/testa_valodas_atskaite_2025.csv\n  output/pisa2025/korekcijas/04_testa_valoda.csv\n ",
    output_file, "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
