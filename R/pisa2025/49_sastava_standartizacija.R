# ============================================================================
# 49. Sastāva standartizācija, A un B plūsma 2003-2025 (docs/51, 3. solis)
# ============================================================================
#
# JAUTĀJUMS. Cik lielu daļu no plūsmu vidējā rezultāta pārmaiņām var izskaidrot
# ar to, ka mainījās skolēnu sastāvs (dzimums, klase, sociāli ekonomiskais
# statuss, migrācijas pieredze), nevis ar to, ka mainījās lasītprasme pie
# nemainīga sastāva?
#
# METODE. Katra cikla plūsmu pārsver uz vienu fiksētu atsauces sastāvu
# (DiNardo-Fortin-Lemieux pārsvēršana). Atsauces sastāvs ir visu iekļauto ciklu
# vidējais, kur katrs cikls sver vienādi. Atsauce ir definīcijas izvēle, tāpēc
# tā tiek turēta fiksēta, bet cikla svari tiek pārsvērti atsevišķi katram no 80
# replikātsvariem, un standarta kļūda ir pilnā Rubina dispersija.
#
#   S1  dzimums                               2003-2025, šūnu pārsvēršana
#   S2  dzimums x klase                       2003-2025, šūnu pārsvēršana
#   S3  S2 + ESCS (trenda skala) + IMMIG      2012-2022, logit pārsvēršana
#   S3j tas pats, 2012-2025                   jutīgums: 2025 ESCS no faila, kas
#                                             nav apstiprināts kā trenda skala
#                                             (sk. 00_ab_dati.R)
#
# KĀ LASĪT. "Sastāva ieguldījums" pārmaiņā = nekoriģētā pārmaiņa mīnus koriģētā.
# Ja nekoriģētā ir -20 un koriģētā -15, tad 5 punkti no krituma ir sastāva
# pārmaiņa, bet 15 punkti paliek neizskaidroti ar šiem mainīgajiem.
#
# PIEŅĒMUMI UN IEROBEŽOJUMI
#   - Klase nav tīri eksogēna: tā atspoguļo arī otrgadniecības politiku. S2
#     atbild uz jautājumu "ja klašu sadalījums būtu nemainīgs", nevis "cik
#     lasītprasme būtu bez politikas".
#   - Standartizācija neizskaidro nenovērotu sastāvu (piem., emigrācija pēc
#     prasmēm) un ne izslēgšanas, ne testa valodas efektu; tie ir citas
#     korekciju ģimenes (docs/51), un tās nesaskaita kopā.
#   - 2003-2009 ESCS nav trenda skalā, tāpēc tur ir tikai S1 un S2.
#
# Izvade:  output/pisa2025/sastavs.csv                   sastāva apraksts
#          output/pisa2025/sastava_standartizacija.csv   līmeņi
#          output/pisa2025/sastava_izmainas.csv          pārmaiņas
#          output/pisa2025/korekcijas/03_sastavs.csv     standarta formāts
#          output/pisa2025/devita_klase.csv              tikai 9. klase
#          output/pisa2025/logs/49_sastava_standartizacija_output.txt
# ============================================================================

source("R/pisa2025/00_ab_dati.R")

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
dir.create("output/pisa2025/korekcijas", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/49_sastava_standartizacija_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

LE_TAB <- utils::read.csv("output/pisa2025/saites_kludas.csv", stringsAsFactors = FALSE)   # viens avots, 78. skripts
le_tab <- function(no, uz, joma = "READ") {
  i <- which(LE_TAB$joma == joma & LE_TAB$no == as.numeric(no) & LE_TAB$uz == as.numeric(uz))
  if (!length(i)) stop("Nav saites kļūdas pārim ", no, " -> ", uz); LE_TAB$LE[i[1]] }
D <- ab_dati()
GADI <- names(D)
S3_GADI <- list(S3  = names(ESCS_TRENDA_APSTIPRINATS)[ESCS_TRENDA_APSTIPRINATS],
                S3j = names(ESCS_TRENDA_APSTIPRINATS))
VARIANTI <- c("S1", "S2", names(S3_GADI))

plusma <- function(g, gr) ab_apakskopa(D[[g]], D[[g]]$sk$grupa %in% gr)

# ---------------------------------------------------------------------------
cat(strrep("=", 78), "\n1. Sastāvs pa cikliem (svērti %)\n", strrep("=", 78), "\n\n")
sastavs <- list()
for (g in GADI) for (gr in c("A", "B")) {
  s <- plusma(g, gr)$sk; w <- s$w
  dal <- function(i) 100 * sum(w[i %in% TRUE]) / sum(w)
  ir_e <- !is.na(s$escs_tr); ir_i <- !is.na(s$imig)
  sastavs[[length(sastavs) + 1]] <- data.frame(
    gads = g, grupa = gr, n = nrow(s), meitenes = dal(s$dzimums == 1),
    klase_7 = dal(s$klase <= 7), klase_8 = dal(s$klase == 8), klase_9 = dal(s$klase == 9),
    klase_10 = dal(s$klase >= 10), klase_nav = dal(is.na(s$klase)),
    imigr = if (any(ir_i)) 100 * sum(w[ir_i & s$imig %in% c(2, 3)]) / sum(w[ir_i]) else NA,
    escs_tr = if (any(ir_e)) sum(w[ir_e] * s$escs_tr[ir_e]) / sum(w[ir_e]) else NA,
    escs_tr_nav = dal(!ir_e))
}
sastavs <- do.call(rbind, sastavs)
print(format(sastavs, digits = 3, nsmall = 1), row.names = FALSE)
utils::write.csv(sastavs, "output/pisa2025/sastavs.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# Šūnu pārsvēršana (S1, S2). Atsauces daļas ir ciklu vidējās šūnu daļas. Ja
# šūnas kādā ciklā nav, atsauce tiek normēta pa tajā ciklā esošajām šūnām.
# ---------------------------------------------------------------------------
atsauce_sunas <- function(gr, sunfun) {
  p <- lapply(GADI, function(g) { s <- plusma(g, gr)$sk
    tapply(s$w, sunfun(s), sum) / sum(s$w) })
  sunas <- sort(unique(unlist(lapply(p, names))))
  vapply(sunas, function(c) mean(vapply(p, function(x)
    if (c %in% names(x)) x[[c]] else 0, numeric(1))), numeric(1))
}
parsvert_sunas <- function(w, c, pref) {
  pt <- tapply(w, c, sum) / sum(w)
  pr <- pref[names(pt)]; pr[is.na(pr)] <- 0; pr <- pr / sum(pr)
  w * (pr / pt)[c]
}

# Logit pārsvēršana (S3): kovar(), dfl_faktors() un n_eff() no 00_ab_dati.R.

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n2. Nekoriģētie un standartizētie vidējie\n", strrep("=", 78), "\n\n")
SUNAS <- list(S1 = function(s) dz_kat(s$dzimums),
              S2 = function(s) paste(dz_kat(s$dzimums), kl_kat(s$klase)))
rez <- list(); kor <- list()
for (gr in c("A", "B")) {
  pref <- lapply(SUNAS, function(f) atsauce_sunas(gr, f))
  # S3 atsauce: ciklu apvienojums, katrs cikls normēts uz 1 / ciklu skaits
  REF <- lapply(S3_GADI, function(gadi) {
    ref <- lapply(gadi, function(g) plusma(g, gr)$sk)
    ir <- unlist(lapply(ref, function(s) !is.na(s$escs_tr)))
    wr <- unlist(lapply(ref, function(s) s$w / sum(s$w) / length(ref)))
    aizp <- sum((wr * unlist(lapply(ref, `[[`, "escs_tr")))[ir]) / sum(wr[ir])
    list(X = do.call(rbind, lapply(ref, kovar, escs_aizp = aizp)), w = wr, aizp = aizp)
  })

  for (g in GADI) {
    s <- plusma(g, gr)
    n0 <- rubin_brr(s$pv, s$sk$w, s$rw, VID)
    for (v in VARIANTI) {
      if (v %in% names(S3_GADI) && !(g %in% S3_GADI[[v]])) next
      if (v %in% names(S3_GADI)) {
        R  <- REF[[v]]
        Xt <- kovar(s$sk, R$aizp)
        f  <- function(w) w * dfl_faktors(Xt, w, R$X, R$w)
      } else {
        c <- SUNAS[[v]](s$sk)
        f <- function(w) parsvert_sunas(w, c, pref[[v]])
      }
      w1 <- f(s$sk$w); rw1 <- lapply(s$rw, f)
      k1 <- rubin_brr(s$pv, w1, rw1, VID)
      fak <- w1 / s$sk$w
      rez[[length(rez) + 1]] <- data.frame(
        gads = g, grupa = gr, variants = v, n = nrow(s$sk),
        vid_nekor = n0[["est"]], se_nekor = n0[["se"]],
        vid_kor = k1[["est"]], se_kor = k1[["se"]], nobide = k1[["est"]] - n0[["est"]],
        faktors_max = max(fak) / stats::weighted.mean(fak, s$sk$w),
        n_eff_attieciba = n_eff(w1) / n_eff(s$sk$w))
      kor[[length(kor) + 1]] <- korekcijas_rinda(
        g, gr, paste0("sastāvs ", v), k1[["est"]] - n0[["est"]],
        pienemumi = switch(v,
          S1 = "atsauce: 2003-2025 ciklu vidējais dzimumu sadalījums",
          S2 = "atsauce: 2003-2025 ciklu vidējais dzimuma x klases sadalījums; klase daļēji politikas rezultāts",
          S3 = "atsauce: 2012-2022 ciklu vidējais; logit ar ESCS trenda skalā, ESCS^2, dzimumu, klasi, IMMIG",
          S3j = paste("kā S3, atsauce 2012-2025; 2025 ESCS no faila pieņemts trenda skalā",
                      "(formula sakrīt 2,4 % robežās, komponentu salīdzināmība nav pārbaudīta)")))
    }
  }
}
rez <- do.call(rbind, rez)
print(format(rez, digits = 4), row.names = FALSE)
utils::write.csv(rez, "output/pisa2025/sastava_standartizacija.csv", row.names = FALSE)
utils::write.csv(do.call(rbind, kor), "output/pisa2025/korekcijas/03_sastavs.csv",
                 row.names = FALSE)

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n3. Pārmaiņas: nekoriģētās un pie nemainīga sastāva\n",
    strrep("=", 78), "\n\n")
cat("  SE ietver katra pāra saites kļūdu (output/pisa2025/saites_kludas.csv). Zvaigznīte: |pārmaiņa / SE| > 1,96.\n\n")
PARI <- list(c("2003", "2012"), c("2003", "2025"), c("2012", "2025"), c("2015", "2025"),
             c("2012", "2018"), c("2018", "2022"), c("2022", "2025"))
izm <- list()
for (gr in c("A", "B")) for (v in VARIANTI) for (p in PARI) {
  a <- rez[rez$grupa == gr & rez$variants == v & rez$gads == p[1], ]
  b <- rez[rez$grupa == gr & rez$variants == v & rez$gads == p[2], ]
  if (!nrow(a) || !nrow(b)) next
  LE <- le_tab(p[1], p[2])
  dn <- b$vid_nekor - a$vid_nekor; sn <- sqrt(a$se_nekor^2 + b$se_nekor^2 + LE^2)
  dk <- b$vid_kor - a$vid_kor;     sk <- sqrt(a$se_kor^2 + b$se_kor^2 + LE^2)
  izm[[length(izm) + 1]] <- data.frame(
    grupa = gr, variants = v, no = p[1], lidz = p[2], le = LE,
    izm_nekor = dn, se_nekor = sn, izm_kor = dk, se_kor = sk, sastava_ieguldijums = dn - dk)
  cat(sprintf("  %s %s %s->%s  nekoriģēti %+6.1f (%4.1f)%s  koriģēti %+6.1f (%4.1f)%s  sastāvs %+5.1f\n",
              gr, v, p[1], p[2], dn, sn, if (abs(dn / sn) > 1.96) "*" else " ",
              dk, sk, if (abs(dk / sk) > 1.96) "*" else " ", dn - dk))
}
utils::write.csv(do.call(rbind, izm), "output/pisa2025/sastava_izmainas.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# Vienkāršākā klases struktūras kontrole bez modeļa: tikai modālā 9. klase.
# Neprasa pieņēmumu par to, kā pārsvērt 8. klases skolēnus, kuru grupa ciklu
# gaitā sarūk.
# ---------------------------------------------------------------------------
cat("\n", strrep("=", 78), "\n4. Tikai 9. klase\n", strrep("=", 78), "\n\n")
dk <- list()
for (gr in c("A", "B")) {
  prev <- NULL; prev_g <- NULL
  for (g in GADI) {
    s <- ab_apakskopa(D[[g]], D[[g]]$sk$grupa %in% gr & D[[g]]$sk$klase %in% 9)
    x <- rubin_brr(s$pv, s$sk$w, s$rw, VID)
    d <- if (is.null(prev)) NA else x[["est"]] - prev[["est"]]
    se <- if (is.null(prev)) NA else sqrt(x[["se"]]^2 + prev[["se"]]^2 + le_tab(prev_g, g)^2)
    dk[[length(dk) + 1]] <- data.frame(gads = g, grupa = gr, n = nrow(s$sk), vid = x[["est"]],
                                       se = x[["se"]], izm_pret_iepr = d, se_izm = se)
    cat(sprintf("  %s %s  n %4d  %5.1f (%.1f)  %s\n", gr, g, nrow(s$sk), x[["est"]], x[["se"]],
                if (is.na(d)) "" else sprintf("%+6.1f (%.1f)%s", d, se, if (abs(d / se) > 1.96) "*" else "")))
    prev <- x; prev_g <- g
  }
}
utils::write.csv(do.call(rbind, dk), "output/pisa2025/devita_klase.csv", row.names = FALSE)

cat("\nIzveidotie faili:\n  output/pisa2025/sastavs.csv\n  output/pisa2025/sastava_standartizacija.csv\n",
    " output/pisa2025/sastava_izmainas.csv\n  output/pisa2025/korekcijas/03_sastavs.csv\n ",
    output_file, "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
