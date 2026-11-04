# ============================================================================
# 23. Iesaistes moderēts kritums: cik no −44.9 punktiem ir prasme
# ============================================================================
#
# Jautājums. OECD publicētie rādītāji rāda, ka Latvijā 2022 līdz 2025 aptuveni
# 6.4 % atbilžu pārcēlās no "nesasniegts, netiek vērtēts" uz "neiesaistīta
# ātra atbilde, vērtēta kā nepareiza". Tā ir vērtēšanas mehānikas maiņa, ne
# prasmes maiņa, un tā pati par sevi pazemina rezultātu.
#
# Šis skripts to izmēra tieši, uz uzdevumu līmeņa datiem, apejot IRT skalu.
#
# Pieeja. Ņem tikai tos lasīšanas uzdevumus, kas ir ABOS gados un kam abos
# gados ir gan vērtējums, gan atbildes laiks. Tad rēķina neapstrādātu pareizo
# atbilžu īpatsvaru pēc četriem konsekventiem noteikumiem:
#
#   R1 oficiālais       nesasniegtie netiek vērtēti
#   R2 konsekventais    nesasniegtie skaitīti kā nepareizi ABOS gados
#   R3 iesaistes mod.   ātrās minēšanas atbildes izmestas ABOS gados
#   R4 stingrais        izmestas gan ātrās, gan neatbildētās
#
# Starpība starp R1 un R3 pārmaiņu ir tā daļa, kas ir neiesaistīšanās, ne
# prasme.
#
# UZMANĪBU. Ātrās minēšanas slieksnis ņemts no 2022. gada sadalījuma un
# piemērots abiem gadiem, lai kritērijs būtu fiksēts laikā. Ja slieksni
# rēķinātu katram gadam atsevišķi, tas pielāgotos tieši tai pārmaiņai, ko
# mēģinām izmērīt.
#
# Ievade:  data/pisa2022/raw/CY08MSP_STU_COG.SAV        (S un TT)
#          ~/pisa2025_sav/CY09_MS_COG_20260806.sav       (S)
#          ~/pisa2025_sav/CY09_MS_COG_PROCESS_...sav     (TT)
#          data/pisa20xx/processed/pisa_lva_20xx_stu.rds (svari, valoda)
# Izvade:  data/pisa2025/processed/uzdevumu_limenis.rds  (kešs)
#          output/pisa2025/iesaistes_moderets.csv
#          output/pisa2025/logs/23_iesaistes_moderets_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/23_iesaistes_moderets_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

for (pkg in c("haven", "tidyselect")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  library(pkg, character.only = TRUE)
}

# Kopīgā klasifikācija un punktu skala, sk. faila komentāru par izlaboto
# daļēja kredīta kļūdu.
source("R/pisa2025/00_kopigas_funkcijas.R")

SAV_DIR <- Sys.getenv("PISA2025_SAV_DIR", unset = file.path(path.expand("~"), "pisa2025_sav"))
KESS <- "data/pisa2025/processed/uzdevumu_limenis.rds"
VALSTIS <- c("LVA", "EST")


# === 1. UZDEVUMU IZGŪŠANA ===================================================
cat(strrep("=", 70), "\n1. Uzdevumu līmeņa datu izgūšana\n", strrep("=", 70), "\n\n")

atrod <- function(sabl) {
  f <- list.files(SAV_DIR, pattern = sabl, full.names = TRUE)
  if (!length(f)) stop("Nav atrasts: ", sabl)
  f[1]
}

if (file.exists(KESS)) {
  U <- readRDS(KESS)
  cat("Nolasīts kešs:", KESS, "\n")
} else {
  cat("Keša nav, lasu .sav failus. Tas prasa vairākas minūtes.\n\n")

  f22  <- "data/pisa2022/raw/CY08MSP_STU_COG.SAV"
  f25c <- atrod("^CY09_MS_COG_[0-9]+\\.sav$")
  f25p <- atrod("^CY09_MS_COG_PROCESS_[0-9]+\\.sav$")

  h22  <- names(read_sav(f22,  n_max = 1))
  h25c <- names(read_sav(f25c, n_max = 1))
  h25p <- names(read_sav(f25p, n_max = 1))

  id <- function(x) unique(sub("(S|TT)$", "", x))
  kopigi <- Reduce(intersect, list(
    id(grep("^CR[0-9].*S$",  h22,  value = TRUE)),
    id(grep("^CR[0-9].*TT$", h22,  value = TRUE)),
    id(grep("^CR[0-9].*S$",  h25c, value = TRUE)),
    id(grep("^CR[0-9].*TT$", h25p, value = TRUE))))
  cat("Kopīgi lasīšanas uzdevumi abos gados:", length(kopigi), "\n")
  cat("  no tiem fluences:", sum(grepl("^CR590", kopigi)), "\n\n")

  # user_na = TRUE, citādi "Not Reached" un "No Response" pazūd kā NA.
  cat("Lasu 2022 COG...\n")
  d22 <- read_sav(f22, user_na = TRUE,
                  col_select = all_of(c("CNT", "CNTSTUID",
                                        paste0(kopigi, "S"), paste0(kopigi, "TT"))))
  d22 <- d22[as.character(d22$CNT) %in% VALSTIS, ]
  cat("  rindas:", nrow(d22), "\n")

  cat("Lasu 2025 COG...\n")
  d25s <- read_sav(f25c, user_na = TRUE,
                   col_select = all_of(c("CNT", "CNTSTUID", paste0(kopigi, "S"))))
  d25s <- d25s[as.character(d25s$CNT) %in% VALSTIS, ]

  cat("Lasu 2025 COG_PROCESS...\n")
  d25t <- read_sav(f25p, user_na = TRUE,
                   col_select = all_of(c("CNTSTUID", paste0(kopigi, "TT"))))
  d25 <- merge(d25s, d25t, by = "CNTSTUID")
  cat("  rindas:", nrow(d25), "\n")

  U <- list(d22 = d22, d25 = d25, uzd = kopigi)
  saveRDS(U, KESS)
  cat("\nSaglabāts kešs:", KESS, "\n")
}

uzd <- U$uzd


# === 2. ATBILŽU KLASIFIKĀCIJA ===============================================
cat("\n", strrep("=", 70), "\n2. Atbilžu klasifikācija\n", strrep("=", 70), "\n\n")


izgust <- function(d, gads) {
  S <- vapply(d[paste0(uzd, "S")], klasificet, character(nrow(d)))
  T <- vapply(d[paste0(uzd, "TT")], tirs_laiks, numeric(nrow(d)))
  dimnames(S) <- list(NULL, uzd); dimnames(T) <- list(NULL, uzd)
  list(S = S, P = punkti_matrica(S), T = T,
       cnt = as.character(d$CNT), id = d$CNTSTUID, gads = gads)
}

E22 <- izgust(U$d22, 2022)
E25 <- izgust(U$d25, 2025)

for (E in list(E22, E25)) {
  i <- E$cnt == "LVA"
  cat(E$gads[1], "Latvija: skolēni", sum(i), " atbildes",
      sum(!is.na(E$S[i, ])), "\n")
  print(round(100 * prop.table(table(E$S[i, ])), 2))
  cat("\n")
}


# === 3. ĀTRĀS MINĒŠANAS SLIEKSNIS ===========================================
cat(strrep("=", 70), "\n3. Ātrās minēšanas slieksnis\n", strrep("=", 70), "\n\n")

cat("Slieksnis katram uzdevumam atsevišķi, ņemts no 2022. gada Latvijas datiem\n")
cat("un piemērots abiem gadiem. Wise un Kong normatīvā pieeja: 10 %% no vidējā\n")
cat("atbildes laika, ne vairāk par 10 sekundēm.\n\n")

DALA <- 0.10
GRIESTI <- 10000   # ms

i22 <- E22$cnt == "LVA"
videjie <- apply(E22$T[i22, ], 2, function(x) mean(x, na.rm = TRUE))
slieksni <- pmin(DALA * videjie, GRIESTI)
cat("Sliekšņi (ms): mediāna", round(median(slieksni, na.rm = TRUE)),
    " diapazons", round(min(slieksni, na.rm = TRUE)), "līdz",
    round(max(slieksni, na.rm = TRUE)), "\n\n")

atrs <- function(E) {
  M <- sweep(E$T, 2, slieksni, "<")
  M[is.na(M)] <- FALSE
  M & !is.na(E$S) & E$S %in% VERTETAS
}
A22 <- atrs(E22); A25 <- atrs(E25)

for (nos in c("2022", "2025")) {
  E <- if (nos == "2022") E22 else E25
  A <- if (nos == "2022") A22 else A25
  i <- E$cnt == "LVA"
  vert <- !is.na(E$S[i, ]) & E$S[i, ] %in% VERTETAS
  cat(nos, "Latvija: ātrās minēšanas atbildes",
      sprintf("%.2f %% no vērtētajām", 100 * sum(A[i, ]) / sum(vert)), "\n")
  cat("      precizitāte ātrajās:",
      sprintf("%.3f", mean(E$P[i, ][A[i, ]], na.rm = TRUE)),
      " pārējās:",
      sprintf("%.3f", mean(E$P[i, ][vert & !A[i, ]], na.rm = TRUE)), "\n")
}


# === 4. PAREIZO ATBILŽU ĪPATSVARS PĒC ČETRIEM NOTEIKUMIEM ===================
cat("\n", strrep("=", 70), "\n4. Neapstrādāts pareizo atbilžu īpatsvars\n",
    strrep("=", 70), "\n\n")

svari <- function(gads) {
  f <- sprintf("data/pisa%d/processed/pisa_lva_%d_stu.rds", gads, gads)
  d <- readRDS(f)
  data.frame(CNTSTUID = d$CNTSTUID, w = d$W_FSTUWT,
             lab = as.character(as_factor(d$LANGN)), stringsAsFactors = FALSE)
}
w22 <- svari(2022); w25 <- svari(2025)

grupa <- function(lab) ifelse(grepl("^Latvian", lab), "Latviesu",
                       ifelse(grepl("^Russian", lab), "Krievu", "Cita"))

# Katram noteikumam: kuras atbildes ieiet saucējā.
NOTEIKUMI <- list(
  R1 = VERTETAS,
  R2 = VERTETAS_AR_NESASNIEGTIEM,
  R3 = VERTETAS,
  R4 = c("pilns", "dalejs", "nulle")
)
IZMET_ATROS <- c(R1 = FALSE, R2 = FALSE, R3 = TRUE, R4 = TRUE)

ipatsvars <- function(E, A, w, nosac = NULL) {
  i <- E$cnt == "LVA"
  S <- E$S[i, ]; P <- E$P[i, ]; Ai <- A[i, ]
  m <- merge(data.frame(CNTSTUID = E$id[i]), w, by = "CNTSTUID", all.x = TRUE)
  wt <- m$w[match(E$id[i], m$CNTSTUID)]
  if (!is.null(nosac)) {
    g <- grupa(m$lab[match(E$id[i], m$CNTSTUID)])
    keep <- !is.na(g) & g == nosac
    S <- S[keep, ]; P <- P[keep, ]; Ai <- Ai[keep, ]; wt <- wt[keep]
  }
  wt[is.na(wt)] <- 0
  W <- matrix(wt, nrow = nrow(S), ncol = ncol(S))
  sapply(names(NOTEIKUMI), function(r) {
    sauc <- !is.na(S) & S %in% NOTEIKUMI[[r]]
    if (IZMET_ATROS[[r]]) sauc <- sauc & !Ai
    100 * sum(W[sauc] * P[sauc], na.rm = TRUE) / sum(W[sauc])
  })
}

cat("Visa Latvijas izlase:\n")
cat(sprintf("  %-4s %8s %8s %9s   %s\n", "", "2022", "2025", "pārmaiņa", "noteikums"))
apr <- c(R1 = "nesasniegtie netiek vērtēti (oficiālais)",
         R2 = "nesasniegtie skaitīti kā nepareizi",
         R3 = "ātrās minēšanas atbildes izmestas",
         R4 = "izmestas ātrās un neatbildētās")
p22 <- ipatsvars(E22, A22, w22); p25 <- ipatsvars(E25, A25, w25)
rez <- list()
for (r in names(NOTEIKUMI)) {
  cat(sprintf("  %-4s %8.2f %8.2f %+9.2f   %s\n", r, p22[[r]], p25[[r]],
              p25[[r]] - p22[[r]], apr[[r]]))
  rez[[length(rez) + 1]] <- data.frame(grupa = "Visi", noteikums = r,
                                       y2022 = p22[[r]], y2025 = p25[[r]],
                                       parmaina = p25[[r]] - p22[[r]])
}

for (g in c("Latviesu", "Krievu")) {
  cat("\n", g, ":\n", sep = "")
  a <- ipatsvars(E22, A22, w22, g); b <- ipatsvars(E25, A25, w25, g)
  for (r in names(NOTEIKUMI)) {
    cat(sprintf("  %-4s %8.2f %8.2f %+9.2f\n", r, a[[r]], b[[r]], b[[r]] - a[[r]]))
    rez[[length(rez) + 1]] <- data.frame(grupa = g, noteikums = r,
                                         y2022 = a[[r]], y2025 = b[[r]],
                                         parmaina = b[[r]] - a[[r]])
  }
}


# === 5. INTERPRETĀCIJA ======================================================
cat("\n", strrep("=", 70), "\n5. Cik no krituma ir neiesaistīšanās\n",
    strrep("=", 70), "\n\n")

d1 <- p25[["R1"]] - p22[["R1"]]
d3 <- p25[["R3"]] - p22[["R3"]]
cat(sprintf("  Oficiālais noteikums R1        %+6.2f procentpunkti\n", d1))
cat(sprintf("  Iesaistes moderētais R3        %+6.2f procentpunkti\n", d3))
cat(sprintf("  Starpība                       %+6.2f\n", d3 - d1))
if (abs(d1) > 0.01)
  cat(sprintf("  Neiesaistīšanās daļa no krituma %5.1f %%\n",
              100 * (1 - d3 / d1)))

cat("\nŠie ir procentpunkti neapstrādātā pareizo atbilžu īpatsvarā, nevis\n")
cat("PISA skalas punkti. Pārrēķins uz skalu prasa IRT modeli, ko šeit apzināti\n")
cat("neizmantojam, jo tieši tā ir kritikas mērķis.\n")


# === 6. APVIENOTĀ KOREKCIJA AR BRR STANDARTKĻŪDĀM ===========================
cat("\n", strrep("=", 70), "\n6. Apvienotā korekcija ar standartkļūdām\n",
    strrep("=", 70), "\n\n")

cat("R5 apvieno abas korekcijas: nesasniegtie skaitīti konsekventi ABOS gados\n")
cat("un ātrās minēšanas atbildes izmestas. Tas ir pilnākais no noteikumiem.\n\n")

RW <- paste0("W_FSTURWT", 1:80)
svari_pilni <- function(gads) {
  d <- readRDS(sprintf("data/pisa%d/processed/pisa_lva_%d_stu.rds", gads, gads))
  cbind(data.frame(CNTSTUID = as.numeric(d$CNTSTUID),
                   w = as.numeric(d$W_FSTUWT),
                   lab = as.character(as_factor(d$LANGN)),
                   stringsAsFactors = FALSE),
        as.data.frame(lapply(d[RW], as.numeric)))
}
W22 <- svari_pilni(2022); W25 <- svari_pilni(2025)

pct2 <- function(E, A, wt, keep) {
  i <- E$cnt == "LVA"
  S <- E$S[i, ][keep, ]; P <- E$P[i, ][keep, ]; Ai <- A[i, ][keep, ]
  wt <- wt[keep]; wt[is.na(wt)] <- 0
  M <- matrix(wt, nrow = nrow(S), ncol = ncol(S)); b <- !is.na(S)
  f <- function(s) 100 * sum(M[s] * P[s], na.rm = TRUE) / sum(M[s])
  c(R1 = f(b & S %in% VERTETAS),
    R5 = f(b & S %in% VERTETAS_AR_NESASNIEGTIEM & !Ai))
}

brr <- function(g = NULL) {
  gads_res <- list()
  for (nos in c("2022", "2025")) {
    E <- if (nos == "2022") E22 else E25
    A <- if (nos == "2022") A22 else A25
    w <- if (nos == "2022") W22 else W25
    m <- w[match(as.numeric(E$id[E$cnt == "LVA"]), w$CNTSTUID), ]
    keep <- if (is.null(g)) rep(TRUE, nrow(m)) else
      (!is.na(grupa(m$lab)) & grupa(m$lab) == g)
    est <- pct2(E, A, m$w, keep)
    rp <- vapply(RW, function(r) pct2(E, A, m[[r]], keep), numeric(2))
    gads_res[[nos]] <- list(est = est, var = rowSums((rp - est)^2) / 20)
  }
  d1 <- gads_res[["2025"]]$est["R1"] - gads_res[["2022"]]$est["R1"]
  d5 <- gads_res[["2025"]]$est["R5"] - gads_res[["2022"]]$est["R5"]
  c(d1 = unname(d1), s1 = unname(sqrt(sum(sapply(gads_res, function(x) x$var["R1"])))),
    d5 = unname(d5), s5 = unname(sqrt(sum(sapply(gads_res, function(x) x$var["R5"])))))
}

cat(sprintf("  %-10s %18s %18s %10s\n", "grupa", "R1 pārmaiņa", "R5 pārmaiņa", "noņemts"))
for (g in list(NULL, "Latviesu", "Krievu", "Cita")) {
  r <- brr(g); nos <- if (is.null(g)) "VISI" else g
  zv <- if (abs(r["d5"] / r["s5"]) > 1.96) "*" else " "
  cat(sprintf("  %-10s %8.2f (SE %.2f) %8.2f (SE %.2f)%s %8.1f %%\n",
              nos, r["d1"], r["s1"], r["d5"], r["s5"], zv,
              100 * (1 - r["d5"] / r["d1"])))
  rez[[length(rez) + 1]] <- data.frame(grupa = nos, noteikums = "R1_vs_R5",
                                       y2022 = NA, y2025 = NA,
                                       parmaina = r["d5"],
                                       se = r["s5"], r1_parmaina = r["d1"],
                                       r1_se = r["s1"])
}
cat("\n  * nozīmīgs pie 95 %\n")

rez_df <- do.call(rbind, lapply(rez, function(x) {
  for (k in c("se", "r1_parmaina", "r1_se")) if (is.null(x[[k]])) x[[k]] <- NA
  x[, c("grupa","noteikums","y2022","y2025","parmaina","se","r1_parmaina","r1_se")]
}))
rownames(rez_df) <- NULL
write.csv(rez_df, "output/pisa2025/iesaistes_moderets.csv", row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/iesaistes_moderets.csv\n ",
    output_file, "\n\nPabeigts:", format(Sys.time()), "\n")

while (sink.number() > 0) sink()
close(log_con)
