# ============================================================================
# 44. Skaitļu verifikācija: pilna Rubina dispersija un saites kļūda
# ============================================================================
#
# Kāpēc šis skripts. Skripts 43 izlaboja divus vienkāršojumus, kas OECD
# metodikā nav pieļaujami, un šis to pārbauda:
#
# 1. IZLASES DISPERSIJA. Skriptā 43 BRR replikācija tika rēķināta tikai uz
#    PIRMO plausible value. OECD rokasgrāmata prasa rēķināt BRR dispersiju
#    KATRAI plausible value un tad tās vidējot. Ar 80 replikātiem un 5 vai 10
#    PV tas ir 400 līdz 800 pārrēķinu vienam skaitlim, tāpēc vienkāršojums ir
#    izplatīts, bet tas nav pareizais.
#
# 2. TICAMĪBAS INTERVĀLA REIZINĀTĀJS. Skriptā 43 lietots 1,96, kas ir normālā
#    sadalījuma vērtība. Ar ierobežotu PV skaitu pareizais ir t sadalījums ar
#    brīvības pakāpēm pēc Rubina formulas. Šeit tas tiek aprēķināts un
#    parādīts, cik daudz tas maina.
#
# 3. SAITES KĻŪDA. Šī ir svarīgākā. Salīdzinot DAŽĀDUS ciklus, OECD prasa
#    standartkļūdai pievienot saites kļūdu, kas atspoguļo nenoteiktību pašā
#    skalas piesaistē starp cikliem un NAV atkarīga no izlases lieluma.
#    PISA 2022 I sējums, A7 pielikums: lasīšanā 2022 pret 2018 tā ir
#    1,47 punkti, un tā ir lielāka salīdzinājumiem ar senākiem cikliem.
#    Skriptā 43 tās nav vispār, tāpēc starpciklu nozīmība tur ir pārvērtēta.
#    Šeit tiek rēķināta jutīgums pret vairākām saites kļūdas vērtībām.
#
# Formulas:
#   theta = vidējais pa PV
#   U     = vidējais pa PV no BRR dispersijas, BRR ar Fay 0.5, dalītājs 20
#   B     = izlases dispersija starp PV aplēsēm
#   V     = U + (1 + 1/M) * B
#   gamma = (1 + 1/M) * B / V                     trūkstošās informācijas daļa
#   df    = (M - 1) / gamma^2                     Rubina brīvības pakāpes
#   starpībai starp cikliem: SE = sqrt(V1 + V2 + saites_kluda^2)
#
# Izvade:  output/pisa2025/verifikacija.csv
#          output/pisa2025/logs/44_verifikacija_output.txt
# ============================================================================

while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/44_verifikacija_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")

veca <- function(g) {
  d <- utils::read.csv(sprintf("data/pisa_veci/lva_%s.csv", g), colClasses = "character")
  list(pv = lapply(paste0("PV", 1:5, "READ"), function(p) as.numeric(d[[p]])),
       w = as.numeric(d$W_FSTUWT),
       rw = lapply(paste0("W_FSTR", 1:80), function(p) as.numeric(d[[p]])),
       lang = trimws(d$LANGN))
}
JAUNIE <- readRDS("data/pisa2025/processed/a_grupa_pisa.rds")
jauna <- function(g) {
  d <- JAUNIE[[g]]; d <- d[as.character(d$CNT) == "LVA", ]
  list(pv = lapply(paste0("PV", 1:10, "READ"), function(p) as.numeric(d[[p]])),
       w = as.numeric(d$W_FSTUWT),
       rw = lapply(paste0("W_FSTURWT", 1:80), function(p) as.numeric(d[[p]])),
       lang = as.character(as.numeric(d$LANGN)))
}
D <- list()
for (g in c("2003", "2006", "2009", "2012")) D[[g]] <- veca(g)
for (g in c("2015", "2018", "2022", "2025")) D[[g]] <- jauna(g)
GADI <- names(D)

grupa <- function(lang, gads) {
  if (gads == "2003") ifelse(lang == "428001", "A", ifelse(lang == "428002", "B", NA))
  else ifelse(lang == "325", "A", ifelse(lang == "495", "B", NA))
}
VID <- function(x, w) sum(x * w) / sum(w)

# Pilnā Rubina dispersija: BRR katrai plausible value, pēc tam vidējošana.
pilna <- function(gads, i, fun = VID) {
  d <- D[[gads]]; M <- length(d$pv)
  th <- numeric(M); U <- numeric(M)
  for (m in seq_len(M)) {
    x <- d$pv[[m]][i]
    th[m] <- fun(x, d$w[i])
    rv <- vapply(d$rw, function(r) fun(x, r[i]), numeric(1))
    U[m] <- sum((rv - th[m])^2) / 20
  }
  B <- stats::var(th)
  V <- mean(U) + (1 + 1 / M) * B
  gam <- ((1 + 1 / M) * B) / V
  df <- if (gam <= 0) Inf else (M - 1) / gam^2
  c(est = mean(th), se = sqrt(V), se_izlase = sqrt(mean(U)),
    se_imput = sqrt((1 + 1 / M) * B), gamma = gam, df = df,
    t95 = stats::qt(0.975, df), M = M)
}

cat(strrep("=", 78), "\n1. Pilnā dispersija pret skriptu 43\n", strrep("=", 78), "\n\n")
cat(sprintf("  %-5s %-5s %9s %8s %8s %8s %7s %8s %6s\n", "gads", "grupa",
            "aplēse", "SE pilna", "izlases", "imput.", "gamma", "df", "t.95"))
R <- list()
for (g in GADI) {
  gr <- grupa(D[[g]]$lang, g)
  for (gn in c("A", "B", "visi")) {
    i <- if (gn == "visi") rep(TRUE, length(gr)) else (!is.na(gr) & gr == gn)
    a <- pilna(g, i)
    R[[paste(g, gn)]] <- a
    cat(sprintf("  %-5s %-5s %9.2f %8.2f %8.2f %8.2f %7.3f %8.0f %6.3f\n",
                g, gn, a["est"], a["se"], a["se_izlase"], a["se_imput"],
                a["gamma"], min(a["df"], 99999), a["t95"]))
  }
}

cat("\n", strrep("=", 78), "\n2. Cik daudz mainās ticamības intervāls\n",
    strrep("=", 78), "\n\n")
cat(sprintf("  %-5s %-5s %26s %26s %8s\n", "gads", "grupa",
            "grafikā: aplēse +/- 1,96*SE", "pilnā: aplēse +/- t*SE", "platums"))
for (g in GADI) for (gn in c("A", "B")) {
  a <- R[[paste(g, gn)]]
  lo1 <- a["est"] - 1.96 * a["se"]; hi1 <- a["est"] + 1.96 * a["se"]
  lo2 <- a["est"] - a["t95"] * a["se"]; hi2 <- a["est"] + a["t95"] * a["se"]
  cat(sprintf("  %-5s %-5s %12.1f %12.1f %12.1f %12.1f %+8.2f\n", g, gn,
              lo1, hi1, lo2, hi2, (hi2 - lo2) - (hi1 - lo1)))
}

# ---------------------------------------------------------------------------
# Saites kļūda. PISA 2022 I sējums, A7 pielikums: lasīšanā 2022 pret 2018 tā
# ir 1,47 punkti un lielāka senākiem salīdzinājumiem. Precīzas vērtības katram
# ciklu pārim mums nav, tāpēc te ir jutīguma analīze.
SAITES <- c(0, 1.5, 3.0, 5.0)
cat("\n", strrep("=", 78), "\n3. Starpciklu salīdzinājumi ar saites kļūdu\n",
    strrep("=", 78), "\n\n")
cat("  Katram salīdzinājumam: starpība, un vai tā ir nozīmīga pie dažādām\n",
    " saites kļūdas vērtībām. 1,47 ir OECD dotā vērtība lasīšanai 2022 pret 2018.\n\n")
PARI <- list(c("2003", "2006"), c("2006", "2009"), c("2012", "2015"),
             c("2015", "2018"), c("2018", "2022"), c("2022", "2025"),
             c("2003", "2025"), c("2003", "2015"), c("2015", "2022"))
cat(sprintf("  %-6s %-14s %9s %8s   %s\n", "grupa", "periods", "starpība", "SE(0)",
            paste(sprintf("%10s", paste0("LE=", SAITES)), collapse = "")))
sal <- list()
for (gn in c("A", "B")) {
  for (p in PARI) {
    a <- R[[paste(p[1], gn)]]; b <- R[[paste(p[2], gn)]]
    d <- b["est"] - a["est"]
    txt <- ""
    for (le in SAITES) {
      s <- sqrt(a["se"]^2 + b["se"]^2 + le^2)
      txt <- paste0(txt, sprintf("%10s", sprintf("%.2f%s", abs(d / s),
                                 if (abs(d / s) > 1.96) "*" else " ")))
    }
    cat(sprintf("  %-6s %-14s %+9.1f %8.2f   %s\n", gn,
                paste0(p[1], "->", p[2]), d, sqrt(a["se"]^2 + b["se"]^2), txt))
    sal[[length(sal) + 1]] <- data.frame(grupa = gn, no = p[1], lidz = p[2],
      starpiba = unname(d), se0 = unname(sqrt(a["se"]^2 + b["se"]^2)),
      se_le15 = unname(sqrt(a["se"]^2 + b["se"]^2 + 1.5^2)),
      se_le30 = unname(sqrt(a["se"]^2 + b["se"]^2 + 3^2)),
      se_le50 = unname(sqrt(a["se"]^2 + b["se"]^2 + 5^2)))
  }
}

cat("\n", strrep("=", 78), "\n4. Salīdzinājumi VIENA cikla ietvaros\n",
    strrep("=", 78), "\n\n")
cat("  Šeit saites kļūda NAV vajadzīga, jo abas grupas ir uz vienas skalas.\n\n")
cat(sprintf("  %-6s %10s %10s %11s %8s %6s\n", "gads", "A", "B", "A mīnus B", "SE", "z"))
for (g in GADI) {
  a <- R[[paste(g, "A")]]; b <- R[[paste(g, "B")]]
  d <- a["est"] - b["est"]; s <- sqrt(a["se"]^2 + b["se"]^2)
  cat(sprintf("  %-6s %10.1f %10.1f %+11.1f %8.2f %6.2f%s\n", g, a["est"],
              b["est"], d, s, abs(d / s), if (abs(d / s) > 1.96) "*" else " "))
}

df <- do.call(rbind, lapply(names(R), function(k) {
  a <- R[[k]]; s <- strsplit(k, " ")[[1]]
  data.frame(gads = s[1], grupa = s[2], aplese = unname(a["est"]),
             se = unname(a["se"]), se_izlase = unname(a["se_izlase"]),
             se_imputacija = unname(a["se_imput"]), gamma = unname(a["gamma"]),
             df = unname(a["df"]), t95 = unname(a["t95"]), M = unname(a["M"]))}))
write.csv(df, "output/pisa2025/verifikacija.csv", row.names = FALSE)
write.csv(do.call(rbind, sal), "output/pisa2025/verifikacija_salidzinajumi.csv",
          row.names = FALSE)
cat("\nIzveidotie faili:\n  output/pisa2025/verifikacija.csv",
    "\n  output/pisa2025/verifikacija_salidzinajumi.csv\n ", output_file,
    "\n\nPabeigts:", format(Sys.time()), "\n")
while (sink.number() > 0) sink()
close(log_con)
