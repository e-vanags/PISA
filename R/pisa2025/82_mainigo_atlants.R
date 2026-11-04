# ============================================================================
# 82. Mainīgo atlants: visi PISA anketu mainīgie 2000–2025 (docs/46 Slānis 13, docs/82)
# ============================================================================
#
# KĀPĒC. Simptomu modeļi (64b–64h) iekļāva tikai konstruktus, kas ir vismaz divos
# ciklos, tāpēc 74 % kataloga mainīgo (arī 2018. gada ST154Q01HA, garākais teksts
# stundās) nevienā modelī neiekļuva. Atlants parāda VISU, kas mērīts, un kas no tā
# ir lietots mūsu skriptos.
#
# Ievade:
#   output/pisa2025/simptomi_inventars_veci.csv, simptomi_inventars_jaunie.csv (64a, py)
#   output/pisa2025/atlants_veci_etiketes.csv (py/atlants_spss_etiketes.py: VALUE LABELS
#     no 2000–2012 SPSS sintaksēm; skripts to palaiž, ja faila nav)
#   data/pisa2025/processed/simptomi_<gads>.rds (64a kešs: skolēnu un skolu anketas
#     8 valstīm ar haven etiķetēm), skolotāju anketas .sav (nolasa un kešo šeit)
#   kognitīvo, laika un procesa failu galvenes (tikai mainīgo skaits)
#   R/pisa2025/simptomi_karte.csv, output/pisa2025/simptomi_karte_pilna.csv (64d)
#   modeļu izvades (64h/64i, 69a/69f, 69c/75, 67d) un R/pisa2025/*.R, py/*.py (grep)
#
# Izvade:
#   output/pisa2025/atlants_mainigie.csv     viena rinda katram cikls × fails × mainīgais
#   output/pisa2025/atlants_konstrukti.csv   saskaņotie konstrukti pa cikliem, arī viena cikla
#   output/pisa2025/atlants_kopsavilkums.csv cikls × fails
#   output/pisa2025/atlants_nelietotie_kandidati.csv 20 kandidāti (lasīšana / skolas prakse)
#   output/pisa2025/vizualizacijas/V7_mainigo_atlants.html  pārlūkojams atlants
#   output/pisa2025/logs/82_mainigo_atlants_output.txt
# ============================================================================

suppressPackageStartupMessages({ library(data.table); library(Matrix) })
while (sink.number() > 0) sink()
dir.create("output/pisa2025/logs", showWarnings = FALSE, recursive = TRUE)
output_file <- "output/pisa2025/logs/82_mainigo_atlants_output.txt"
log_con <- file(output_file, open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)
cat("Izvade tiek saglabāta:", output_file, "\n\n")
t_sakums <- Sys.time()

GADI <- c("2000", "2003", "2006", "2009", "2012", "2015", "2018", "2022", "2025")
VALSTIS <- c("LVA", "EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU")
S25 <- Sys.getenv("PISA2025_SAV_DIR", unset = path.expand("~/pisa2025_sav"))

# ----------------------------------------------------------------------------
# 1. Inventāri (64a) un vecie vērtību etiķešu faili
# ----------------------------------------------------------------------------
if (!file.exists("output/pisa2025/atlants_veci_etiketes.csv")) {
  cat("Palaiž py/atlants_spss_etiketes.py\n"); system2("python3", "py/atlants_spss_etiketes.py")
}
VE <- fread("output/pisa2025/atlants_veci_etiketes.csv", colClasses = "character")
KOGV <- fread("output/pisa2025/atlants_veci_kog_skaits.csv", colClasses = "character")

IV <- fread("output/pisa2025/simptomi_inventars_veci.csv", colClasses = c(gads = "character"))
IJ <- fread("output/pisa2025/simptomi_inventars_jaunie.csv", colClasses = c(gads = "character"))
IV[, mainigais := toupper(mainigais)]
IV <- merge(IV, VE[, .(gads, fails, mainigais, vertibu_etiketes, vertibu_etikesu_skaits = as.integer(vertibu_etikesu_skaits))],
            by = c("gads", "fails", "mainigais"), all.x = TRUE)
IV[is.na(vertibu_etiketes), `:=`(vertibu_etiketes = "", vertibu_etikesu_skaits = 0L)]
IV[, tips_raw := tips]
cat("Vecie cikli (2000–2012):", nrow(IV), "mainīgie; ar vērtību etiķetēm:", sum(IV$vertibu_etikesu_skaits > 0), "\n")

# ----------------------------------------------------------------------------
# 2. Jaunie cikli: etiķetes un vērtību etiķetes no kešiem; skolotāju anketas; galvenes
# ----------------------------------------------------------------------------
TRUKST_RE <- "^(n/?a|invalid|miss|missing|mis|m/r|not applicable|valid skip.*|no response|not reached|system missing|not administered.*|valid skip \\(filter rule\\)|missing by design|not applicable.*)$"
vert_etik <- function(x) {
  l <- attr(x, "labels"); if (is.null(l) || !length(l)) return(c("", "0"))
  n <- names(l); keep <- !grepl(TRUKST_RE, trimws(n), ignore.case = TRUE)
  l <- l[keep]; n <- n[keep]; if (!length(l)) return(c("", "0"))
  c(paste0(as.character(l), "=", n, collapse = "|"), as.character(length(l)))
}
meta_no_df <- function(d, g, fails) {
  lab <- vapply(d, function(x) { l <- attr(x, "label"); if (is.null(l)) "" else paste(l, collapse = " ") }, "")
  ve <- vapply(d, vert_etik, character(2))
  data.table(gads = g, fails = fails, mainigais = names(d), etikete = unname(lab), vertibu_etiketes = ve[1, ],
             vertibu_etikesu_skaits = as.integer(ve[2, ]), tips_raw = vapply(d, function(x) if (is.character(x)) "a" else "num", ""))
}
lasa_valstis <- function(f) {
  cnt <- as.character(haven::read_sav(f, col_select = "CNT")$CNT)
  r <- rle(cnt %in% VALSTIS); beigas <- cumsum(r$lengths); sakums <- beigas - r$lengths + 1
  bl <- which(r$values)
  if (!length(bl)) return(NULL)
  do.call(rbind, lapply(bl, function(b) haven::read_sav(f, skip = sakums[b] - 1, n_max = r$lengths[b])))
}
derigums <- function(d, g, fails) {
  cnt <- as.character(d$CNT); lv <- cnt == "LVA"; sal <- !lv
  der <- vapply(d, function(x) !is.na(x) & (if (is.character(x)) nzchar(trimws(x)) else TRUE), logical(nrow(d)))
  data.table(gads = g, fails = fails, mainigais = names(d), lva_n = sum(lv),
             lva_derigi_pct = if (sum(lv)) round(100 * colMeans(der[lv, , drop = FALSE]), 1) else NA_real_,
             sal_derigi_pct = if (sum(sal)) round(100 * colMeans(der[sal, , drop = FALSE]), 1) else NA_real_,
             sal_valstis_ar_datiem = vapply(seq_along(d), function(j) length(unique(cnt[sal & der[, j]])), 1L),
             lva_vertibu_skaits = vapply(d, function(x) length(unique(x[lv & !is.na(x)])), 1L))
}

FAILI_ANK <- list(
  "2015" = c(TCH = "data/pisa2015/raw/CY6_MS_CMB_TCH_QQQ.sav"),
  "2018" = c(TCH = "data/pisa2018/raw/TCH/CY07_MSU_TCH_QQQ.sav"),
  "2022" = c(TCH = "data/pisa2022/raw/CY08MSP_TCH_QQQ.SAV"),
  "2025" = c(TCH = file.path(S25, "CY09_MS_TCH_PUF.sav")))
FAILI_SKAITS <- list(
  "2015" = c(COG = "data/pisa2015/raw/CY6_MS_CMB_STU_COG.sav"),
  "2018" = c(COG = "data/pisa2018/raw/COG/CY07_MSU_STU_COG.sav", TIM = "data/pisa2018/raw/TIM/CY07_MSU_STU_TIM.sav"),
  "2022" = c(COG = "data/pisa2022/raw/CY08MSP_STU_COG.SAV", TIM = "data/pisa2022/raw/CY08MSP_STU_TIM.SAV", CRT = "data/pisa2022/raw/CY08MSP_CRT_COG.SAV"),
  "2025" = c(COG = file.path(S25, "CY09_MS_COG_20260806.sav"), COG_PROCESS = file.path(S25, "CY09_MS_COG_PROCESS_20260806.sav"),
             TT = file.path(S25, "CY09_MS_STU_TT_PUF.sav")))

MJ <- list(); DJ <- list(); SKAITI <- list()
for (g in c("2015", "2018", "2022", "2025")) {
  X <- readRDS(sprintf("data/pisa2025/processed/simptomi_%s.rds", g))
  MJ[[paste(g, "STU")]] <- meta_no_df(X$stu, g, "STU"); MJ[[paste(g, "SCH")]] <- meta_no_df(X$sch, g, "SCH")
  rm(X); invisible(gc(verbose = FALSE))
  for (ft in names(FAILI_ANK[[g]])) {
    f <- FAILI_ANK[[g]][[ft]]; if (!file.exists(f)) { cat("  nav faila:", f, "\n"); next }
    kess <- sprintf("data/pisa2025/processed/atlants_%s_%s.rds", tolower(ft), g)
    if (file.exists(kess)) d <- readRDS(kess) else { d <- lasa_valstis(f); saveRDS(d, kess) }
    if (is.null(d)) { cat("  ", g, ft, ": neviena no 8 valstīm\n"); next }
    MJ[[paste(g, ft)]] <- meta_no_df(d, g, ft); DJ[[paste(g, ft)]] <- derigums(d, g, ft)
    cat(sprintf("  %s %s: %d mainīgie; valstis: %s\n", g, ft, ncol(d), paste(names(table(d$CNT)), table(d$CNT), collapse = " ")))
    rm(d)
  }
  for (ft in names(FAILI_SKAITS[[g]])) {
    f <- FAILI_SKAITS[[g]][[ft]]; if (!file.exists(f)) { cat("  nav faila:", f, "\n"); next }
    h <- tryCatch(haven::read_sav(f, n_max = 0), error = function(e) haven::read_sav(f, n_max = 0, encoding = "latin1"))   # 2025 TT fails: kodējuma kļūda
    SKAITI[[length(SKAITI) + 1]] <- data.table(gads = g, fails = ft, mainigo_skaits = ncol(h), avots = f)
  }
}
MJ <- rbindlist(MJ); DJ <- rbindlist(DJ)
IJ <- rbind(IJ[, .(gads, fails, mainigais, lva_n, lva_derigi_pct, sal_derigi_pct, sal_valstis_ar_datiem, lva_vertibu_skaits)], DJ, fill = TRUE)
IJ <- merge(MJ, IJ, by = c("gads", "fails", "mainigais"), all.x = TRUE)
cat("Jaunie cikli (2015–2025):", nrow(IJ), "mainīgie (STU, SCH, TCH)\n")
SKAITI <- rbind(rbindlist(SKAITI), KOGV[, .(gads, fails, mainigo_skaits = as.integer(mainigo_skaits), avots)], fill = TRUE)

# ----------------------------------------------------------------------------
# 3. Vienota tabula, tips, bloks, tēma
# ----------------------------------------------------------------------------
A <- rbind(IV[, .(gads, fails, mainigais, etikete, tips_raw, vertibu_etiketes, vertibu_etikesu_skaits, lva_n, lva_derigi_pct,
                  sal_derigi_pct, sal_valstis_ar_datiem, lva_vertibu_skaits)],
           IJ[, .(gads, fails, mainigais, etikete, tips_raw, vertibu_etiketes, vertibu_etikesu_skaits, lva_n, lva_derigi_pct,
                  sal_derigi_pct, sal_valstis_ar_datiem, lva_vertibu_skaits)])
A[, etikete := gsub("[\r\n\t]+", " ", etikete)]
A[, vertibu_etiketes := gsub("[\r\n\t]+", " ", vertibu_etiketes)]
A[, mainigais_u := toupper(mainigais)]

ID_RE <- "^(CNT|CNTRYID|CNTSCHID|CNTSTUID|CNTTCHID|COUNTRY|SCHOOLID|STIDSTD|SUBNATIO|STRATUM|OECD|NC|REGION|NATCEN|CYC|TEACHERID|SUBREGION|SCHID|STUID|TCHID|CNTTCHRID|CNTSCHRID|SCHOOL_ID|STUDENT_ID)$"
SVARS_RE <- "^(W_|SENWT|WVARSTR|VARUNIT|UNIT$|RANDUNIT|VAR_UNIT|STRATID|SCWEIGHT|TCHWGT|W_TCH|STUWT|SCHWGT|.*_WT$|.*WT[0-9]*$|WVAR|SENWGT)"
DIZ_RE <- "^(BOOKID|ADMINMODE|LANGTEST|OPTION_|TESTLANG|CBASCI|QUESTID|VER_|RDESIGN|RCORE|RS[12]_|CLCUSE|BOOKLET|FORM|TEST$|EFFORT[0-9]$|ISCEDO$|MODE$|VERSION|UH$|COBN_|EASY|PAQ_|ICTQ$|WBQ$|FLQ$|GCQ$|QMODE|.*_FORM$|SUBNAT|PROGN$|ST_FORM|NCLEV|OPTION|TESTLANG)"
A[, bloks := ifelse(grepl("^[A-Z]{2,3}[0-9]{2,3}", mainigais_u), sub("^([A-Z]{2,3}[0-9]{2,3}).*", "\\1", mainigais_u), "")]
A[grepl("^PV[0-9]", mainigais_u), bloks := ""]
A[, jautajums := bloks != ""]
A[, tips := fifelse(grepl(ID_RE, mainigais_u), "ID",
            fifelse(grepl("^PV[0-9]+", mainigais_u), "PV",
            fifelse(grepl(SVARS_RE, mainigais_u), "svars",
            fifelse(grepl(DIZ_RE, mainigais_u) & !jautajums, "dizains",
            fifelse(!jautajums & vertibu_etikesu_skaits < 2, "indekss",
            fifelse(!jautajums & vertibu_etikesu_skaits >= 2, "atvasināts kategoriāls",
            fifelse(tips_raw == "a", "teksts",
            fifelse(vertibu_etikesu_skaits >= 2 | (!is.na(lva_vertibu_skaits) & lva_vertibu_skaits <= 12 & lva_vertibu_skaits > 0), "kategoriāls", "skaitlisks"))))))))]
A[bloks == "", bloks := fifelse(tips %in% c("ID", "PV", "svars", "dizains"), "tehnisks", "atvasināts")]
A[, anketa := fcase(
  fails == "SCH", "skolas",
  fails == "TCH", "skolotāju",
  grepl("^PA[0-9]", mainigais_u) | grepl("^(PA|PQ)", bloks) | (gads %in% c("2006", "2009", "2012") & grepl("^PA", mainigais_u)), "vecāku",
  grepl("^IC[0-9]", mainigais_u), "IKT",
  grepl("^WB[0-9]", mainigais_u), "labbūtības",
  grepl("^FL[0-9]", mainigais_u), "finanšu pratības",
  grepl("^EC[0-9]", mainigais_u), "izglītības gaitas",
  grepl("^(GC|CR)[0-9]", mainigais_u), "papildu moduļa",
  tips %in% c("ID", "PV", "svars", "dizains"), "tehniskie",
  !jautajums, "atvasinātie",
  default = "skolēnu")]
A[, valsts_specifisks := grepl("<[^>]+>", etikete)]

# Tēma pēc atslēgvārdiem etiķetē (pirmais sakritušais; pēc tam bloka noklusējums)
TEMAS <- list(
  c("MI", "artificial intelligence|\\bAI\\b|chatbot|chatgpt|generative|machine learning|large language"),
  c("radošums", "creativ|imagin|inventiv|original idea|novel idea|brainstorm"),
  c("globālā kompetence", "global (issue|competen|aware)|intercultural|other cultures|cultural background|people from other|different culture|immigrants? (should|are)|world (issue|event)|climate change|poverty|refugee|equal rights|migration"),
  c("ģimene", "country of birth|\\bborn\\b|mother|father|sibling|brother|sister|immigra|household|home possess|\\bat home\\b|guardian|parents? (help|talk|ask|discuss|spend|eat|read)"),
  c("finanses", "financ(ial)? (literac|matter|education|product|decision|knowledge|behaviou?r|goal|risk|service)|money|bank account|pocket money|saving|purchase|price|\\bpay|salary|insurance|invest|loan|credit card|debt|allowance"),
  c("vide", "environment(al)? (issue|problem|protect|awareness|friendly)|pollution|recycl|sustainab|energy (use|saving)|ecolog|natural resource|nuclear|water shortage|extinction|deforest"),
  c("IKT", "computer|internet|digital|\\bict\\b|online|device|software|laptop|tablet|smartphone|mobile phone|cell phone|social (media|network)|video ?game|e-?mail|screen|\\bapps?\\b|website|\\bweb\\b|electronic|programm|e-?book|technolog|typing|keyboard|wi-?fi|game console|chat|download"),
  c("lasīšana", "\\bread|\\bbook|librar|\\btext|literac|novel|fiction|magazine|newspaper|comic|\\bpages?\\b|writing|\\bwrite|\\bwritten|literature|poetry|poem|\\bstor(y|ies)|vocabular|summar|paragraph|\\bauthors?\\b"),
  c("matemātika", "math|calculat|algebra|geometr|arithmetic|numer|equation|formula|\\bgraph|statistic"),
  c("dabaszinātnes", "scien|biolog|chemi|physic|experiment|laborator|scientist|natural phenomen|hypothes"),
  c("karjera", "career|occupation|\\bjob|future (education|study|work|occupation)|expect.* (education|degree|complete|qualif)|work(ing)? (life|experience|placement)|internship|apprentice|vocational|university|college|training (for|course)|employ|profession(al)? (future|plan)|adult (life|role)"),
  c("labbūtība", "life satisf|happy|well-?being|health|anxi|worr|stress|sleep|exercise|sport|physical activ|\\beat|breakfast|\\bfeel|emotion|bully|bullied|lonel|belong|confiden|resilien|growth mindset|meaning in life|purpose|\\bfear|\\bsad\\b|afraid|proud|nervous|cheerful|lively|miserable|scared|calm|joyful|energetic|hopeless|tense"),
  c("mācīšanās stratēģijas", "strateg|memori|underline|elaborat|\\beffort|persever|motivat|self-?efficacy|\\bgoal|homework|\\bstudy|learn(ing)? (time|activit|approach|strateg)|hours? (per|a|each) week|out-?of-?school|additional instruction|tutor|revis|attention|concentrat|self-?regul|metacogn|\\bplan|give up|hard work|try hard|interest in|enjoy learning|ambitio|competit|mastery|control (over|of)"),
  c("skolotāji", "teacher|instructor|\\bteach|staff|principal|professional development|feedback|\\bexplain|lesson|classroom (management|disciplin)|adapt|\\bsupport|encourag|mentor|qualification"),
  c("skola/klimats", "school|\\bclass|disciplin|truan|\\blate\\b|skip|absent|admission|curricul|extra-?curricular|assess|track|repeat|\\bgrade|program|climate|\\bsafe|educational|policy|resourc|shortage|building|isced|attend|pre-?primary|kindergarten|enrol|subject|course|exam|noise|disorder|student behaviour|instruction time|minutes|budget|responsib"),
  c("ģimene", "mother|father|parent|\\bhome|family|sibling|household|\\bborn|immigra|language|possession|\\broom|\\bcar\\b|dishwasher|guardian|brother|sister|\\badult|income|wealth|socio|isei|country of birth|age (when|at)|\\bage\\b|gender|\\bsex\\b|female|male|migrant|native"))
BLOKA_TEMA <- c(IC = "IKT", PA = "ģimene", TC = "skolotāji", WB = "labbūtība", FL = "finanses", GC = "globālā kompetence", CR = "radošums",
                EC = "karjera", SC = "skola/klimats")
tema_f <- function(etik, mainigais_u, tips) {
  out <- rep(NA_character_, length(etik)); teh <- tips %in% c("ID", "PV", "svars", "dizains")
  out[teh] <- "tehnisks"
  for (tm in TEMAS) { i <- is.na(out) & grepl(tm[2], etik, ignore.case = TRUE, perl = TRUE); out[i] <- tm[1] }
  # bloka noklusējums, tad indeksu nosaukumi
  pref <- substr(mainigais_u, 1, 2)
  i <- is.na(out) & pref %in% names(BLOKA_TEMA) & grepl("^[A-Z]{2}[0-9]", mainigais_u); out[i] <- BLOKA_TEMA[pref[i]]
  i <- is.na(out) & grepl("READ|JOY|DIV|META|UND|STIM|LIT", mainigais_u); out[i] <- "lasīšana"
  i <- is.na(out) & grepl("MATH|MAT$|ANX", mainigais_u); out[i] <- "matemātika"
  i <- is.na(out) & grepl("SCI", mainigais_u); out[i] <- "dabaszinātnes"
  i <- is.na(out) & grepl("ICT|COMP|ENTUSE|HOMESCH|USESCH|INTUSE|PRGUSE", mainigais_u); out[i] <- "IKT"
  i <- is.na(out) & grepl("ESCS|HOMEPOS|HISEI|HISCED|PARED|IMMIG|LANGN|BMMJ|BFMJ|BSMJ|MISCED|FISCED|HEDRES|CULTPOSS|WEALTH|FAMSTRUC|COBN|OCOD|AGE$|GENDER", mainigais_u); out[i] <- "ģimene"
  i <- is.na(out) & grepl("BELONG|ATSCHL|STUREL|TEACH|DISC|CLSIZE|SCHSIZE|STRATIO|PROP|RATCMP|SCHAUT|LEAD|EDUSHORT|STAFFSHORT|SCHSEL|SCMAT|CLIMATE|ABGROUP|GRADE|REPEAT|OUTHOURS|LMINS|MMINS|SMINS|TMINS|PROGN|ISCEDL", mainigais_u); out[i] <- "skola/klimats"
  i <- is.na(out) & grepl("EFF|MEMOR|ELAB|CSTRAT|CONTROL|PERSEV|OPENPS|INTMAT|INSTMOT|WORKMAST|MOTIV|GFOFAIL|COMPETE|MASTGOAL", mainigais_u); out[i] <- "mācīšanās stratēģijas"
  i <- is.na(out) & grepl("LIFE|SWBP|EMO|RESIL|BODY|SLEEP|EXER|PHYS|WB", mainigais_u); out[i] <- "labbūtība"
  out[is.na(out)] <- "cits"; out
}
A[, tema := tema_f(etikete, mainigais_u, tips)]
cat("\nVienotā tabula:", nrow(A), "rindas\n"); print(dcast(A[, .N, by = .(gads, fails)], fails ~ gads, value.var = "N", fill = 0))
cat("\nTipi:\n"); print(table(A$tips)); cat("\nTēmas:\n"); print(sort(table(A$tema), decreasing = TRUE))

# ----------------------------------------------------------------------------
# 4. Konstrukti pa cikliem: manuāls > vienāds nosaukums > etiķetes līdzība
# ----------------------------------------------------------------------------
A[, id := seq_len(.N)]
A[, grupa_f := fifelse(fails %in% c("STU", "QQ2"), "STU", fails)]
STOP <- c("the", "a", "an", "of", "to", "in", "on", "for", "and", "or", "your", "you", "do", "does", "how", "what", "which",
          "is", "are", "this", "that", "at", "with", "by", "from", "as", "be", "have", "has", "q", "st", "sc", "i", "my",
          "me", "it", "about", "during", "last", "following", "these", "those", "derived", "index", "wle", "student", "school",
          "please", "think", "much", "often", "would", "were", "was", "when", "there", "than", "any", "all", "not", "did")
norm_v <- function(s) {
  s <- tolower(s); s <- gsub("<[^>]*>|\\[[^]]*\\]", " ", s); s <- gsub("\\b(st|sc|ec|ic|pa|tc|wb|fl|gc|q)[0-9]+[a-z0-9]*\\b", " ", s)
  s <- gsub("[^a-z ]", " ", s)
  lapply(strsplit(s, " +"), function(w) { w <- unique(w[nchar(w) > 2 & !(w %in% STOP)]); w })
}
aste <- function(s) { s2 <- sub(".*( - |: |\\? )", "", s); ifelse(s2 == s, "", s2) }
A[, tok := norm_v(etikete)]; A[, tok_a := norm_v(aste(etikete))]
VOC <- unique(unlist(c(A$tok, A$tok_a)))
smat <- function(tokens, ids) {
  l <- lengths(tokens); if (!sum(l)) return(NULL)
  sparseMatrix(i = rep(seq_along(tokens), l), j = match(unlist(tokens), VOC), x = 1, dims = c(length(tokens), length(VOC)))
}
lidz_pari <- function(ia, ib, kol, min_tok = 1) {
  ta <- A[[kol]][ia]; tb <- A[[kol]][ib]; oka <- lengths(ta) >= min_tok; okb <- lengths(tb) >= min_tok
  if (!any(oka) || !any(okb)) return(NULL)
  ia <- ia[oka]; ib <- ib[okb]; ta <- ta[oka]; tb <- tb[okb]
  Ma <- smat(ta); Mb <- smat(tb); S <- summary(Ma %*% t(Mb))
  na <- lengths(ta); nb <- lengths(tb)
  data.table(a = ia[S$i], b = ib[S$j], sim = S$x / (na[S$i] + nb[S$j] - S$x))
}
PARI <- list()
for (fg in unique(A$grupa_f)) for (i in seq_len(length(GADI) - 1)) for (j in (i + 1):length(GADI)) {
  ia <- A[grupa_f == fg & gads == GADI[i] & !tips %in% c("ID", "PV", "svars", "dizains")]$id
  ib <- A[grupa_f == fg & gads == GADI[j] & !tips %in% c("ID", "PV", "svars", "dizains")]$id
  if (!length(ia) || !length(ib)) next
  # vienāds nosaukums: droši 2015+ stila kodi un atvasinātie (bez Q) vienmēr; vecie Q-kodi tikai ar etiķetes līdzību ≥ 0,4
  m <- match(A$mainigais_u[ia], A$mainigais_u[ib]); ok <- !is.na(m)
  if (any(ok)) {
    a <- ia[ok]; b <- ib[m[ok]]
    drošs <- grepl("(TA|NA|JA|HA|IA|DA|JB|WA|D0[0-9]T|_[0-9])$", A$mainigais_u[a]) | !grepl("Q[0-9]", A$mainigais_u[a])
    jac <- mapply(function(x, y) if (!length(x) || !length(y)) 0 else length(intersect(x, y)) / length(union(x, y)), A$tok[a], A$tok[b])
    keep <- drošs | jac >= 0.4
    if (any(keep)) PARI[[length(PARI) + 1]] <- data.table(a = a[keep], b = b[keep], sim = 1, veids = "nosaukums", attalums = j - i)
  }
  p1 <- lidz_pari(ia, ib, "tok"); p2 <- lidz_pari(ia, ib, "tok_a", min_tok = 2)
  p <- rbind(p1, p2); if (is.null(p) || !nrow(p)) next
  p <- p[, .(sim = max(sim)), by = .(a, b)][sim >= 0.6]
  if (nrow(p)) PARI[[length(PARI) + 1]] <- p[, .(a, b, sim, veids = "etikete", attalums = j - i)]
}
PARI <- rbindlist(PARI)
# manuālā karte (64d): visi konstrukta locekļi savienoti, augstākā prioritāte
M <- fread("R/pisa2025/simptomi_karte.csv", colClasses = "character")[, mainigais := toupper(mainigais)]
M[, fails := ifelse(grepl("^SK_", konstrukts), "SCH", "STU")]
M <- merge(M, A[grupa_f %in% c("STU", "SCH"), .(gads, mainigais_u, grupa_f, id)], by.x = c("gads", "mainigais", "fails"), by.y = c("gads", "mainigais_u", "grupa_f"))
MP <- M[, if (.N > 1) data.table(a = id[1], b = id[-1]) else NULL, by = konstrukts][, .(a, b, sim = 2, veids = "manuāls", attalums = 0)]
PARI <- rbind(MP, PARI)
PARI <- PARI[order(-sim, attalums)]
cat("\nKandidātpāri:", nrow(PARI), "(manuāli", sum(PARI$veids == "manuāls"), ", nosaukums", sum(PARI$veids == "nosaukums"),
    ", etiķete", sum(PARI$veids == "etikete"), ")\n")

# Savienošana: konstrukts nedrīkst saturēt divus mainīgos no viena cikla (konflikts → pāri izlaiž)
sak <- seq_len(nrow(A)); cikli_k <- lapply(seq_len(nrow(A)), function(i) A$gads[i])
atrod <- function(x) { while (sak[x] != x) { sak[x] <<- sak[sak[x]]; x <- sak[x] }; x }
veidi <- vector("list", nrow(A)); veidi[] <- list(character(0))
for (r in seq_len(nrow(PARI))) {
  ra <- atrod(PARI$a[r]); rb <- atrod(PARI$b[r]); if (ra == rb) next
  if (length(intersect(cikli_k[[ra]], cikli_k[[rb]]))) next
  sak[rb] <- ra; cikli_k[[ra]] <- c(cikli_k[[ra]], cikli_k[[rb]]); veidi[[ra]] <- c(veidi[[ra]], veidi[[rb]], PARI$veids[r])
}
A[, sakne := vapply(id, atrod, 1L)]
manual_nos <- M[, .(sakne = vapply(id, atrod, 1L), konstrukts)][, .(konstrukts = konstrukts[1]), by = sakne]
A[, konstrukts := paste0(grupa_f, ":", gads, ":", mainigais_u)[match(sakne, id)]]
A[sakne %in% manual_nos$sakne, konstrukts := manual_nos$konstrukts[match(sakne, manual_nos$sakne)]]
A[, harmonizacija := vapply(sakne, function(s) { v <- unique(veidi[[s]])
  if (!length(v)) "nav" else if ("manuāls" %in% v) "manuāls" else if (all(v == "nosaukums")) "vienāds nosaukums"
  else if (all(v == "etikete")) "etiķetes līdzība" else "nosaukums + etiķete" }, "")]
A[tips %in% c("ID", "PV", "svars", "dizains"), harmonizacija := "tehnisks"]
KP <- fread("output/pisa2025/simptomi_karte_pilna.csv", colClasses = "character")[, mainigais := toupper(mainigais)]
A[, konstrukts_64d := KP$konstrukts[match(paste(gads, grupa_f, mainigais_u), paste(KP$gads, KP$fails, KP$mainigais))]]
A[is.na(konstrukts_64d), konstrukts_64d := ""]
A[, ciklu_skaits := uniqueN(gads), by = konstrukts]
cat("Konstrukti:", uniqueN(A$konstrukts), "; harmonizācijas veidi (konstruktu līmenī):\n")
print(table(A[, .(harmonizacija = harmonizacija[1]), by = konstrukts]$harmonizacija))

# ----------------------------------------------------------------------------
# 5. Lietojums mūsu skriptos un modeļu izvadēs
# ----------------------------------------------------------------------------
tok_fails <- function(f) { t <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  unique(regmatches(t, gregexpr("[A-Za-z][A-Za-z0-9_]+", t))[[1]]) }
izv_kol <- function(f, kol) { if (!file.exists(f)) return(character(0)); d <- fread(f, colClasses = "character"); unique(unlist(d[, ..kol])) }
VARS <- unique(c(A$mainigais, A$mainigais_u)); KONS <- unique(c(KP$konstrukts, sub("__B$", "", KP$konstrukts)))
sk_ceļš <- function(x) sprintf("R/pisa2025/%s", x)
GRUPAS <- list(
  "64h" = list(skripti = c("64h_simptomu_modelis_kopigas.R", "64i_simptomi_nozimiba.R"),
               izvades = list(c("output/pisa2025/simptomi_modelis_C_shap.csv", "pazime"), c("output/pisa2025/simptomi_C_nozimiba_svarigums.csv", "vieniba"))),
  "69a_f" = list(skripti = c("69a_citi_iznakumi.R", "69f_raitums_iesaiste_iznakumi.R"),
                 izvades = list(c("output/pisa2025/citi_iznakumi_shap.csv", "pazime"), c("output/pisa2025/citi_iznakumi_2_shap.csv", "pazime"))),
  "69c_75" = list(skripti = c("69c_tikla_modelis.R", "75_bayesa_tikls.R"),
                  izvades = c(list(c("output/pisa2025/tikls_malas.csv", "no"), c("output/pisa2025/tikls_malas.csv", "uz")),
                              lapply(list.files("output/pisa2025", "^bn_loki_.*\\.csv$", full.names = TRUE), function(f) c(f, "no", "uz")))),
  "67d" = list(skripti = "67d_dinamiskais_faktoru_modelis.R", izvades = list()),
  "58" = list(skripti = c("55_agrie_signali_sadalijums.R", "56_citas_jomas.R", "57_apakskalas_paradumi.R", "58a_pisa_valstu_videjie.R", "58b_iea_kohortas.R"),
              izvades = list(), doki = c("docs/58_agrie_signali_rezultati.md", "docs/57_agrie_signali_plans.md")))
NELIETO <- c("64a_simptomi_izvilkums.R", "64b_simptomi_katalogs.R", "64c_simptomi_domeni.R", "64d_simptomi_karte.R", "64_simptomi_riki.R",
             "82_mainigo_atlants.R", "14_datu_inventarizacija_2025.R", "19_mainigo_audits.R")
visi_sk <- list.files("R/pisa2025", "\\.R$"); py_sk <- list.files("py", "\\.py$")
citur_sk <- setdiff(visi_sk, c(NELIETO, unlist(lapply(GRUPAS, `[[`, "skripti"))))
py_nelieto <- c("simptomi_veci_izvilkums.py", "atlants_spss_etiketes.py", "pisa_veci_izvilkums.py", "kog_veci_izvilkums.py")
GRUPAS[["citur"]] <- list(skripti = citur_sk, py = setdiff(py_sk, py_nelieto), izvades = list())

# Katram skriptam: mainīgo un konstruktu kopas; katrai grupai apvieno
SK_TOK <- list()
for (s in visi_sk) SK_TOK[[s]] <- tok_fails(sk_ceļš(s))
for (s in py_sk) SK_TOK[[paste0("py/", s)]] <- tok_fails(file.path("py", s))
lietots_kopa <- function(gr) {
  tk <- unique(unlist(c(lapply(gr$skripti, function(s) SK_TOK[[s]]), lapply(gr$py, function(s) SK_TOK[[paste0("py/", s)]]),
                        lapply(gr$doki, tok_fails), lapply(gr$izvades, function(x) izv_kol(x[1], x[-1])))))
  tk <- sub("__B$", "", tk)
  list(vars = intersect(tk, VARS), kons = intersect(tk, KONS))
}
A[, k64d_bez := sub("__B$", "", konstrukts_64d)]
for (gn in names(GRUPAS)) {
  L <- lietots_kopa(GRUPAS[[gn]])
  A[, (paste0("lietots_", gn)) := mainigais %in% L$vars | mainigais_u %in% L$vars | (k64d_bez != "" & k64d_bez %in% L$kons)]
}
# 64e apvienotā tabula (konstrukti ≥ 2 cikli; datu sagatavošana, ne modelis)
APV <- setdiff(names(readRDS("data/pisa2025/processed/simptomi_apvienots.rds")), c("gads", "cnt", "skola", "skolens", "w", "langn", "plusma", paste0("PV", 1:10, "READ"), "PVREAD", "Q_TRUKST"))
A[, apvienots_64e := k64d_bez != "" & k64d_bez %in% sub("__B$", "", APV)]
# skriptu saraksts katram mainīgajam
A[, lietots_skripti := ""]
for (s in c(setdiff(visi_sk, NELIETO), paste0("py/", setdiff(py_sk, py_nelieto)))) {
  tk <- sub("__B$", "", SK_TOK[[s]]); v <- intersect(tk, VARS); k <- intersect(tk, KONS)
  i <- A$mainigais %in% v | A$mainigais_u %in% v | (A$k64d_bez != "" & A$k64d_bez %in% k)
  if (any(i)) A[i, lietots_skripti := ifelse(lietots_skripti == "", sub("\\.R$", "", s), paste(lietots_skripti, sub("\\.R$", "", s), sep = ", "))]
}
LIET <- paste0("lietots_", names(GRUPAS))
A[, lietots := Reduce(`|`, lapply(LIET, function(k) A[[k]]))]
A[, saturisks := !tips %in% c("ID", "PV", "svars", "dizains")]
cat("\nLietojums (saturiskie mainīgie, bez ID/PV/svariem/dizaina):", sum(A$saturisks), "; lietoti kaut kur:", sum(A$saturisks & A$lietots),
    sprintf("(%.1f %%)\n", 100 * mean(A$lietots[A$saturisks])))
for (k in c(LIET, "apvienots_64e")) cat(sprintf("  %-16s %5d\n", k, sum(A$saturisks & A[[k]])))

# ----------------------------------------------------------------------------
# 6. Izvades: mainīgie, konstrukti, kopsavilkums, kandidāti
# ----------------------------------------------------------------------------
isi <- function(v, n = 8, maxc = 220) { p <- strsplit(v, "|", fixed = TRUE)
  vapply(p, function(x) { if (!length(x) || identical(x, "")) return(""); s <- paste(head(x, n), collapse = "; "); if (length(x) > n) s <- paste0(s, " (+", length(x) - n, ")")
    if (nchar(s) > maxc) s <- paste0(substr(s, 1, maxc), "…"); s }, "") }
A[, vertibu_etiketes_isi := isi(vertibu_etiketes)]
MAT <- A[, .(matrica = paste(ifelse(GADI %in% gads, "●", "·"), collapse = "")), by = konstrukts]
A[, matrica := MAT$matrica[match(konstrukts, MAT$konstrukts)]]
setorder(A, gads, fails, id)
MAIN <- A[, .(gads, fails, anketa, mainigais, etikete, tips, bloks, tema, valsts_specifisks, lva_n, lva_derigi_pct, sal_derigi_pct,
              sal_valstis_ar_datiem, lva_vertibu_skaits, vertibu_etikesu_skaits, vertibu_etiketes = vertibu_etiketes_isi,
              konstrukts, ciklu_skaits, matrica, harmonizacija, konstrukts_64d, apvienots_64e,
              lietots_64h, lietots_69a_f, lietots_69c_75, lietots_67d, lietots_58, lietots_citur, lietots, lietots_skripti)]
fwrite(MAIN, "output/pisa2025/atlants_mainigie.csv")
cat("\nIzvade: output/pisa2025/atlants_mainigie.csv,", nrow(MAIN), "rindas\n")

KON <- A[, {
  z <- .SD[order(match(gads, GADI))]
  r <- data.table(fails = grupa_f[1], etikete = z$etikete[nrow(z)], tema = names(sort(table(tema), decreasing = TRUE))[1],
                  tips = names(sort(table(tips), decreasing = TRUE))[1], ciklu_skaits = uniqueN(gads), no = z$gads[1], lidz = z$gads[nrow(z)],
                  matrica = paste(ifelse(GADI %in% gads, "●", "·"), collapse = ""), harmonizacija = harmonizacija[1],
                  konstrukts_64d = paste(unique(konstrukts_64d[konstrukts_64d != ""]), collapse = "|"), apvienots_64e = any(apvienots_64e),
                  lietots_64h = any(lietots_64h), lietots_69a_f = any(lietots_69a_f), lietots_69c_75 = any(lietots_69c_75),
                  lietots_67d = any(lietots_67d), lietots_58 = any(lietots_58), lietots_citur = any(lietots_citur), lietots = any(lietots),
                  lva_derigi_pct_max = suppressWarnings(max(lva_derigi_pct, na.rm = TRUE)),
                  lietots_skripti = paste(unique(unlist(strsplit(lietots_skripti[lietots_skripti != ""], ", "))), collapse = ", "))
  for (g in GADI) r[[paste0("v", g)]] <- paste(z$mainigais[z$gads == g], collapse = "|")
  r }, by = konstrukts]
KON[!is.finite(lva_derigi_pct_max), lva_derigi_pct_max := NA]
setorder(KON, -ciklu_skaits, fails, konstrukts)
fwrite(KON, "output/pisa2025/atlants_konstrukti.csv")
cat("Izvade: output/pisa2025/atlants_konstrukti.csv,", nrow(KON), "konstrukti\n")
KS <- KON[harmonizacija != "tehnisks"]
cat("\nSaturiskie konstrukti pa ciklu skaitu (rindas: fails):\n"); print(table(KS$fails, KS$ciklu_skaits))
cat("\nViena cikla konstrukti:", sum(KS$ciklu_skaits == 1), sprintf("(%.1f %% no %d)", 100 * mean(KS$ciklu_skaits == 1), nrow(KS)), "\n")
cat("Nelietoti nekur:", sum(!KS$lietots), sprintf("(%.1f %%)", 100 * mean(!KS$lietots)), "; no tiem viena cikla:", sum(!KS$lietots & KS$ciklu_skaits == 1), "\n")
cat("\nNelietotie saturiskie mainīgie pa tēmām (mainīgo līmenī):\n")
NT <- A[(saturisks), .(kopa = .N, nelietoti = sum(!lietots), viena_cikla = sum(ciklu_skaits == 1), nelietoti_viena_cikla = sum(!lietots & ciklu_skaits == 1)), by = tema][order(-nelietoti)]
print(NT, row.names = FALSE)
cat("\nNelietotie saturiskie mainīgie pa cikliem:\n")
NC <- A[(saturisks), .(kopa = .N, nelietoti = sum(!lietots), nelietoti_pct = round(100 * mean(!lietots), 1)), by = gads][order(gads)]
print(NC, row.names = FALSE)

# Kopsavilkums cikls × fails
KOPS <- A[, .(mainigo_skaits = .N, anketas_jautajumi = sum(jautajums), indeksi = sum(tips %in% c("indekss", "atvasināts kategoriāls")),
              tehniskie = sum(!saturisks), viena_cikla = sum(saturisks & ciklu_skaits == 1), lietoti = sum(saturisks & lietots),
              nelietoti = sum(saturisks & !lietots), lva_derigi_vid_pct = round(mean(lva_derigi_pct[saturisks], na.rm = TRUE), 1),
              lva_n = max(lva_n, na.rm = TRUE)), by = .(gads, fails)]
KOPS <- rbind(KOPS, SKAITI[, .(gads, fails, mainigo_skaits, anketas_jautajumi = NA_integer_, indeksi = NA_integer_, tehniskie = NA_integer_,
                                viena_cikla = NA_integer_, lietoti = NA_integer_, nelietoti = NA_integer_, lva_derigi_vid_pct = NA_real_, lva_n = NA_integer_)])
setorder(KOPS, gads, fails)
fwrite(KOPS, "output/pisa2025/atlants_kopsavilkums.csv")
cat("\nKopsavilkums cikls × fails:\n"); print(KOPS, row.names = FALSE)

# 20 nelietoti kandidāti: lasīšanas prakses, skolas prakses un laikmeta signāli (kā ST154Q01HA),
# skolēnu vai IKT anketa, Latvijā derīgi ≥ 60 %, salīdzinājuma valstīs ≥ 40 %. Bloki, kuru vienumi
# veido jau lietotus indeksus (2018 metakognīcija ST164/ST165 → METASUM/UNDREM; 2009 ST41/ST42;
# lasītprieks ST160/ST24; lasāmviela ST167/ST25), netiek piedāvāti otrreiz.
INDEKSU_BLOKI <- c("ST164", "ST165", "ST160", "ST167", "ST24", "ST25", "ST41", "ST42", "ST26")
KAND <- A[saturisks & !lietots & !is.na(lva_derigi_pct) & lva_derigi_pct >= 60 & !is.na(sal_derigi_pct) & sal_derigi_pct >= 40 &
          jautajums & anketa %in% c("skolēnu", "IKT") & !paste(gads, bloks) %in% paste(c(2018, 2018, 2018, 2018, 2009, 2009, 2009, 2009, 2012), INDEKSU_BLOKI)]
KAND[, prioritate := fcase(
  tema == "lasīšana" & grepl("page|long|how (much|often|many|long)|hour|minute|time|librar|book|text|homework|lesson|class|assign|school", etikete, ignore.case = TRUE), 1L,
  tema == "lasīšana", 2L,
  tema %in% c("mācīšanās stratēģijas", "skolotāji", "skola/klimats") & grepl("read|text|homework|lesson|instruction|feedback|explain|write|essay|assign", etikete, ignore.case = TRUE), 2L,
  tema %in% c("IKT", "MI") & grepl("hour|minute|time|how often|use|read|write|school ?work|homework", etikete, ignore.case = TRUE), 3L,
  tema %in% c("labbūtība", "skola/klimats", "mācīšanās stratēģijas"), 4L,
  default = 5L)]
KAND <- KAND[prioritate <= 3]
KAND <- KAND[order(prioritate, -as.integer(gads), -ciklu_skaits, -lva_derigi_pct)]
KAND <- KAND[!duplicated(konstrukts)]
KAND[, bloka_kartas := seq_len(.N), by = .(gads, bloks)]
KAND <- KAND[bloka_kartas <= 2][order(prioritate, -(tema == "MI"), -as.integer(gads), -ciklu_skaits)]   # MI vienumi laikmeta signālos pirmie
# kvotas: 12 lasīšanas prakses, 4 skolas prakses (skolotāja darbs ar tekstu, mājasdarbi), 4 laikmeta signāli (IKT, MI)
KAND[, kategorija := fcase(prioritate <= 2 & tema == "lasīšana", "lasīšanas prakse", prioritate == 2, "skolas prakse", default = "laikmeta signāls")]
KAND[, kat_kartas := seq_len(.N), by = kategorija]
K20 <- KAND[(kategorija == "lasīšanas prakse" & kat_kartas <= 12) | (kategorija == "skolas prakse" & kat_kartas <= 4) | (kategorija == "laikmeta signāls" & kat_kartas <= 4)]
K20 <- K20[order(match(kategorija, c("lasīšanas prakse", "skolas prakse", "laikmeta signāls")), -as.integer(gads)),
           .(kategorija, gads, fails, anketa, mainigais, bloks, tema, etikete, lva_derigi_pct, sal_derigi_pct, ciklu_skaits, matrica, vertibu_etiketes = vertibu_etiketes_isi)]
fwrite(K20, "output/pisa2025/atlants_nelietotie_kandidati.csv")
cat("\n20 nelietoti kandidāti (lasīšana, skolas prakse, laikmeta signāli):\n")
print(K20[, .(kategorija, gads, mainigais, ciklu_skaits, lva_derigi_pct, etikete = substr(etikete, 1, 80))], row.names = FALSE)

# ----------------------------------------------------------------------------
# 7. Pārlūkojams HTML (viens fails, JSON iekšā, bez ārējām bibliotēkām)
# ----------------------------------------------------------------------------
dir.create("output/pisa2025/vizualizacijas", showWarnings = FALSE, recursive = TRUE)
J <- A[, .(g = gads, f = fails, an = anketa, v = mainigais, e = etikete, t = tips, b = bloks, tm = tema, ns = as.integer(valsts_specifisks),
           lv = lva_derigi_pct, sl = sal_derigi_pct, nv = lva_vertibu_skaits, ve = isi(vertibu_etiketes, 14, 600), k = konstrukts, ck = ciklu_skaits,
           m = matrica, h = harmonizacija, k64 = konstrukts_64d, u64h = as.integer(lietots_64h), u69a = as.integer(lietots_69a_f),
           u69c = as.integer(lietots_69c_75), u67 = as.integer(lietots_67d), u58 = as.integer(lietots_58), uc = as.integer(lietots_citur),
           apv = as.integer(apvienots_64e), u = as.integer(lietots), sk = lietots_skripti)]
KJ <- KON[, .(k = konstrukts, f = fails, e = etikete, ck = ciklu_skaits, m = matrica, h = harmonizacija, u = as.integer(lietots),
              v2000, v2003, v2006, v2009, v2012, v2015, v2018, v2022, v2025)]
KOPJ <- KOPS[, .(g = gads, f = fails, n = mainigo_skaits, q = anketas_jautajumi, i = indeksi, teh = tehniskie, vc = viena_cikla, l = lietoti, nl = nelietoti)]
json <- jsonlite::toJSON(list(mainigie = J, konstrukti = KJ, kopsavilkums = KOPJ, gadi = GADI,
                              meta = list(datums = format(Sys.Date()), n_main = nrow(J), n_kon = nrow(KJ), n_sat = sum(A$saturisks),
                                          n_nelietoti = sum(A$saturisks & !A$lietots), n_viena = sum(A$saturisks & A$ciklu_skaits == 1))),
                         dataframe = "rows", na = "null", auto_unbox = TRUE)
json <- gsub("</", "<\\/", json, fixed = TRUE)
veidne <- paste(readLines("R/pisa2025/82_V7_veidne.html", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
html <- sub("__DATI_JSON__", json, veidne, fixed = TRUE)
writeLines(html, "output/pisa2025/vizualizacijas/V7_mainigo_atlants.html", useBytes = TRUE)
cat("\nIzvade: output/pisa2025/vizualizacijas/V7_mainigo_atlants.html,", round(file.size("output/pisa2025/vizualizacijas/V7_mainigo_atlants.html") / 1e6, 1), "MB\n")

cat("\nPabeigts:", format(Sys.time()), "| ilgums", round(as.numeric(difftime(Sys.time(), t_sakums, units = "mins")), 1), "min\n")
while (sink.number() > 0) sink()
close(log_con)
