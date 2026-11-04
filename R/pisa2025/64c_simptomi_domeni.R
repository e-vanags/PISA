# ============================================================================
# 64c. Simptomu modelis, 1b. solis: kandidāti pa simptomu jomām (manuālai kartei)
# ============================================================================
#
# No inventāriem (64a, py/simptomi_veci_izvilkums.py) atlasa mainīgos pēc atslēgvārdiem
# etiķetēs un nosaukumos katrā jomā. Rezultāts ir pārskatīšanai paredzēts saraksts, no
# kura tiek veidota saskaņotā konstruktu karte (simptomi_karte.csv). Mainīgajam nav
# jābūt visos ciklos: longitudinālajā modelī tas piedalās tajos gados, kad ir izmērīts.
#
# Izvade: output/pisa2025/simptomi_kandidati_pa_domeniem.csv
# ============================================================================

I <- rbind(
  utils::read.csv("output/pisa2025/simptomi_inventars_veci.csv", stringsAsFactors = FALSE)[,
    c("gads", "fails", "mainigais", "etikete", "lva_derigi_pct", "sal_derigi_pct", "lva_vertibu_skaits")],
  utils::read.csv("output/pisa2025/simptomi_inventars_jaunie.csv", stringsAsFactors = FALSE)[,
    c("gads", "fails", "mainigais", "etikete", "lva_derigi_pct", "sal_derigi_pct", "lva_vertibu_skaits")])
I$mainigais <- toupper(I$mainigais)
I <- I[I$lva_derigi_pct >= 20 & I$lva_vertibu_skaits >= 2 & !grepl("^(W_|PV[0-9]|CNT|STIDSTD|SCHOOLID|STRATUM|BOOKID|SENWT|WVARSTR|UNIT$)", I$mainigais), ]

DOM <- list(
  lasisana = "read|book|librar|magazin|newspaper|comic|fiction|novel|e-book|ebook|poem|poetry|story|stories|summar|text",
  gimene = "escs|homepos|hisei|pared|isei|mother|father|parent|possession|langn|language .*home|immigr|born|country of birth|wealth|cultpos|hedres",
  skolas_gaita = "grade|repeat|isced 0|<isced 0>|pre-?school|kindergarten|skip|truan|late for|arrived late|absent|miss|homework|study|learning time|hours|tutor|lessons per|minutes|work for pay|paid work",
  klimats = "belong|lonely|outsider|awkward|bull|made fun|threat|rumou|hit or pushed|disciplin|noise|disorder|teacher support|teacher.*help|interest in every|safe|fair|relationship|teacher.*respect",
  labbutiba = "satisf|anxi|worr|stress|nervous|happy|sad|joyful|scared|miserable|proud|lively|well-?being|sleep|breakfast|exercise|physical",
  digitala_vide = "computer|internet|digital|ict|smartphone|cell|mobile|social network|video game|games|chat|e-?mail|screen|tablet|online|browsing|device",
  iesaiste = "effort|persever|motiv|interest|enjoy|waste of time|expect|goal|ambition|curio|self-?efficacy|confident|give up|hard work|test.*important",
  skola_resursi = "shortage|lack of|staff|teacher.*(qualif|certif|full|part)|class size|student-teacher|ratio|school size|enrol|resource|library|extracurricular|autonomy|accountab|assessment|absentee|disrupt|parent.*involv|computer.*ratio|instruct.*hinder|hinder")
I$domeni <- vapply(paste(I$mainigais, I$etikete), function(s)
  paste(names(DOM)[vapply(DOM, function(p) grepl(p, s, ignore.case = TRUE), TRUE)], collapse = ";"), "")
K <- I[I$domeni != "", ]
K$etikete <- gsub("[[:space:]]+", " ", K$etikete)
K <- K[order(K$domeni, K$fails, K$gads, K$mainigais), ]
utils::write.csv(K, "output/pisa2025/simptomi_kandidati_pa_domeniem.csv", row.names = FALSE)
cat("Kandidāti:", nrow(K), "no", nrow(I), "\n")
tb <- do.call(rbind, lapply(names(DOM), function(d) { x <- K[grepl(d, K$domeni), ]
  as.data.frame.matrix(t(table(factor(x$gads, levels = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025))))) |>
  (\(m) { rownames(m) <- d; m })() }))
print(tb)
