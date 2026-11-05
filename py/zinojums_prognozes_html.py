#!/usr/bin/env python3
"""Saliek ziņojuma HTML lapu "Ieskats aktuālajos OECD, IEA, SPKC u. c. datos: ko varam prognozēt?"

Veidne: R/pisa2025/86_zinojums_prognozes_veidne.html
  - {{DATA}}          -> interaktīvo grafiku dati (JSON), sagatavoti šeit no output/pisa2025/*.csv
  - {{JS_DZINEJS}}    -> R/pisa2025/86_zinojums_grafiki_dzinejs.js (SVG grafiku dzinējs)
  - {{JS_GRAFIKI}}    -> R/pisa2025/86_zinojums_grafiki_lapa.js (katra grafika definīcija)
  - {{IMG:fails.png}} -> attēls base64 (ja veidnē vēl ir statiski attēli)
Izvade: output/pisa2025/vizualizacijas/V9_zinojums_prognozes_2026-11-05.html (viens fails)

Lietošana: python3 py/zinojums_prognozes_html.py
"""
import base64
import csv
import json
import math
import re
import struct
import sys
from collections import defaultdict
from pathlib import Path
from statistics import mean, pstdev

SAKNE = Path(__file__).resolve().parent.parent
VEIDNE = SAKNE / "R" / "pisa2025" / "86_zinojums_prognozes_veidne.html"
IZVADE = SAKNE / "output" / "pisa2025" / "vizualizacijas" / "V9_zinojums_prognozes_2026-11-05.html"
OUT = SAKNE / "output" / "pisa2025"
MAPES = [OUT / "plots", OUT / "vizualizacijas"]

VALSTIS7 = ["EST", "LTU", "POL", "FIN", "CZE", "HUN", "DEU"]
NOS = {"LVA": "Latvija", "EST": "Igaunija", "LTU": "Lietuva", "POL": "Polija", "FIN": "Somija",
       "CZE": "Čehija", "HUN": "Ungārija", "DEU": "Vācija"}


def lasit(cels):
    with open(cels, encoding="utf-8-sig") as f:
        return list(csv.DictReader(f))


def num(x):
    try:
        v = float(x)
        return None if math.isnan(v) else v
    except (TypeError, ValueError):
        return None


def r(x, d=2):
    return None if x is None else round(x, d)


def h_fun(p1, p2):
    return 2 * math.asin(math.sqrt(p2)) - 2 * math.asin(math.sqrt(p1))


def h_se(p1, se1, p2, se2):
    return math.sqrt(se2 ** 2 / (p2 * (1 - p2)) + se1 ** 2 / (p1 * (1 - p1)))


# ---------------------------------------------------------------------------
# 1. Koriģētā rinda (72) un 7 valstis
# ---------------------------------------------------------------------------
def d_korigeta():
    K = [x for x in lasit(OUT / "korigetie_raditaji.csv") if x["raditajs"] == "vid"]
    V = lasit(OUT / "pisa_valstu_videjie.csv")
    S = lasit(OUT / "salidzinajuma_kopa_valodas_plusmas.csv")
    gadi = sorted({int(x["gads"]) for x in K})
    rinda = []
    for g in gadi:
        k = next(x for x in K if int(x["gads"]) == g)
        v7 = [num(x["lasisana"]) for x in V if int(x["gads"]) == g and x["cnt"] in VALSTIS7]
        s7 = [num(x["vid"]) for x in S if int(x["gads"]) == g and x["kopa"] == "galvenā valoda" and x["cnt"] in VALSTIS7]
        rinda.append({"gads": g,
                      "neapstr": r(num(k["neapstr"]), 1), "neapstr_lo": r(num(k["neapstr_ti_zem"]), 1), "neapstr_hi": r(num(k["neapstr_ti_augs"]), 1),
                      "korig": r(num(k["korigets"]), 1), "korig_lo": r(num(k["korigets_ti_zem"]), 1), "korig_hi": r(num(k["korigets_ti_augs"]), 1),
                      "v7_visi": r(mean(v7), 1) if v7 else None, "v7_n": len(v7),
                      "v7_valoda": r(mean(s7), 1) if s7 else None, "v7v_n": len(s7)})
    return rinda


# ---------------------------------------------------------------------------
# 2. Paaudžu (Lexis) diagramma (V1)
# ---------------------------------------------------------------------------
ZIN = [
    {"datums": 2007.9, "tips": "brīdina", "teksts": "2007-11: PIRLS 2006 nacionālā monogrāfija: \"nopietns brīdinājums\""},
    {"datums": 2017.9, "tips": "nomierina", "teksts": "2017-11: PIRLS 2016 ziņojums: \"ievērojami uzlabojušies\""},
    {"datums": 2019.95, "tips": "nomierina", "teksts": "2019-12: PISA 2018 pirmie rezultāti: \"kopumā labi, stabils\""},
    {"datums": 2023.35, "tips": "brīdina", "teksts": "2023-05: PIRLS 2021 ziņojums: \"drauds visas izglītības sistēmas sniegumam\""},
    {"datums": 2023.92, "tips": "nomierina", "teksts": "2023-12: PISA 2022 pirmie rezultāti: \"labu un stabilu\""},
    {"datums": 2025.3, "tips": "mērījums", "teksts": "2025-03: PISA 2025 tests"},
]


def d_lexis():
    R = lasit(OUT / "vizualizacijas" / "V1_relativa_vieta.csv")
    punkti = []
    for x in R:
        punkti.append({"petijums": x["petijums"], "gads": int(x["gads"]), "vecums": num(x["vecums"]),
                       "dz": r(num(x["dz_gads"]), 1), "rel": r(num(x["rel"]), 1), "se": r(num(x["se"]), 1),
                       "z": r(num(x["rel_sd"]), 3), "valstis": x["valstis"]})
    PA = [x for x in lasit(OUT / "piaac_paaudzes_pisa.csv") if x["joma"] == "LIT" and x["kopa"] == "7 valstis" and x["lv_grupa"] == "A"]
    VID = {"16-19": 17.5, "20-24": 22, "25-29": 27, "30-34": 32, "35-39": 37}
    for x in PA:
        v = VID[x["grupa"]]
        punkti.append({"petijums": "PIAAC lasītprasme", "gads": 2023, "vecums": v, "dz": r(2023 - v, 1), "rel": r(num(x["piaac_starp"]), 1),
                       "se": r(num(x["piaac_se"]), 1), "z": r(num(x["piaac_sd"]), 3), "valstis": "7"})
    return {"punkti": punkti, "zinojumi": ZIN}


# ---------------------------------------------------------------------------
# 3. Brīdinājuma panelis (V3), atkārto R/pisa2025/70_vizualizacijas.R V3 daļu
# ---------------------------------------------------------------------------
def d_panelis():
    L = [x for x in lasit(OUT / "agrie_signali_limeni.csv") if x["grupa"] == "A visi"]
    PR = lasit(OUT / "paradumi.csv")
    PI = lasit(OUT / "latviesu_plusma_pirls.csv")
    PA = [x for x in lasit(OUT / "piaac_paaudzes_pisa.csv") if x["joma"] == "LIT" and x["kopa"] == "7 valstis" and x["lv_grupa"] == "A"]
    serijas = []

    def sig(k, nos, veids, zime=1):
        xs = sorted([x for x in L if x["radītājs"] == k], key=lambda x: int(x["gads"]))
        b = xs[0]
        pts = []
        for x in xs:
            g, v, se = int(x["gads"]), num(x["vertiba"]), num(x["se"])
            b_v, b_se = num(b["vertiba"]), num(b["se"])
            if veids == "d":
                es = (v - b_v) / 88
                s = math.sqrt(se ** 2 + b_se ** 2 + (25 if g != int(b["gads"]) else 0)) / 88
            else:
                es = h_fun(b_v / 100, v / 100)
                s = h_se(b_v / 100, b_se / 100, v / 100, se / 100)
            pts.append({"x": g, "y": r(zime * es, 3), "se": r(s, 3), "vert": r(v, 1)})
        serijas.append({"id": k, "nos": nos, "pts": pts})

    sig("vid", "PISA vidējais rezultāts: pārmaiņa pret 2003. gadu", "d")
    sig("P95", "PISA augšgals (95. procentile): pārmaiņa pret 2003. gadu", "d")
    sig("L5_un_virs", "PISA 5. un 6. līmeņa daļa: pārmaiņa pret 2003. gadu", "h")
    sig("zem_L2", "PISA zem 2. līmeņa daļa: pārmaiņa pret 2003. gadu (zīme apgriezta)", "h", zime=-1)
    gr = sorted([x for x in PR if x["radītājs"] == "grāmatas: vairāk nekā 200 (2000: >250)" and x["grupa"] == "A"], key=lambda x: int(x["gads"]))
    b = gr[0]
    serijas.append({"id": "gramatas", "nos": "Vairāk par 200 grāmatām mājās: pārmaiņa pret 2003. gadu",
                    "pts": [{"x": int(x["gads"]), "y": r(h_fun(num(b["pct"]) / 100, num(x["pct"]) / 100), 3),
                             "se": r(h_se(num(b["pct"]) / 100, num(b["se"]) / 100, num(x["pct"]) / 100, num(x["se"]) / 100), 3),
                             "vert": r(num(x["pct"]), 1)} for x in gr]})
    nl = sorted([x for x in PR if x["radītājs"] == "nelasa priekam" and x["grupa"] == "Latvija"], key=lambda x: int(x["gads"]))
    b2 = nl[0]
    serijas.append({"id": "nelasa", "nos": "Nelasa priekam, visa Latvija: pret 2000. gadu (zīme apgriezta)",
                    "pts": [{"x": int(x["gads"]), "y": r(-h_fun(num(b2["pct"]) / 100, num(x["pct"]) / 100), 3),
                             "se": r(h_se(num(b2["pct"]) / 100, num(b2["se"]) / 100, num(x["pct"]) / 100, num(x["se"]) / 100), 3),
                             "vert": r(num(x["pct"]), 1)} for x in nl]})
    b3 = PI[0]
    serijas.append({"id": "pirls", "nos": "PIRLS 4. klases vidējais rezultāts: pārmaiņa pret 2001. gadu",
                    "pts": [{"x": int(x["gads"]), "y": r((num(x["pamat"]) - num(b3["pamat"])) / 68, 3),
                             "se": r(math.sqrt(num(x["pamat_se"]) ** 2 + num(b3["pamat_se"]) ** 2) / 68, 3),
                             "vert": r(num(x["pamat"]), 1)} for x in PI]})
    serijas.append({"id": "piaac", "nos": "PIAAC 2023 pieaugušie: katra paaudze pret 7 valstīm",
                    "pts": sorted([{"x": r(num(x["pisa_gads"]), 1), "y": r(num(x["piaac_sd"]), 3),
                                    "se": r(num(x["piaac_se"]) * abs(num(x["piaac_sd"]) / num(x["piaac_starp"])), 3),
                                    "vert": r(num(x["piaac_starp"]), 1), "grupa": x["grupa"]} for x in PA], key=lambda p: p["x"])})
    serijas.append({"id": "lpp100", "nos": "Skolā lasa tekstus virs 100 lappusēm, pret 7 valstīm (tikai 2018)",
                    "pts": [{"x": 2018, "y": r(2 * math.asin(math.sqrt(0.215)) - 2 * math.asin(math.sqrt(0.474)), 3),
                             "se": r(math.sqrt(3 / 3304 + 3 / 39461), 3), "vert": 21.5}]})
    return {"serijas": serijas, "zinojumi": ZIN}


# ---------------------------------------------------------------------------
# 4. Efekta lielumi (V2)
# ---------------------------------------------------------------------------
def d_efekti():
    E = lasit(OUT / "efekta_lielumi.csv")
    PAZ = {"vid": "Vidējais", "P95": "Augšgals (P95)", "P10": "Apakšgals (P10)", "L5_un_virs": "5. un 6. līmeņa daļa", "zem_L2": "Zem 2. līmeņa daļa"}
    rindas = []
    for x in E:
        b, rad = x["bloks"], x["raditajs"]
        logs, paz = None, None
        if b == "A. PISA A plūsma" and rad in PAZ:
            logs, paz = f"{x['no']} → {x['lidz']}", PAZ[rad]
        elif b == "B. Grāmatas" and "200" in rad:
            logs, paz = f"{x['no']} → {x['lidz']}", "Vairāk par 200 grāmatām mājās"
        elif b == "B. Paradumi" and "nelasa priekam [Latvija]" in rad:
            logs, paz = f"{x['no']} → {x['lidz']}", "Nelasa priekam, visa Latvija"
        elif b == "C. PIRLS A plūsma" and rad in ("vid (4. kl.)", "P10 (4. kl.)"):
            logs, paz = "PIRLS 2016 → 2021", "PIRLS 4. klase, vidējais" if rad.startswith("vid") else "PIRLS 4. klase, apakšgals (P10)"
        elif b == "D. PIAAC A plūsma pret 7 valstīm" and ("16-19" in rad or "25-29" in rad):
            logs, paz = "PIAAC 2023 līmenis", "PIAAC, vecums " + rad.split("vecums ")[-1].replace("-", " līdz ")
        if logs is None:
            continue
        es, lo, hi = num(x["es"]), num(x["es_ti_zem"]), num(x["es_ti_augs"])
        if paz in ("Zem 2. līmeņa daļa", "Nelasa priekam, visa Latvija"):
            es, lo, hi = -es, -hi, -lo
        rindas.append({"logs": logs, "paz": paz, "es": r(es, 3), "lo": r(lo, 3), "hi": r(hi, 3), "veids": x["veids"],
                       "no": r(num(x["vertiba_no"]), 1), "lidz": r(num(x["vertiba_lidz"]), 1), "klase": x["klase"]})
    LOGI = ["2003 → 2015", "2003 → 2018", "2003 → 2025", "2018 → 2022", "2022 → 2025", "2000 → 2018", "PIRLS 2016 → 2021", "PIAAC 2023 līmenis"]
    rindas = [x for x in rindas if x["logs"] in LOGI]
    PAZ_ORDER = ["Vidējais", "Augšgals (P95)", "Apakšgals (P10)", "5. un 6. līmeņa daļa", "Zem 2. līmeņa daļa", "Vairāk par 200 grāmatām mājās",
                 "Nelasa priekam, visa Latvija", "PIRLS 4. klase, vidējais", "PIRLS 4. klase, apakšgals (P10)", "PIAAC, vecums 16 līdz 19", "PIAAC, vecums 25 līdz 29"]
    return {"logi": LOGI, "pazimes": PAZ_ORDER, "rindas": rindas}


# ---------------------------------------------------------------------------
# 5. DFM ex ante (67d)
# ---------------------------------------------------------------------------
def d_dfm():
    D = [x for x in lasit(OUT / "dfm_ex_ante.csv") if x["grupa"] == "LVA A"]
    modeli = []
    for m in ["M0 tikai prasme (67b)", "M1 DFM diagonāla Q", "M2 DFM brīva Q", "M3 DFM vadība k=1"]:
        pts = sorted([{"x": int(x["merkis"]), "prog": r(num(x["prog_rel"]), 2), "fakt": r(num(x["fakt"]), 2)} for x in D if x["modelis"] == m], key=lambda p: p["x"])
        modeli.append({"id": m, "pts": pts})
    return {"modeli": modeli}


# ---------------------------------------------------------------------------
# 6. CE un DD paaudzes (81, 79)
# ---------------------------------------------------------------------------
def d_ce():
    C = lasit(OUT / "ce_9_12_paaudzes.csv")
    SER = {
        "9. kl. latviešu valoda A: lasīšana (+ valodas sistēmas izpratne no 2011./12.); 2022./23.+ CE 1. daļa": "9. kl. latviešu valoda: lasīšana (1. daļa)",
        "9. kl. latviešu valoda A: kopā; 2022./23.+ CE visiem": "9. kl. latviešu valoda: kopā",
        "9. kl. matemātika: kopā; 2022./23.+ CE visiem": "9. kl. matemātika: kopā",
        "12. kl. latviešu valoda CE: kopā, A (latviešu programmas skolas)": "12. kl. latviešu valoda CE: kopā, latviešu programmas",
        "12. kl. matemātika CE: kopā, visi": "12. kl. matemātika CE: kopā",
    }
    serijas = []
    for k, nos in SER.items():
        pts = sorted([{"x": int(x["dz_gads"]), "y": r(num(x["z"]), 2), "gads": x["gads"], "vert": r(num(x["videja"]), 1),
                       "rangs": x["rangs"], "n_gadi": x["gadu_sk"], "klase": int(x["klase"])} for x in C if x["serija"] == k], key=lambda p: p["x"])
        serijas.append({"id": k, "nos": nos, "klase": pts[0]["klase"] if pts else None, "pts": pts})
    pisa = {}
    for x in C:
        if x["pisa_A_z"] and x["pisa_paaudze"]:
            pisa[int(x["dz_gads"]) if int(x["klase"]) == 9 else int(x["dz_gads"])] = {"x": int(x["pisa_gads"]) - 16, "y": r(num(x["pisa_A_z"]), 2), "pisa": x["pisa_paaudze"], "vert": r(num(x["pisa_A_vid"]), 1)}
    pisa_pts = sorted({(v["x"], v["y"], v["pisa"], v["vert"]) for v in pisa.values()})
    return {"serijas": serijas, "pisa": [{"x": a, "y": b, "pisa": c, "vert": d} for a, b, c, d in pisa_pts]}


def d_dd():
    D = lasit(OUT / "dd_paaudzu_rinda.csv")
    P = [x for x in lasit(SAKNE / "data" / "pisa2025" / "processed" / "paaudzu_panelis.csv") if x["grupa"] == "LVA A" and x["radītājs"] == "vid"]
    serijas = []
    for pet, grupa, nos in [("DD3 lasīšana", "LVA", "3. klase, lasīšana (kopā; līdz 2015./16. rekonstruēta)"), ("DD6 lasīšana", "LVA A", "6. klase, lasīšana (latviešu valodas plūsma)")]:
        pts = sorted([{"x": r(num(x["dz_gads"]), 1), "y": r(num(x["est_z"]), 2), "vert": r(num(x["est"]), 1), "gads": x["gads"]} for x in D if x["petijums"] == pet and x["grupa"] == grupa], key=lambda p: p["x"])
        serijas.append({"id": pet, "nos": nos, "pts": pts})
    for pet, nos in [("PISA lasīšana", "PISA lasīšana, 15 gadi"), ("PIRLS lasīšana", "PIRLS lasīšana, 4. klase"), ("TIMSS4 matemātika", "TIMSS matemātika, 4. klase")]:
        xs = [x for x in P if x["petijums"] == pet]
        vals = [num(x["est"]) for x in xs]
        m, sd = mean(vals), pstdev(vals)
        pts = sorted([{"x": r(num(x["dz_gads"]), 1), "y": r((num(x["est"]) - m) / sd, 2), "vert": r(num(x["est"]), 1), "gads": x["gads"]} for x in xs], key=lambda p: p["x"])
        serijas.append({"id": pet, "nos": nos, "pts": pts})
    return {"serijas": serijas}


# ---------------------------------------------------------------------------
# 7. TALIS 2024 jaunie vienumi (77b)
# ---------------------------------------------------------------------------
def d_talis():
    KODI = ["dir_aizgajuso_dala", "dir_jauno_dala", "maina_a", "maina_b", "maina_c", "maina_d", "maina_e", "maina_f",
            "stress_labklajiba", "stress_pl", "stress_progr_mainas", "stress_traucejumi"]
    ISS = {"dir_aizgajuso_dala": "Aizgājušo skolotāju attiecība pret skolotāju skaitu skolā", "dir_jauno_dala": "Jauno skolotāju attiecība pret skolotāju skaitu skolā",
           "maina_a": "Skolā ievieš pārāk daudz pārmaiņu iniciatīvu, % piekrīt", "maina_b": "Esmu noguris no visām pārmaiņām skolā, % piekrīt",
           "maina_c": "Mums prasa mainīt pārāk daudz, % piekrīt", "maina_d": "Šķiet, ka vienmēr prasa kaut ko mainīt, % piekrīt",
           "maina_e": "Gribētu stabilitātes periodu pirms nākamajām pārmaiņām, % piekrīt", "maina_f": "Prasa ieviest pārmaiņas bez vajadzīgajiem resursiem, % piekrīt",
           "stress_labklajiba": "Stress: atbildība par skolēnu labklājību (%)", "stress_pl": "Stress: sekot līdzi profesionālajai mācīšanās (%)",
           "stress_progr_mainas": "Stress: sekot mācību programmas pārmaiņām skolā (%)", "stress_traucejumi": "Stress: pielāgot darbu negaidītu traucējumu dēļ (%)"}
    T = [x for x in lasit(OUT / "talis_rinda_raditaji.csv") if x["cikls"] == "2024" and x["kods"] in KODI]
    vienumi = []
    for k in KODI:
        rows = [x for x in T if x["kods"] == k]
        if not rows:
            continue
        dala = rows[0]["veids"] == "daļa"
        vienumi.append({"id": k, "nos": ISS[k], "joma": rows[0]["joma"], "dala": dala,
                        "valstis": [{"cnt": x["cnt"], "nos": x["valsts"], "est": r(num(x["est"]) * (100 if dala else 1), 1 if dala else 3),
                                     "se": r(num(x["se"]) * (100 if dala else 1), 2 if dala else 4)} for x in rows]})
    return {"vienumi": vienumi}


# ---------------------------------------------------------------------------
# 8. Izdevumi (71b, 71d)
# ---------------------------------------------------------------------------
def d_izdevumi():
    R = [x for x in lasit(OUT / "izdevumi_rinda_realie.csv") if x["cnt"] in NOS]
    agg = defaultdict(list)
    for x in R:
        agg[(x["cnt"], int(x["gads"]))].append(num(x["vertiba"]))
    rinda = defaultdict(list)
    for (c, g), v in sorted(agg.items()):
        if len(v) == 2:
            rinda[c].append({"x": g, "y": r(mean(v), 0)})
    A = [x for x in lasit(OUT / "izdevumi_atlikumi_visas_valstis.csv") if x["mers"] == "vienlaicīgs" and x["cnt"] in NOS]
    atl = defaultdict(list)
    for x in sorted(A, key=lambda x: int(x["cikls"])):
        atl[x["cnt"]].append({"x": int(x["cikls"]), "y": r(num(x["atlikums"]), 1), "izd": r(num(x["kumul"]), 0), "las": r(num(x["lasisana"]), 1)})
    P = [x for x in lasit(OUT / "izdevumi_atlikumi_pa_cikliem.csv") if x["mers"].startswith("vienlaicīgs")]
    a_pl = [{"x": int(x["cikls"]), "y": r(num(x["a_atlikums"]), 1), "se": r(num(x["a_se"]), 1), "lva_se": r(num(x["lva_se"]), 1)} for x in sorted(P, key=lambda x: int(x["cikls"]))]
    return {"nos": NOS, "rinda": rinda, "atlikumi": atl, "a_plusma": a_pl}


# ---------------------------------------------------------------------------
# 9. HBSC (65c): ex ante un Latvijas paaudžu rinda, tikai skolas un labbūtības rādītāji
# ---------------------------------------------------------------------------
HBSC_NOS = {"sch_satis": "Patīk skola ļoti", "sch_pressure": "Skolas spiediens (apgriezts)", "lifesat_mean": "Dzīves apmierinātība",
            "sr_health": "Pašvērtētā veselība teicama", "multihealth": "Psihosomatiskas sūdzības (apgriezts)", "d_sleep": "Miega grūtības (apgriezts)",
            "d_feellow": "Nomākts (apgriezts)", "d_nervous": "Nervozs (apgriezts)", "beenbullied_2_3": "Ņirgāšanās upuris (apgriezts)",
            "studsup_cutoff": "Klasesbiedru atbalsts", "teachersup_cutoff": "Skolotāju atbalsts", "famsup_hi": "Ģimenes atbalsts",
            "peersup_hi": "Vienaudžu atbalsts", "fas2_mean": "Ģimenes labklājība", "acachieve_good": "Pašvērtētās sekmes labas", "saliktais": "Saliktais (12 rādītāji)"}
KEY = ["sch_pressure", "sch_satis", "teachersup_cutoff", "studsup_cutoff", "d_sleep", "lifesat_mean", "beenbullied_2_3", "peersup_hi", "famsup_hi", "multihealth", "d_feellow", "d_nervous"]


def d_hbsc():
    X = lasit(OUT / "hbsc_mikro_pisa_paaudzes.csv")
    exante = [{"pisa": int(x["pisa"]), "id": x["HBSC_id"], "nos": HBSC_NOS.get(x["HBSC_id"], x["HBSC_id"]), "vecums": int(x["vecums"]),
               "gads": int(x["hbsc_gads"]), "z": r(num(x["z7"]), 2), "se": r(num(x["se_z"]), 2), "avots": x["avots"], "pisa_rel": r(num(x["pisa_rel"]), 1)}
              for x in X if x["HBSC_id"] in HBSC_NOS]
    M = [x for x in lasit(OUT / "hbsc_mikro_paaudzes.csv") if x["grupa"] == "LVA" and x["HBSC_id"] in HBSC_NOS and x["z7"]]
    rinda = defaultdict(list)
    for x in sorted(M, key=lambda x: (int(x["vecums"]), int(x["gads"]))):
        rinda[x["HBSC_id"]].append({"vecums": int(x["vecums"]), "gads": int(x["gads"]), "b": int(x["b"]), "z": r(num(x["z7"]), 2), "se": r(num(x["se_z"]), 2), "vert": r(num(x["est"]), 1)})
    # saliktais pa vecumiem un gadiem no KEY rādītājiem
    grp = defaultdict(list)
    for x in M:
        if x["HBSC_id"] in KEY:
            grp[(int(x["vecums"]), int(x["gads"]), int(x["b"]))].append(num(x["z7"]))
    rinda["saliktais"] = [{"vecums": v, "gads": g, "b": b, "z": r(mean(z), 2), "se": None, "vert": None} for (v, g, b), z in sorted(grp.items())]
    return {"nos": HBSC_NOS, "exante": exante, "rinda": rinda}


# ---------------------------------------------------------------------------
# 10. Robežjosla (83), atgriešanās horizonts
# ---------------------------------------------------------------------------
def d_aploksne():
    P = [x for x in lasit(OUT / "aploksne_pari.csv") if x["logs"] == "1"]
    punkti = [{"joma": x["joma"], "valsts": x["valsts"], "no": int(x["no"]), "uz": int(x["uz"]), "l": r(num(x["l_no"]), 1),
               "g": r(num(x["gada"]), 2), "m": x["markets"] == "TRUE"} for x in P]
    Q = [x for x in lasit(OUT / "aploksne_kvantiles_rq.csv") if x["logs"] == "1" and x["kopa"] == "visi" and x["attirits"] == "nē"]
    kvant = defaultdict(list)
    for x in sorted(Q, key=lambda x: (x["joma"], num(x["limenis"]))):
        kvant[x["joma"]].append({"l": num(x["limenis"]), "P5": r(num(x["P5"]), 2), "P25": r(num(x["P25"]), 2), "P50": r(num(x["P50"]), 2), "P75": r(num(x["P75"]), 2), "P95": r(num(x["P95"]), 2)})
    LV = [x for x in lasit(OUT / "aploksne_latvija.csv") if x["logs"] == "1" and x["kopa"] == "visi" and x["attirits"] == "nē"]
    latv = [{"joma": x["joma"], "valsts": x["valsts"], "no": int(x["no"]), "uz": int(x["uz"]), "l": r(num(x["l_no"]), 1), "g": r(num(x["gada"]), 2),
             "proc": r(num(x["procentile"]), 0)} for x in LV]
    valstis = sorted({x["valsts"] for x in punkti}); vi = {v: i for i, v in enumerate(valstis)}
    kompakts = [[x["joma"], vi[x["valsts"]], x["no"], x["uz"], x["l"], x["g"], 1 if x["m"] else 0] for x in punkti]
    return {"punkti_k": ["joma", "valsts", "no", "uz", "l", "g", "m"], "punkti_valstis": valstis, "punkti": kompakts, "kvantiles": kvant, "latvija": latv}


def d_atgriesanas():
    K = [x for x in lasit(OUT / "korigetie_raditaji.csv") if x["raditajs"] == "vid"]
    rinda = [{"x": int(x["gads"]), "y": r(num(x["korigets"]), 1), "lo": r(num(x["korigets_ti_zem"]), 1), "hi": r(num(x["korigets_ti_augs"]), 1)} for x in sorted(K, key=lambda x: int(x["gads"]))]
    A = [x for x in lasit(OUT / "aploksne_atgriesanas.csv") if x["kopa"] == "visi" and x["attirits"] == "nē"]
    l1 = next(x for x in A if x["logs"] == "1")
    l2 = next(x for x in A if x["logs"] == "2")
    return {"rinda": rinda, "limenis2018": r(num(next(x["korigets"] for x in K if x["gads"] == "2018")), 1), "vajadzigs": r(num(l2["vajadzigs"]), 1),
            "atrumi": [{"id": "P50", "nos": "P50 kāpums, 6 līdz 7 gadu logi", "gada": r(num(l2["P50"]), 2)},
                       {"id": "P95_2", "nos": "P95 kāpums, 6 līdz 7 gadu logi", "gada": r(num(l2["P95"]), 2)},
                       {"id": "P95_1", "nos": "P95 kāpums, 3 līdz 4 gadu logi", "gada": r(num(l1["P95"]), 2)},
                       {"id": "max", "nos": "vēsturiskais maksimums, 6 līdz 7 gadu logi", "gada": r(num(l2["max"]), 2)}]}


# ---------------------------------------------------------------------------
# 11. Scenāriji (84, 84b)
# ---------------------------------------------------------------------------
def d_scenariji():
    V = lasit(OUT / "merku_scenariji_videjie.csv")
    SC = ["Ļoti slikts", "Slikts gadījums", "Tipiska valsts", "Labākā ceturtdaļa", "Vēsturiskais maksimums"]
    vid = defaultdict(dict)
    for x in V:
        key = f"{x['joma']}|{x['skala']}|{x['kopa']}"
        vid[key].setdefault(int(x["gads"]), {})[x["scenarijs"]] = r(num(x["vertiba"]), 1)
    AB = lasit(OUT / "ab_rinda_no_2003.csv")
    K = [x for x in lasit(OUT / "korigetie_raditaji.csv") if x["raditajs"] == "vid"]
    vest = {"oficiāli": [{"x": int(x["gads"]), "y": r(num(x["vid"]), 1)} for x in AB if x["grupa"] == "A"],
            "koriģēti": [{"x": int(x["gads"]), "y": r(num(x["korigets"]), 1)} for x in sorted(K, key=lambda x: int(x["gads"]))]}
    piem = [{"valsts": x["valsts"], "no": int(x["no"]), "uz": int(x["uz"]), "parmaina": r(num(x["parmaina"]), 1), "gadi": int(x["gadi"])} for x in lasit(OUT / "merku_piemeru_valstis.csv")]
    I = lasit(OUT / "merku_scenariji_izm.csv")
    dalas = defaultdict(dict)
    for x in I:
        key = f"{x['joma_nos']}|{x['skala']}|{x['mala_nos']}"
        dalas[key][int(x["gads"])] = {s: r(num(x[s]), 1) for s in SC}
        dalas[key]["2025"] = r(num(x["dala_2025"]), 1)
    L = [x for x in lasit(OUT / "merku_scenariji_a_limeni.csv") if x["grupa"] == "A visi" and x["raditajs"] in ("zem_L2", "L5")]
    JN = {"READ": "lasīšana", "MATH": "matemātika", "SCIE": "dabaszinātnes"}
    vest_d = defaultdict(list)
    for x in sorted(L, key=lambda x: int(x["gads"])):
        mala = "zem 2. līmeņa, %" if x["raditajs"] == "zem_L2" else "5. un 6. līmenis, %"
        vest_d[f"{JN[x['joma']]}|{x['skala']}|{mala}"].append({"x": int(x["gads"]), "y": r(num(x["vertiba"]), 1)})
    return {"scenariji": SC, "videjie": vid, "vesture": vest, "piemeri": piem, "dalas": dalas, "dalu_vesture": vest_d}


# ---------------------------------------------------------------------------
# 12. Garu tekstu lasīšana skolā, PISA 2018 ST154 (86b)
# ---------------------------------------------------------------------------
def d_escs():
    """ESCS ceturtdaļu lasīšanas vidējie pa cikliem (R/pisa2025/87, escs_gradienti.csv)."""
    out = {}
    for r in lasit(OUT / "escs_gradienti.csv"):
        if r["grupa"] not in ("LVA A", "7 valstis", "LVA") or not r["raditajs"].endswith("_vid") or not r["raditajs"].startswith("Q"):
            continue
        q = r["raditajs"][:2]
        d = out.setdefault(r["grupa"], {}).setdefault(int(r["gads"]), {"gads": int(r["gads"])})
        d[q] = num(r["est"]); d[q + "_se"] = num(r["se"])
    return {g: [v[k] for k in sorted(v)] for g, v in out.items()}

def d_skolas():
    """Lasīšana pēc skolas apdzīvotās vietas, latviešu valodas plūsma (R/pisa2025/89, skolu_pisa_urban.csv)."""
    out = {}
    for r in lasit(OUT / "skolu_pisa_urban.csv"):
        if r["grupa"] not in ("LVA A", "LVA") or not (r["raditajs"].startswith("urb") and r["raditajs"].endswith("_vid")):
            continue
        k = "u" + r["raditajs"][3]
        d = out.setdefault(r["grupa"], {}).setdefault(int(r["gads"]), {"gads": int(r["gads"])})
        d[k] = num(r["est"]); d[k + "_se"] = num(r["se"])
    return {g: [v[k] for k in sorted(v)] for g, v in out.items()}


def d_skolu_ce():
    """9. klases CE skolu vidējie 2022./23. līdz 2024./25. (R/pisa2025/89, skolu_ce.csv), skolas ar vismaz 10 kārtotājiem."""
    out = []
    for r in lasit(OUT / "skolu_ce.csv"):
        riga = r["urban_nos"] == "Rīga"
        out.append([r["eks"][:3], r["mg"][:4], r["skola"].strip('"') if riga else "", {"Rīga": 1, "valstspilsētas": 2, "pilsētas": 3, "lauki": 4}.get(r["urban_nos"], 0),
                    {"valsts ģimnāzija": 1, "vidusskola": 2, "pamatskola": 3}.get(r["tips_nos"], 0), int(r["n"]), round(num(r["vid"]), 1)])
    return {"k": ["e", "m", "s", "u", "t", "n", "v"], "r": out}


def d_garie():
    V = lasit(OUT / "st154_virs100.csv")
    K = lasit(OUT / "st154_garie_teksti.csv")
    return {"virs100": [{"grupa": x["grupa"], "nos": x["nosaukums"], "dala": num(x["dala"]), "se": num(x["se"]), "n": int(x["n"])} for x in V],
            "kategorijas": [{"grupa": x["grupa"], "nos": x["nosaukums"], "kat": int(x["kategorija"]), "kat_nos": x["nos"], "dala": num(x["dala"]), "se": num(x["se"]), "lasisana": num(x["lasisana"])} for x in K]}


# ---------------------------------------------------------------------------
# 13. Politikas lēmumi un lasīšanas rindas: Latvija, Igaunija, Somija
# ---------------------------------------------------------------------------
def d_politika():
    V = lasit(OUT / "pisa_valstu_videjie.csv")
    AB = lasit(OUT / "ab_rinda_no_2003.csv")
    rindas = {c: [{"x": int(x["gads"]), "y": r(num(x["lasisana"]), 1)} for x in sorted(V, key=lambda x: int(x["gads"])) if x["cnt"] == c] for c in ("LVA", "EST", "FIN")}
    rindas["LVA A"] = [{"x": int(x["gads"]), "y": r(num(x["vid"]), 1)} for x in AB if x["grupa"] == "A"]
    LV = [{"valsts": "LVA", "gads": num(x["gads"]), "kategorija": x["kategorija"], "nos": x["nosaukums"], "isais": x["isais"], "apraksts": x["apraksts"], "avots": x["avots"], "ticamiba": x["ticamiba"]} for x in lasit(SAKNE / "data" / "politika" / "notikumi_laika_josla.csv")]
    EF = [{"valsts": x["valsts"], "gads": num(x["gads"]), "kategorija": x["kategorija"], "nos": x["nosaukums"], "isais": x["isais"], "apraksts": x["apraksts"], "avots": x["avots"], "ticamiba": x["ticamiba"]} for x in lasit(SAKNE / "data" / "politika" / "notikumi_est_fin.csv")]
    return {"rindas": rindas, "notikumi": LV + EF}


# ---------------------------------------------------------------------------
def atrast(nosaukums):
    for mape in MAPES:
        if (mape / nosaukums).exists():
            return mape / nosaukums
    raise FileNotFoundError(nosaukums)


def main():
    dati = {"skolu_ce": d_skolu_ce(), "skolas": d_skolas(), "escs": d_escs(), "korigeta": d_korigeta(), "lexis": d_lexis(), "panelis": d_panelis(), "efekti": d_efekti(), "dfm": d_dfm(),
            "ce": d_ce(), "dd": d_dd(), "talis": d_talis(), "izdevumi": d_izdevumi(), "hbsc": d_hbsc(),
            "aploksne": d_aploksne(), "atgriesanas": d_atgriesanas(), "scenariji": d_scenariji(), "garie": d_garie(), "politika": d_politika()}
    js = json.dumps(dati, ensure_ascii=False, separators=(",", ":"))
    print(f"Dati: {len(js) / 1024:.0f} KB JSON; " + ", ".join(f"{k} {len(json.dumps(v)) // 1024} KB" for k, v in dati.items()))
    (OUT / "zinojums_prognozes_dati.json").write_text(json.dumps(dati, ensure_ascii=False, indent=0), encoding="utf-8")
    teksts = VEIDNE.read_text(encoding="utf-8")
    assert teksts.count("{{DATA}}") == 1, "veidnē jābūt vienam {{DATA}}"
    teksts = teksts.replace("{{DATA}}", js.replace("</", "<\\/"))
    for vieta, fails in [("{{JS_DZINEJS}}", "86_zinojums_grafiki_dzinejs.js"), ("{{JS_GRAFIKI}}", "86_zinojums_grafiki_lapa.js")]:
        assert teksts.count(vieta) == 1, vieta
        teksts = teksts.replace(vieta, (SAKNE / "R" / "pisa2025" / fails).read_text(encoding="utf-8"))
    for nos in sorted(set(re.findall(r"\{\{IMG:([^}]+)\}\}", teksts))):
        baiti = atrast(nos).read_bytes()
        w, h = struct.unpack(">II", baiti[16:24])
        uri = "data:image/png;base64," + base64.b64encode(baiti).decode("ascii")
        teksts = teksts.replace('<img src="{{IMG:' + nos + '}}"', f'<img width="{w}" height="{h}" src="{uri}"').replace("{{IMG:" + nos + "}}", uri)
        print(f"  attēls {nos}: {len(baiti) / 1024:.0f} KB")
    bez_tagiem = re.sub(r"<script>.*?</script>", " ", teksts, flags=re.S)
    bez_tagiem = re.sub(r"<[^>]+>", " ", bez_tagiem)
    slikti = len(re.findall("[—–]", bez_tagiem))
    if slikti:
        print(f"BRĪDINĀJUMS: tekstā atrastas {slikti} domuzīmes")
    IZVADE.write_text(teksts, encoding="utf-8")
    print(f"Ierakstīts {IZVADE} ({IZVADE.stat().st_size / 1024:.0f} KB)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
