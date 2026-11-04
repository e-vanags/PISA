#!/usr/bin/env python3
"""Ziņojuma lapas tekstu eksports labošanai un imports atpakaļ veidnē.

  python3 py/zinojums_teksti.py eksports
      -> docs/zinojums_teksti_2026-11-05.md un output/docx/zinojums_teksti_2026-11-05.docx
         Katram teksta blokam ir identifikators rindā pirms tā, piemēram [s04.h2], [s04.li2], [s04.sikak.p1].
  python3 py/zinojums_teksti.py imports <labotais.docx vai .md>
      -> ieliek labotos tekstus veidnē R/pisa2025/86_zinojums_prognozes_veidne.html (pēc identifikatoriem),
         rindas "JAUTĀJUMS: ..." savāc docs/zinojums_jautajumi.md un veidnē neliek.

Labošanas noteikumi dokumentā: identifikatora rindu neaiztikt; teksts zem tās ir brīvi labojams;
**treknraksts** paliek treknraksts; blokam, kas jāsvītro, raksta DZĒST; jautājumu raksta kā "JAUTĀJUMS: ...".
"""
import html
import re
import subprocess
import sys
from pathlib import Path

SAKNE = Path(__file__).resolve().parent.parent


def py_docx():
    """Interpretators, kurā ir python-docx (homebrew python3.14 tā nav, /usr/bin/python3 ir)."""
    try:
        import docx  # noqa: F401
        return sys.executable
    except ImportError:
        return "/usr/bin/python3"
VEIDNE = SAKNE / "R" / "pisa2025" / "86_zinojums_prognozes_veidne.html"
MD = SAKNE / "docs" / "zinojums_teksti_2026-11-05.md"
DOCX = SAKNE / "output" / "docx" / "zinojums_teksti_2026-11-05.docx"
JAUT = SAKNE / "docs" / "zinojums_jautajumi.md"


# ---------------------------------------------------------------------------
# HTML <-> Markdown rindas līmenī
# ---------------------------------------------------------------------------
def html_uz_md(s):
    s = re.sub(r"<strong>(.*?)</strong>", r"**\1**", s, flags=re.S)
    s = re.sub(r"<a [^>]*>(.*?)</a>", r"\1", s, flags=re.S)
    s = re.sub(r"<[^>]+>", "", s)
    return html.unescape(re.sub(r"\s+", " ", s)).strip()


def md_uz_html(s):
    s = html.escape(s, quote=False)
    s = re.sub(r"\*\*(.+?)\*\*", r"<strong>\1</strong>", s)
    return s


# ---------------------------------------------------------------------------
# Bloku atrašana veidnē: katrā sadaļā secīgi h1/h2, lead, kpi, li, box p, p, tabulas, notes
# ---------------------------------------------------------------------------
BLOKS = re.compile(r"<(h1|h2|p|li|td|th|div|details)\b")

# Interaktīvo grafiku statiskie attēli Word dokumentam (tas pats saturs, projekta PNG)
GRAFIKI = {
    "garie": ("output/pisa2025/plots/86b_st154_garie_teksti.png", "Skolēnu daļa, kam stundās bija jālasa teksts, garāks par 100 lappusēm, PISA 2018, pa valstīm"),
    "korigeta": ("output/pisa2025/plots/72_korigeta_josla.png", "Latviešu valodas plūsmas lasītprasme 2003 līdz 2025: neapstrādātā un koriģētā rinda, 7 valstis"),
    "lexis": ("output/pisa2025/vizualizacijas/V1_paaudzu_diagramma.png", "Paaudžu diagramma: relatīvā vieta katrā mērījumā pa dzimšanas gadiem un vecumiem"),
    "panelis": ("output/pisa2025/vizualizacijas/V3_bridinajuma_panelis.png", "Brīdinājuma panelis: deviņi rādītāji kā efekta lielumi ar ziņojumu datumiem"),
    "efekti": ("output/pisa2025/vizualizacijas/V2_efekta_lielumi.png", "Ilgtermiņa pazīmes kā efekta lielumi ar Kraft etaloniem"),
    "dfm": ("output/pisa2025/plots/67d_dfm_ex_ante.png", "Ex ante prognozes pa PISA cikliem: faktiskā un prognozētā relatīvā pārmaiņa"),
    "ce": ("output/pisa2025/plots/81_ce_paaudzes.png", "PISA paaudzes 9. un 12. klases eksāmenos"),
    "dd": ("output/pisa2025/plots/79_dd_paaudzes.png", "3. un 6. klases darbi blakus PISA, PIRLS un TIMSS pa paaudzēm"),
    "talis": ("output/pisa2025/plots/77b_talis_rinda_2024_parmainas.png", "TALIS 2024 jaunie vienumi: pārmaiņu nogurums, stress, skolotāju aizplūšana"),
    "izdevumi_rinda": ("output/pisa2025/plots/71b_realie_izdevumi_2000_2023.png", "Reālie izdevumi uz skolēnu 2000 līdz 2023, 8 valstis"),
    "izdevumi_atlikumi": ("output/pisa2025/plots/71d_atlikumi_pa_cikliem.png", "Atlikums pret izdevumu līkni pa PISA cikliem"),
    "hbsc_exante": ("output/pisa2025/plots/65c_pisa_paaudzes_ex_ante.png", "HBSC rādītāji PISA paaudzēm 11 un 13 gadu vecumā pirms PISA"),
    "hbsc_rinda": ("output/pisa2025/plots/65c_latvija_paaudzes.png", "Latvija pret 7 valstīm pa paaudzēm, HBSC rādītāji"),
    "aploksne": ("output/pisa2025/plots/83_aploksne_lasisana.png", "Lasīšanas pārmaiņu robežjosla un Latvijas pozīcija"),
    "atgriesanas": ("output/pisa2025/plots/83_atgriesanas_horizonts.png", "Atgriešanās horizonts: koriģētā rinda un vēsturiskie kāpuma ātrumi"),
    "koridors": ("output/pisa2025/plots/84b_koridors_valstis.png", "Lasīšana: vēsture un iespēju koridors līdz 2035. gadam ar piemēru valstīm"),
    "dalas": ("output/pisa2025/plots/84b_zem_L2_koridors.png", "Skolēnu daļa zem 2. līmeņa trijās jomās: vēsture un koridors"),
}


def sadalas(teksts):
    for m in re.finditer(r'<section class="s" id="(s\d+)" data-title="([^"]*)">(.*?)</section>', teksts, flags=re.S):
        yield m.group(1), m.group(2), m.group(3), m.start(3), m.end(3)


def bloki(sid, ker):
    """Atgriež sarakstu (id, tips, sākums, beigas, iekšējais HTML) sadaļas iekšējā HTML koordinātēs."""
    out = []
    # notes daļu apstrādā atsevišķi
    nm = re.search(r"<details class=\"notes\">.*?</details>", ker, flags=re.S)
    pamat = ker[: nm.start()] if nm else ker
    sk = {}

    def pievieno(prefix, tips, m, ofs=0):
        sk[prefix + tips] = sk.get(prefix + tips, 0) + 1
        out.append((f"{sid}.{prefix}{tips}{sk[prefix + tips]}", tips, m.start(1) + ofs, m.end(1) + ofs, m.group(1)))

    def apstrada(htmls, prefix, ofs):
        # virsraksti
        for m in re.finditer(r"<h[12]>(.*?)</h[12]>", htmls, flags=re.S):
            pievieno(prefix, "h2", m, ofs)
        for m in re.finditer(r'<p class="lead">(.*?)</p>', htmls, flags=re.S):
            pievieno(prefix, "lead", m, ofs)
        for m in re.finditer(r'<div class="kpi[^"]*"><div class="v">(.*?)</div><div class="l">(.*?)</div></div>', htmls, flags=re.S):
            sk[prefix + "kpi"] = sk.get(prefix + "kpi", 0) + 1
            n = sk[prefix + "kpi"]
            out.append((f"{sid}.{prefix}kpi{n}.v", "kpiv", m.start(1) + ofs, m.end(1) + ofs, m.group(1)))
            out.append((f"{sid}.{prefix}kpi{n}.l", "kpil", m.start(2) + ofs, m.end(2) + ofs, m.group(2)))
        for m in re.finditer(r"<li>(.*?)</li>", htmls, flags=re.S):
            pievieno(prefix, "li", m, ofs)
        for m in re.finditer(r'<div class="box[^"]*"><p>(.*?)</p></div>', htmls, flags=re.S):
            pievieno(prefix, "box", m, ofs)
        for m in re.finditer(r"<p>(.*?)</p>", htmls, flags=re.S):
            # izlaiž box rindkopas (tās jau ir) un sadaļu kicker
            if any(b[2] == m.start(1) + ofs for b in out):
                continue
            pievieno(prefix, "p", m, ofs)
        for m in re.finditer(r'<p class="(?:small|src)">(.*?)</p>', htmls, flags=re.S):
            pievieno(prefix, "small", m, ofs)
        for m in re.finditer(r"<h3>(.*?)</h3>", htmls, flags=re.S):
            pievieno(prefix, "h3", m, ofs)
        for m in re.finditer(r"<table>(.*?)</table>", htmls, flags=re.S):
            pievieno(prefix, "tab", m, ofs)
        for m in re.finditer(r'<div class="chart" data-chart="([^"]+)"></div>', htmls, flags=re.S):
            pievieno(prefix, "graf", m, ofs)

    apstrada(pamat, "", 0)
    if nm:
        iek = nm.group(0)
        apstrada(iek, "sikak.", nm.start())
    out.sort(key=lambda b: b[2])
    return out


def tabula_uz_md(thtml):
    rindas = []
    for tr in re.finditer(r"<tr[^>]*>(.*?)</tr>", thtml, flags=re.S):
        sunas = [html_uz_md(c) for c in re.findall(r"<t[hd](?=[\s>])[^>]*>(.*?)</t[hd]>", tr.group(1), flags=re.S)]
        rindas.append("| " + " | ".join(sunas) + " |")
    if len(rindas) > 1:
        n = rindas[0].count("|") - 1
        rindas.insert(1, "|" + "---|" * n)
    return "\n".join(rindas)


def eksports():
    teksts = VEIDNE.read_text(encoding="utf-8")
    md = ["# Ziņojuma teksti labošanai (2026-11-05)", "",
          "Katram teksta blokam pirms tā ir identifikators kvadrātiekavās (piemēram, [s04.li2]). Identifikatoru rindas neaiztikt; tekstu zem tām labot brīvi. "
          "Treknraksts (**tā**) paliek treknraksts. Bloku, kas jāsvītro, aizstāt ar vārdu DZĒST. Jautājumu vai komentāru rakstīt kā atsevišķu rindkopu, kas sākas ar \"JAUTĀJUMS:\"; "
          "to es savākšu atsevišķi un atbildēšu. Tabulās labot šūnu tekstus, ne struktūru. Grafiku paraksti un slāņu nosaukumi ir atsevišķi (JavaScript failā), tos labošu pēc jūsu piezīmēm.", ""]
    for sid, nos, ker, _, _ in sadalas(teksts):
        md.append(f"## {sid[1:].lstrip('0') or '0'}. {nos}  [{sid}]")
        md.append("")
        for bid, tips, _, _, iek in bloki(sid, ker):
            if tips == "graf":
                cels, nos = GRAFIKI.get(iek, (None, iek))
                md.append(f"Grafiks (lapā interaktīvs; šeit statisks attēls ar to pašu saturu): {nos}")
                if cels:
                    md.append(f"![]({cels})")
                md.append("")
                continue
            if tips == "tab":
                md.append(f"[{bid}]")
                md.append(tabula_uz_md(iek))
            else:
                md.append(f"[{bid}]")
                s = html_uz_md(iek)
                # rinda, kas sākas ar "5. ", Word pārveidotājā kļūtu par numurētu sarakstu; neredzams vārda savienotājs to novērš
                md.append(("\u2060" + s) if re.match(r"\d+\.\s", s) else s)
            md.append("")
    MD.write_text("\n".join(md), encoding="utf-8")
    DOCX.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run([py_docx(), str(SAKNE / "py" / "md_uz_docx.py"), str(MD), str(DOCX)], check=True)
    print(f"Ierakstīts {MD} un {DOCX}; bloku: {sum(1 for l in md if l.startswith('[s'))}")


# ---------------------------------------------------------------------------
# Imports: no .md vai .docx
# ---------------------------------------------------------------------------
def nolasit_docx(cels):
    from docx import Document
    doc = Document(str(cels))
    body = doc.element.body
    rindas = []
    for el in body.iterchildren():
        tag = el.tag.split("}")[-1]
        if tag == "p":
            # treknraksts no run
            from docx.text.paragraph import Paragraph
            p = Paragraph(el, doc)
            s = "".join(("**" + r.text + "**") if r.bold and r.text.strip() else r.text for r in p.runs)
            s = re.sub(r"\*\*\s*\*\*", "", s).replace("****", "")
            rindas.append(s)
        elif tag == "tbl":
            from docx.table import Table
            t = Table(el, doc)
            for i, row in enumerate(t.rows):
                rindas.append("| " + " | ".join(c.text.strip() for c in row.cells) + " |")
                if i == 0:
                    rindas.append("|" + "---|" * len(row.cells))
            rindas.append("")
    return rindas


def imports(cels):
    cels = Path(cels)
    if cels.suffix.lower() == ".docx" and py_docx() != sys.executable:
        sys.exit(subprocess.call([py_docx(), __file__, "imports", str(cels)]))
    rindas = nolasit_docx(cels) if cels.suffix.lower() == ".docx" else cels.read_text(encoding="utf-8").splitlines()
    jauni, jaut = {}, []
    bid = None
    PARMAP = {"s07.sikak.p3": "s07.sikak.p6"}  # bloki, kas pēc pirmā eksporta pārnumurēti (2026-11-04: s07 pievienoti p3 līdz p5)
    for r in rindas:
        r = r.replace("\u2060", "").rstrip()
        m = re.fullmatch(r"\s*\[(s\d+(?:\.[\w.]+)?)\]\s*(.*)", r, flags=re.S)
        if m:
            bid = m.group(1) if "." in m.group(1) else None
            if bid:
                bid = PARMAP.get(bid, bid)
                jauni.setdefault(bid, [])
                if m.group(2).strip():
                    jauni[bid].append(m.group(2).strip())
            continue
        if r.startswith("#") or (bid is None) or r.startswith("![") or r.startswith("Grafiks (lapā interaktīvs"):
            continue
        if re.fullmatch(r"\s*\d+\.\s.*\[s\d+\]\s*", r):  # sadaļas virsraksta rinda Word dokumentā
            bid = None
            continue
        if r.strip().upper().startswith("JAUTĀJUMS"):
            jaut.append(f"- **{bid}**: {r.strip()[len('JAUTĀJUMS'):].lstrip(': ')}")
            continue
        if r.strip():
            jauni[bid].append(r.strip())
    teksts = VEIDNE.read_text(encoding="utf-8")
    mainits = 0
    jauns = []
    pos = 0
    for sid, nos, ker, s0, s1 in sadalas(teksts):
        jauns.append(teksts[pos:s0])
        kera = ker
        for bid, tips, b0, b1, iek in sorted(bloki(sid, ker), key=lambda b: -b[2]):
            if tips == "graf" or bid not in jauni or not jauni[bid]:
                continue
            txt = jauni[bid]
            if tips == "tab":
                mdr = [l for l in txt if l.startswith("|") and not re.fullmatch(r"\|(-+\|)+", l)]
                md_rows = [[c.strip() for c in l.strip("|").split("|")] for l in mdr]
                cells = list(re.finditer(r"(<t[hd](?=[\s>])[^>]*>)(.*?)(</t[hd]>)", iek, flags=re.S))
                vec_rows = re.findall(r"<tr[^>]*>(.*?)</tr>", iek, flags=re.S)
                if len(md_rows) == len(vec_rows) and sum(len(r) for r in md_rows) == len(cells):
                    flat = [c for r in md_rows for c in r]
                    out, last = [], 0
                    for i, m in enumerate(cells):
                        out.append(iek[last:m.start(2)]); out.append(md_uz_html(flat[i])); last = m.end(2)
                    out.append(iek[last:]); nov = "".join(out)
                else:
                    rows = []
                    for i, r in enumerate(md_rows):
                        tag = "th" if i == 0 else "td"
                        rows.append("<tr>" + "".join(f"<{tag}>{md_uz_html(c)}</{tag}>" for c in r) + "</tr>")
                    nov = f"\n    <thead>{rows[0]}</thead>\n    <tbody>\n      " + "\n      ".join(rows[1:]) + "\n    </tbody>\n  "
            else:
                sav = " ".join(txt)
                if sav.strip().upper() == "DZĒST":
                    nov = ""
                else:
                    nov = md_uz_html(sav)
            if nov != iek:
                kera = kera[:b0] + nov + kera[b1:]
                mainits += 1
                print("  mainīts:", bid, "|", html_uz_md(iek)[:50], "->", html_uz_md(nov)[:50])
        jauns.append(kera)
        pos = s1
    jauns.append(teksts[pos:])
    VEIDNE.write_text("".join(jauns), encoding="utf-8")
    if jaut:
        JAUT.write_text("# Jautājumi no tekstu labošanas\n\n" + "\n".join(jaut) + "\n", encoding="utf-8")
    print(f"Mainīti bloki: {mainits}; jautājumi: {len(jaut)} ({JAUT if jaut else 'nav'})")


if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] not in ("eksports", "imports"):
        print(__doc__)
        sys.exit(1)
    if sys.argv[1] == "eksports":
        eksports()
    else:
        imports(sys.argv[2])
