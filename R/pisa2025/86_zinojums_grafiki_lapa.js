/* Grafiku definīcijas ziņojuma lapai. Katrs grafiks: funkcija, kas saņem konteineru un DATA, un zīmē ar PZ. */
const GRAFIKI = (() => {
  const C = { s1: "var(--s1)", s2: "var(--s2)", k: "var(--k)", c: "var(--c)", bad: "var(--bad)", ref: "var(--ref)", ink: "var(--ink)", ink2: "var(--ink-2)", warn: "var(--warn)", good: "var(--good)" };
  const PISA_CIKLI = [2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025];
  const f1 = PZ.f1, fs = PZ.fs, el = PZ.el;
  const NOS = { LVA: "Latvija", EST: "Igaunija", LTU: "Lietuva", POL: "Polija", FIN: "Somija", CZE: "Čehija", HUN: "Ungārija", DEU: "Vācija" };
  const VK = { LVA: C.s1, EST: C.s2, LTU: C.k, POL: C.ref, FIN: C.ref, CZE: C.ref, HUN: C.ref, DEU: C.ref };
  const pct = v => f1(v, 1) + " %";
  const zinVlines = (Z, layer) => Z.map(z => ({ x: z.datums, color: z.tips === "brīdina" ? C.bad : z.tips === "nomierina" ? C.good : C.ink2, dash: z.tips === "nomierina" ? "2,3" : z.tips === "mērījums" ? "6,4" : null, width: z.tips === "mērījums" ? 1 : 1.8, layer }));

  // 1. Koriģētā rinda
  function korigeta(root, D) {
    const R = D.korigeta;
    PZ.line(root, {
      w: 1100, h: 480, title: "Latviešu valodas plūsmas lasītprasme 2003 līdz 2025",
      x: { min: 2002, max: 2026, ticks: PISA_CIKLI, tipLabel: "PISA cikls" }, y: { min: 450, max: 520, step: 10, label: "Lasītprasme, PISA punkti" },
      layers: [{ id: "neap", nos: "neapstrādātā rinda" }, { id: "kor", nos: "koriģētā rinda", on: false }, { id: "v7", nos: "7 valstis, visi skolēni", on: false }, { id: "v7v", nos: "7 valstis, galvenā mājas valoda", on: false }],
      series: [
        { id: "neap", nos: "Latviešu valodas plūsma, neapstrādāta", color: C.ref, width: 2.4, band: true, layer: "neap", pts: R.map(r => ({ x: r.gads, y: r.neapstr, lo: r.neapstr_lo, hi: r.neapstr_hi })), endLabel: "neapstrādāta" },
        { id: "kor", nos: "Latviešu valodas plūsma, koriģēta (+9,6 no 2018, +1,9 2025)", color: C.s1, width: 2.6, band: true, layer: "kor", pts: R.map(r => ({ x: r.gads, y: r.korig, lo: r.korig_lo, hi: r.korig_hi })), endLabel: "koriģēta" },
        { id: "v7", nos: "7 valstis, visi skolēni (2003: 5 valstis)", color: C.s2, dash: "2,4", width: 2, marker: "square", layer: "v7", pts: R.map(r => ({ x: r.gads, y: r.v7_visi })), endLabel: "7 valstis, visi" },
        { id: "v7v", nos: "7 valstis, galvenās mājas valodas plūsma", color: C.k, dash: "7,4", width: 2, marker: "square", layer: "v7v", pts: R.map(r => ({ x: r.gads, y: r.v7_valoda })), endLabel: "7 valstis, galvenā valoda" },
      ],
      rects: [{ x0: 2016.5, x1: 2026, y0: 450, y1: 520, fill: "var(--s1)", opacity: 0.06, layer: "kor", nos: "2018 līdz 2025: instrumenta nobīdes korekcija" }],
      piezime: "Joslas: 95 % intervāls (koriģētajai arī korekciju nenoteiktība). 7 valstis: Igaunija, Lietuva, Polija, Somija, Čehija, Ungārija, Vācija. Avots: R/pisa2025/72, docs/67.",
    });
  }

  // 2. Paaudžu diagramma: x = mērījuma gads, y = vecums; viena paaudze savienota ar līniju
  function lexis(root, D) {
    const L = D.lexis;
    const col = z => { const v = Math.max(-0.5, Math.min(0.5, z)); const a = Math.abs(v) / 0.5; return v < 0 ? `rgba(179,38,30,${0.25 + 0.75 * a})` : `rgba(42,120,214,${0.25 + 0.75 * a})`; };
    const kind = p => /PISA/.test(p) ? "pisa" : /PIRLS|TIMSS/.test(p) ? "iea4" : /ICCS|ICILS/.test(p) ? "iea8" : "piaac";
    const shape = p => kind(p) === "pisa" ? "circle" : kind(p) === "piaac" ? "diamond" : "tri";
    const pts = L.punkti.filter(p => !/TIMSS/.test(p.petijums)).map(p => ({ x: p.gads, y: p.vecums, color: col(p.z), r: 7, opacity: 0.95, stroke: "var(--ink)", shape: shape(p.petijums), layer: kind(p.petijums), dz: p.dz,
      tip: `${p.petijums.replace("TIMSS4", "TIMSS")} ${p.gads}: ${fs(p.rel)} punkti pret 7 valstu vidējo (${fs(p.z, 2)} skolēnu SD), paaudze dz. ${Math.round(p.dz)}${kind(p.petijums) === "piaac" ? ", vecums " + f1(p.vecums, 1) : ""}`, label: kind(p.petijums) === "pisa" ? "dz. " + Math.round(p.dz) : "", labelPos: "bl" }));
    // paaudžu savienojumi: no katra PISA punkta uz citu pētījumu punktiem ar tuvu dzimšanas gadu
    const lines = [];
    pts.filter(p => p.layer === "pisa").forEach(p => { pts.filter(q => q.layer !== "pisa" && Math.abs(q.dz - p.dz) <= 1.6).forEach(q => { lines.push({ layer: "paaudzes", color: "var(--muted)", width: 1, pts: [{ x: p.x, y: p.y }, { x: q.x, y: q.y }] }); }); });
    const ZIN_ISS = ["2007-11 PIRLS 2006: \"nopietns brīdinājums\"", "2017-11 PIRLS 2016: \"ievērojami uzlabojušies\"", "2019-12 PISA 2018: \"kopumā labi, stabils\"", "2023-05 PIRLS 2021: \"drauds sistēmai\"", "2023-12 PISA 2022: \"labu un stabilu\"", "2025-03 PISA 2025 tests"];
    L.zinojumi.forEach((z, i) => { lines.push({ layer: "zin", color: z.tips === "brīdina" ? C.bad : z.tips === "nomierina" ? C.good : C.ink2, dash: z.tips === "nomierina" ? "2,4" : z.tips === "mērījums" ? "6,4" : null, width: 1.4, along: true, at: [0.96, 0.96, 0.84, 0.78, 0.72, 0.66][i] ?? 0.9, side: "right",
      pts: [{ x: z.datums, y: 8 }, { x: z.datums, y: 40 }], nos: ZIN_ISS[i] }); });
    PZ.scatter(root, {
      w: 1100, h: 520, m: { l: 56, r: 210, t: 18, b: 44 }, title: "Paaudžu diagramma",
      x: { min: 2000, max: 2026.5, step: 2, label: "Mērījuma gads", tipLabel: "mērījuma gads" }, y: { min: 8, max: 40, step: 4, label: "Vecums mērījumā", fmt: v => f1(v, 0) },
      x2: { label: "PISA dalībnieku dzimšanas gads (mērījuma gads mīnus 16)", ticks: [2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025], fmt: v => String(v - 16), chip: "dzimšanas gads augšā" },
      layers: [{ id: "pisa", nos: "PISA lasīšana, 15 gadi" }, { id: "iea4", nos: "PIRLS lasīšana, 4. klase" }, { id: "iea8", nos: "ICCS un ICILS 8. klase (pastarpināti: teksta uztvere citā jomā)" }, { id: "piaac", nos: "PIAAC pieaugušo lasītprasme 2023" }, { id: "paaudzes", nos: "vienas paaudzes savienojumi" }, { id: "zin", nos: "ziņojumu datumi" }],
      pts, lines,
      legend: [{ color: "rgba(179,38,30,.9)", nos: "sarkans: zem 7 valstu vidējā" }, { color: "rgba(42,120,214,.9)", nos: "zils: virs 7 valstu vidējā (krāsas stiprums = novirze līdz 0,5 SD)" }, { color: "var(--muted)", nos: "plāna līnija: viena paaudze dažādos vecumos", dash: "solid" }, { color: C.bad, nos: "vertikāle: brīdinošs ziņojums", dash: "solid" }, { color: C.good, nos: "nomierinošs ziņojums", dash: "dotted" }, { color: C.ink2, nos: "PISA 2025 tests", dash: "dashed" }],
      piezime: "Tikai lasītprasmes mērījumi: tieši (PISA, PIRLS, PIAAC lasītprasme) un pastarpināti (ICCS pilsoniskās zināšanas un ICILS datorpratība 8. klasē, kur uzdevumi balstās teksta uztverē). Katrs punkts ir viens mērījums: horizontāli tā gads, vertikāli skolēnu vecums tajā (4. klase ap 10,5 gadi, 8. klase ap 14,3, PISA 15,8, PIAAC pa vecuma grupām). Plānās līnijas savieno vienas paaudzes mērījumus; PISA punktiem norādīts dzimšanas gads. Ziņojumu datumi ir vertikālas līnijas. Avots: docs/61, 63; R/pisa2025/70.",
    });
  }

  // 3. Brīdinājuma panelis: deviņi mazi grafiki ar kopīgu slēdzi ziņojumu datumiem
  function panelis(root, D) {
    const P = D.panelis; root.innerHTML = ""; root.classList.add("pz-grid");
    const top = el("div", { class: "pz-tools" }); root.append(top);
    const grid = el("div", { class: "pz-grid2" }); root.append(grid);
    let zin = false;
    const cells = P.serijas.map(sr => { const c = el("div", { class: "pz-cell" }); grid.append(c); return { c, sr }; });
    function drawAll() {
      cells.forEach(({ c, sr }) => {
        const ys = sr.pts.map(p => p.y - 1.96 * p.se).concat(sr.pts.map(p => p.y + 1.96 * p.se), [0.25, -0.25]);
        const ymin = Math.floor(Math.min(...ys) * 5) / 5, ymax = Math.ceil(Math.max(...ys) * 5) / 5;
        PZ.line(c, { w: 520, h: 230, m: { l: 44, r: 12, t: 24, b: 30 }, title: sr.nos,
          x: { min: 2000, max: 2026, step: 3, fmt: v => String(v), tipLabel: "gads" }, y: { min: ymin, max: ymax, step: 0.2, fmt: v => f1(v, 1) },
          series: [{ id: sr.id, nos: sr.nos, color: C.s1, band: true, pts: sr.pts.map(p => ({ x: p.x, y: p.y, lo: p.y - 1.96 * p.se, hi: p.y + 1.96 * p.se, label: sr.nos + (p.grupa ? ", " + p.grupa + " g." : "") })), fmt: v => fs(v, 2), noLegend: true }],
          rects: [{ x0: 2000, x1: 2026, y0: -0.2, y1: 0.2, fill: "var(--ink)", opacity: 0.06 }], hlines: [{ y: 0, color: "var(--ink-2)" }],
          vlines: zin ? zinVlines(P.zinojumi) : [], texts: [{ x: 2000.3, y: ymax - 0.02, t: sr.nos, cls: "ptitle" }] });
        c.querySelector(".pz-tools").remove(); c.querySelector(".pz-legend").remove();
      });
    }
    top.append(el("span", { class: "pz-lab" }, "Datu slāņi:"), PZ.chip("ziņojumu datumi (sarkans brīdina, zaļš nomierina)", false, on => { zin = on; drawAll(); }));
    drawAll();
    root.append(el("p", { class: "pz-note" }, "Vertikālā ass: pārmaiņa pret pirmo mērījumu efekta lieluma vienībās: vidējiem un procentilēm skolēnu standartnovirzēs (d), daļām Koena h; negatīvs vienmēr nozīmē sliktāk. Pelēkā josla ±0,20 ir Kraft (2020) robeža lielam efektam. Gaišās joslas: 95 % intervāls. Vertikālās līnijas: sarkana nepārtraukta brīdinošs nacionālais ziņojums, zaļa punktēta nomierinošs, pelēka raustīta PISA 2025 tests. Avots: docs/58, 63; ziņojumu citāti docs/41, 45."));
  }

  // 4. Efekta lielumi
  function efekti(root, D) {
    const E = D.efekti;
    const panels = E.logi.map(lg => ({ id: lg, nos: lg, rows: E.pazimes.filter(pz => E.rindas.some(r => r.logs === lg && r.paz === pz)).map(pz => { const r = E.rindas.find(x => x.logs === lg && x.paz === pz); return { nos: pz, items: [{ v: r.es, lo: r.lo, hi: r.hi, color: r.veids.startsWith("d") ? C.ink : C.c, tip: `${pz}, ${lg}`, extra: `no ${f1(r.no, 1)} uz ${f1(r.lidz, 1)}; ${r.klase}` }] }; }) }));
    PZ.dots(root, { w: 1100, cols: 2, labW: 200, rowH: 22, title: "Ilgtermiņa pazīmes kā efekta lielumi", panels,
      x: { min: -1, max: 0.5, step: 0.25, fmt: v => f1(v, 2), label: "Efekta lielums: negatīvs = sliktāk; 95 % intervāls ar saites kļūdu" },
      rects: [{ x0: -0.05, x1: 0.05, fill: "var(--ink)", opacity: 0.05 }, { x0: -0.2, x1: -0.05, fill: "var(--warn)", opacity: 0.12 }, { x0: 0.05, x1: 0.2, fill: "var(--warn)", opacity: 0.12 }, { x0: -1, x1: -0.2, fill: "var(--bad)", opacity: 0.08 }, { x0: 0.2, x1: 0.5, fill: "var(--bad)", opacity: 0.08 }],
      vlines: [{ x: 0, color: "var(--ink-2)" }, { x: -0.24, color: C.k, dash: "4,3" }, { x: 0.24, color: C.k, dash: "4,3" }],
      legend: [{ color: C.ink, nos: "melns: vidējo un procentiļu pārmaiņa skolēnu standartnovirzēs (d)" }, { color: C.c, nos: "violets: daļu pārmaiņa, Koena h" }, { color: C.k, nos: "zaļā punktētā: ±0,24 = viens mācību gads lasīšanā 8. un 9. klasē (Bloom et al. 2008)", dash: "dotted" }],
      piezime: "Krāsu joslas: Kraft (2020) etaloni izglītības intervenciju efektiem: pelēks mazs (< 0,05), oranžs vidējs (0,05 līdz 0,20), sarkans liels (≥ 0,20). Zem 2. līmeņa un nelasīšanas zīme apgriezta, lai negatīvs būtu sliktāk. Avots: R/pisa2025/68, docs/63." });
  }

  // 5. DFM ex ante
  function dfm(root, D) {
    const M = D.dfm.modeli; const col = { "M0 tikai prasme (67b)": C.s2, "M1 DFM diagonāla Q": C.s1, "M2 DFM brīva Q": C.k, "M3 DFM vadība k=1": C.c };
    const nos = { "M0 tikai prasme (67b)": "M0: tikai prasmes stāvoklis", "M1 DFM diagonāla Q": "M1: prasme, vide, iesaiste (neatkarīgi)", "M2 DFM brīva Q": "M2: prasme, vide, iesaiste (saistīti)", "M3 DFM vadība k=1": "M3: vide un iesaiste vada prasmi" };
    PZ.line(root, { w: 1100, h: 440, title: "Ex ante prognozes pa PISA cikliem",
      x: { min: 2005, max: 2026, ticks: [2006, 2009, 2012, 2015, 2018, 2022, 2025], tipLabel: "PISA mērķa gads" }, y: { min: -18, max: 10, step: 5, label: "Relatīvā pārmaiņa pret iepriekšējo ciklu, punkti", fmt: v => fs(v, 0) },
      layers: [{ id: "fakt", nos: "faktiskā pārmaiņa" }, { id: "m0", nos: "M0", on: false }, { id: "m1", nos: "M1", on: false }, { id: "m2", nos: "M2", on: false }, { id: "m3", nos: "M3", on: false }],
      series: [{ id: "fakt", nos: "Faktiskā relatīvā pārmaiņa (latviešu valodas plūsma pret 7 valstīm)", color: C.ink, width: 3, layer: "fakt", pts: M[0].pts.map(p => ({ x: p.x, y: p.fakt })), fmt: v => fs(v, 1), endLabel: "faktiskā" },
        ...M.map((m, i) => ({ id: "m" + i, nos: nos[m.id], color: col[m.id], width: 2, layer: "m" + i, pts: m.pts.map(p => ({ x: p.x, y: p.prog })), fmt: v => fs(v, 1), endLabel: "M" + i }))],
      hlines: [{ y: 0, color: "var(--ink-2)" }],
      piezime: "Katram mērķa ciklam modelis novērtēts tikai ar datiem, kas publicēti pirms testa (7 valstis un latviešu valodas plūsma). Avots: R/pisa2025/67d, docs/73." });
  }

  // 6. CE paaudzes
  function ce(root, D) {
    const S = D.ce.serijas; const col = [C.s1, C.k, C.s2, C.c, C.bad];
    PZ.line(root, { w: 1100, h: 480, title: "PISA paaudzes valsts eksāmenos",
      x: { min: 1989.5, max: 2010.5, step: 2, label: "Dzimšanas gads (paaudze); 9. klase = mācību gads mīnus 15, 12. klase = mīnus 18", tipLabel: "dzimšanas gads" }, y: { min: -4.5, max: 2.5, step: 1, label: "z: novirze no sērijas stabilo gadu vidējā (SD vienībās)", fmt: v => fs(v, 1) },
      x2: { label: "9. klases eksāmena mācību gads (dzimšanas gads + 15); PISA cikls = dzimšanas gads + 16", ticks: [1991, 1993, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 2009], fmt: v => `${v + 15}./${String(v + 16).slice(2)}.`, chip: "eksāmena gads augšā" },
      layers: [{ id: "l9", nos: "9. kl. latviešu valoda" }, { id: "m9", nos: "9. kl. matemātika", on: false }, { id: "k12", nos: "12. klase", on: false }, { id: "pisa", nos: "PISA latviešu valodas plūsma", on: false }],
      series: [...S.map((sr, i) => ({ id: sr.id, nos: sr.nos, color: col[i], width: sr.klase === 12 ? 1.8 : 2.2, dash: sr.klase === 12 ? "6,4" : null, marker: sr.klase === 12 ? "square" : "circle",
          layer: sr.klase === 12 ? "k12" : /matemātika/.test(sr.nos) ? "m9" : "l9", pts: sr.pts.map(p => ({ x: p.x, y: p.y, label: `${sr.nos}, ${p.gads}: ${f1(p.vert, 1)} %, ${p.rangs}. no ${p.n_gadi} gadiem` })), fmt: v => fs(v, 2) })),
        { id: "pisa", nos: "PISA lasīšana, latviešu valodas plūsma (tajā pašā z)", color: C.ink, marker: "cross", layer: "pisa", dash: "2,4", width: 1.2, pts: D.ce.pisa.map(p => ({ x: p.x, y: p.y, label: `${p.pisa}: ${f1(p.vert, 1)} punkti` })), fmt: v => fs(v, 2) }],
      hlines: [{ y: 0, color: "var(--ink-2)" }],
      vlines: [{ x: 2006.5, color: C.bad, dash: "4,3", nos: "2022./23.: 9. kl. CE visiem, centralizēta vērtēšana (dz. 2007 un vēlāk)" }],
      rects: [2002, 2006, 2009].map(b => ({ x0: b - 0.4, x1: b + 0.4, y0: -4.5, y1: 2.5, fill: "var(--bad)", opacity: 0.07 })),
      texts: [{ x: 2002, y: 2.35, t: "PISA 2018", anchor: "middle" }, { x: 2006, y: 2.35, t: "PISA 2022", anchor: "middle" }, { x: 2009, y: 2.35, t: "PISA 2025", anchor: "middle" }],
      piezime: "Katra sērija standartizēta pēc savu gadu vidējā un SD līdz 2018./19., lai 2025. gads nenostieptu atskaiti. Gadi nav IRT saskaņoti, tāpēc tikai relatīvs lasījums. 9. klase: dzimšanas gads = mācību gads mīnus 15; 12. klase: mīnus 18. Avots: VIAA, R/pisa2025/81, docs/81." });
  }

  // 7. Diagnosticējošie darbi
  function dd(root, D) {
    const S = D.dd.serijas; const col = { "DD3 lasīšana": C.s2, "DD6 lasīšana": C.s1, "PISA lasīšana": C.ink, "PIRLS lasīšana": C.k, "TIMSS4 matemātika": C.c };
    PZ.line(root, { w: 1100, h: 440, title: "3. un 6. klases darbi blakus PISA, PIRLS un TIMSS",
      x: { min: 1995.5, max: 2013, step: 2, label: "Dzimšanas gads (paaudze); 3. klase = mācību gads mīnus 9, 6. klase = mīnus 12", tipLabel: "dzimšanas gads" }, y: { min: -2.5, max: 2.5, step: 1, label: "z: novirze no sērijas visu gadu vidējā", fmt: v => fs(v, 1) },
      x2: { label: "PISA mērījuma gads (dzimšanas gads + 16)", ticks: [1996, 1999, 2002, 2006, 2009, 2012], fmt: v => String(v + 16), chip: "PISA gads augšā" },
      layers: [{ id: "dd6", nos: "6. klase" }, { id: "dd3", nos: "3. klase", on: false }, { id: "pisa", nos: "PISA", on: false }, { id: "iea", nos: "PIRLS un TIMSS 4. klase", on: false }],
      series: S.map(sr => ({ id: sr.id, nos: sr.nos, color: col[sr.id], width: /DD/.test(sr.id) ? 2.2 : 1.8, dash: /DD/.test(sr.id) ? null : "6,4", marker: /PISA/.test(sr.id) ? "square" : "circle",
        layer: sr.id === "DD6 lasīšana" ? "dd6" : sr.id === "DD3 lasīšana" ? "dd3" : sr.id === "PISA lasīšana" ? "pisa" : "iea", pts: sr.pts.map(p => ({ x: p.x, y: p.y, label: `${sr.nos}, ${p.gads}: ${f1(p.vert, 1)}` })), fmt: v => fs(v, 2) })),
      hlines: [{ y: 0, color: "var(--ink-2)" }],
      piezime: "Diagnosticējošie darbi: 3. klase = mācību gads mīnus 9, 6. klase = mīnus 12; PISA, PIRLS un TIMSS z pēc tās pašas sērijas visu gadu vidējā. Avots: VISC/VIAA, R/pisa2025/79, docs/75." });
  }

  // 8. TALIS 2024 jaunie vienumi
  function talis(root, D) {
    const V = D.talis.vienumi; const order = ["LVA", "EST", "LTU", "POL", "FIN", "CZE", "HUN"];
    const panels = V.map(v => { const xs = v.valstis.flatMap(c => [c.est - 1.96 * c.se, c.est + 1.96 * c.se]); const lo = Math.min(...xs), hi = Math.max(...xs); const pad = (hi - lo) * 0.12;
      return { id: v.id, nos: v.nos, x: { min: lo - pad, max: hi + pad, step: v.dala ? 5 : 0.02 }, rows: order.filter(c => v.valstis.some(x => x.cnt === c)).map(c => { const x = v.valstis.find(x => x.cnt === c); return { nos: NOS[c], items: [{ v: x.est, lo: x.est - 1.96 * x.se, hi: x.est + 1.96 * x.se, color: c === "LVA" ? C.bad : C.ref, r: c === "LVA" ? 5.5 : 4.5, tip: `${NOS[c]}: ${v.nos}` }] }; }) }; });
    PZ.dots(root, { w: 1100, cols: 2, labW: 80, rowH: 19, title: "TALIS 2024 jaunie vienumi", panels, x: { min: 0, max: 100, step: 10, fmt: v => f1(v, v < 1 ? 2 : 0) },
      legend: [{ color: C.bad, nos: "Latvija" }, { color: C.ref, nos: "6 salīdzinājuma valstis" }],
      piezime: "ISCED 2 skolotāji un direktori, 2023./24. mācību gads; 95 % intervāls (BRR). Daļas procentos; attiecības kā skaitlis. PISA 2025 paaudze tobrīd mācījās 8. klasē. Avots: OECD TALIS 2024, R/pisa2025/77b, docs/72." });
  }

  // 9. Izdevumi: rinda un atlikumi
  function izdevumi_rinda(root, D) {
    const I = D.izdevumi; const order = ["LVA", "EST", "LTU", "POL", "CZE", "DEU", "FIN", "HUN"];
    PZ.line(root, { w: 1100, h: 480, m: { l: 64, r: 110, t: 18, b: 44 }, title: "Reālie izdevumi uz skolēnu 2000 līdz 2023",
      x: { min: 2000, max: 2023, step: 2, tipLabel: "gads" }, y: { min: 0, max: 18000, step: 2000, label: "USD pēc pirktspējas paritātes uz skolēnu gadā, 2020. gada cenas", fmt: v => f1(v, 0) },
      layers: [{ id: "lva", nos: "Latvija" }, { id: "balt", nos: "Igaunija un Lietuva", on: false }, { id: "cit", nos: "pārējās 5 valstis", on: false }],
      series: order.map(c => ({ id: c, nos: NOS[c], color: VK[c], width: c === "LVA" ? 3.2 : c === "EST" || c === "LTU" ? 2.2 : 1.4, marker: "none", layer: c === "LVA" ? "lva" : c === "EST" || c === "LTU" ? "balt" : "cit", opacity: VK[c] === C.ref ? 0.8 : 1, pts: I.rinda[c].map(p => ({ x: p.x, y: p.y })), fmt: v => f1(v, 0), endLabel: true })),
      vlines: [{ x: 2008.5, nos: "2009 krīze", dash: "3,3" }, { x: 2016.5, nos: "2016 algu modelis", dash: "3,3" }, { x: 2020.5, nos: "Skola2030, Covid", dash: "3,3" }],
      piezime: "ISCED 1 un 2 vidējais. OECD Education at a Glance no 2005. gada; pirms tam Pasaules Bankas daļa reizināta ar IKP uz iedzīvotāju, piesaistīta OECD līmenim. Somijas un Vācijas lēciens 2012. gadā ir ISCED pārklasifikācija OECD datos. Avots: R/pisa2025/71, docs/65." });
  }
  function izdevumi_atlikumi(root, D) {
    const I = D.izdevumi; const order = ["EST", "LTU", "POL", "CZE", "DEU", "FIN", "HUN"];
    PZ.line(root, { w: 1100, h: 460, m: { l: 56, r: 190, t: 18, b: 44 }, title: "Lasīšanas rezultāts pret izdevumu līkni",
      x: { min: 2005, max: 2026, ticks: [2006, 2009, 2012, 2015, 2018, 2022, 2025], tipLabel: "PISA cikls" }, y: { min: -25, max: 55, step: 10, label: "Punkti virs (+) vai zem (−) izdevumu līknes", fmt: v => fs(v, 0) },
      layers: [{ id: "lva", nos: "Latvija, visi skolēni" }, { id: "a", nos: "latviešu valodas plūsma", on: false }, { id: "cit", nos: "7 salīdzinājuma valstis", on: false }],
      series: [{ id: "LVA", nos: "Latvija, visi skolēni (95 % josla)", color: C.bad, width: 3, band: true, layer: "lva", pts: I.atlikumi.LVA.map(p => { const a = I.a_plusma.find(q => q.x === p.x); return { x: p.x, y: p.y, lo: a ? p.y - 1.96 * a.lva_se : null, hi: a ? p.y + 1.96 * a.lva_se : null }; }), fmt: v => fs(v, 1), endLabel: "Latvija, visi" },
        { id: "A", nos: "Latvija, latviešu valodas plūsma", color: C.s1, width: 2.6, dash: "7,4", layer: "a", pts: I.a_plusma.map(p => ({ x: p.x, y: p.y })), fmt: v => fs(v, 1), endLabel: "latviešu valodas plūsma" },
        ...order.map(c => ({ id: c, nos: NOS[c], color: VK[c], width: 1.4, marker: "none", opacity: 0.85, layer: "cit", pts: I.atlikumi[c].map(p => ({ x: p.x, y: p.y })), fmt: v => fs(v, 1), endLabel: true }))],
      hlines: [{ y: 0, color: "var(--ink-2)" }],
      piezime: "Līkne katrā ciklā no jauna visām valstīm ar datiem: lasīšana pret kumulatīvo izdevumu logaritmu (6 gadi ISCED 1 plus 4 gadi ISCED 2, finanšu gads ap PISA mīnus 2). Avots: R/pisa2025/71, docs/65." });
  }

  // 10. HBSC
  function hbsc_exante(root, D) {
    const H = D.hbsc; const ID = ["sch_satis", "sch_pressure", "teachersup_cutoff", "studsup_cutoff", "beenbullied_2_3", "lifesat_mean", "sr_health", "multihealth", "d_sleep", "d_feellow", "d_nervous", "famsup_hi", "peersup_hi", "acachieve_good", "saliktais"];
    let mode = "4";
    function draw() {
      const years = mode === "4" ? [2015, 2018, 2022, 2025] : [2003, 2006, 2009, 2012];
      const panels = years.map(y => ({ id: String(y), nos: `PISA ${y} paaudze (dz. ${y - 16})`, rows: ID.filter(id => H.exante.some(e => e.pisa === y && e.id === id)).map(id => ({ nos: H.nos[id], items: H.exante.filter(e => e.pisa === y && e.id === id).sort((a, b) => a.vecums - b.vecums).map(e => ({ v: e.z, lo: e.z - 1.96 * e.se, hi: e.z + 1.96 * e.se, color: e.vecums === 11 ? C.s1 : C.k, shape: /pārlūks/i.test(e.avots) ? "square" : "circle", tip: `${H.nos[id]}, ${e.vecums} gadi (HBSC ${e.gads}, ${e.avots})` })) })) }));
      PZ.dots(root, { w: 1100, cols: 2, labW: 236, rowH: 20, title: "HBSC rādītāji pirms PISA", panels, x: { min: -4, max: 2.5, step: 1, fmt: v => fs(v, 1), label: "z pret 7 salīdzinājuma valstu vidējo (+ = Latvijai labāk)" },
        vlines: [{ x: 0, color: "var(--ink-2)" }], legend: [{ color: C.s1, nos: "11 gadu vecumā" }, { color: C.k, nos: "13 gadu vecumā" }],
        controls: [PZ.sel([{ v: "4", t: "paaudzes: PISA 2015 līdz 2025" }, { v: "v", t: "paaudzes: PISA 2003 līdz 2012" }], mode, v => { mode = v; draw(); })],
        piezime: "Latvijas relatīvā vieta tai pašai dzimšanas paaudzei 4 līdz 5 gadus pirms PISA; 95 % intervāls (skolu klasteru SE; pārlūka rindām tuvinājums). Rādītājiem, kur lielāka vērtība ir sliktāk, zīme apgriezta, tāpēc pa labi no nulles vienmēr ir Latvijai labvēlīgāk. Avots: HBSC mikrodati un pārlūks, R/pisa2025/65c, docs/78." });
    }
    draw();
  }
  function hbsc_rinda(root, D) {
    const H = D.hbsc; let id = "saliktais"; let ass = "gads";
    const ids = ["saliktais", "sch_satis", "sch_pressure", "teachersup_cutoff", "studsup_cutoff", "beenbullied_2_3", "lifesat_mean", "sr_health", "multihealth", "d_sleep", "famsup_hi", "peersup_hi", "acachieve_good"];
    function draw() {
      const R = H.rinda[id] || []; const col = { 11: C.s1, 13: C.k, 15: C.c };
      const gadaAss = ass === "gads";
      PZ.line(root, { w: 1100, h: 400, title: "Latvija pret 7 valstīm, HBSC",
        x: gadaAss ? { min: 2000, max: 2024, ticks: [2002, 2006, 2010, 2014, 2018, 2022], fmt: v => `${v - 1}/${String(v).slice(2)}`, label: "HBSC aptaujas kārta", tipLabel: "HBSC kārta" } : { min: 1985, max: 2012, step: 3, label: "Dzimšanas gads (paaudze) = aptaujas gads mīnus vecums", tipLabel: "dzimšanas gads" },
        x2: gadaAss ? { label: "PISA cikls, kura paaudze ir 15 gadu vecumā (aptaujas gads + 1)", ticks: [2002, 2006, 2010, 2014, 2018, 2022], fmt: v => String(v + 1), chip: "PISA cikls augšā", on: false } : { label: "PISA mērījuma gads (dzimšanas gads + 16)", ticks: [1987, 1990, 1993, 1996, 1999, 2002, 2006, 2009], fmt: v => String(v + 16), chip: "PISA gads augšā" },
        y: { min: -3, max: 2, step: 1, label: "z pret 7 valstu vidējo: uz augšu Latvijai labvēlīgāk", fmt: v => fs(v, 1) },
        layers: [{ id: "11", nos: "11 gadi" }, { id: "13", nos: "13 gadi" }, { id: "15", nos: "15 gadi" }],
        series: [11, 13, 15].map(v => ({ id: String(v), nos: `${v} gadu vecumā`, color: col[v], layer: String(v), width: 2.2, pts: R.filter(r => r.vecums === v).map(r => ({ x: gadaAss ? r.gads : r.b, y: r.z, label: `${H.nos[id]}, ${v} gadi, HBSC ${r.gads - 1}/${String(r.gads).slice(2)}, dz. ${r.b}${r.vert !== null && r.vert !== undefined ? ": " + f1(r.vert, 1) : ""}` })), fmt: v => fs(v, 2) })),
        hlines: [{ y: 0, color: "var(--ink-2)" }],
        controls: [PZ.sel(ids.map(i => ({ v: i, t: H.nos[i] })), id, v => { id = v; draw(); }), PZ.sel([{ v: "gads", t: "x ass: aptaujas kārta" }, { v: "b", t: "x ass: dzimšanas gads" }], ass, v => { ass = v; draw(); })],
        piezime: "Viena kārta mēra trīs vecuma grupas (11, 13, 15 gadi), tāpēc pēc aptaujas kārtas līnijas ir salīdzināmas laikā, pēc dzimšanas gada tā pati paaudze ir trīs punkti dažādās kārtās. Saliktais: 12 skolas un labbūtības rādītāju vidējais z. Rādītājiem, kur lielāka vērtība ir sliktāk (nosaukumā \"apgriezts\"), zīme apgriezta, tāpēc uz augšu vienmēr ir Latvijai labvēlīgāk. Avots: HBSC Open Access 2001/02 līdz 2017/18 un pārlūks 2021/22, R/pisa2025/65c, docs/78." });
    }
    draw();
  }

  // 11. Robežjosla
  function aploksne(root, D) {
    const A = D.aploksne; let joma = "READ"; const JN = { READ: "lasīšana", MATH: "matemātika", SCIE: "dabaszinātnes" };
    function draw() {
      const Q = A.kvantiles[joma] || []; const lines = ["P5", "P25", "P50", "P75", "P95"].map(q => ({ layer: "kv", color: "var(--ink-2)", width: q === "P50" ? 1.6 : 1, dash: q === "P50" ? null : q === "P25" || q === "P75" ? "2,3" : "6,4", pts: Q.map(r => ({ x: r.l, y: r[q] })), nos: q }));
      const pts = A.punkti.filter(p => p.joma === joma).map(p => ({ x: p.l, y: p.g, color: C.ref, r: 3, opacity: 0.55, shape: p.m ? "tri" : "circle", layer: "cit", tip: `${p.valsts} ${p.no} → ${p.uz}${p.m ? " (marķēts cikls)" : ""}` }));
      A.latvija.filter(p => p.joma === joma).forEach(p => { const a = /plūsma/.test(p.valsts); pts.push({ x: p.l, y: p.g, color: a ? C.s1 : C.bad, r: 6, opacity: 1, stroke: "var(--card)", layer: a ? "a" : "lva", label: `${p.no} → ${p.uz}`, tip: `${p.valsts}: ${p.no} → ${p.uz}, procentile ${f1(p.proc, 0)}` }); });
      PZ.scatter(root, { w: 1100, h: 520, title: "Pārmaiņu robežjosla",
        x: { min: 340, max: 600, step: 50, label: "Sākuma cikla vidējais, punkti", tipLabel: "sākuma līmenis" }, y: { min: -16, max: 16, step: 5, label: "Pārmaiņa gadā, punkti", fmt: v => fs(v, 1) },
        layers: [{ id: "cit", nos: "visas valstis, secīgu ciklu pāri" }, { id: "kv", nos: "kvantiles P5, P25, P50, P75, P95", on: false }, { id: "lva", nos: "Latvija, visa valsts (oficiāli)", on: false }, { id: "a", nos: "latviešu valodas plūsma (koriģēti)", on: false }],
        pts, lines, controls: [PZ.sel(Object.keys(JN).map(k => ({ v: k, t: "joma: " + JN[k] })), joma, v => { joma = v; draw(); })],
        legend: [{ color: C.ref, nos: "citas valstis (trijstūris: marķēts cikls: datorizācija 2015, adaptīvā lasīšana 2018, Covid 2022)" }, { color: C.bad, nos: "Latvija, visa valsts" }, { color: C.s1, nos: "latviešu valodas plūsma, koriģēti" }],
        piezime: "Pārmaiņa gadā = pārmaiņa punktos dalīta ar ciklu gadu starpību. Kvantiles no regresijas ar pāra fiksētajiem efektiem. Avots: OECD PISA 2025 I sējums B1 pielikums, R/pisa2025/83, docs/84." });
    }
    draw();
  }

  // 12. Atgriešanās horizonts
  function atgriesanas(root, D) {
    const A = D.atgriesanas; const last = A.rinda[A.rinda.length - 1]; const col = { P50: C.ref, P95_2: C.s2, P95_1: C.k, max: C.c };
    PZ.line(root, { w: 1100, h: 460, m: { l: 56, r: 200, t: 18, b: 44 }, title: "Atgriešanās horizonts",
      x: { min: 2002, max: 2041, step: 3, tipLabel: "gads" }, y: { min: 455, max: 520, step: 10, label: "Lasīšana, koriģētais vidējais" },
      layers: [{ id: "rinda", nos: "koriģētā rinda" }, { id: "P50", nos: "tipiska valsts (P50)", on: false }, { id: "P95_2", nos: "P95, 6 līdz 7 gadu logi", on: false }, { id: "P95_1", nos: "P95, 3 līdz 4 gadu logi", on: false }, { id: "max", nos: "vēsturiskais maksimums", on: false }],
      series: [{ id: "rinda", nos: "Latviešu valodas plūsma, koriģēta", color: C.s1, width: 2.6, band: true, layer: "rinda", pts: A.rinda.map(p => ({ x: p.x, y: p.y, lo: p.lo, hi: p.hi })) },
        ...A.atrumi.map(a => ({ id: a.id, nos: `${a.nos} (${fs(a.gada, 1)} gadā)`, color: col[a.id], width: 2, dash: a.id === "P50" ? null : "7,4", marker: "none", layer: a.id, pts: [{ x: 2025, y: last.y }, { x: 2041, y: last.y + a.gada * 16 }], endLabel: a.nos.split(",")[0] }))],
      hlines: [{ y: A.limenis2018, color: C.s1, dash: "4,4", nos: `2018: ${f1(A.limenis2018, 1)}` }], vlines: [{ x: 2029, nos: "PISA 2029", dash: "3,3" }, { x: 2032, nos: "PISA 2032", dash: "3,3" }, { x: 2035, nos: "PISA 2035", dash: "3,3" }],
      piezime: `Vajadzīgais kāpums līdz 2018. gada līmenim: ${f1(A.vajadzigs, 1)} punkti. Ātrumi: valstis 440 līdz 500 punktu līmenī, visi PISA cikli. Avots: R/pisa2025/83, docs/84.` });
  }

  // 13. Koridors (vidējais) ar piemēru valstīm
  function koridors(root, D) {
    const S = D.scenariji; let skala = "oficiāli", kopa = "visi";
    function draw() {
      const V = S.videjie[`READ|${skala}|${kopa}`] || {}; const gadi = [2029, 2032, 2035].filter(g => V[g]); const vest = S.vesture[skala]; const l25 = vest[vest.length - 1].y;
      const band = (id, lo, hi, op, nos) => ({ id, nos, color: C.s1, band: true, bandOpacity: op, marker: "none", opacity: 0, layer: "kor", noLegend: true, pts: [{ x: 2025, y: l25, lo: l25, hi: l25 }, ...gadi.map(g => ({ x: g, y: V[g][lo], lo: V[g][lo], hi: V[g][hi] }))], fmt: v => f1(v, 0) });
      const ln = (id, sc, color, dash, nos) => ({ id, nos: nos || sc, color, dash, width: 2, marker: "none", layer: "sc", pts: [{ x: 2025, y: l25 }, ...gadi.map(g => ({ x: g, y: V[g][sc] }))], fmt: v => f1(v, 0), endLabel: nos || sc });
      PZ.line(root, { w: 1100, h: 520, m: { l: 56, r: 230, t: 18, b: 44 }, title: "Lasīšana: vēsture un iespēju koridors līdz 2035",
        x: { min: 2002, max: 2036, ticks: [2003, 2009, 2015, 2018, 2022, 2025, 2029, 2032, 2035], tipLabel: "gads" }, y: { min: 400, max: 505, step: 20, label: "PISA lasīšanas punkti, latviešu valodas plūsma" },
        layers: [{ id: "vest", nos: "vēsture 2003 līdz 2025" }, { id: "kor", nos: "koridors (gaišs 90 %, tumšs vidējā puse)", on: false }, { id: "sc", nos: "tipiska valsts un vēsturiskais maksimums", on: false }, { id: "piem", nos: "piemēru valstis", on: false }],
        series: [{ id: "vest", nos: `Latviešu valodas plūsma, ${skala}`, color: C.bad, width: 2.8, layer: "vest", pts: vest.map(p => ({ x: p.x, y: p.y })), fmt: v => f1(v, 1) },
          band("b90", "Ļoti slikts", "Vēsturiskais maksimums", 0.1, "90 % gadījumu (P10 līdz P95)"), band("b50", "Slikts gadījums", "Labākā ceturtdaļa", 0.22, "grīda līdz mērķis (P25 līdz P75)"),
          ln("p50", "Tipiska valsts", C.s1, "7,4"), ln("p95", "Vēsturiskais maksimums", C.s1, "2,4", "vēsturiskais maksimums"), ln("p25", "Slikts gadījums", C.s1, null, "grīda"), ln("p75", "Labākā ceturtdaļa", C.s1, null, "mērķis"),
          ...S.piemeri.map(p => ({ id: p.valsts, nos: `${p.valsts} ${p.no} līdz ${p.uz} (${fs(p.parmaina, 0)})`, color: C.ink2, width: 1.2, dash: "2,3", layer: "piem", marker: "open", pts: [{ x: 2025, y: l25 }, { x: 2025 + p.gadi, y: l25 + p.parmaina }], fmt: v => f1(v, 0), endLabel: `${p.valsts} ${p.no} līdz ${p.uz} (${fs(p.parmaina, 0)})` }))],
        legendExtra: [{ color: C.s1, nos: "koridors: valstis, kas kādā ciklā bijušas Latvijas līmenī (± 30 punkti), to pārmaiņas 4, 7 un 10 gadu laikā" }],
        controls: [PZ.sel([{ v: "oficiāli", t: "skala: oficiālā" }, { v: "koriģēti", t: "skala: koriģētā" }], skala, v => { skala = v; draw(); }), PZ.sel([{ v: "visi", t: "atsauces klase: visi pāri" }, { v: "bez marķētajiem", t: "bez marķētajiem cikliem" }, { v: "Covid laikmets (sākums ≥ 2018)", t: "tikai Covid laikmets" }], kopa, v => { kopa = v; draw(); })],
        piezime: "Piemēru valstu faktiskās pārmaiņas pārbīdītas uz Latvijas 2025. gada līmeni. Koriģētā skala pieskaita 2018. gada instrumenta nobīdi (+9,6) un 2025. gada iesaisti (+1,9). Avots: R/pisa2025/84, 84b; docs/85, 87." });
    }
    draw();
  }

  // 14. Daļas zem 2. līmeņa un augšgalā, trīs jomas
  function dalas(root, D) {
    const S = D.scenariji; let mala = "zem 2. līmeņa, %"; root.innerHTML = ""; root.classList.add("pz-grid");
    const top = el("div", { class: "pz-tools" }); const grid = el("div", { class: "pz-grid3" }); root.append(top, grid);
    const JN = ["lasīšana", "matemātika", "dabaszinātnes"]; const cells = JN.map(() => { const c = el("div", { class: "pz-cell" }); grid.append(c); return c; });
    let on = { vest: true, kor: false, merk: false };
    function drawAll() {
      JN.forEach((j, i) => {
        const key = `${j}|oficiāli|${mala}`; const Dd = S.dalas[key] || {}; const vest = S.dalu_vesture[key] || []; const l25 = Dd["2025"]; const gadi = [2029, 2032, 2035].filter(g => Dd[g]);
        const zem = mala.startsWith("zem"); const ymax = zem ? 45 : 13;
        const band = (id, lo, hi, op) => ({ id, nos: id, color: C.s1, band: true, bandOpacity: op, marker: "none", opacity: 0, layer: "kor", noLegend: true, pts: [{ x: 2025, y: l25, lo: l25, hi: l25 }, ...gadi.map(g => ({ x: g, y: Dd[g][lo], lo: Dd[g][lo], hi: Dd[g][hi] }))], fmt: pct });
        const ln = (sc, dash, nos) => ({ id: sc, nos, color: C.s1, dash, width: 1.8, marker: "none", layer: "kor", pts: [{ x: 2025, y: l25 }, ...gadi.map(g => ({ x: g, y: Dd[g][sc] }))], fmt: pct, endLabel: nos });
        PZ.line(cells[i], { w: 520, h: 330, m: { l: 60, r: 96, t: 24, b: 30 }, title: j,
          x: { min: 2002, max: 2036, ticks: [2003, 2009, 2015, 2022, 2029, 2035], tipLabel: "gads" }, y: { min: 0, max: ymax, step: zem ? 10 : 2.5, fmt: v => f1(v, 0), label: zem ? "% skolēnu zem 2. līmeņa: uz augšu sliktāk" : "% skolēnu 5. un 6. līmenī: uz augšu labāk" },
          layers: [{ id: "vest", nos: "vēsture", on: on.vest }, { id: "kor", nos: "scenāriji", on: on.kor }, { id: "merk", nos: "mērķa līnija", on: on.merk }],
          series: [{ id: "vest", nos: "latviešu valodas plūsma, oficiālā skala", color: C.ink, width: 2.4, layer: "vest", pts: vest.map(p => ({ x: p.x, y: p.y })), fmt: pct },
            band("90 %", "Ļoti slikts", "Vēsturiskais maksimums", 0.1), band("vidējā puse", "Slikts gadījums", "Labākā ceturtdaļa", 0.22), ln("Tipiska valsts", "7,4", "tipiska valsts"), ln("Slikts gadījums", null, "slikts gadījums"), ln("Labākā ceturtdaļa", null, "labākā ceturtdaļa")],
          hlines: zem ? [{ y: 15, color: C.bad, nos: "ES mērķis 15 %" }] : [{ y: 10, color: C.bad, nos: "mērķis 10 %" }, { y: 12, color: C.bad, dash: "4,3", nos: "ES 12 %" }],
          texts: [{ x: 2002.5, y: ymax * 0.97, t: j, cls: "ptitle" }] });
        const c = cells[i]; c.querySelector(".pz-tools").remove(); c.querySelector(".pz-legend").remove();
      });
      // mērķa līnijas slānis: noņem hlines, ja izslēgts
      if (!on.merk) cells.forEach(c => c.querySelectorAll("svg line[stroke='var(--bad)'], svg text[fill='var(--bad)']").forEach(e => e.remove()));
    }
    const chips = [["vest", "vēsture 2003 līdz 2025"], ["kor", "scenāriji: slikts, tipisks, labākā ceturtdaļa"], ["merk", "mērķa līnijas"]].map(([id, nos]) => PZ.chip(nos, on[id], v => { on[id] = v; drawAll(); }));
    top.append(el("span", { class: "pz-lab" }, "Datu slāņi:"), ...chips, PZ.sel([{ v: "zem 2. līmeņa, %", t: "mala: zem 2. līmeņa" }, { v: "5. un 6. līmenis, %", t: "mala: 5. un 6. līmenis" }], mala, v => { mala = v; drawAll(); }));
    drawAll();
    root.append(el("p", { class: "pz-note" }, "Daļas no vidējā ar visu valstu sakarību (daļa ~ vidējais + vidējais²), piesaistītas Latvijas 2025. gada daļai; pārbaudītas pret tiešo daļu robežjoslu (sakrīt ± 2 pp). Koridors: valstis ar sākuma līmeni ± 30 ap latviešu valodas plūsmas 2025. gada līmeni. Avots: R/pisa2025/84, docs/85."));
  }

  // 15. Garu tekstu lasīšana skolā (PISA 2018 ST154)
  function garie(root, D) {
    const G = D.garie; const lv = g => /^LVA/.test(g);
    const virs = G.virs100.filter(x => x.grupa !== "7 valstis").slice().sort((a, b) => b.dala - a.dala);
    const kat = [...new Set(G.kategorijas.map(x => x.kat))].sort((a, b) => a - b);
    const katNos = k => G.kategorijas.find(x => x.kat === k).kat_nos;
    const grupas = [["LVA A", C.bad], ["7 valstis", C.s1], ["EST", C.k], ["FIN", C.c]];
    const panels = [
      { id: "virs", nos: "Teksts garāks par 100 lappusēm, % skolēnu", x: { min: 0, max: 85, step: 10 }, rows: virs.map(x => ({ nos: x.nos.replace("Latvija, ", "Latvija: "), items: [{ v: x.dala, lo: x.dala - 1.96 * x.se, hi: x.dala + 1.96 * x.se, color: lv(x.grupa) ? C.bad : C.s1, r: lv(x.grupa) ? 5.5 : 4.5, tip: x.nos, extra: `n = ${x.n}; 95 % intervāls (BRR)` }] })) },
      { id: "kat", nos: "Garākā teksta garums: sadalījums, % skolēnu", x: { min: 0, max: 75, step: 10 }, rows: kat.map(k => ({ nos: katNos(k), items: grupas.map(([g, c]) => { const x = G.kategorijas.find(y => y.grupa === g && y.kat === k); return x ? { v: x.dala, lo: x.dala - 1.96 * x.se, hi: x.dala + 1.96 * x.se, color: c, r: 4, tip: `${x.nos}: ${katNos(k)}`, extra: x.lasisana ? `vidējā lasīšana šajā grupā ${f1(x.lasisana, 0)} punkti` : "" } : null; }).filter(Boolean) })) },
    ];
    PZ.dots(root, { w: 1100, cols: 2, labW: 215, rowH: 24, title: "Garu tekstu lasīšana skolā, PISA 2018", panels, x: { min: 0, max: 85, step: 10, fmt: v => f1(v, 0) },
      legend: [{ color: C.bad, nos: "Latvija (kreisajā panelī visas trīs rindas; labajā latviešu valodas plūsma)" }, { color: C.s1, nos: "salīdzinājuma valstis (labajā panelī 7 valstu vidējais)" }, { color: C.k, nos: "Igaunija" }, { color: C.c, nos: "Somija" }],
      piezime: "PISA 2018 skolēnu anketas jautājums: cik lappušu garš bija garākais teksts, kas šajā mācību gadā bija jālasa dzimtās valodas stundās (ST154Q01HA; 2022. un 2025. gadā nav uzdots). Latvijas plūsmas pēc mājās runātās valodas. 7 valstu vidējais ir valstu daļu vienkāršais vidējais. Avots: OECD PISA 2018 mikrodati; R/pisa2025/86b, docs/58 papildinājums." });
  }

  // 16. Politikas lēmumi un lasīšanas rindas: Latvija, Igaunija, Somija
  function politika(root, D) {
    const P = D.politika; const VN = { LVA: "Latvija", EST: "Igaunija", FIN: "Somija" }; const VC = { LVA: C.bad, EST: C.s2, FIN: C.c };
    const ROW = { LVA: 412, EST: 402, FIN: 392 };
    const KAT = { reforma: "reforma vai lēmums", merijums: "mērījums", sokss: "ārējs notikums", tehnologija: "tehnoloģija" };
    const ev = v => ({ id: "ev" + v, nos: `${VN[v]}: lēmumi un notikumi`, color: VC[v], marker: "diamond", layer: "ev" + v, width: 0, opacity: 0, fmt: () => "",
      pts: P.notikumi.filter(n => n.valsts === v && n.kategorija !== "merijums").map(n => ({ x: n.gads, y: ROW[v], label: `${Math.floor(n.gads)}: ${n.nos} (${KAT[n.kategorija] || n.kategorija}). ${n.apraksts}` })) });
    const mer = { id: "mer", nos: "Mērījumi: kad notika mērījums vai mainījās tā forma", color: C.ink2, marker: "open", layer: "mer", width: 0, opacity: 0, fmt: () => "",
      pts: P.notikumi.filter(n => n.kategorija === "merijums").map(n => ({ x: n.gads, y: 382, label: `${Math.floor(n.gads)}: ${n.nos}. ${n.apraksts}` })) };
    PZ.line(root, { w: 1100, h: 560, m: { l: 56, r: 150, t: 18, b: 44 }, title: "Politikas lēmumi un lasīšanas rindas",
      x: { min: 1999, max: 2026.5, ticks: [2000, 2003, 2006, 2009, 2012, 2015, 2018, 2022, 2025], tipLabel: "gads" }, y: { min: 375, max: 560, step: 20, label: "PISA lasīšana, punkti (notikumi apakšā: rombi pa valstīm)", fmt: v => v < 420 ? "" : f1(v, 0) },
      layers: [{ id: "lva", nos: "Latvija: lasīšanas rinda" }, { id: "est", nos: "Igaunija: lasīšanas rinda" }, { id: "fin", nos: "Somija: lasīšanas rinda" }, { id: "evLVA", nos: "Latvijas notikumi", on: false }, { id: "evEST", nos: "Igaunijas notikumi", on: false }, { id: "evFIN", nos: "Somijas notikumi", on: false }, { id: "mer", nos: "Mērījumi", on: false }],
      series: [
        { id: "LVA A", nos: "Latvija, latviešu valodas plūsma", color: C.bad, width: 2.8, layer: "lva", pts: P.rindas["LVA A"], endLabel: "latviešu valodas plūsma" },
        { id: "LVA", nos: "Latvija, visi skolēni", color: C.bad, width: 1.4, dash: "4,3", layer: "lva", pts: P.rindas.LVA, endLabel: "Latvija, visi" },
        { id: "EST", nos: "Igaunija", color: C.s2, width: 2.2, layer: "est", pts: P.rindas.EST, endLabel: "Igaunija" },
        { id: "FIN", nos: "Somija", color: C.c, width: 2.2, layer: "fin", pts: P.rindas.FIN, endLabel: "Somija" },
        ev("LVA"), ev("EST"), ev("FIN"), mer],
      hlines: [{ y: 420, color: "var(--grid)" }],
      texts: [{ x: 1999.2, y: 414, t: "Latvija", color: C.bad, cls: "small", layer: "evLVA" }, { x: 1999.2, y: 404, t: "Igaunija", color: C.s2, cls: "small", layer: "evEST" }, { x: 1999.2, y: 394, t: "Somija", color: C.c, cls: "small", layer: "evFIN" }, { x: 1999.2, y: 384, t: "Mērījumi", color: C.ink2, cls: "small", layer: "mer" }],
      piezime: "Notikumu rombi: kursors uz tā paša gada rāda notikuma aprakstu. Mērījumi (PISA cikli, to formas maiņas, diagnosticējošo darbu ieviešana un atcelšana) ir atsevišķā slānī, jo tie nav lēmumi, kas varētu ietekmēt rezultātu. Latvijas notikumi no docs/45 hronoloģijas (data/politika/notikumi_laika_josla.csv), Igaunijas un Somijas no data/politika/notikumi_est_fin.csv ar avotiem (Riigi Teataja, Opetushallitus, Eurydice). Lēmuma datums un tā ietekme uz skolēniem ir dažādas lietas: lēmums skar tās paaudzes, kas tobrīd ir skolā, un PISA to redz pēc gadiem. Avots: OECD PISA valstu vidējie; docs/46 Slānis 17." });
  }


  // 21. Rīgas skolu izkliede 9. klases eksāmenā (9. sadaļa)
  function skolu_ce(root, D) {
    const ALLD = D.skolu_ce; let eks = "LAT", mg = "2024"; const UN = { 1: "Rīga", 2: "valstspilsētas", 3: "pilsētas", 4: "lauki" }, TN = { 1: "valsts ģimnāzija", 2: "vidusskola", 3: "pamatskola", 0: "cits tips" }; const esc = s => String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/"/g, "&quot;");
    root.innerHTML = ""; const top = el("div", { class: "pz-tools" }); const host = el("div"); root.append(top, host);
    const ROWS = [["Rīga: valsts ģimnāzijas", d => d.u === 1 && d.t === 1], ["Rīga: vidusskolas", d => d.u === 1 && d.t === 2], ["Rīga: pamatskolas", d => d.u === 1 && d.t === 3],
      ["Rīga: visas skolas", d => d.u === 1], ["valstspilsētas", d => d.u === 2], ["pilsētas", d => d.u === 3], ["lauki", d => d.u === 4]];
    const q = (a, p) => { const b = a.slice().sort((x, y) => x - y); return b[Math.min(b.length - 1, Math.floor(p * (b.length - 1)))]; };
    const hash = s => { let h = 0; for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) % 1000; return h / 1000; };
    function draw() {
      const D = ALLD.filter(d => d.e === eks && d.m === mg); const W = 1100, rowH = 56, m = { l: 190, r: 40, t: 34, b: 44 }, H = m.t + ROWS.length * rowH + m.b;
      const xs = v => m.l + (v / 100) * (W - m.l - m.r);
      let s = `<svg viewBox="0 0 ${W} ${H}" width="100%" style="display:block;font-family:inherit" role="img" aria-label="Skolu vidējie 9. klases eksāmenā">`;
      for (let v = 0; v <= 100; v += 10) s += `<line x1="${xs(v)}" x2="${xs(v)}" y1="${m.t - 6}" y2="${H - m.b + 4}" class="grid"/><text x="${xs(v)}" y="${H - m.b + 20}" class="tick" text-anchor="middle">${v}</text>`;
      s += `<text x="${(m.l + W - m.r) / 2}" y="${H - 6}" class="alab" text-anchor="middle">skolas vidējais kopvērtējums, % no maksimālā</text>`;
      ROWS.forEach(([nos, f], i) => {
        const g = D.filter(f); const y0 = m.t + i * rowH; const yc = y0 + rowH / 2; const vals = g.map(d => d.v);
        if (i) s += `<line x1="${m.l}" x2="${W - m.r}" y1="${y0}" y2="${y0}" class="grid" opacity=".5"/>`;
        s += `<text x="${m.l - 10}" y="${yc - 4}" class="small" fill="var(--ink)" text-anchor="end" font-weight="${nos.startsWith("Rīga: visas") ? 700 : 500}">${esc(nos)}</text><text x="${m.l - 10}" y="${yc + 12}" class="small" fill="var(--muted)" text-anchor="end">${g.length} skolas</text>`;
        if (!g.length) return;
        const p10 = q(vals, .1), p50 = q(vals, .5), p90 = q(vals, .9);
        s += `<rect x="${xs(p10)}" y="${yc - 14}" width="${xs(p90) - xs(p10)}" height="28" fill="var(--s1)" opacity=".10"/>`;
        g.forEach(d => { const jy = yc + (hash(d.s + d.n + d.v) - .5) * 24; const c = d.t === 1 ? C.s1 : d.t === 3 ? C.s2 : d.u === 1 ? C.c : "var(--ink-2)";
          s += `<circle cx="${xs(d.v)}" cy="${jy}" r="3.6" fill="${c}" opacity=".55" stroke="var(--card)" stroke-width=".6"><title>${d.s ? esc(d.s) : "skola ārpus Rīgas (" + UN[d.u] + ")"}; ${TN[d.t]}; ${d.n} kārtotāji; ${f1(d.v, 1)} %</title></circle>`; });
        s += `<line x1="${xs(p50)}" x2="${xs(p50)}" y1="${yc - 17}" y2="${yc + 17}" stroke="var(--ink)" stroke-width="2.2"/>`;
        s += `<text x="${xs(p90) + 6}" y="${yc - 18}" class="small" fill="var(--muted)">P10 ${f1(p10, 0)}, mediāna ${f1(p50, 0)}, P90 ${f1(p90, 0)}</text>`;
      });
      s += "</svg>"; host.innerHTML = s;
    }
    top.append(el("span", { class: "pz-lab" }, "Eksāmens:"), PZ.sel([{ v: "LAT", t: "latviešu valoda 9. klasei" }, { v: "MAT", t: "matemātika 9. klasei" }], eks, v => { eks = v; draw(); }),
      PZ.sel([{ v: "2022", t: "2022./23." }, { v: "2023", t: "2023./24." }, { v: "2024", t: "2024./25." }], mg, v => { mg = v; draw(); }));
    draw();
    root.append(el("div", { class: "pz-legend" }), el("p", { class: "pz-note" }, "Katrs punkts ir viena skola ar vismaz 10 kārtotājiem (pele uz punkta rāda Rīgas skolas nosaukumu); melnā svītra ir skolu mediāna, zilā josla no zemākās desmitdaļas skolu līdz augšējai (P10 līdz P90). Krāsas: zils valsts ģimnāzijas, oranžs pamatskolas, violets pārējās Rīgas skolas, pelēks pārējās. Avots: VIAA kārtotāju līmeņa dati, R/pisa2025/89, docs/92."));
    const leg = root.querySelector(".pz-legend"); [[C.s1, "valsts ģimnāzija"], [C.s2, "pamatskola"], [C.c, "cita Rīgas skola"], ["var(--ink-2)", "cita skola ārpus Rīgas"]].forEach(([c, n]) => { const it = el("span"); it.append(legKeyDot(c), document.createTextNode(n)); leg.append(it); });
  }
  function legKeyDot(c) { const sp = el("span"); sp.setAttribute("style", `display:inline-block;width:10px;height:10px;border-radius:50%;background:${c};margin-right:6px;vertical-align:-1px`); return sp; }

  // 20. Skolu faktors: apdzīvotā vieta (9. sadaļa)
  function skolas(root, D) {
    const S = D.skolas; const UN = ["ciems, līdz 3 tūkst.", "mazpilsēta, 3 līdz 15 tūkst.", "pilsēta, 15 līdz 100 tūkst.", "Rīga"]; const UC = [C.k, C.s2, C.c, C.s1];
    const ser = (g, layer, dash, suf) => [1, 2, 3, 4].map(u => ({ id: `${g}_u${u}`, nos: `${suf}: ${UN[u - 1]}`, color: UC[u - 1], width: dash ? 1.8 : 2.4, dash, marker: dash ? "square" : undefined, band: !dash, layer,
      pts: S[g].filter(r => r[`u${u}`] !== null && r[`u${u}`] !== undefined).map(r => ({ x: r.gads, y: r[`u${u}`], lo: r[`u${u}`] - 1.96 * r[`u${u}_se`], hi: r[`u${u}`] + 1.96 * r[`u${u}_se`] })), fmt: v => f1(v, 0), endLabel: dash ? undefined : UN[u - 1] }));
    PZ.line(root, { w: 1100, h: 500, m: { l: 60, r: 190, t: 18, b: 44 }, title: "Lasīšana pēc skolas apdzīvotās vietas",
      x: { min: 2002, max: 2026, ticks: PISA_CIKLI, tipLabel: "PISA cikls" }, y: { min: 340, max: 540, step: 20, label: "Lasītprasme, PISA punkti" },
      layers: [{ id: "lva", nos: "latviešu valodas plūsma" }, { id: "visi", nos: "Latvija, visi skolēni", on: false }],
      series: [...ser("LVA A", "lva", null, "latviešu valodas plūsma"), ...ser("LVA", "visi", "7,4", "Latvija, visi")],
      piezime: "Apdzīvotās vietas tips no PISA skolu anketas (skolas atrašanās vieta); Latvijā virs 100 tūkst. iedzīvotāju ir tikai Rīga. Joslas: 95 % intervāls (skolu klasteru sāknēšana). Avots: R/pisa2025/89, docs/92." });
  }

  // 19. ESCS ceturtdaļas (9. sadaļa)
  function escs(root, D) {
    const E = D.escs; const QN = ["zemākā ceturtdaļa", "2. ceturtdaļa", "3. ceturtdaļa", "augstākā ceturtdaļa"]; const QC = [C.bad, C.s2, C.k, C.s1];
    const ser = (g, layer, dash, suf) => [1, 2, 3, 4].map(q => ({ id: `${g}_Q${q}`, nos: `${suf}: ${QN[q - 1]}`, color: QC[q - 1], width: dash ? 1.8 : 2.4, dash, marker: dash ? "square" : undefined, band: !dash, layer,
      pts: E[g].map(r => ({ x: r.gads, y: r[`Q${q}`], lo: r[`Q${q}`] - 1.96 * r[`Q${q}_se`], hi: r[`Q${q}`] + 1.96 * r[`Q${q}_se`] })), fmt: v => f1(v, 0), endLabel: dash ? undefined : QN[q - 1] }));
    PZ.line(root, { w: 1100, h: 500, m: { l: 60, r: 175, t: 18, b: 44 }, title: "Lasīšana pa sociālā fona ceturtdaļām",
      x: { min: 2002, max: 2026, ticks: PISA_CIKLI, tipLabel: "PISA cikls" }, y: { min: 400, max: 570, step: 20, label: "Lasītprasme, PISA punkti" },
      layers: [{ id: "lva", nos: "latviešu valodas plūsma" }, { id: "v7", nos: "7 valstu vidējais", on: false }, { id: "visi", nos: "Latvija, visi skolēni", on: false }],
      series: [...ser("LVA A", "lva", null, "latviešu valodas plūsma"), ...ser("7 valstis", "v7", "2,4", "7 valstis"), ...ser("LVA", "visi", "7,4", "Latvija, visi")],
      piezime: "Ceturtdaļas pēc PISA sociāli ekonomiskā un kultūras statusa indeksa (ESCS) katras grupas un cikla iekšienē; 7 valstīm katras valsts ceturtdaļu vidējo vienkāršais vidējais. Joslas: 95 % intervāls (skolu klasteru sāknēšana). 2025. gadā bez ESCS ir 8 % latviešu valodas plūsmas skolēnu, tie lasa vājāk. Avots: R/pisa2025/87, docs/90." });
  }

  // 18. Mērīšanas, prognozes un lēmumu cikls (19. sadaļa)
  function cikls(root) {
    root.innerHTML = ""; const on = { prog: false, parb: false };
    const top = el("div", { class: "pz-tools" }); const host = el("div"); root.append(top, host);
    const B = [
      { id: 1, x: 30, y: 60, t: "1. Pareiza mērīšana", l: ["regulāri, visa populācija", "psihometriski saskaņoti starp gadiem", "4., 6., 9. klase, ne tikai 15 gadi"] },
      { id: 2, x: 300, y: 60, t: "2. Labi dati", l: ["salīdzināmi laikā un starp avotiem", "savienojami ar skolu un paaudzi", "pieejami pētniekiem bez kavēšanās"] },
      { id: 3, x: 570, y: 60, t: "3. Prognoze", l: ["kā mainīsies nākamā paaudze", "ar nenoteiktības koridoru", "4 līdz 6 gadi pirms PISA"] },
      { id: 4, x: 840, y: 60, t: "4. Lēmums par intervenci", l: ["ko, kur un kad mainīt", "ar iepriekš pateiktu gaidāmo efektu", "un ar termiņu, kad to pārbaudīt"] },
      { id: 5, x: 840, y: 330, t: "5. Reālie rezultāti", l: ["prospektīvi: jauni mērījumi pēc lēmuma", "retrospektīvi: jau zināmie dati", "tā pati skala, tā pati paaudze"] },
      { id: 6, x: 300, y: 330, t: "6. Prognozes pārbaude", l: ["prognoze pret faktu: cik liela kļūda", "fakts mīnus prognoze bez intervences", "= intervences novērtētais efekts"] }];
    const W = 1100, H = 470, bw = 230, bh = 96;
    function box(b) {
      const lines = b.l.map((s, i) => `<text x="${b.x + 12}" y="${b.y + 46 + i * 16}" class="small" fill="var(--ink-2)">${s}</text>`).join("");
      return `<rect x="${b.x}" y="${b.y}" width="${bw}" height="${bh}" rx="8" fill="var(--card)" stroke="var(--grid)"/><text x="${b.x + 12}" y="${b.y + 24}" font-weight="700" font-size="13.5" fill="var(--ink)">${b.t}</text>${lines}`;
    }
    const arrow = (d, color, dash, w) => `<path d="${d}" fill="none" stroke="${color}" stroke-width="${w || 1.8}" ${dash ? `stroke-dasharray="${dash}"` : ""} marker-end="url(#${color === "var(--s1)" ? "mS" : color === "var(--good)" ? "mG" : "mI"})"/>`;
    const lab = (x, y, s, color, anchor) => `<text x="${x}" y="${y}" font-size="12" font-weight="600" fill="${color}" text-anchor="${anchor || "middle"}">${s}</text>`;
    function draw() {
      let s = `<svg viewBox="0 0 ${W} ${H}" width="100%" style="display:block;font-family:inherit" role="img" aria-label="Mērīšanas, prognozes un lēmumu cikls">
        <defs>${[["mI", "var(--ink-2)"], ["mS", "var(--s1)"], ["mG", "var(--good)"]].map(([id, c]) => `<marker id="${id}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="8" markerHeight="8" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="${c}"/></marker>`).join("")}</defs>`;
      // pamata cikls
      const ink = "var(--ink-2)";
      s += arrow("M260,108 L298,108", ink) + arrow("M530,108 L568,108", ink) + arrow("M800,108 L838,108", ink);
      s += arrow("M955,156 L955,328", ink) + arrow("M838,378 L532,378", ink) + arrow("M300,378 L145,378 L145,158", ink);
      s += lab(145, 250, "nākamais cikls: mēra atkal", ink, "middle");
      // uz priekšu
      if (on.prog) {
        s += arrow("M260,100 L298,100", "var(--s1)", null, 3) + arrow("M530,100 L568,100", "var(--s1)", null, 3) + arrow("M800,100 L838,100", "var(--s1)", null, 3);
        s += `<rect x="296" y="22" width="548" height="26" rx="13" fill="var(--s1-band)"/>` + lab(570, 40, "uz priekšu: no šodienas datiem par nākamo paaudzi, pirms lēmuma un pirms PISA", "var(--s1)");
      }
      // atpakaļ
      if (on.parb) {
        s += arrow("M415,330 C 500,250 600,220 680,160", "var(--good)", "6,4", 2.4);
        s += arrow("M530,340 C 640,280 760,230 838,150", "var(--good)", "6,4", 2.4);
        s += arrow("M300,350 C 200,300 150,250 145,160", "var(--good)", "6,4", 2.4);
        s += `<rect x="330" y="436" width="520" height="26" rx="13" fill="var(--good-band)"/>` + lab(590, 454, "atpakaļ: kļūda labo modeli, parāda intervences reālo efektu un to, kas jāmēra precīzāk", "var(--good)");
        s += lab(540, 236, "modeļa labojums", "var(--good)", "middle") + lab(730, 210, "intervences efekts", "var(--good)", "middle") + lab(236, 300, "ko mērīt precīzāk", "var(--good)", "end");
      }
      s += B.map(box).join("") + "</svg>";
      host.innerHTML = s;
    }
    top.append(el("span", { class: "pz-lab" }, "Datu slāņi:"), PZ.chip("cikls", true, () => {}, "Pamata cikls vienmēr redzams"),
      PZ.chip("uz priekšu: prognoze", on.prog, v => { on.prog = v; draw(); }), PZ.chip("atpakaļ: pārbaude", on.parb, v => { on.parb = v; draw(); }));
    draw();
    root.append(el("p", { class: "pz-note" }, "Cikls atkārtojas ar katru mērījumu kārtu. Prognoze uz priekšu dod laiku lēmumam; pārbaude atpakaļ pret prospektīvajiem un retrospektīvajiem datiem labo gan modeli, gan secinājumus par intervenču efektiem. Šajā ziņojumā 2025. gada rezultāts ir šāda pārbaude: paaudžu modeļa prognoze −4,3 pret faktisko −4,7."));
  }

  const ALL = { skolu_ce, skolas, escs, cikls, politika, garie, korigeta, lexis, panelis, efekti, dfm, ce, dd, talis, izdevumi_rinda, izdevumi_atlikumi, hbsc_exante, hbsc_rinda, aploksne, atgriesanas, koridors, dalas };
  // kompakto datu atpakošana (faila apjoma dēļ dati glabāti kā masīvi)
  function atpakot(D) {
    const obj = (k, a) => { const o = {}; k.forEach((n, i) => { o[n] = a[i]; }); return o; };
    if (D.aploksne && D.aploksne.punkti_k) { const V = D.aploksne.punkti_valstis; D.aploksne.punkti = D.aploksne.punkti.map(a => { const o = obj(D.aploksne.punkti_k, a); o.valsts = V[o.valsts]; o.m = !!o.m; return o; }); delete D.aploksne.punkti_k; }
    if (D.skolu_ce && D.skolu_ce.k) D.skolu_ce = D.skolu_ce.r.map(a => obj(D.skolu_ce.k, a));
  }
  function init(D) { atpakot(D); document.querySelectorAll("[data-chart]").forEach(root => { const f = ALL[root.dataset.chart]; if (!f) { root.textContent = "Grafiks nav atrasts: " + root.dataset.chart; return; } try { f(root, D); } catch (e) { root.textContent = "Grafika kļūda: " + e.message; console.error(root.dataset.chart, e); } }); }
  return { init, ALL, C, NOS, f1, fs, PISA_CIKLI };
})();
