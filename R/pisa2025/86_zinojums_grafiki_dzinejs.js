/* Grafiku dzinējs ziņojuma lapai: tīrs SVG un JavaScript, bez bibliotēkām.
   PZ.line(el, spec)    līniju grafiks ar joslām, slāņiem, krustiņa rīklodziņu un tabulu
   PZ.dots(el, spec)    horizontāls punktu grafiks ar intervāliem, paneļiem un tabulu
   PZ.scatter(el, spec) izkliedes grafiks ar līnijām, izceltiem punktiem un tuvākā punkta rīklodziņu
   Krāsas: CSS mainīgie (--s1 zils, --s2 oranžs, --k zaļš, --c violets, --bad sarkans, --ref pelēks). */
const PZ = (() => {
  const f1 = (x, d = 1) => x === null || x === undefined || Number.isNaN(x) ? "" : (Math.round(x * 10 ** d) / 10 ** d).toLocaleString("lv-LV", { minimumFractionDigits: d, maximumFractionDigits: d });
  const fs = (x, d = 1) => (x > 0 ? "+" : "") + f1(x, d);
  const esc = s => String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/"/g, "&quot;");
  const el = (tag, attrs = {}, text) => { const e = document.createElement(tag); for (const k in attrs) e.setAttribute(k, attrs[k]); if (text !== undefined) e.textContent = text; return e; };

  // Kopīgais rāmis: rīku rinda (slāņi, filtri, tabula), SVG, rīklodziņš, tabula
  function frame(root, spec) {
    root.innerHTML = "";
    root.classList.add("pz");
    const tools = el("div", { class: "pz-tools" });
    const wrap = el("div", { class: "pz-wrap" });
    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("viewBox", `0 0 ${spec.w} ${spec.h}`);
    svg.setAttribute("role", "img");
    svg.setAttribute("aria-label", spec.title || "");
    const tip = el("div", { class: "pz-tip", hidden: "" });
    const tbl = el("div", { class: "pz-table", hidden: "" });
    wrap.append(svg, tip);
    root.append(tools, wrap, tbl);
    if (spec.piezime) root.append(el("p", { class: "pz-note" }, spec.piezime));
    return { tools, svg, tip, tbl, wrap };
  }
  function tipShow(F, html, ev) {
    F.tip.innerHTML = "";
    html.forEach(row => { const d = el("div"); if (row.c) { const k = el("i"); k.style.background = row.c; d.append(k); } const b = el("b", {}, row.v); d.append(b, document.createTextNode(" " + row.t)); F.tip.append(d); });
    F.tip.hidden = false;
    const r = F.wrap.getBoundingClientRect();
    const x = ev.clientX - r.left, y = ev.clientY - r.top;
    F.tip.style.left = Math.min(x + 14, r.width - F.tip.offsetWidth - 6) + "px";
    F.tip.style.top = (y + 14 > r.height - F.tip.offsetHeight ? y - F.tip.offsetHeight - 10 : y + 14) + "px";
  }
  function tipHide(F) { F.tip.hidden = true; }
  function chip(label, on, cb, title) {
    const b = el("button", { type: "button", class: "pz-chip", "aria-pressed": on ? "true" : "false" }, label);
    if (title) b.title = title;
    b.addEventListener("click", () => { const now = b.getAttribute("aria-pressed") !== "true"; b.setAttribute("aria-pressed", now ? "true" : "false"); cb(now, b); });
    return b;
  }
  function tableToggle(F, build) {
    const b = el("button", { type: "button", class: "pz-chip pz-tbl", "aria-pressed": "false" }, "Tabula ar vērtībām");
    b.addEventListener("click", () => { const on = F.tbl.hidden; b.setAttribute("aria-pressed", on ? "true" : "false"); if (on) { F.tbl.innerHTML = ""; F.tbl.append(build()); } F.tbl.hidden = !on; });
    return b;
  }
  function table(head, rows) {
    const t = el("table"); const th = el("tr"); head.forEach((h, i) => th.append(el("th", i ? { class: "n" } : {}, h))); const thead = el("thead"); thead.append(th); t.append(thead);
    const tb = el("tbody"); rows.forEach(r => { const tr = el("tr"); r.forEach((c, i) => tr.append(el("td", i ? { class: "n" } : {}, c === null || c === undefined ? "" : c))); tb.append(tr); }); t.append(tb); return t;
  }
  // Otra laika ass augšā: x2 = { label, ticks: [datu x vērtības], fmt: v => uzraksts, chip }
  function x2axis(x2, xs, m, W) {
    let s = `<line x1="${m.l}" x2="${W - m.r}" y1="${m.t}" y2="${m.t}" class="axis"/>`;
    x2.ticks.forEach(v => { s += `<line x1="${xs(v)}" x2="${xs(v)}" y1="${m.t - 4}" y2="${m.t}" class="axis"/><text x="${xs(v)}" y="${m.t - 8}" class="tick" text-anchor="middle">${esc(x2.fmt(v))}</text>`; });
    if (x2.label) s += `<text x="${(m.l + W - m.r) / 2}" y="${m.t - 26}" class="alab" text-anchor="middle">${esc(x2.label)}</text>`;
    return s;
  }
  // Leģendas atslēga: punkts, nepārtraukta, raustīta vai punktēta līnija sērijas krāsā
  function legKey(t) {
    const k = el("i");
    if (t.pt || (!t.dash && t.marker === "only")) { k.className = "pt"; k.style.background = t.color; return k; }
    const stils = t.dash === "solid" || !t.dash ? "solid" : t.dash === true ? "dashed" : t.dash === "dotted" || /^2,/.test(String(t.dash)) ? "dotted" : "dashed";
    k.className = "ln"; k.style.borderTop = `3px ${stils} ${t.color}`; k.style.background = "none"; k.style.height = "0";
    return k;
  }
  const ticksOf = (min, max, step) => { const t = []; for (let v = Math.ceil(min / step) * step; v <= max + 1e-9; v += step) t.push(Math.round(v * 1000) / 1000); return t; };
  const sel = (opts, value, cb) => { const s = el("select", { class: "pz-sel" }); opts.forEach(o => { const op = el("option", { value: o.v }, o.t); if (o.v === value) op.selected = true; s.append(op); }); s.addEventListener("change", () => cb(s.value)); return s; };

  // ---------------------------------------------------------------------
  // Līniju grafiks
  // spec: w,h,m, x:{min,max,step,fmt,label}, y:{min,max,step,label,fmt}, series:[{id,nos,color,dash,width,pts:[{x,y,lo,hi,label}],band,layer,marker,endLabel,opacity}],
  //       layers:[{id,nos,on}], vlines:[{x,nos,color,dash,layer}], hlines:[{y,nos,color,dash}], rects:[{x0,x1,y0,y1,fill,nos}], texts:[{x,y,t,anchor,color,layer}]
  // ---------------------------------------------------------------------
  function line(root, spec) {
    const F = frame(root, spec);
    const m = Object.assign({}, spec.m || { l: 56, r: 150, t: 18, b: 44 });
    if (spec.x2) m.t = Math.max(m.t, 44);
    const W = spec.w, H = spec.h;
    const xs = v => m.l + (v - spec.x.min) / (spec.x.max - spec.x.min) * (W - m.l - m.r);
    const ys = v => H - m.b - (v - spec.y.min) / (spec.y.max - spec.y.min) * (H - m.t - m.b);
    const fx = spec.x.fmt || (v => String(v)), fy = spec.y.fmt || (v => f1(v, 0));
    const layers = spec.layers || [];
    const state = { layers: Object.fromEntries(layers.map(l => [l.id, l.on !== false])), x2: spec.x2 ? spec.x2.on !== false : false };
    const visible = s => !s.layer || state.layers[s.layer] !== false;
    const sx = [...new Set(spec.series.flatMap(s => s.pts.map(p => p.x)))].sort((a, b) => a - b);

    function draw() {
      let s = "";
      // režģis
      ticksOf(spec.y.min, spec.y.max, spec.y.step).forEach(v => { s += `<line x1="${m.l}" x2="${W - m.r}" y1="${ys(v)}" y2="${ys(v)}" class="grid"/><text x="${m.l - 8}" y="${ys(v) + 4}" class="tick" text-anchor="end">${esc(fy(v))}</text>`; });
      (spec.x.ticks || ticksOf(spec.x.min, spec.x.max, spec.x.step)).forEach(v => { s += `<text x="${xs(v)}" y="${H - m.b + 18}" class="tick" text-anchor="middle">${esc(fx(v))}</text>`; });
      s += `<line x1="${m.l}" x2="${W - m.r}" y1="${H - m.b}" y2="${H - m.b}" class="axis"/>`;
      if (spec.x2 && state.x2) { s += x2axis(spec.x2, xs, m, W); }
      if (spec.y.label) s += `<text transform="translate(14,${(m.t + H - m.b) / 2}) rotate(-90)" class="alab" text-anchor="middle">${esc(spec.y.label)}</text>`;
      if (spec.x.label) s += `<text x="${(m.l + W - m.r) / 2}" y="${H - 6}" class="alab" text-anchor="middle">${esc(spec.x.label)}</text>`;
      (spec.rects || []).forEach(q => { if (q.layer && !state.layers[q.layer]) return; s += `<rect x="${xs(q.x0)}" y="${ys(q.y1)}" width="${xs(q.x1) - xs(q.x0)}" height="${ys(q.y0) - ys(q.y1)}" fill="${q.fill}" opacity="${q.opacity ?? 0.12}"/>`; if (q.nos) s += `<text x="${xs(q.x0) + 4}" y="${ys(q.y1) + 12}" class="small">${esc(q.nos)}</text>`; });
      (spec.hlines || []).forEach(q => { s += `<line x1="${m.l}" x2="${W - m.r}" y1="${ys(q.y)}" y2="${ys(q.y)}" stroke="${q.color || "var(--ink-2)"}" stroke-width="${q.width || 1}" ${q.dash ? `stroke-dasharray="${q.dash}"` : ""}/>`; if (q.nos) s += `<text x="${W - m.r + 6}" y="${ys(q.y) + 4}" class="small" fill="${q.color || "var(--ink-2)"}">${esc(q.nos)}</text>`; });
      (spec.vlines || []).forEach(q => { if (q.layer && !state.layers[q.layer]) return; s += `<line x1="${xs(q.x)}" x2="${xs(q.x)}" y1="${m.t}" y2="${H - m.b}" stroke="${q.color || "var(--muted)"}" stroke-width="${q.width || 1}" ${q.dash ? `stroke-dasharray="${q.dash}"` : ""}/>`; if (q.nos) s += `<text transform="translate(${xs(q.x) - 4},${m.t + 4}) rotate(90)" class="small" fill="${q.color || "var(--muted)"}">${esc(q.nos)}</text>`; });
      // joslas, tad līnijas, tad punkti
      const vis = spec.series.filter(visible);
      vis.forEach(sr => { if (!sr.band) return; const p = sr.pts.filter(q => q.lo !== null && q.lo !== undefined); if (p.length < 2) return; s += `<path d="M${p.map(q => xs(q.x) + "," + ys(q.hi)).join("L")}L${p.slice().reverse().map(q => xs(q.x) + "," + ys(q.lo)).join("L")}Z" fill="${sr.color}" opacity="${sr.bandOpacity ?? 0.13}"/>`; });
      vis.forEach(sr => { const p = sr.pts.filter(q => q.y !== null && q.y !== undefined); if (p.length > 1 && sr.marker !== "only") s += `<path d="M${p.map(q => xs(q.x) + "," + ys(q.y)).join("L")}" fill="none" stroke="${sr.color}" stroke-width="${sr.width || 2}" ${sr.dash ? `stroke-dasharray="${sr.dash}"` : ""} opacity="${sr.opacity ?? 1}" stroke-linejoin="round"/>`; });
      vis.forEach(sr => { if (sr.marker === "none") return; const p = sr.pts.filter(q => q.y !== null && q.y !== undefined); p.forEach(q => { const sh = sr.marker || "circle"; const c = sr.color, x = xs(q.x), y = ys(q.y); if (sh === "square") s += `<rect x="${x - 4.5}" y="${y - 4.5}" width="9" height="9" fill="${c}" stroke="var(--card)" stroke-width="1.5"/>`; else if (sh === "cross") s += `<path d="M${x - 5},${y - 5}L${x + 5},${y + 5}M${x - 5},${y + 5}L${x + 5},${y - 5}" stroke="${c}" stroke-width="2.2"/>`; else if (sh === "open") s += `<circle cx="${x}" cy="${y}" r="4.5" fill="var(--card)" stroke="${c}" stroke-width="2"/>`; else if (sh === "diamond") s += `<polygon points="${x},${y - 6} ${x + 6},${y} ${x},${y + 6} ${x - 6},${y}" fill="${c}" stroke="var(--card)" stroke-width="1.5"/>`; else s += `<circle cx="${x}" cy="${y}" r="${sr.r || 4}" fill="${c}" stroke="var(--card)" stroke-width="1.5" opacity="${sr.opacity ?? 1}"/>`; }); });
      // tiešie paraksti līniju galos
      const labs = [];
      vis.forEach(sr => { if (!sr.endLabel) return; const p = sr.pts.filter(q => q.y !== null && q.y !== undefined); if (!p.length) return; const q = p[p.length - 1]; labs.push({ x: xs(q.x) + 8, y: ys(q.y), t: sr.endLabel === true ? sr.nos : sr.endLabel, c: sr.color }); });
      labs.sort((a, b) => a.y - b.y); for (let i = 1; i < labs.length; i++) if (labs[i].y - labs[i - 1].y < 14) labs[i].y = labs[i - 1].y + 14;
      labs.forEach(l => { s += `<text x="${l.x}" y="${l.y + 4}" class="elab" fill="${l.c}">${esc(l.t)}</text>`; });
      (spec.texts || []).forEach(t => { if (t.layer && !state.layers[t.layer]) return; s += `<text x="${xs(t.x)}" y="${ys(t.y)}" class="${t.cls || "small"}" text-anchor="${t.anchor || "start"}" fill="${t.color || "var(--ink-2)"}">${esc(t.t)}</text>`; });
      s += `<line id="pzx" x1="0" x2="0" y1="${m.t}" y2="${H - m.b}" class="xhair" visibility="hidden"/>`;
      F.svg.innerHTML = s;
    }
    // krustiņš un rīklodziņš
    F.svg.addEventListener("mousemove", ev => {
      const pt = F.svg.createSVGPoint(); pt.x = ev.clientX; pt.y = ev.clientY; const p = pt.matrixTransform(F.svg.getScreenCTM().inverse());
      if (p.x < m.l || p.x > W - m.r) { tipHide(F); return; }
      const vx = spec.x.min + (p.x - m.l) / (W - m.l - m.r) * (spec.x.max - spec.x.min);
      let best = null; sx.forEach(x => { if (best === null || Math.abs(x - vx) < Math.abs(best - vx)) best = x; });
      const xh = F.svg.querySelector("#pzx"); xh.setAttribute("x1", xs(best)); xh.setAttribute("x2", xs(best)); xh.setAttribute("visibility", "visible");
      const rows = [{ v: fx(best), t: spec.x.tipLabel || "" }];
      spec.series.filter(visible).forEach(sr => { const q = sr.pts.find(q => q.x === best); if (!q || q.y === null || q.y === undefined) return; rows.push({ c: sr.color, v: (sr.fmt || fy)(q.y) + (q.lo !== undefined && q.lo !== null && sr.band ? ` (${(sr.fmt || fy)(q.lo)} līdz ${(sr.fmt || fy)(q.hi)})` : ""), t: q.label || sr.nos }); });
      tipShow(F, rows, ev);
    });
    F.svg.addEventListener("mouseleave", () => { tipHide(F); const xh = F.svg.querySelector("#pzx"); if (xh) xh.setAttribute("visibility", "hidden"); });
    // rīki: slāņi
    if (layers.length) {
      F.tools.append(el("span", { class: "pz-lab" }, "Datu slāņi:"));
      const chips = layers.map(l => chip(l.nos, state.layers[l.id], on => { state.layers[l.id] = on; draw(); }));
      chips.forEach(c => F.tools.append(c));
      const next = el("button", { type: "button", class: "pz-chip pz-plus", title: "Pievieno nākamo slāni" }, "+ pievienot nākamo slāni");
      next.addEventListener("click", () => { const i = layers.findIndex(l => !state.layers[l.id]); if (i < 0) return; state.layers[layers[i].id] = true; chips[i].setAttribute("aria-pressed", "true"); draw(); });
      const first = el("button", { type: "button", class: "pz-chip pz-plus", title: "Atstāj tikai pirmo slāni" }, "atstāt tikai pirmo slāni");
      first.addEventListener("click", () => { layers.forEach((l, i) => { state.layers[l.id] = i === 0; chips[i].setAttribute("aria-pressed", i === 0 ? "true" : "false"); }); draw(); });
      F.tools.append(next, first);
      root.pzNext = () => next.click();
    }
    if (spec.x2) F.tools.append(chip(spec.x2.chip || "otra laika ass augšā", state.x2, on => { state.x2 = on; draw(); }, "Rāda augšā otru laika asi"));
    (spec.controls || []).forEach(c => F.tools.append(c));
    F.tools.append(tableToggle(F, () => table([spec.x.tipLabel || "x", ...spec.series.map(s => s.nos)], sx.map(x => [fx(x), ...spec.series.map(sr => { const q = sr.pts.find(q => q.x === x); return q && q.y !== null && q.y !== undefined ? (sr.fmt || fy)(q.y) : ""; })]))));
    // leģenda (HTML, zem grafika)
    const leg = el("div", { class: "pz-legend" });
    spec.series.filter(s => !s.noLegend).forEach(sr => { const it = el("span"); it.append(legKey({ color: sr.color, dash: sr.dash, pt: sr.marker === "only" || sr.marker === "cross" || sr.marker === "diamond" }), document.createTextNode(sr.nos)); leg.append(it); });
    (spec.legendExtra || []).forEach(t => { const it = el("span"); it.append(legKey(t), document.createTextNode(t.nos)); leg.append(it); });
    root.insertBefore(leg, F.tbl);
    draw();
    return { draw, state };
  }

  // ---------------------------------------------------------------------
  // Punktu grafiks ar intervāliem (horizontāls): paneļi × rindas
  // spec: w, panels:[{id,nos,rows:[{nos,items:[{v,lo,hi,color,shape,label,tip}]}]}], x:{min,max,step,label,fmt}, rects, vlines, cols (paneļu kolonnu skaits), rowH
  // ---------------------------------------------------------------------
  function dots(root, spec) {
    const cols = spec.cols || 2, rowH = spec.rowH || 22, labW = spec.labW || 190, padT = 30, padB = spec.x.label ? 52 : 34, gapX = 26;
    const panels = spec.panels;
    const nrow = Math.ceil(panels.length / cols);
    const pw = (spec.w - gapX * (cols - 1)) / cols, plotW = pw - labW - 10;
    const heights = []; for (let i = 0; i < nrow; i++) heights.push(Math.max(...panels.slice(i * cols, i * cols + cols).map(p => p.rows.length)) * rowH + padT + padB);
    const H = heights.reduce((a, b) => a + b, 0);
    spec.h = H;
    const F = frame(root, spec);
    const fx = spec.x.fmt || (v => f1(v, 2));
    let s = "";
    const items = [];
    panels.forEach((p, i) => {
      const ci = i % cols, ri = Math.floor(i / cols);
      const ox = ci * (pw + gapX), oy = heights.slice(0, ri).reduce((a, b) => a + b, 0);
      const x0 = ox + labW, x1 = ox + pw - 10;
      const X = p.x || spec.x;
      const xs = v => x0 + (v - X.min) / (X.max - X.min) * (x1 - x0);
      const ph = p.rows.length * rowH;
      s += `<text x="${ox + labW}" y="${oy + 16}" class="ptitle">${esc(p.nos)}</text>`;
      (spec.rects || []).forEach(q => { s += `<rect x="${xs(Math.max(q.x0, X.min))}" y="${oy + padT - 6}" width="${xs(Math.min(q.x1, X.max)) - xs(Math.max(q.x0, X.min))}" height="${ph + 12}" fill="${q.fill}" opacity="${q.opacity ?? 0.1}"/>`; });
      ticksOf(X.min, X.max, X.step).forEach(v => { s += `<line x1="${xs(v)}" x2="${xs(v)}" y1="${oy + padT - 6}" y2="${oy + padT + ph + 6}" class="grid"/><text x="${xs(v)}" y="${oy + padT + ph + 20}" class="tick" text-anchor="middle">${esc(fx(v))}</text>`; });
      (spec.vlines || []).forEach(q => { s += `<line x1="${xs(q.x)}" x2="${xs(q.x)}" y1="${oy + padT - 6}" y2="${oy + padT + ph + 6}" stroke="${q.color || "var(--ink-2)"}" ${q.dash ? `stroke-dasharray="${q.dash}"` : ""} stroke-width="${q.width || 1}"/>`; });
      p.rows.forEach((r, j) => {
        const y = oy + padT + j * rowH + rowH / 2;
        s += `<text x="${ox + labW - 8}" y="${y + 4}" class="rlab" text-anchor="end">${esc(r.nos)}</text>`;
        if (j) s += `<line x1="${x0}" x2="${x1}" y1="${y - rowH / 2}" y2="${y - rowH / 2}" class="grid" opacity=".5"/>`;
        r.items.forEach((it, k) => {
          const yy = y + (r.items.length > 1 ? (k - (r.items.length - 1) / 2) * 7 : 0);
          if (it.lo !== undefined && it.lo !== null) s += `<line x1="${xs(Math.max(it.lo, X.min))}" x2="${xs(Math.min(it.hi, X.max))}" y1="${yy}" y2="${yy}" stroke="${it.color}" stroke-width="1.6" opacity=".8"/>`;
          const id = items.length; items.push({ it, p, r, cx: xs(it.v), cy: yy });
          if (it.shape === "square") s += `<rect data-i="${id}" class="hit" x="${xs(it.v) - 4.5}" y="${yy - 4.5}" width="9" height="9" fill="${it.color}" stroke="var(--card)" stroke-width="1.5"/>`;
          else s += `<circle data-i="${id}" class="hit" cx="${xs(it.v)}" cy="${yy}" r="${it.r || 4.5}" fill="${it.color}" stroke="var(--card)" stroke-width="1.5"/>`;
          if (it.label) s += `<text x="${xs(it.v) + 8}" y="${yy + 4}" class="small" fill="${it.color}">${esc(it.label)}</text>`;
        });
      });
      if (spec.x.label && ri === nrow - 1) s += `<text x="${(x0 + x1) / 2}" y="${oy + heights[ri] - 4}" class="alab" text-anchor="middle">${esc(spec.x.label)}</text>`;
    });
    F.svg.innerHTML = s;
    F.svg.addEventListener("mousemove", ev => {
      const pt = F.svg.createSVGPoint(); pt.x = ev.clientX; pt.y = ev.clientY; const p = pt.matrixTransform(F.svg.getScreenCTM().inverse());
      let best = null, bd = 1e9; items.forEach(o => { const d = (o.cx - p.x) ** 2 + (o.cy - p.y) ** 2; if (d < bd) { bd = d; best = o; } });
      if (!best || bd > 18 * 18) { tipHide(F); return; }
      const it = best.it; const rows = [{ c: it.color, v: fx(it.v) + (it.lo !== undefined && it.lo !== null ? ` (${fx(it.lo)} līdz ${fx(it.hi)})` : ""), t: it.tip || (best.r.nos + ", " + best.p.nos) }];
      if (it.extra) rows.push({ v: "", t: it.extra });
      tipShow(F, rows, ev);
    });
    F.svg.addEventListener("mouseleave", () => tipHide(F));
    (spec.controls || []).forEach(c => F.tools.append(c));
    F.tools.append(tableToggle(F, () => table(["Panelis", "Rinda", "Sērija", "Vērtība", "no", "līdz"], items.map(o => [o.p.nos, o.r.nos, o.it.tip || "", fx(o.it.v), o.it.lo !== undefined && o.it.lo !== null ? fx(o.it.lo) : "", o.it.hi !== undefined && o.it.hi !== null ? fx(o.it.hi) : ""]))));
    if (spec.legend) { const leg = el("div", { class: "pz-legend" }); spec.legend.forEach(t => { const it = el("span"); const k = el("i", { class: "pt" }); k.style.background = t.color; it.append(k, document.createTextNode(t.nos)); leg.append(it); }); root.insertBefore(leg, F.tbl); }
    return F;
  }

  // ---------------------------------------------------------------------
  // Izkliedes grafiks
  // spec: w,h,m, x, y, pts:[{x,y,color,r,shape,opacity,tip,label,cls}], lines:[{pts,color,dash,nos,width}], layers, texts
  // ---------------------------------------------------------------------
  function scatter(root, spec) {
    const F = frame(root, spec);
    const m = Object.assign({}, spec.m || { l: 56, r: 24, t: 18, b: 44 });
    if (spec.x2) m.t = Math.max(m.t, 44);
    const W = spec.w, H = spec.h;
    const xs = v => m.l + (v - spec.x.min) / (spec.x.max - spec.x.min) * (W - m.l - m.r);
    const ys = v => H - m.b - (v - spec.y.min) / (spec.y.max - spec.y.min) * (H - m.t - m.b);
    const fx = spec.x.fmt || (v => String(v)), fy = spec.y.fmt || (v => f1(v, 1));
    const layers = spec.layers || []; const state = { layers: Object.fromEntries(layers.map(l => [l.id, l.on !== false])), x2: spec.x2 ? spec.x2.on !== false : false };
    const vis = o => !o.layer || state.layers[o.layer] !== false;
    let drawn = [];
    function draw() {
      let s = ""; drawn = [];
      ticksOf(spec.y.min, spec.y.max, spec.y.step).forEach(v => { s += `<line x1="${m.l}" x2="${W - m.r}" y1="${ys(v)}" y2="${ys(v)}" class="${v === 0 ? "axis" : "grid"}"/><text x="${m.l - 8}" y="${ys(v) + 4}" class="tick" text-anchor="end">${esc(fy(v))}</text>`; });
      ticksOf(spec.x.min, spec.x.max, spec.x.step).forEach(v => { s += `<line x1="${xs(v)}" x2="${xs(v)}" y1="${m.t}" y2="${H - m.b}" class="grid"/><text x="${xs(v)}" y="${H - m.b + 18}" class="tick" text-anchor="middle">${esc(fx(v))}</text>`; });
      if (spec.x2 && state.x2) { s += x2axis(spec.x2, xs, m, W); }
      if (spec.y.label) s += `<text transform="translate(14,${(m.t + H - m.b) / 2}) rotate(-90)" class="alab" text-anchor="middle">${esc(spec.y.label)}</text>`;
      if (spec.x.label) s += `<text x="${(m.l + W - m.r) / 2}" y="${H - 6}" class="alab" text-anchor="middle">${esc(spec.x.label)}</text>`;
      (spec.lines || []).filter(vis).forEach(l => {
        let pts = l.pts;
        if (l.clip && pts.length === 2) { // taisnes nogrieznis, apgriezts pie zīmējuma malām
          const [a, b] = pts; const seg = [];
          const lerp = (t0) => ({ x: a.x + (b.x - a.x) * t0, y: a.y + (b.y - a.y) * t0 });
          let t0 = 0, t1 = 1;
          [[spec.x.min, spec.x.max, "x"], [spec.y.min, spec.y.max, "y"]].forEach(([lo, hi, k]) => { const d = b[k] - a[k]; if (Math.abs(d) < 1e-9) return; let ta = (lo - a[k]) / d, tb = (hi - a[k]) / d; if (ta > tb) [ta, tb] = [tb, ta]; t0 = Math.max(t0, ta); t1 = Math.min(t1, tb); });
          if (t0 >= t1) return; pts = [lerp(t0), lerp(t1)];
        }
        s += `<path d="M${pts.map(q => xs(q.x) + "," + ys(q.y)).join("L")}" fill="none" stroke="${l.color}" stroke-width="${l.width || 1.5}" ${l.dash ? `stroke-dasharray="${l.dash}"` : ""}/>`;
        if (l.nos) {
          if (l.along && pts.length === 2) { const p0 = { x: xs(pts[0].x), y: ys(pts[0].y) }, p1 = { x: xs(pts[1].x), y: ys(pts[1].y) }; const f = l.at ?? 0.5; const mx = p0.x + (p1.x - p0.x) * f, my = p0.y + (p1.y - p0.y) * f; const wdt = l.nos.length * 6.3 + 10; const left = l.side === "left"; let rx = left ? mx - 8 - wdt : mx + 8; rx = Math.max(m.l + 2, Math.min(W - wdt - 4, rx)); s += `<rect x="${rx}" y="${my - 9}" width="${wdt}" height="16" rx="3" fill="var(--card)" opacity=".9"/><text x="${rx + 5}" y="${my + 3}" class="small" fill="${l.color}" font-weight="600">${esc(l.nos)}</text>`; }
          else { const q = pts[pts.length - 1]; s += `<text x="${xs(q.x) + 4}" y="${ys(q.y) + 4}" class="small" fill="${l.color}">${esc(l.nos)}</text>`; }
        }
      });
      spec.pts.filter(vis).forEach(p => { const x = xs(p.x), y = ys(p.y); drawn.push({ p, x, y }); const r = p.r || 3.5; if (p.shape === "tri") s += `<polygon points="${x},${y - r - 1} ${x + r + 1},${y + r} ${x - r - 1},${y + r}" fill="${p.color}" opacity="${p.opacity ?? 0.6}"/>`; else if (p.shape === "diamond") s += `<polygon points="${x},${y - r - 2} ${x + r + 2},${y} ${x},${y + r + 2} ${x - r - 2},${y}" fill="${p.color}" opacity="${p.opacity ?? 0.6}" stroke="${p.stroke || "none"}" stroke-width="1.5"/>`; else s += `<circle cx="${x}" cy="${y}" r="${r}" fill="${p.color}" opacity="${p.opacity ?? 0.6}" stroke="${p.stroke || "none"}" stroke-width="1.5"/>`; if (p.label) s += p.labelPos === "bl" ? `<text x="${x - r - 2}" y="${y + r + 11}" class="small" text-anchor="end" fill="${p.color}" font-weight="600">${esc(p.label)}</text>` : `<text x="${x + r + 4}" y="${y + 4}" class="small" fill="${p.color}" font-weight="600">${esc(p.label)}</text>`; });
      (spec.texts || []).forEach(t => { s += `<text x="${xs(t.x)}" y="${ys(t.y)}" class="small" text-anchor="${t.anchor || "start"}" fill="${t.color || "var(--ink-2)"}">${esc(t.t)}</text>`; });
      s += `<circle id="pzh" r="9" fill="none" stroke="var(--ink)" stroke-width="1.5" visibility="hidden"/>`;
      F.svg.innerHTML = s;
    }
    F.svg.addEventListener("mousemove", ev => {
      const pt = F.svg.createSVGPoint(); pt.x = ev.clientX; pt.y = ev.clientY; const p = pt.matrixTransform(F.svg.getScreenCTM().inverse());
      let best = null, bd = 1e9; drawn.forEach(o => { const d = (o.x - p.x) ** 2 + (o.y - p.y) ** 2; if (d < bd) { bd = d; best = o; } });
      const h = F.svg.querySelector("#pzh");
      if (!best || bd > 30 * 30) { tipHide(F); h.setAttribute("visibility", "hidden"); return; }
      h.setAttribute("cx", best.x); h.setAttribute("cy", best.y); h.setAttribute("visibility", "visible");
      tipShow(F, [{ c: best.p.color, v: fy(best.p.y), t: best.p.tip || "" }, { v: fx(best.p.x), t: spec.x.tipLabel || "" }], ev);
    });
    F.svg.addEventListener("mouseleave", () => { tipHide(F); const h = F.svg.querySelector("#pzh"); if (h) h.setAttribute("visibility", "hidden"); });
    if (layers.length) { F.tools.append(el("span", { class: "pz-lab" }, "Datu slāņi:")); layers.forEach(l => F.tools.append(chip(l.nos, state.layers[l.id], on => { state.layers[l.id] = on; draw(); }))); }
    if (spec.x2) F.tools.append(chip(spec.x2.chip || "otra laika ass augšā", state.x2, on => { state.x2 = on; draw(); }, "Rāda augšā otru laika asi"));
    (spec.controls || []).forEach(c => F.tools.append(c));
    F.tools.append(tableToggle(F, () => table([spec.x.tipLabel || "x", spec.y.label || "y", "Kas"], spec.pts.filter(vis).map(p => [fx(p.x), fy(p.y), p.tip || ""]))));
    if (spec.legend) { const leg = el("div", { class: "pz-legend" }); spec.legend.forEach(t => { const it = el("span"); it.append(legKey(t.dash ? t : Object.assign({ pt: true }, t)), document.createTextNode(t.nos)); leg.append(it); }); root.insertBefore(leg, F.tbl); }
    draw();
    return { draw, state };
  }

  return { line, dots, scatter, f1, fs, el, sel, chip };
})();
