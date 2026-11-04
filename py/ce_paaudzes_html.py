"""V6: interaktīvs paaudžu grafiks — valsts eksāmenu sērijas pa dzimšanas gadiem blakus PISA.

Ievade:  output/pisa2025/ce_9_12_paaudzes.csv (skripts 81), output/pisa2025/ab_rinda_no_2003.csv
Izvade:  output/pisa2025/vizualizacijas/V6_ce_paaudzes.html

Standartizācija un gludināšana notiek pārlūkā (JS), lai lietotājs var pārslēgt:
bāze "stabilie gadi līdz 2018./19." vai "visi gadi"; gludas līknes (monotons Hermita
splains, Fritsch–Carlson) vai lauztas; sērijas pieliek/noņem ar izvēles rūtiņām.
"""
import csv, json
from pathlib import Path

SAKNE = Path(__file__).resolve().parent.parent
ser = list(csv.DictReader(open(SAKNE / "output/pisa2025/ce_9_12_paaudzes.csv", encoding="utf-8")))
ab = [r for r in csv.DictReader(open(SAKNE / "output/pisa2025/ab_rinda_no_2003.csv", encoding="utf-8")) if r["grupa"] in ("A", "B")]

KR = {
    "9. kl. latviešu valoda A: lasīšana (+ valodas sistēmas izpratne no 2011./12.); 2022./23.+ CE 1. daļa": ("#1f4e9c", "9. kl. latviešu valoda A: lasīšana / 1. daļa", 9, True),
    "9. kl. latviešu valoda A: kopā; 2022./23.+ CE visiem": ("#6fa8dc", "9. kl. latviešu valoda A: kopā", 9, True),
    "9. kl. matemātika: kopā; 2022./23.+ CE visiem": ("#1a9850", "9. kl. matemātika: kopā", 9, True),
    "9. kl. LV9 (B plūsma, latviešu kā otrā valoda): kopā": ("#8fbc8f", "9. kl. latviešu valoda B plūsmai (LV9, CE): kopā", 9, False),
    "12. kl. latviešu valoda CE: kopā, A (latviešu programmas skolas)": ("#c0392b", "12. kl. latviešu valoda CE: kopā, A", 12, True),
    "12. kl. latviešu valoda CE: kopā, B (mazākumtautību programmas skolas)": ("#e8a09a", "12. kl. latviešu valoda CE: kopā, B", 12, False),
    "12. kl. latviešu valoda CE: kopā, visi": ("#7f1d1d", "12. kl. latviešu valoda CE: kopā, visi", 12, False),
    "12. kl. latviešu valoda CE: 2. daļa teksta izpratne, visi": ("#e07b39", "12. kl. latviešu valoda CE: teksta izpratne (2. daļa), visi", 12, False),
    "12. kl. matemātika CE: kopā, visi": ("#7a57c7", "12. kl. matemātika CE: kopā", 12, True),
    "12. kl. latviešu valoda OL CE (2021./22.+): kopā": ("#b03060", "12. kl. latviešu valoda OL CE (2021./22.+)", 12, False),
    "12. kl. matemātika OL CE (2021./22.+): kopā": ("#9b59b6", "12. kl. matemātika OL CE (2021./22.+)", 12, False),
}
series = []
for key, (c, nos, kl, on) in KR.items():
    pts = sorted([r for r in ser if r["serija"] == key], key=lambda r: int(r["gads_sak"]))
    if not pts:
        continue
    series.append({"id": len(series), "nos": nos, "krasa": c, "klase": kl, "on": on,
                   "p": [{"g": r["gads"], "gs": int(r["gads_sak"]), "b": int(r["dz_gads"]), "v": float(r["videja"]), "n": int(float(r["n"])) if r["n"] else None} for r in pts]})
# PISA A un B plūsma (absolūtie vidējie; z rēķina JS)
for gr, nos, c in (("A", "PISA lasīšana: latviešu valodas plūsma", "#000000"), ("B", "PISA lasīšana: krievu valodas plūsma", "#888888")):
    pts = sorted([r for r in ab if r["grupa"] == gr], key=lambda r: int(r["gads"]))
    series.append({"id": len(series), "nos": nos, "krasa": c, "klase": 0, "on": gr == "A", "pisa": True,
                   "p": [{"g": r["gads"], "gs": int(r["gads"]) - 1, "b": int(r["gads"]) - 16, "v": round(float(r["vid"]), 1), "n": int(float(r["n"]))} for r in pts]})

ev = list(csv.DictReader(open(SAKNE / "data/politika/notikumi_laika_josla.csv", encoding="utf-8")))
D = {"notikumi": [{"g": float(r["gads"]), "k": r["kategorija"], "n": r["nosaukums"], "i": r["isais"], "a": r["apraksts"], "av": r["avots"], "t": r["ticamiba"]} for r in ev], "series": series, "pisa_paaudzes": [{"b": 1987, "c": 2003}, {"b": 1990, "c": 2006}, {"b": 1993, "c": 2009}, {"b": 1996, "c": 2012}, {"b": 1999, "c": 2015}, {"b": 2002, "c": 2018}, {"b": 2006, "c": 2022}, {"b": 2009, "c": 2025}]}

HTML = r'''<!doctype html><html lang="lv"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Paaudzes valsts eksāmenos un PISA</title>
<style>
:root{--ink:#1a1a1a;--muted:#5a5a56;--line:#dcdcd5;--bg:#fbfbf8;--card:#fff}
body{font-family:system-ui,-apple-system,"Segoe UI",sans-serif;margin:0;background:var(--bg);color:var(--ink);line-height:1.45}
.wrap{max-width:1180px;margin:0 auto;padding:22px 20px 40px}
h1{font-size:21px;margin:0 0 6px}.lede{font-size:14.5px;max-width:90ch;margin:0 0 12px}
.kalasit{background:#f1f5fb;border:1px solid #d4e0f2;border-radius:6px;padding:8px 14px;margin:0 0 12px;font-size:13px}.kalasit summary{cursor:pointer;font-weight:600}
.kalasit ul{margin:6px 0 2px 18px;padding:0}
.grid{display:grid;grid-template-columns:270px 1fr;gap:16px;align-items:start}
.panel{background:var(--card);border:1px solid var(--line);border-radius:6px;padding:10px 12px;font-size:13px}
.panel h3{font-size:12px;text-transform:uppercase;letter-spacing:.04em;color:var(--muted);margin:8px 0 4px}
.panel label{display:flex;gap:6px;align-items:center;margin:3px 0;cursor:pointer}
.sw{display:inline-block;width:14px;height:14px;border-radius:3px;flex:none}
.seg{display:inline-flex;border:1px solid #bbb;border-radius:6px;overflow:hidden;margin:2px 0 6px}.seg button{border:0;background:#fff;padding:4px 9px;font:inherit;cursor:pointer;font-size:12.5px}.seg button.on{background:#333;color:#fff}
svg{width:100%;height:auto;background:var(--card);border:1px solid var(--line);border-radius:4px}
.tip{position:absolute;pointer-events:none;background:#222;color:#fff;font-size:12.5px;padding:6px 9px;border-radius:4px;max-width:320px;display:none;z-index:9}
.foot{font-size:12px;color:var(--muted);margin-top:12px;max-width:110ch}
.hint{font-size:12px;color:var(--muted);margin:2px 0 6px;line-height:1.35}
.btn{font:inherit;font-size:12px;padding:2px 8px;border:1px solid #bbb;border-radius:4px;background:#fff;cursor:pointer;margin-right:4px}
</style></head><body><div class="wrap">
<h1>Vai PISA paaudzes ir tikpat stipras vai vājas arī valsts eksāmenos?</h1>
<p class="lede">Grafiks liek blakus divus mērījumus vienām un tām pašām <b>skolēnu paaudzēm</b>: valsts eksāmenu rezultātus un PISA lasīšanas rezultātus. Uz horizontālās ass ir skolēnu dzimšanas gads: 9. klases eksāmenu paaudze ir tie, kas dzimuši 15 gadus pirms mācību gada sākuma, 12. klases eksāmenu paaudze dzimusi 18 gadus pirms, PISA paaudze 16 gadus pirms mērījuma gada.</p>
<p class="lede">Katra krāsainā līnija ir viens eksāmens. Tā kā katru gadu eksāmens ir cits un rezultāti nav saskaņoti kopīgā skalā, procentus nevar salīdzināt tieši. Tāpēc katram eksāmenam katra gada vidējais rezultāts pārvērsts par <b>novirzi no šī eksāmena parastā līmeņa</b> (z, tas ir, cik standartnovirzes virs vai zem gadu vidējā). Nulle nozīmē parastu gadu šim eksāmenam, mīnus divi nozīmē par divām standartnovirzēm vājāku nekā parasti. Salīdzināt var tikai to, <b>vai paaudze bija vājāka vai stiprāka par parasto</b>, ne absolūto līmeni. Krustiņi ir PISA lasīšanas vidējais tām pašām paaudzēm, pārvērsts tāpat.</p>
<details class="kalasit"><summary>Kā lasīt</summary><ul>
<li><b>z</b> = (gada vidējais − eksāmena "parastais līmenis") / gadu svārstību lielums (SD). "Parasto līmeni" var rēķināt tikai no gadiem līdz 2018./19. (noklusējums: tad vēlākie gadi mērīti pret laiku pirms krituma) vai no visiem gadiem. Stabilajos gados eksāmenu vidējie svārstās maz (PISA ap 2,6 punktiem), tāpēc 2025. gada 20 punktu kritums PISA sanāk ap mīnus 5,6 z.</li>
<li>Līnijas pārtrūkst, kur gads trūkst (2019./20. eksāmeni atcelti) vai instruments mainījās (2020./21. attālinātais darbs; 12. kl. OL eksāmeni no 2021./22.; 9. kl. CE no 2022./23.).</li>
<li>9. klases latviešu valodas A rinda līdz 2021./22. ir skolu vērtētu eksāmenu grafiku nolasījums; 12. klases CE un LV9 ir centralizēti vērtēti kārtotāju dati.</li>
<li>Pelēkās joslas ir stabilās PISA paaudzes, sarkanās ir 2018., 2022. un 2025. gada paaudzes. Uzbraucot uz punkta, parādās vērtība vārdos.</li></ul></details>
<div class="grid"><div class="panel">
<h3>Kas skaitās "parastais līmenis"</h3><div class="hint">Novirze (z) tiek rēķināta pret eksāmena vidējo un svārstību lielumu noteiktā gadu kopā. Ja kopā ieskaita arī kritumu no 2022. līdz 2025. gadam, "parastais" pats nobīdās uz leju un kritums izskatās mazāks.</div>
<span class="seg" id="baze"><button data-v="stab" class="on">tikai gadi līdz 2018./19.</button><button data-v="visi">visi gadi</button></span>
<div class="hint" id="bazehint"></div>
<h3>Līnijas</h3><span class="seg" id="glud"><button data-v="1" class="on">gludas</button><button data-v="0">lauztas</button></span>
<h3>Sērijas</h3><div id="ser"></div>
<div style="margin-top:8px"><button class="btn" id="visas">visas</button><button class="btn" id="nevienas">nevienas</button><button class="btn" id="noklus">noklusējums</button></div>
<h3>Notikumi laika joslā</h3><div class="hint">Notikumi likti pēc kalendārā gada uz augšējās ass; tie skar visas paaudzes, kas tobrīd bija skolā (tooltip rāda, kurā klasē bija PISA paaudzes).</div><label><input type="checkbox" class="ev" value="reforma" checked> <span class="sw" style="background:#b3261e"></span> reformas un politika</label><label><input type="checkbox" class="ev" value="tehnologija" checked> <span class="sw" style="background:#7a57c7"></span> tehnoloģijas un laikmeta zīmes</label><label><input type="checkbox" class="ev" value="sokss" checked> <span class="sw" style="background:#e0641f"></span> ārēji satricinājumi</label><label><input type="checkbox" class="ev" value="merijums" checked> <span class="sw" style="background:#5a5a56"></span> mērījumu un instrumentu izmaiņas</label>
<h3>Cits</h3><label><input type="checkbox" id="joslas" checked> PISA paaudžu joslas</label><label><input type="checkbox" id="punkti" checked> punkti</label>
</div><div style="position:relative"><svg id="s" viewBox="0 0 900 720"></svg><div class="tip" id="tip"></div></div></div>
<p class="foot">Avots: VIAA valsts pārbaudes darbu statistika (kārtotāju līmeņa faili no 2011./12. līdz 2024./25., tabulas par 2008./09. līdz 2010./11., 9. klases necentralizētie eksāmeni no grafikiem), skripts R/pisa2025/81_ce_9_12_klase.R, docs/81; PISA: OECD mikrodati, latviešu un krievu valodas plūsma pēc mājas valodas (skripts 43). Gludās līknes iet precīzi caur katru gada punktu un starp punktiem neliec augstāk vai zemāk par pašiem punktiem (tās ir tikai vizuāls savienojums, ne modelis).</p>
</div>
<script>
const D=__JSDATA__;const W=900,H=720,m={l:56,r:16,t:190,b:58};const EVC={reforma:"#b3261e",tehnologija:"#7a57c7",sokss:"#e0641f",merijums:"#5a5a56"};const svg=document.getElementById("s"),tip=document.getElementById("tip");
const X0=1986,X1=2011;const xs=b=>m.l+(b-X0)/(X1-X0)*(W-m.l-m.r);
let baze="stab",glud=1;
const on=new Map(D.series.map(s=>[s.id,s.on]));
const box=document.getElementById("ser");D.series.forEach(s=>{const l=document.createElement("label");l.innerHTML=`<input type="checkbox" data-id="${s.id}" ${s.on?"checked":""}> <span class="sw" style="background:${s.krasa}"></span> ${s.nos}`;box.appendChild(l);l.querySelector("input").addEventListener("change",e=>{on.set(s.id,e.target.checked);draw()})});
document.getElementById("visas").onclick=()=>{D.series.forEach(s=>on.set(s.id,true));sync();draw()};document.getElementById("nevienas").onclick=()=>{D.series.forEach(s=>on.set(s.id,false));sync();draw()};document.getElementById("noklus").onclick=()=>{D.series.forEach(s=>on.set(s.id,s.on));sync();draw()};
function sync(){box.querySelectorAll("input").forEach(i=>i.checked=on.get(+i.dataset.id))}
const BH={stab:"Parastais līmenis ir katra eksāmena vidējais un SD gados līdz 2018./19. (PISA līdz 2018. ciklam). Vēlākie gadi mērīti pret to, kā bija pirms krituma.",visi:"Parastais līmenis ir katra eksāmena vidējais un SD visos gados, arī no 2022. līdz 2025. Kritums šeit ir daļa no \"parastā\", tāpēc novirzes ir mazākas."};
function bazeHint(){document.getElementById("bazehint").textContent=BH[baze]}
document.querySelectorAll("#baze button").forEach(b=>b.onclick=()=>{baze=b.dataset.v;document.querySelectorAll("#baze button").forEach(x=>x.classList.toggle("on",x===b));bazeHint();draw()});bazeHint();
document.querySelectorAll("#glud button").forEach(b=>b.onclick=()=>{glud=+b.dataset.v;document.querySelectorAll("#glud button").forEach(x=>x.classList.toggle("on",x===b));draw()});
document.querySelectorAll("#joslas,#punkti,.ev").forEach(e=>e.addEventListener("change",draw));
function zser(s){const v=s.p.map(p=>p.v);const idx=baze==="stab"?s.p.map((p,i)=>(s.pisa?p.gs+1<=2018:p.gs<=2018)?i:-1).filter(i=>i>=0):v.map((_,i)=>i);const bv=(idx.length>=3?idx:v.map((_,i)=>i)).map(i=>v[i]);const mu=bv.reduce((a,b)=>a+b,0)/bv.length;const sd=Math.sqrt(bv.reduce((a,b)=>a+(b-mu)*(b-mu),0)/(bv.length-1));return s.p.map(p=>Object.assign({},p,{z:(p.v-mu)/sd,mu,sd}))}
// monotons Hermita splains (Fritsch un Carlson)
function hermite(x,y){const n=x.length,h=[],d=[];for(let i=0;i<n-1;i++){h.push(x[i+1]-x[i]);d.push((y[i+1]-y[i])/h[i])}const mm=new Array(n);mm[0]=d[0];mm[n-1]=d[n-2];for(let i=1;i<n-1;i++)mm[i]=(d[i-1]*d[i]<=0)?0:(d[i-1]+d[i])/2;for(let i=0;i<n-1;i++){if(d[i]===0){mm[i]=0;mm[i+1]=0;continue}const a=mm[i]/d[i],b=mm[i+1]/d[i],t=a*a+b*b;if(t>9){const tau=3/Math.sqrt(t);mm[i]=tau*a*d[i];mm[i+1]=tau*b*d[i]}}return t=>{let i=0;while(i<n-2&&t>x[i+1])i++;const hh=h[i],s=(t-x[i])/hh,s2=s*s,s3=s2*s;return (2*s3-3*s2+1)*y[i]+(s3-2*s2+s)*hh*mm[i]+(-2*s3+3*s2)*y[i+1]+(s3-s2)*hh*mm[i+1]}}
function paths(pts,maxgap,ys){let out="";const segs=[];let cur=[pts[0]];for(let i=1;i<pts.length;i++){if(pts[i].b-pts[i-1].b>maxgap){segs.push(cur);cur=[pts[i]]}else cur.push(pts[i])}segs.push(cur);
segs.forEach(sg=>{if(sg.length<2)return;if(glud&&sg.length>=3){const f=hermite(sg.map(p=>p.b),sg.map(p=>p.z));const xsS=[];for(let t=sg[0].b;t<=sg[sg.length-1].b+1e-9;t+=0.1)xsS.push(t);out+="M"+xsS.map(t=>xs(t).toFixed(1)+","+ys(f(t)).toFixed(1)).join("L")}else out+="M"+sg.map(p=>xs(p.b).toFixed(1)+","+ys(p.z).toFixed(1)).join("L")});return out}
function draw(){const act=D.series.filter(s=>on.get(s.id)).map(s=>Object.assign({},s,{p:zser(s)}));let zmin=-2.5,zmax=2.5;act.forEach(s=>s.p.forEach(p=>{zmin=Math.min(zmin,p.z);zmax=Math.max(zmax,p.z)}));zmin=Math.floor(zmin-0.3);zmax=Math.ceil(zmax+0.3);
const ys=z=>m.t+(zmax-z)/(zmax-zmin)*(H-m.t-m.b);let s="";
if(document.getElementById("joslas").checked)D.pisa_paaudzes.forEach(pp=>{if(pp.b<X0||pp.b>X1)return;const red=pp.c>=2018;s+=`<rect x="${xs(pp.b-0.5)}" y="${m.t}" width="${xs(pp.b+0.5)-xs(pp.b-0.5)}" height="${H-m.t-m.b}" fill="${red?"#c0392b":"#666"}" opacity="${red?0.11:0.08}"/><text x="${xs(pp.b)}" y="${m.t+14}" font-size="10.5" text-anchor="middle" fill="${red?"#c0392b":"#555"}">PISA ${pp.c}</text><text x="${xs(pp.b)}" y="${m.t+26}" font-size="10.5" text-anchor="middle" fill="${red?"#c0392b":"#555"}">dz. ${pp.b}</text>`});
for(let z=zmin;z<=zmax;z++)s+=`<line x1="${m.l}" x2="${W-m.r}" y1="${ys(z)}" y2="${ys(z)}" stroke="${z===0?"#333":"#e6e6e0"}" stroke-width="${z===0?1.4:1}"/><text x="${m.l-6}" y="${ys(z)+4}" font-size="11" text-anchor="end" fill="#555">${z>0?"+":""}${z}</text>`;
for(let b=X0;b<=X1;b++)s+=`<text x="${xs(b)}" y="${H-m.b+16}" font-size="10.5" text-anchor="middle" fill="#555" transform="rotate(0)">${b}</text>`;
[2003,2006,2009,2012,2015,2018,2022,2025].forEach(c=>{s+=`<line x1="${xs(c-16)}" x2="${xs(c-16)}" y1="${m.t}" y2="${H-m.b}" stroke="#dedcd4" stroke-dasharray="3,4"/><text x="${xs(c-16)}" y="${m.t-8}" font-size="11" text-anchor="middle" fill="#555">${c}</text>`});
const kats=[...document.querySelectorAll(".ev:checked")].map(e=>e.value);const evs=D.notikumi.filter(e=>kats.includes(e.k)&&e.g-16>=X0-0.5&&e.g-16<=X1+0.5).sort((a,b)=>a.g-b.g);
const yb=m.t-40;s+=`<line x1="${m.l}" x2="${W-m.r}" y1="${yb}" y2="${yb}" stroke="#bbb"/><text x="${m.l}" y="${m.t-166}" font-size="11.5" font-weight="600" fill="#333">Notikumi (kalendāra gads uz augšējās ass)</text>`;
const lanes=[];evs.forEach(e=>{const x=xs(e.g-16);let lane=0;while(lanes[lane]!==undefined&&x-lanes[lane]<78)lane++;lanes[lane]=x;const y=yb-14-lane*20;const c=EVC[e.k];
const kl=D.pisa_paaudzes.filter(pp=>{const g=Math.floor(e.g)-pp.b-6;return g>=1&&g<=12}).map(pp=>`PISA ${pp.c} paaudze ${Math.floor(e.g)-pp.b-6}. kl.`).join("; ");
const t=`${e.n} (${e.g.toFixed(1).replace(".",",").replace(",0","")}): ${e.a} Avots: ${e.av}. Ticamība: ${e.t}.${kl?" Tobrīd skolā: "+kl+".":""}`;
s+=`<line x1="${x}" x2="${x}" y1="${y+3}" y2="${yb}" stroke="${c}" stroke-width="1"/><line x1="${x}" x2="${x}" y1="${yb}" y2="${H-m.b}" stroke="${c}" stroke-width="0.8" stroke-dasharray="2,4" opacity=".5"/><circle class="pt" data-tip="${t.replace(/"/g,"&quot;")}" cx="${x}" cy="${y}" r="4" fill="${c}"/><text class="pt" data-tip="${t.replace(/"/g,"&quot;")}" x="${x+6}" y="${y+4}" font-size="10" fill="${c}">${e.i}</text>`});
s+=`<text x="${(m.l+W-m.r)/2}" y="${m.t-26}" font-size="12" text-anchor="middle" font-weight="600" fill="#333">PISA mērījuma gads (dzimšanas gads + 16)</text><text x="${(m.l+W-m.r)/2}" y="${H-m.b+42}" font-size="12" text-anchor="middle" font-weight="600" fill="#333">Dzimšanas gads (paaudze)</text><text transform="translate(14,${(m.t+H-m.b)/2}) rotate(-90)" font-size="12" text-anchor="middle" font-weight="600" fill="#333">z (novirze no bāzes gadu vidējā, bāzes SD vienībās)</text>`;
act.forEach(sr=>{const pts=sr.p.filter(p=>p.b>=X0&&p.b<=X1);if(!pts.length)return;const dash=sr.pisa?'stroke-dasharray="3,4"':"";s+=`<path d="${paths(pts,sr.pisa?5:1,ys)}" fill="none" stroke="${sr.krasa}" stroke-width="${sr.pisa?1.6:2.2}" ${dash}/>`;
if(document.getElementById("punkti").checked)pts.forEach(p=>{const t=`${sr.nos}, ${p.g} (paaudze dz. ${p.b}): ${String(p.v).replace(".",",")}${sr.pisa?" punkti":" %"}${p.n?", N = "+p.n.toLocaleString("lv"):""}; z = ${(p.z>0?"+":"")+p.z.toFixed(2).replace(".",",")} (bāze ${p.mu.toFixed(1).replace(".",",")} ± ${p.sd.toFixed(1).replace(".",",")})`;const x=xs(p.b),y=ys(p.z);
if(sr.pisa)s+=`<path class="pt" data-tip="${t}" d="M${x-5},${y-5}L${x+5},${y+5}M${x-5},${y+5}L${x+5},${y-5}" stroke="${sr.krasa}" stroke-width="2"/><circle class="pt" data-tip="${t}" cx="${x}" cy="${y}" r="7" fill="transparent"/>`;else if(sr.klase===12)s+=`<rect class="pt" data-tip="${t}" x="${x-4}" y="${y-4}" width="8" height="8" fill="${sr.krasa}" stroke="#fff"/>`;else s+=`<circle class="pt" data-tip="${t}" cx="${x}" cy="${y}" r="4.2" fill="${sr.krasa}" stroke="#fff"/>`})});
svg.innerHTML=s;const bx=svg.parentElement;svg.querySelectorAll(".pt").forEach(el=>{el.addEventListener("mousemove",e=>{tip.style.display="block";tip.textContent=el.dataset.tip;const r=bx.getBoundingClientRect();tip.style.left=Math.min(e.clientX-r.left+14,r.width-330)+"px";tip.style.top=(e.clientY-r.top+14)+"px"});el.addEventListener("mouseleave",()=>tip.style.display="none")})}
draw();
</script></body></html>'''
out = SAKNE / "output/pisa2025/vizualizacijas/V6_ce_paaudzes.html"
out.write_text(HTML.replace("__JSDATA__", json.dumps(D, ensure_ascii=False)), encoding="utf-8")
print("Ierakstīts:", out, "| sērijas:", len(series))
