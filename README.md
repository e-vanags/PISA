# PISA: Latvijas lasītprasmes datu analīze

Lapas (GitHub Pages: https://e-vanags.github.io/PISA/):

- `index.html`: PISA lasītprasmes datu salīdzinājums 2003 līdz 2025, latviešu un krievu valodas plūsma.
- `zinojums_2026-11-05.html`: ziņojums "Ko no OECD, IEA, SPKC un nacionālajiem datiem varēja pamanīt pirms PISA 2025, un ko tie ļauj prognozēt turpmāk" (2026. gada 5. novembris). Viens fails bez ārējām bibliotēkām; grafiki interaktīvi, ar datu slāņiem un tabulu skatu.
- `V6_ce_paaudzes.html`: centralizēto eksāmenu rezultāti pa PISA paaudzēm.

Aprēķinu skripti (bez datiem; mikrodati nav repozitorijā):

- `R/pisa2025/`: R skripti analīzēm, ko izmanto ziņojums (korekcijas, paaudžu modelis, ilgtermiņa pazīmes, HBSC, izdevumi, scenāriji u. c.), kā arī lapas veidne `86_zinojums_prognozes_veidne.html` un grafiku dzinējs `86_zinojums_grafiki_dzinejs.js`, `86_zinojums_grafiki_lapa.js`.
- `py/zinojums_prognozes_html.py`: ziņojuma lapas būvētājs (iegulda datus no `output/pisa2025/*.csv` un JS failus veidnē).
- `py/zinojums_teksti.py`: lapas tekstu eksports uz Markdown un Word un imports atpakaļ.
- `py/ce_paaudzes_html.py`: V6 lapas būvētājs.

Dati: OECD PISA, IEA PIRLS un TIMSS, OECD TALIS un PIAAC, HBSC, VIAA, OECD Education at a Glance. Skripti pieņem projekta struktūru `data/`, `output/`, kas šeit nav iekļauta.
