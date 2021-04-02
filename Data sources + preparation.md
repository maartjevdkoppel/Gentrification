# Data collection log

### Data sources

2006: [https://data.amsterdam.nl/datasets/xgPglw9eHtYqgQ/verkiezingen-gemeenteraad-2006/](https://data.amsterdam.nl/datasets/xgPglw9eHtYqgQ/verkiezingen-gemeenteraad-2006/)

2010: [https://data.amsterdam.nl/datasets/gLE0oWwKahx4Ww/verkiezingen-gemeenteraad-2010/](https://data.amsterdam.nl/datasets/gLE0oWwKahx4Ww/verkiezingen-gemeenteraad-2010/)

2014: [https://data.amsterdam.nl/datasets/gauvtc4HE3JSvg/verkiezingen-gemeenteraad-2014/](https://data.amsterdam.nl/datasets/gauvtc4HE3JSvg/verkiezingen-gemeenteraad-2014/)

2018: [https://data.amsterdam.nl/datasets/DFUrmyLoia4l2w/verkiezingen-gemeenteraad-2018/](https://data.amsterdam.nl/datasets/DFUrmyLoia4l2w/verkiezingen-gemeenteraad-2018/)

Lijst met wijken: [https://data.amsterdam.nl/datasets/5L2CSm77FLZGYA/registratie-gebieden/](https://data.amsterdam.nl/datasets/5L2CSm77FLZGYA/registratie-gebieden/) (CSV 'Gebieden wijk')

### Changes to data

2 April 2021 (9:00-10:00; 10:45- 12:30; 13:00-

- Downloaded files from OIS website. Raw file names:
    - 2006
        - website name: Verkiezingen Gemeenteraad 2006 naar buurtcombinaties en stadsdelen (procenten)
        - download name: 2006_gemeenteraad_tabel3_buurtcombinaties_c.xls
    - 2010
        - website name: Verkiezingen Gemeenteraad 2010 naar buurtcombinaties en stadsdelen (procenten)
        - download name: 2010_gemeenteraad_tabel4.xls
    - 2014
        - website name: Verkiezingen Gemeenteraad Amsterdam naar stembureaus, 19 maart 2014
        - download name: 2014_gemeenteraad_stembureaus
    - 2018
        - website name: Uitslag Verkiezingen Gemeenteraad Amsterdam naar wijken, stadsdelen en 22 gebieden, 21 maart 2018
        - download name: 2018_gemeenteraad_wijken_gebieden.xlsx
    - note: for 2010, I used files provided by OIS which were already sorted according to 'buurtcombinatie'. However, the file of this sort for 2014 only contained the votes for the bigger parties, grouping smaller parties such as M+ under 'overig'. Therefore, for this year, the excel file detailing all vote shares from the stembureaus is used (bc codes are indicated for each stembureau)
- Renamed files: 2006_buurtcombinaties; 2010_buurtcombinaties; 2014_stembureaus; 2014_buurtcombinaties
- Changes to election data:
    - Removed subtotal rows for 'stadsdelen' from the excel files
    - Removed subtotal rows for 22 'gebieden' (DX01, DX02, etc.) from the excel files
    - Removed headers / extra information from Excel files (leaving only row with variables names + actual data) for all 4 years
    - Removed extra spaces and spaces before / in BC names
    - In 2010_buurtcombinaties changed commas to periods
    - Saved Excel (.xls or .xlsx) as .csv files
- Downloaded file with all 'buurtcombinaties' names and codes
    - Added (by hand) 3 'buurtcombinaties' to 'Overzicht buurten Amsterdam'
        - F75 Spieringhorn
        - B11 Bedrijventerrein Sloterdijk
        - K50 Duivelseiland
- All other steps are recorded in the R script