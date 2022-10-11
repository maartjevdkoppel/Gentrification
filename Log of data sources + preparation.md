### Data sources

2006: [https://data.amsterdam.nl/datasets/xgPglw9eHtYqgQ/verkiezingen-gemeenteraad-2006/](https://data.amsterdam.nl/datasets/xgPglw9eHtYqgQ/verkiezingen-gemeenteraad-2006/)

2010: [https://data.amsterdam.nl/datasets/gLE0oWwKahx4Ww/verkiezingen-gemeenteraad-2010/](https://data.amsterdam.nl/datasets/gLE0oWwKahx4Ww/verkiezingen-gemeenteraad-2010/)

2014: [https://data.amsterdam.nl/datasets/gauvtc4HE3JSvg/verkiezingen-gemeenteraad-2014/](https://data.amsterdam.nl/datasets/gauvtc4HE3JSvg/verkiezingen-gemeenteraad-2014/)

2018: [https://data.amsterdam.nl/datasets/DFUrmyLoia4l2w/verkiezingen-gemeenteraad-2018/](https://data.amsterdam.nl/datasets/DFUrmyLoia4l2w/verkiezingen-gemeenteraad-2018/)

Lijst met wijken: [https://data.amsterdam.nl/datasets/5L2CSm77FLZGYA/registratie-gebieden/](https://data.amsterdam.nl/datasets/5L2CSm77FLZGYA/registratie-gebieden/) (CSV 'Gebieden wijk')

### Changes to data

2 April 2021 - 16 April 2021: collecting election data 

- Downloaded files from OIS website. Raw file names:
    - 2006
        - website name: Verkiezingen Gemeenteraad 2006 naar buurtcombinaties en stadsdelen
        - download name: 2006_gemeenteraad_tabel3_buurtcombinaties_a.xls
    - 2010
        - website name: Verkiezingen Gemeenteraad 2010 naar buurtcombinaties en stadsdelen
        - download name: 2010_gemeenteraad_tabel3.xls
    - 2014
        - website name: Verkiezingen Gemeenteraad Amsterdam naar stembureaus, 19 maart 2014
        - download name: 2014_gemeenteraad_stembureaus
    - 2018
        - website name: Uitslag Verkiezingen Gemeenteraad Amsterdam naar wijken, stadsdelen en 22 gebieden, 21 maart 2018
        - download name: 2018_gemeenteraad_wijken_gebieden.xlsx
    - Note: I used files provided by OIS which were already sorted according to 'buurtcombinatie'. However, the file of this sort for 2014 only contained the votes for the bigger parties, grouping smaller parties such as M+ under 'overig'. Therefore, for this year, the excel file detailing all vote shares from the stembureaus is used (bc codes are indicated for each stembureau)
- Renamed files: 2006_buurtcombinaties; 2010_buurtcombinaties; 2014_stembureaus; 2018_buurtcombinaties
- Changes to election data:
    - Removed subtotal rows for 'stadsdelen' from the excel files
    - Removed subtotal rows for 22 'gebieden' (DX01, DX02, etc.) from the excel files
    - Removed headers / extra information from Excel files (leaving only row with variables names + actual data) for all 4 years
    - Removed extra spaces and spaces before / in BC names
    - Saved Excel (.xls or .xlsx) as .csv files
        - 2006_buurtcombinaties
        - 2010_buurtcombinaties
        - 2014_stembureaus
        - 2018_buurtcombinaties
- *In 2010_buurtcombinaties_procenten I changed commas to periods
- Downloaded file with all 'buurtcombinaties' names and codes (file 'CSV gebieden wijk', see above)
    - Added (by hand) 3 'buurtcombinaties' to 'Overzicht buurten Amsterdam'
        - F75 Spieringhorn
        - B11 Bedrijventerrein Sloterdijk
        - K50 Duivelseiland
- All other steps are recorded in the R script

20 April 2021: linking neighbourhood characteristics

- Downloaded file with 500+ variables on areas in Amsterdam from OIS:
    - "Basisbestand Gebieden Amsterdam (BBGA)", download file named 'het BBGA in csv formaat'
    - retrieved from [https://data.amsterdam.nl/datasets/G5JpqNbhweXZSw/basisbestand-gebieden-amsterdam-bbga/](https://data.amsterdam.nl/datasets/G5JpqNbhweXZSw/basisbestand-gebieden-amsterdam-bbga/)
- Saved file as 'Buurtkenmerken (versie 10-3-21)'
- All other steps are recorded in the R script

27 April 2021: gentrification variables & finalisation of dataset

- NB. Buurtcombinatie 'Zuidas' (K23) is dropped
    - it was newly created in 2018 from components of 4 other BCs, i.e. Station Zuid/WTC e.o. (K59), Scheldebuurt (K52), Buitenveldert West (K90) en Buitenveldert Oost (K91)
    - the constitutive BCs do remain in the data and all have observations available for 2018 too.
- Note: Housing stock data is incomplete in the BBGA file used for the other neighbourhood characteristics data; missing for 2005 and 2009. This variable is needed to switch the % social housing variable (IV) between relative and absolute numbers (absolute numbers needed for merging of neighbourhoods).
- Downloaded file with time-series data on the housing stock (1995-2021)
    - "Woningvoorraad, 1 januari 1995-2021", download file named '2021-tijdreeksen-69'
    - retrieved from [https://data.amsterdam.nl/datasets/XBHSC-xM8UmROQ/tijdreeksen-wijken/](https://data.amsterdam.nl/datasets/XBHSC-xM8UmROQ/tijdreeksen-wijken/)
- Changes to housing stock data:
    - Removed subtotal rows for 'stadsdelen' from the excel files
    - Removed empty rows + rows with 'nvt'
    - Removed headers / extra information from Excel files (leaving only row with variables names + actual data) for all 4 years
    - Split Excel into 2 files: 1995-2014 (old BC codes) and 2011-2021 (post-2015 BC codes)
        - file 1 = 'Woningvoorraad 1995-2014'
        - file 2 = 'Woningvoorraad 2011-2021'
    - Exported Excel file to .csv
