*** OPKOMST AMSTERDAM ***

* OPEN DATA (DEPENDING ON DATA SOURCE)
* Compile data: PS2019
cd "/Users/Maartje/Desktop/LJA/Data POLetmaal/Data Eelco"
use "uitslagen Kiesraad.dta", clear
keep if Regio=="Amsterdam"
gen postcode4 = substr(postcode, 1, 4)
gen postcode2 = substr(postcode, 6, 2)
gen postcode6 = postcode4 + postcode2
rename postcode postcode_old
rename postcode6 postcode
merge m:1 postcode using "koppeltabel postcode naar buurt.dta"
drop if Regio==""
merge m:1 buurt_vollcode using "buurtkenmerken Amsterdam.dta", gen(merge_kenm)
merge m:1 buurt_vollcode using "buurtkenmerken Amsterdam - 2016.dta", gen(merge16)
capture drop Pregwerkl_p
merge m:1 buurt_vollcode using "buurtkenmerken Amsterdam - 2017.dta", gen(merge17)
drop if Regio==""

gen opkomst = (geldigestemmen/kiesgerechtigden)*100


* Compile data: GM2018
cd "/Users/Maartje/Desktop/LJA/Data POLetmaal/Data Eelco"
use "uitslagen GM2018.dta", clear
merge m:1 postcode using "koppeltabel postcode naar buurt.dta"
merge m:1 buurt_vollcode using "buurtkenmerken Amsterdam.dta", gen(merge_kenm)
merge m:1 buurt_vollcode using "buurtkenmerken Amsterdam - 2016.dta", gen(merge16)
capture drop Pregwerkl_p
merge m:1 buurt_vollcode using "buurtkenmerken Amsterdam - 2017.dta", gen(merge17)
keep if lokaal!=""

gen opkomst = (geldigestembiljetten/opgeroepenen)*100


* COMBINE AND RECODE DATA
* Leisure organizations
merge m:1 buurt_vollcode using "leisure.dta", gen(merge_leisure)
gen leisurepp = leisure/Bevtotaal

* Stembureau density
bys buurt_vollcode: gen bureaus = _N
gen bureauspp = bureaus/Bevtotaal
recode bureauspp (2=.)

* Recode data: IVs
rename Bevtotaal bevolking
rename (BevSur_p BevAntil_p BevTurk_p BevMarok_p) (imm_Sur imm_Ant imm_Tur imm_Mar)
rename (BevovNW_p BevWest_p BevAutoch_p) (imm_NW imm_W imm_autoch) 
rename (Bev0_18_p Bev18_26p Bev27_65p Bev66plus_p) (age_0t18 age_18t26 age_27t65 age_65plus)
rename (Bevopllaag_p-Bevoplhoog_p) (edu_lo edu_mid edu_hi)
rename Pregwerkl_p unempl

* HHI
egen hhi = rowtotal(imm_Sur imm_Ant imm_Tur imm_Mar)


* Standardize
foreach var of varlist imm_* age_18t26 age_65plus edu_lo edu_hi unempl leisurepp bureauspp {
	egen z_`var' = std(`var')
	}

* Stadsdeel
encode sd, gen(stadsdeel)
lab define stadsdeel2 1 "Centrum" 2 "West" 3 "Nieuw-West" 4 "Zuid" 5 "Oost" 6 "Noord" 7 "Zuidoost"
lab values stadsdeel stadsdeel2
	
* Labels
lab var z_imm_Sur "% Surinaams"
lab var z_imm_Ant "% Antilliaans"
lab var z_imm_Tur "% Turks"
lab var z_imm_Mar "% Marokkaans"
lab var z_imm_NW "% NW-immigr."
lab var z_imm_W "% W-immigr."
lab var z_imm_autoch "% autochtoon"
lab var z_age_18t26 "% 18 tot 26 jaar"
lab var z_age_65plus "% 65 plus"
lab var z_edu_lo "% laag opgeleid"
lab var z_edu_hi "% hoog opgeleid"
lab var z_unempl "% werkloos"
lab var z_bureauspp "Aantal stembureaus (p.p.)"
lab var z_leisurepp "Vrijetijdsorganisaties (p.p.)"
lab var imm_Sur "% Surinaams"
lab var imm_Ant "% Antilliaans"
lab var imm_Tur "% Turks"
lab var imm_Mar "% Marokkaans"
lab var imm_NW "% NW-immigr."
lab var imm_W "% W-immigr."
lab var imm_autoch "% autochtoon"
lab var age_18t26 "% 18 tot 26 jaar"
lab var age_65plus "% 65 plus"
lab var edu_lo "% laag opgeleid"
lab var edu_hi "% hoog opgeleid"
lab var unempl "% werkloos"
lab var bureauspp "Aantal stembureaus"
lab var leisurepp "Vrijetijdsorganisaties"

cd  "/Users/Maartje/Desktop/LJA/Data POLetmaal/Data Eelco"



use "/Users/Maartje/Desktop/LJA/Data POLetmaal/Data Eelco/buurtkenmerken Amsterdam.dta", clear


* Analysis

* 3. Regressie
* (1) algemeen
*drop if opkomst>100
collapse opkomst imm_NW imm_W age_18t26 age_65plus edu_lo edu_hi unempl bureauspp leisurepp, by(buurt_vollcode)
regress opkomst z_age_18t26 z_age_65plus, cluster(buurt_vollcode)
estimates store Leeftijd
regress opkomst z_imm_NW z_imm_W
estimates store Immigrantengroepen
regress opkomst z_edu_lo z_edu_hi 
estimates store Opleiding
regress opkomst z_unempl 
estimates store Werkloosheid
regress opkomst z_bureauspp z_leisurepp
estimates store Dichtheid
regress opkomst z_imm_NW z_imm_W z_age_18t26 z_age_65plus z_edu_lo z_edu_hi z_unempl z_bureauspp z_leisurepp
estimates store Allemaal
coefplot Leeftijd Immigrantengroepen Opleiding Werkloosheid Dichtheid Allemaal, drop(_cons) scheme(plotplainblind) xline(0)
graph export "Coefplot GM2018 (buurtcombinaties).png", replace


