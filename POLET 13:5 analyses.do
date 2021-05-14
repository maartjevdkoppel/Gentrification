*****************************************
**     PvdA / gentrification paper     **
**              4 May 2021             **
*****************************************

********************************************************************************
* PREPARE DATA *
********************************************************************************

* Settings * 
	
	clear
	clear matrix
	set more off
	set seed 20082013
	cap log close
	set scheme plotplain

* Set paths *

	global dir "/Users/Maartje/Desktop/LJA/Paper politicologenetmaal/Analyses"

	global data 	"$dir/data"
	global posted 	"$dir/posted" 	
	global tables 	"$dir/tables"	
	global figures 	"$dir/figures"

* Open data * 

	import delimited "$data/data_sub_merged_long.csv", clear case(preserve) ///
           numericcols(3/34) stringcols(1/2)

* Clean data *

// Label all variables 
	lab var PVDA "% PvdA vote"
	lab var MPP "% M+ vote"
	lab var DENK "% DENK vote"
	lab var BIJ1 "% BIJ1 vote"
	lab var MCparties "% Multicultural parties vote"
	lab var imm_Sur "% Surinamese"
	lab var imm_Ant "% Antillean"
	lab var imm_Tur "% Turkish"
	lab var imm_Mar "% Moroccan"
	lab var imm_otherNW "% Other non-western immigrant"
	lab var imm_W "% Western immigrant "
	lab var imm_autoch "% Autochthonous"
	lab var age_18t26 "% 0 to 18 year-olds"
	lab var age_18t26 "% 18 to 26 year-olds"
	lab var age_66plus "% 66 plus"
	lab var unempl "% Unemployed"
	lab var edu_low "% Lower educated"
	lab var edu_mid "% Medium educated"
	lab var edu_hi "% Higher educated"
	lab var housing_soc "% Social housing"
	lab var housing_soc_delta2005 "∆ % Social housing since 2005"
	lab var housing_soc_delta2009 "∆ % Social housing since 2006"
	lab var housing_soc_delta2013 "∆ % Social housing since 2013"
	lab var housing_soc_delta "Decline in social housing"
	lab var housing_pub_delta "Decline in public housing"
	lab var PVDA_delta2006 "∆ % PvdA vote since 2005"
	lab var PVDA_delta2010 "∆ % PvdA vote since 2006"
	lab var PVDA_delta2014 "∆ % PvdA vote since 2013"
	lab var PVDA_delta "∆ % PvdA vote since previous election"
	lab var WWB "% Recipients unemployment benefits"
	
*TO DO: add new variables

// Encode neighbourhood variable
	encode bc_code, gen(c)


********************************************************************************
* EXPLORE THE VARIABLES *
********************************************************************************

* Explore the need for multilevel modelling *

// Plot between-neighbourhood variation
	sum PVDA 
	gen o_mean = r(mean)
		lab var o_mean "Overall mean"

	gen c_mean = .
		lab var c_mean "Neighbourhood mean"
	forvalues x = 1/84 {								
		sum PVDA if c == `x'
		replace c_mean = r(mean) if c == `x'
	}

	egen pickone = tag(c)
	sort c_mean
	gen c_mean_rank = sum(pickone)
	twoway 				   				   			 ///
		(scatter c_mean c_mean_rank, 				 ///
			ytitle("PVDA vote share") ///
			ylab(10(5)50, angle(0)) 						 ///
			yscale(range(10 50)) ///
			xlab("") 								 ///
			mlab(c) mlabpos(12)) 					 ///
		(line o_mean c) 							 ///
			if pickone == 1
	drop pickone

// Test for model improvement with random intercepts 
	xtmixed c.PVDA, mle var
	eststo A

	xtmixed c.PVDA || c:, mle var
	eststo B
	xtmrho // calculate the VPC: share of variance at level 2 is virtually 0

	lrtest A B // test for model improvement: no significant improvement
	est clear
	
// Perform a Wald test for the addition of dummies 

	reg PVDA i.c
	testparm i.c // test for neighbourhood dummies: non-significant

	reg PVDA i.year
	testparm i.year // test for year dummies: significant
	
********************************************************************************
* Prepare panel data *
********************************************************************************

// Rename panel variable: name 'year' introduces wrong assumptions about time units
	rename year election

// Set data as panel data
	xtset c election, delta(4)

// Create lagged dependent variables
	gen laggedPVDA      = l.PVDA
	gen laggedMCparties = l.MCparties
	
	lab var laggedPVDA      "% PvdA vote at previous election"
	lab var laggedMCparties "% Multicultural parties vote at previous election"

// Save panel data
	save "$posted/data_sub_merged_long_panel", replace

********************************************************************************
* GENTRIFICATION 1: ∆ % social housing *
********************************************************************************
	
// Remove missings on main predictor
	keep if !missing(housing_soc_delta)
	
* Predict support for the PvdA: OLS with time dummies and LDV *

// Model 1: gentrification
	reg PVDA housing_soc_delta
	eststo PVDA_M1
	
// Model 1A: add lagged dependent variable
	reg PVDA housing_soc_delta laggedPVDA
	eststo PVDA_M1A

// Model 1B: add time dummies
	reg PVDA housing_soc_delta laggedPVDA i.election
	eststo PVDA_M1B
				   
// Model 2: add control variables 
	reg PVDA housing_soc_delta laggedPVDA i.election imm_Sur imm_Ant imm_Tur ///
	    imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB
	eststo PVDA_M2
		   
// Model 2A: add control variables + education
	reg PVDA housing_soc_delta laggedPVDA i.election imm_Sur imm_Ant imm_Tur ///
	    imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB edu_low edu_high 
	eststo PVDA_M2A
	
// Export regression table: Model 1B and 2
	esttab PVDA_M1B PVDA_M2 using "$tables/Gentrification-1-PvdA.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") replace
	
* Predict support for multicultural parties: 2018 only *

// Model 1: gentrification (2014 & 2018)
	reg MCparties housing_soc_delta
	eststo MC_M1

// Model 1A: add lagged dependent variable (2018 only)
	reg MCparties housing_soc_delta laggedMCparties
	eststo MC_M1A
	
// Model 2: add control variables (including education) (2018 only)
	reg MCparties housing_soc_delta laggedMCparties imm_Sur imm_Ant imm_Tur ///
	    imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB edu_low edu_high 
	eststo MC_M2
	
// Export regression table: Model 1A and 2
	esttab MC_M1A MC_M2 using "$tables/Gentrification-1-MCparties.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") replace
		   
********************************************************************************
* GENTRIFICATION 2: ∆ % corporation-owned housing (2018 only) *
********************************************************************************

// Restore data from before first analysis 
	use "$posted/data_sub_merged_long_panel", clear
	
// Remove missings on main predictor
	keep if !missing(housing_pub_delta)
	
* Predict support for the PvdA *

// Model 1: gentrification
	reg PVDA housing_pub_delta
	eststo PVDA_P_M1
		   
// Model 2A: add control variables (including education)
	reg PVDA housing_pub_delta imm_Sur imm_Ant imm_Tur ///
	    imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB edu_low edu_high 
	eststo PVDA_P_M2
	
// Export regression table: Model 1B and 2
	esttab PVDA_P_M1 PVDA_P_M2 using "$tables/Gentrification-2-PvdA.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") replace
		   
* Predict support for multicultural parties *

// Model 1: gentrification
	reg MCparties housing_pub_delta
	eststo MC_P_M1
		   
// Model 2A: add control variables (including education)
	reg MCparties housing_pub_delta imm_Sur imm_Ant imm_Tur ///
	    imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB edu_low edu_high 
	eststo MC_P_M2
	
// Export regression table: Model 1B and 2
	esttab MC_P_M1 MC_P_M2 using "$tables/Gentrification-2-MC.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") replace
		   
	   		   
********************************************************************************
* PREDICTING TURNOUT *
********************************************************************************

* Prepare data - data and do-file from Eelco * 

// Compile data: PS2019
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


// Compile data: GM2018
	cd "/Users/Maartje/Desktop/LJA/Data POLetmaal/Data Eelco"
	use "uitslagen GM2018.dta", clear
	merge m:1 postcode using "koppeltabel postcode naar buurt.dta"
	merge m:1 buurt_vollcode using "buurtkenmerken Amsterdam.dta", gen(merge_kenm)
	merge m:1 buurt_vollcode using "buurtkenmerken Amsterdam - 2016.dta", gen(merge16)
	capture drop Pregwerkl_p
	merge m:1 buurt_vollcode using "buurtkenmerken Amsterdam - 2017.dta", gen(merge17)
	keep if lokaal!=""

	gen opkomst = (geldigestembiljetten/opgeroepenen)*100

	
// Add gentrification variables (alleen op wijkniveau beschikbaar)

	
	

* Combine and recode data* 

// Leisure organizations
	merge m:1 buurt_vollcode using "leisure.dta", gen(merge_leisure)
	gen leisurepp = leisure/Bevtotaal

// Stembureau density
	bys buurt_vollcode: gen bureaus = _N
	gen bureauspp = bureaus/Bevtotaal
	recode bureauspp (2=.)

// Recode data: IVs
	rename Bevtotaal bevolking
	rename (BevSur_p BevAntil_p BevTurk_p BevMarok_p) (imm_Sur imm_Ant imm_Tur imm_Mar)
	rename (BevovNW_p BevWest_p BevAutoch_p) (imm_NW imm_W imm_autoch) 
	rename (Bev0_18_p Bev18_26p Bev27_65p Bev66plus_p) (age_0t18 age_18t26 age_27t65 age_65plus)
	rename (Bevopllaag_p-Bevoplhoog_p) (edu_lo edu_mid edu_hi)
	rename Pregwerkl_p unempl

// HHI
	egen hhi = rowtotal(imm_Sur imm_Ant imm_Tur imm_Mar)

// Standardize
	*foreach var of varlist imm_* age_18t26 age_65plus edu_lo edu_hi unempl leisurepp bureauspp {
		*egen z_`var' = std(`var')
		*}
	
// Stadsdeel
	encode sd, gen(stadsdeel)
	lab define stadsdeel2 1 "Centrum" 2 "West" 3 "Nieuw-West" 4 "Zuid" 5 "Oost" 6 "Noord" 7 "Zuidoost"
	lab values stadsdeel stadsdeel2

// Labels
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
	lab var bureauspp "Aantal stembureaus (p.p.)"
	lab var leisurepp "Vrijetijdsorganisaties (p.p.)"

	cd  "/Users/Maartje/Desktop/LJA/Data POLetmaal/Data Eelco"

* Add gentrification variables to data 
	
