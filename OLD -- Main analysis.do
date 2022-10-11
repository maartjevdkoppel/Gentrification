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

	global dir "/Users/Maartje/Desktop/LJA/Paper politicologenetmaal/Analyses"

	global data 	"$dir/data"
	global posted 	"$dir/posted" 	
	global tables 	"$dir/tables"	
	global figures 	"$dir/figures"

* Open data * 

	import delimited "$data/data_sub_merged_long 2.csv", clear case(preserve) ///
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
	lab var housing_soc_delta "∆ % Social housing (t-1)"
	lab var housing_pub_delta "∆ % Public housing (t-1)"
	lab var PVDA_delta2006 "∆ % PvdA vote since 2005"
	lab var PVDA_delta2010 "∆ % PvdA vote since 2006"
	lab var PVDA_delta2014 "∆ % PvdA vote since 2013"
	lab var PVDA_delta "∆ % PvdA vote since previous election"
	lab var unempl "% Recipients unemployment benefits"
	lab var turnout "Turnout"

// Encode neighbourhood variable
	encode bc_code, gen(c)

// Change housing_pub_delta from string to numeric
	gen housing_pub_delta1 = real(housing_pub_delta)
	drop housing_pub_delta
	rename housing_pub_delta1 housing_pub_delta
	lab var housing_pub_delta "∆ % Public housing (t-1)"
	
	gen netincomedelta1 = real(netincomedelta)
	drop netincomedelta
	rename netincomedelta1 netincomedelta
	lab var netincomedelta "∆ Net income per household (t-1)"
	
	gen newbuildingsdelta1 = real(newbuildingsdelta)
	drop newbuildingsdelta
	rename newbuildingsdelta1 newbuildingsdelta
	lab var newbuildingsdelta "∆ % Share of new buildings (t-1)"
	
	gen housing_soc_delta20051 = real(housing_soc_delta2005)
	drop housing_soc_delta2005
	rename housing_soc_delta20051 housing_soc_delta2005
	lab var housing_soc_delta2005 "∆ % Social housing since 2005"
	
	gen housing_soc_delta20091 = real(housing_soc_delta2009)
	drop housing_soc_delta2009
	rename housing_soc_delta20091 housing_soc_delta2009
	lab var housing_soc_delta2009 "∆ % Social housing since 2009"
	
	gen housing_soc_delta20131 = real(housing_soc_delta2013)
	drop housing_soc_delta2013
	rename housing_soc_delta20131 housing_soc_delta2013
	lab var housing_soc_delta2013 "∆ % Social housing since 2013"
	
	gen housing_soc_delta1 = real(housing_soc_delta)
	drop housing_soc_delta
	rename housing_soc_delta1 housing_soc_delta
	lab var housing_soc_delta "∆ % Social housing (t-1)"
	
	gen GLdelta20101 = real(GLdelta2010)
	drop GLdelta2010
	rename GLdelta20101 GLdelta2010
	lab var GLdelta2010 "∆ % GroenLinks vote since 2010"
	
	gen GLdelta20141 = real(GLdelta2014)
	drop GLdelta2014
	rename GLdelta20141 GLdelta2014
	lab var GLdelta2014 "∆ % GroenLinks vote since 2014"
	
	gen PVDA_delta20061 = real(PVDA_delta2006)
	drop PVDA_delta2006
	rename PVDA_delta20061 PVDA_delta2006
	lab var PVDA_delta2006 "∆ % PvdA vote since 2006"
	
	gen PVDA_delta20101 = real(PVDA_delta2010)
	drop PVDA_delta2010
	rename PVDA_delta20101 PVDA_delta2010
	lab var PVDA_delta2010 "∆ % PvdA vote since 2010"
	
	gen PVDA_delta20141 = real(PVDA_delta2014)
	drop PVDA_delta2014
	rename PVDA_delta20141 PVDA_delta2014
	lab var PVDA_delta2014 "∆ % PvdA vote since 2014"
	
	gen PVDA_delta1 = real(PVDA_delta)
	drop PVDA_delta
	rename PVDA_delta1 PVDA_delta
	lab var PVDA_delta "∆ % PvdA vote since last election"
	
	
	
* Check whether manually combined neighbourhoods are significantly different *

	ttest PVDA,              by(bc_combined)
	ttest MCparties,         by(bc_combined)
	ttest turnout,           by(bc_combined)
	ttest housing_soc_delta, by(bc_combined)
	ttest housing_pub_delta, by(bc_combined) // significant difference
	ttest newbuildingsdelta, by(bc_combined)
	ttest netincomedelta,    by(bc_combined)
	ttest imm_Sur,           by(bc_combined)
	ttest imm_Ant,           by(bc_combined)
	ttest imm_Tur,           by(bc_combined) // significant difference
	ttest imm_Mar,           by(bc_combined) // significant difference
	ttest imm_otherNW,       by(bc_combined)
	ttest imm_W,             by(bc_combined) // significant difference
	ttest imm_Ant,           by(bc_combined)
	ttest age_18t26,         by(bc_combined)
	ttest age_66plus,        by(bc_combined)
	ttest unempl,               by(bc_combined) // significant difference
	ttest edu_low,           by(bc_combined)
	ttest edu_high,          by(bc_combined)

* Check for multicollinearity gentrification variables *

	correlate housing_soc_delta housing_pub_delta newbuildingsdelta netincomedelta
	
	
********************************************************************************
* Explore the need for multilevel modelling **
********************************************************************************

// Test for model improvement with random intercepts 
	xtmixed c.PVDA, mle var
	eststo A

	xtmixed c.PVDA || c:, mle var
	eststo B
	xtmrho // calculate the VPC: share of variance at level 2 is virtually 0

	lrtest A B // test for model improvement: no significant improvement
	est clear
	
	
	xtmixed c.turnout, mle var
	eststo C

	xtmixed c.turnout || c:, mle var
	eststo D
	xtmrho // calculate the VPC: 0.70084

	lrtest C D // test for model improvement: significant improvement
	est clear
	
	
	xtmixed c.MCparties, mle var
	eststo E

	xtmixed c.MCparties || c:, mle var
	eststo F
	xtmrho // calculate the VPC: 0.10934

	lrtest E F // test for model improvement: no significant improvement
	est clear

	
// Perform Wald tests for the addition of dummies 
	reg PVDA i.c
	testparm i.c // test for neighbourhood dummies: non-significant

	reg PVDA i.year
	testparm i.year // test for year dummies: significant
	
	reg turnout i.c
	testparm i.c // test for neighbourhood dummies: significant
	
	reg turnout i.year
	testparm i.year // test for year dummies: non-significant
	
	reg MCparties i.c
	testparm i.c // test for neighbourhood dummies: non-significant
	
	reg MCparties i.year
	testparm i.year // test for year dummies: significant
	
	
********************************************************************************
* Prepare panel data *
********************************************************************************

// Rename panel variable: name 'year' introduces wrong assumptions about time units
	rename year election

// Set data as panel data
	xtset c election, delta(4)

// Create lagged dependent variables (NO LONGER NEEDED)
	*gen laggedPVDA      = l.PVDA
	*gen laggedMCparties = l.MCparties
	*gen laggedTURN      = l.turnout
	
	*lab var laggedPVDA      "% PvdA vote at previous election"
	*lab var laggedMCparties "% Multicultural parties vote at previous election"
	*lab var laggedTURN      "Turnout at previous election"

// Save panel data
	save "$posted/data_sub_merged_long_panel", replace

	
********************************************************************************
* GENTRIFICATION - all indicators (TEMPORARY, clean up this code!)
********************************************************************************
	
// Remove missings on main predictor
	keep if !missing(housing_soc_delta)
	keep if !missing(netincomedelta)
	keep if !missing(newbuildingsdelta)	
	
* Predict support for the PvdA: OLS with time dummies (2010-2018) *

// Model 1: gentrification
	reg PVDA housing_soc_delta netincomedelta newbuildingsdelta
	eststo PVDA_temp1
	
// Model 1B: add time dummies
	reg PVDA housing_soc_delta netincomedelta newbuildingsdelta i.election
	eststo PVDA_temp1B
				   
// Model 2: add control variables 
	reg PVDA housing_soc_delta netincomedelta newbuildingsdelta i.election ///
	    imm_Sur imm_Ant imm_Tur imm_Mar imm_otherNW imm_W age_18t26 ///
		age_66plus unempl
	eststo PVDA_temp2
		   
// Model 2A: add control variables + education
	reg PVDA housing_soc_delta netincomedelta newbuildingsdelta i.election ///
	    imm_Sur imm_Ant imm_Tur imm_Mar imm_otherNW imm_W age_18t26 ///
		age_66plus unempl edu_low edu_high 
	eststo PVDA_temp2A
	
// Export regression table: Model temp1B and temp2
	esttab PVDA_temp1B PVDA_temp2 using "$tables/Newgentvars-PvdA.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") ///
		   title("Fixed effects model for PvdA support (2010-2018, all gent. variables)") ///
		   replace	
		   

* Predict change in support for PvdA (between 2010 and 2018) *
	
// Change ∆2010 to 2010-2018 only 
	gen PVDA_delta20102018 = PVDA_delta2010 if election == 2018
	
// Model 1: gentrification variables
	reg PVDA_delta20102018 housing_soc_delta netincomedelta newbuildingsdelta
	eststo PVDA_change_temp1
	
// Model 2: add control variables (including education)	
	reg PVDA_delta20102018 housing_soc_delta netincomedelta newbuildingsdelta ///
		imm_Sur imm_Ant imm_Tur imm_Mar imm_otherNW imm_W age_18t26 age_66plus ///
		unempl edu_low edu_high
	eststo PVDA_change_temp2
	
// Export regression table
	esttab PVDA_change_temp1 PVDA_change_temp2 using "$tables/Newgentvars-changePVDA.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") ///
		   title("OLS model for change in PvdA support (between 2010 and 2018, all gent. variables)") ///
		   replace	
		   
	

* Predict voter turnout: OLS with neighbourhood fixed effects (2010-2018) * 

// Generate neighbourhood fixed effects with de-meaning
	sum turnout  // calculate overall mean and neighbourhood means
	gen o_mean = r(mean)
		lab var o_mean "Overall mean"

	gen c_mean = .
		lab var c_mean "Neighbourhood mean"
	forvalues x = 1/84 {								
		sum turnout if c == `x'
		replace c_mean = r(mean) if c == `x'
	}

	gen fe = c_mean - o_mean // create fixed effects variable

// Model 1: gentrification
	reg turnout housing_soc_delta netincomedelta newbuildingsdelta
	eststo TURN_temp1
	
// Model 1A: add lagged dependent variable
	*reg turnout housing_soc_delta laggedTURN
	*eststo TURN_M1A

// Model 1B: add neighbourhood fixed effects 
	reg turnout housing_soc_delta netincomedelta newbuildingsdelta fe
	eststo TURN_temp1B
				   
// Model 2: add control variables 
	reg turnout housing_soc_delta netincomedelta newbuildingsdelta fe imm_Sur ///
		imm_Ant imm_Tur imm_Mar imm_otherNW imm_W age_18t26 age_66plus unempl
	eststo TURN_temp2
		   
// Model 2A: add control variables + education
	reg turnout housing_soc_delta netincomedelta newbuildingsdelta fe imm_Sur ///
		imm_Ant imm_Tur imm_Mar imm_otherNW imm_W age_18t26 age_66plus unempl ///
		edu_low edu_high 
	eststo TURN_temp2A
	
// Export regression table: Model 1B and 2
	esttab TURN_temp1B TURN_temp2 using "$tables/Newgentvars-turnout.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") ///
		   title("Fixed effects model for turnout (2010-2018, all gent. variables)") ///
		   replace

		   
* Predict support for multicultural parties: 2018 only *

// Only consider multicultural party support for 2018: DENK & BIJ1
	gen MCparties18 = MCparties if election == 2018

// Model 1: gentrification (2014 & 2018)
	reg MCparties18 housing_soc_delta netincomedelta newbuildingsdelta
	eststo MC18_temp1

// Model 2: add control variables (including education) (2018 only)
	reg MCparties18 housing_soc_delta netincomedelta newbuildingsdelta ///
		imm_Sur imm_Ant imm_Tur imm_Mar imm_otherNW imm_W age_18t26 age_66plus ///
		unempl edu_low edu_high 
	eststo MC18_temp2
	
// Export regression table: Model 1A and 2
	esttab MC18_temp1 MC18_temp2 using "$tables/Newgentvars-MCparties18.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") ///
		   title("OLS model for multicultural party support (2018, all gent. variables)") ///
		   replace
	
	
********************************************************************************
* GENTRIFICATION 1: ∆ % social housing *
********************************************************************************
	
// Remove missings on main predictor
	keep if !missing(housing_soc_delta)
	
* Predict support for the PvdA: OLS with time dummies (2010-2018) *

// Model 1: gentrification
	reg PVDA housing_soc_delta
	eststo PVDA_M1
	
// Model 1A: add lagged dependent variable
	*reg PVDA housing_soc_delta laggedPVDA
	*eststo PVDA_M1A
	
// Model 1B: add time dummies
	reg PVDA housing_soc_delta i.election
	eststo PVDA_M1B
				   
// Model 2: add control variables 
	reg PVDA housing_soc_delta i.election imm_Sur imm_Ant imm_Tur ///
	    imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB
	eststo PVDA_M2
		   
// Model 2A: add control variables + education
	reg PVDA housing_soc_delta i.election imm_Sur imm_Ant imm_Tur ///
	    imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB edu_low edu_high 
	eststo PVDA_M2A
	
// Export regression table: Model 1B and 2
	esttab PVDA_M1B PVDA_M2 using "$tables/Gentrification-1-PvdA.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") replace
	
	
* Predict voter turnout: OLS with neighbourhood fixed effects (2010-2018) * 

// Generate neighbourhood fixed effects with de-meaning
	sum turnout  // calculate overall mean and neighbourhood means
	gen o_mean = r(mean)
		lab var o_mean "Overall mean"

	gen c_mean = .
		lab var c_mean "Neighbourhood mean"
	forvalues x = 1/84 {								
		sum turnout if c == `x'
		replace c_mean = r(mean) if c == `x'
	}

	gen fe = c_mean - o_mean // create fixed effects variable

// Model 1: gentrification
	reg turnout housing_soc_delta
	eststo TURN_M1
	
// Model 1A: add lagged dependent variable
	*reg turnout housing_soc_delta laggedTURN
	*eststo TURN_M1A

// Model 1B: add neighbourhood fixed effects 
	reg turnout housing_soc_delta fe
	eststo TURN_M1B
				   
// Model 2: add control variables 
	reg turnout housing_soc_delta fe imm_Sur imm_Ant imm_Tur ///
	    imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB
	eststo TURN_M2
		   
// Model 2A: add control variables + education
	reg turnout housing_soc_delta fe imm_Sur imm_Ant imm_Tur ///
	    imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB edu_low edu_high 
	eststo TURN_M2A
	
// Export regression table: Model 1B and 2
	esttab TURN_M1B TURN_M2 using "$tables/Gentrification-1-turnout.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") replace

	
* Predict support for multicultural parties: 2014 & 2018 only, time dummies *

// Model 1: gentrification (2014 & 2018)
	reg MCparties housing_soc_delta
	eststo MC_M1
	
// Model 1A: add time dummies  
	reg MCparties housing_soc_delta i.election
	eststo MC_M1A
	
// Model 2: add control variables (including education) (2018 only)
	reg MCparties housing_soc_delta i.election imm_Sur imm_Ant imm_Tur ///
	    imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB edu_low edu_high 
	eststo MC_M2
	
// Export regression table: Model 1A and 2
	esttab MC_M1A MC_M2 using "$tables/Gentrification-1-MCparties.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") replace
		   
* Predict support for multicultural parties: 2018 only *

// Only consider multicultural party support for 2018: DENK & BIJ1
	gen MCparties18 = MCparties if election == 2018

// Model 1: gentrification (2014 & 2018)
	reg MCparties18 housing_soc_delta
	eststo MC_M1_18

// Model 2: add control variables (including education) (2018 only)
	reg MCparties18 housing_soc_delta imm_Sur imm_Ant imm_Tur ///
	    imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB edu_low edu_high 
	eststo MC_M2_18
	
// Export regression table: Model 1A and 2
	esttab MC_M1_18 MC_M2_18 using "$tables/Gentrification-1-MCparties18.rtf", ///
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

		   
* Predict turnout *

// Model 1: gentrification
	reg turnout housing_pub_delta
	eststo TURN_P_M1
		   
// Model 2A: add control variables (including education)
	reg turnout housing_pub_delta imm_Sur imm_Ant imm_Tur ///
	    imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB edu_low edu_high 
	eststo TURN_P_M2
	
// Export regression table: Model 1B and 2
	esttab TURN_P_M1 TURN_P_M2 using "$tables/Gentrification-2-turnout.rtf", ///
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
* BOTH GENTRIFICATION INDICATORS + CHANGE IN PVDA SUPPORT					   *
********************************************************************************

// Change delta variables from string to numeric
	gen PVDA_delta2014_1 = real(PVDA_delta2014)
	drop PVDA_delta2014
	rename PVDA_delta2014_1 PVDA_delta2014
	
	gen PVDA_delta2010_1 = real(PVDA_delta2010)
	drop PVDA_delta2010
	rename PVDA_delta2010_1 PVDA_delta2010
	
* Predict change in support for PvdA (2014-2018) *

// Model 1: gentrification variables
	reg PVDA_delta2014 housing_pub_delta housing_soc_delta
	eststo PVDA_change_M1
	
// Model 2: add control variables (including education)	
	reg PVDA_delta2014 housing_pub_delta housing_soc_delta imm_Sur imm_Ant ///
	    imm_Tur imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB edu_low ///
		edu_high 
	eststo PVDA_change_M2

// Export regression table
	esttab PVDA_change_M1 PVDA_change_M2 using "$tables/PvdAchange2014-2018.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") replace

* Predict change in support for PvdA (2010-2018) *
	
// Change ∆2010 to 2010-2018 only 
	gen PVDA_delta20102018 = PVDA_delta2010 if election == 2018
	
// Model 1: gentrification variables
	reg PVDA_delta20102018 housing_pub_delta housing_soc_delta
	eststo PVDA_change_M3
	
// Model 2: add control variables (including education)	
	reg PVDA_delta20102018 housing_pub_delta housing_soc_delta imm_Sur  ///
	    imm_Ant imm_Tur imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB  ///
		edu_low edu_high
	eststo PVDA_change_M4
	
// Export regression table
	esttab PVDA_change_M3 PVDA_change_M4 using "$tables/PvdAchange2010-2018.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") replace

********************************************************************************
* BOTH GENTRIFICATION INDICATORS + BIJ1, DENK SEPARATE			         	   *
********************************************************************************

// Restore data from before first analysis 
	use "$posted/data_sub_merged_long_panel", clear
	
// Remove missings on main predictors
	keep if !missing(housing_pub_delta)
	keep if !missing(housing_soc_delta)
	
* Predict support for BIJ1 (2018) *

// Model 1: gentrification variables
	reg BIJ1 housing_pub_delta housing_soc_delta
	eststo BIJ1_M1
	
// Model 2: add control variables (including education)	
	reg BIJ1 housing_pub_delta housing_soc_delta imm_Sur  ///
	    imm_Ant imm_Tur imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB  ///
		edu_low edu_high
	eststo BIJ1_M2
	
// Export regression table
	esttab BIJ1_M1 BIJ1_M2 using "$tables/BIJ12018.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") replace

		   
* Predict support for DENK (2018) *
		   

// Model 1: gentrification variables
	reg DENK housing_pub_delta housing_soc_delta
	eststo DENK_M1
	
// Model 2: add control variables (including education)	
	reg DENK housing_pub_delta housing_soc_delta imm_Sur  ///
	    imm_Ant imm_Tur imm_Mar imm_otherNW imm_W age_18t26 age_66plus WWB  ///
		edu_low edu_high
	eststo DENK_M2
	
// Export regression table
	esttab DENK_M1 DENK_M2 using "$tables/DENK2018.rtf", ///
	       b(%5.3f) se(%5.3f) ar2(3) obslast label mlabels(none) ar2 ///
           addnotes("Note. Data from OIS Amsterdam, own adaption") replace	   		   

	
