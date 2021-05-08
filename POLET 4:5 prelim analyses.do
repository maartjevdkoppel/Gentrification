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
lab var MCparties "% multicultural parties vote"
lab var imm_Sur "% Surinamese"
lab var imm_Ant "% Antillian"
lab var imm_Tur "% Turkish"
lab var imm_Mar "% Moroccan"
lab var imm_otherNW "% Other non-western immigrant"
lab var imm_W "% Western immigrant "
lab var imm_autoch "% autochthonous"
lab var age_18t26 "% 0 to 18 year-olds"
lab var age_18t26 "% 18 to 26 year-olds"
lab var age_18t26 "% 27 to 65 year-olds"
lab var age_66plus "% 66 plus"
lab var unempl "% unemployed"
lab var edu_low "% lower educated"
lab var edu_mid "% medium educated"
lab var edu_hi "% higher educated"
lab var housing_soc "% social housing"
lab var housing_soc_delta2005 "∆ % social housing since 2005"
lab var housing_soc_delta2009 "∆ % social housing since 2006"
lab var housing_soc_delta2013 "∆ % social housing since 2013"
lab var housing_soc_delta "∆ % social housing w.r.t. t-1"
lab var PVDA_delta2006 "∆ % PvdA vote since 2005"
lab var PVDA_delta2010 "∆ % PvdA vote since 2006"
lab var PVDA_delta2014 "∆ % PvdA vote since 2013"
lab var PVDA_delta "∆ % PvdA vote since previous election"

// Encode neighbourhood variable
encode bc_code, gen(c)

********************************************************************************
* EXPLORE THE VARIABLES *
********************************************************************************

* Explore the dependent variable descriptively * 

// Distribution of the outcome
// Overall
hist PVDA, percent col("55 126 184") xlabel(0(10)100) ylabel(, angle(0))width(.3)

// Seperately by neighbourhood
twoway line PVDA year, sort by(c) scheme(plotplain)

// Overall mean
sum PVDA, d

* Between-neighbourhood variation * 

// Calculate overall mean and neighbourhood means
sum PVDA 
gen o_mean = r(mean)
	lab var o_mean "Overall mean"

gen c_mean = .
	lab var c_mean "Neighbourhood mean"
forvalues x = 1/84 {								
	sum PVDA if c == `x'
	replace c_mean = r(mean) if c == `x'
}

// Table for neighbourhood means
asdoc table c, c(mean c_mean) format(%9.2f)

// Plot neighbourhood means and overall mean (fixed-effects logic)
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

********************************************************************************

* Explore the outcome variable in a multilevel framework *

// The most simple model (only for the mean)
xtmixed c.PVDA, mle var
eststo A

// An empty model with random country-deviations from the mean (Step 1 in Hox 2010)
xtmixed c.PVDA || c:, mle var
eststo B
xtmrho
/*Somehow, this model shows the between-cluster variance to be really small, 
approaching 0 (4.45e-22), while the graphs generated above do show that the 
cluster means differ quite a lot. Is this between-cluster variance really so small,
and virtually negliglible compared to the within-cluster (over time) variance*/

// Test for improvement in model fit
lrtest A B 
est clear

********************************************************************************
* ANALYSES *
********************************************************************************

* STANDARD OLS REGRESSION *

* WITHOUT control variables
	// model with housing_soc only
	reg c.PVDA c.housing_soc

	// model with housing_soc_delta only
	reg c.PVDA c.housing_soc_delta

	// model with ∆ variables for both PvdA & social housing
	reg c.PVDA_delta c.housing_soc_delta

	// model with housing_soc & multicultural parties vote (individually)
	reg c.PVDA c.housing_soc c.MCparties

* WITH control variables
	// model with housing_soc + controls
	reg c.PVDA c.housing_soc c.imm_Sur c.imm_Ant c.imm_Tur c.imm_Mar c.imm_otherNW ///
	c.imm_W c.age_18t26 c.age_66plus c.unempl c.edu_low c.edu_high

	// model with housing_soc_delta only
	reg c.PVDA c.housing_soc_delta c.imm_Sur c.imm_Ant c.imm_Tur c.imm_Mar c.imm_otherNW ///
	c.imm_W c.age_18t26 c.age_66plus c.unempl c.edu_low c.edu_high

	// model with ∆ variables for both PvdA & social housing
	reg c.PVDA_delta c.housing_soc_delta c.imm_Sur c.imm_Ant c.imm_Tur c.imm_Mar ///
	c.imm_otherNW c.imm_W c.age_18t26 c.age_66plus c.unempl c.edu_low ///
	c.edu_high

	// model with housing_soc & multicultural parties vote
	reg c.PVDA c.housing_soc c.MCparties c.imm_Sur c.imm_Ant c.imm_Tur c.imm_Mar ///
	c.imm_otherNW c.imm_W c.age_18t26 c.age_66plus c.unempl c.edu_low ///
	c.edu_hi
	
	
* FIXED EFFECTS REGRESSION *

* Using de-meaning
	// Generate fixed effects variable with demeaning 
	gen fe = c_mean - o_mean
	asdoc table bc_name, contents(mean fe)
	
	// Model with fixed effects only
	reg PVDA fe
	
	// Model with housing_soc
	reg PVDA housing_soc fe
	
	// Model with housing_soc_delta
	reg PVDA housing_soc_delta fe
	
	// Model with ∆ variables for both PvdA & social housing
	reg PVDA_delta housing_soc_delta fe
	
	// Model with housing_soc & multicultural parties vote (individually)
	reg PVDA housing_soc MCparties fe
* NOTE: with addition of MC parties var, the effect of social housing virtually disappears
	
	// Ful model with controls 
	reg c.PVDA c.housing_soc c.MCparties c.imm_Sur c.imm_Ant c.imm_Tur c.imm_Mar ///
		c.imm_otherNW c.imm_W c.age_18t26 c.age_66plus c.unempl c.edu_low ///
		c.edu_hi fe
* NOTE: significance and effect of social housing returns
		
* Using dummy variables
	// Model with dummies only 
	reg PVDA i.c
	/*Dummies have quite substantive effect size, but none are statistically 
	significant */
	
	// Model with housing_soc 
	reg PVDA housing_soc i.c.
	
	// Model with housing_soc_delta
	reg PVDA housing_soc_delta i.c.
	
	// Model with ∆ variables for both PvdA & social housing
	reg c.PVDA_delta c.housing_soc_delta i.c

	// Model with housing_soc & multicultural parties vote (individually)
	reg c.PVDA c.housing_soc c.MCparties i.c
	
	// Full model with controls
	reg c.PVDA c.housing_soc c.MCparties c.imm_Sur c.imm_Ant c.imm_Tur c.imm_Mar ///
		c.imm_otherNW c.imm_W c.age_18t26 c.age_66plus c.unempl c.edu_low ///
	    c.edu_hi i.c
	
/*Eelco's regressie: regress opkomst z_imm_NW z_imm_W z_age_18t26 z_age_65plus z_edu_lo z_edu_hi z_unempl z_bureauspp z_leisurepp
