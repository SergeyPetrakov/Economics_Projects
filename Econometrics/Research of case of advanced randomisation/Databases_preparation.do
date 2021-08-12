clear all
set matsize 11000
set maxvar 20000

** Name: Databases_preparation.do
** Date Created: 10/23/2017 by Vincent Pons (vpons@hbs.edu)
** Last Updated: 

* Data In: [randomization_precinct&municipality  & results_pr12t1  & results_pr07t1  & results_pr02t1 & results_eur14 & activists_reports & activists_survey & insee_population_11 & insee_activity_11 & insee_income_11 & insee_population_06 & insee_activity_06 ]
* Data Out: [analysis]

* Purpose of do-file: Prepare database used to generate results

* Organization :
/* 
*** PART I *** Create sample databases
*** PART II *** Work on electoral results
*** PART III *** Work on activists' reports
*** PART IV *** Work on postelectoral survey
*** PART V *** Work on Insee census data
*** PART VI *** Merge randomization database with other databases
*/

* Setting file path

cd "${data}"

****************************************
*** PART I *** Create sample databases
****************************************

** full sample

use "Intermediate\randomization_precinct&municipality", clear

keep municipality_code precinct_code municipality precinct departement_code territory level_randomization territory_excluded sample
save "Intermediate\sample", replace


** sample municipalities

so municipality_code
by municipality_code: gen id = _n
keep if id == 1 
drop id precinct_code precinct
save "Intermediate\sample_municipalities", replace


** sample territories

so departement_code territory
by departement_code territory: gen id = _n
keep if id == 1
drop id municipality_code municipality
save "Intermediate\sample_territories", replace

****************************************
*** PART II *** Work on electoral results
****************************************

** 2012: merge all rounds together; generate municipality-level outcomes

* merge electoral rounds of 2012

use "Original\results_pr12t1", clear

merge 1:1 municipality_code precinct_code using "Original\results_pr12t2"
drop _merge
merge 1:1 municipality_code precinct_code using "Original\results_leg12t1"
drop _merge
merge 1:1 municipality_code precinct_code using "Original\results_leg12t2"
drop _merge

* check consistency between constituency_leg12t1 and constituency_leg12t2

count if constituency_leg12t1 ~= constituency_leg12t2 & constituency_leg12t2 ~= ""
drop constituency_leg12t2
rename constituency_leg12t1 constituency_leg12

* merge with database listing municipalities in the sample; drop excluded territories and municipalities not in the sample

merge m:1 municipality_code using "Intermediate\sample_municipalities"
drop if _merge ~= 3 | sample ~= 1
drop _merge

* define municipality-level variables by summing over the municipality's precincts

foreach var of varlist nb_registered_pr12t1 - nb_hollande_pr12t1 nb_registered_pr12t2 - nb_sarkozy_pr12t2 nb_registered_leg12t1 - nb_vec_leg12t1 nb_registered_leg12t2 - nb_vec_leg12t2{
so municipality_code
by municipality_code: egen `var'_mun = total(`var')
}

* keep 1 obs per municipality in territories where randomization was conducted at municipality level

so municipality_code 
by municipality_code: gen id = _n
drop if id ~= 1 & level_randomization ~= 1
drop id
foreach var of varlist nb_registered_pr12t1 - nb_hollande_pr12t1 nb_registered_pr12t2 - nb_sarkozy_pr12t2 nb_registered_leg12t1 - nb_vec_leg12t1 nb_registered_leg12t2 - nb_vec_leg12t2{
replace `var' = . if level_randomization ~= 1
}
replace precinct_code = "" if level_randomization ~= 1

* define all variables at the level of randomization

foreach var of varlist nb_registered_pr12t1 - nb_hollande_pr12t1 nb_registered_pr12t2 - nb_sarkozy_pr12t2 nb_registered_leg12t1 - nb_vec_leg12t1 nb_registered_leg12t2 - nb_vec_leg12t2{
rename `var' `var'_an
replace `var'_an = `var'_mun if level_randomization ~= 1
drop `var'_mun
}
drop departement_code territory municipality level_randomization territory_excluded sample 

save "Intermediate\results_12", replace


** 2007: merge all rounds together; generate municipality-level outcomes

* merge electoral rounds of 2007

use "Original\results_pr07t1", clear

merge 1:1 municipality_code precinct_code using "Original\results_pr07t2"
drop _merge

* merge with database listing municipalities in the sample; drop excluded territories and municipalities not in the sample

merge m:1 municipality_code using "Intermediate\sample_municipalities"
drop if _merge ~= 3 | sample ~= 1
drop _merge

* define municipality-level variables by summing over the municipality's precincts

foreach var of varlist nb_registered_pr07t1 - nb_devilliers_pr07t1 nb_registered_pr07t2 - nb_sarkozy_pr07t2{
so municipality_code
by municipality_code: egen `var'_mun = total(`var')
}

* keep 1 obs per municipality in territories where randomization was conducted at municipality level

so municipality_code 
by municipality_code: gen id = _n
drop if id ~= 1 & level_randomization ~= 1
drop id
foreach var of varlist nb_registered_pr07t1 - nb_devilliers_pr07t1 nb_registered_pr07t2 - nb_sarkozy_pr07t2{
replace `var' = . if level_randomization ~= 1
}
replace precinct_code = "" if level_randomization ~= 1

* define all variables at the level of randomization

foreach var of varlist nb_registered_pr07t1 - nb_devilliers_pr07t1 nb_registered_pr07t2 - nb_sarkozy_pr07t2{
rename `var' `var'_an
replace `var'_an = `var'_mun if level_randomization ~= 1
drop `var'_mun
}
drop departement_code territory municipality level_randomization territory_excluded sample 

save "Intermediate\results_07", replace


** 2002: merge all rounds together; generate municipality-level outcomes

* merge electoral rounds of 2002

use "Original\results_pr02t1", clear

merge 1:1 municipality_code precinct_code using "Original\results_pr02t2"
drop _merge

* merge with database listing municipalities in the sample; drop excluded territories and municipalities not in the sample

merge m:1 municipality_code using "Intermediate\sample_municipalities"
drop if _merge ~= 3 | sample ~= 1
drop _merge

* define municipality-level variables by summing over the municipality's precincts

foreach var of varlist nb_registered_pr02t1 - nb_taubira_pr02t1 nb_registered_pr02t2 - nb_lepen_pr02t2{
so municipality_code
by municipality_code: egen `var'_mun = total(`var')
}

* keep 1 obs per municipality in territories where randomization was conducted at municipality level

so municipality_code 
by municipality_code: gen id = _n
drop if id ~= 1 & level_randomization ~= 1
drop id
foreach var of varlist nb_registered_pr02t1 - nb_taubira_pr02t1 nb_registered_pr02t2 - nb_lepen_pr02t2{
replace `var' = . if level_randomization ~= 1
}
replace precinct_code = "" if level_randomization ~= 1

* define all variables at the level of randomization

foreach var of varlist nb_registered_pr02t1 - nb_taubira_pr02t1 nb_registered_pr02t2 - nb_lepen_pr02t2{
rename `var' `var'_an
replace `var'_an = `var'_mun if level_randomization ~= 1
drop `var'_mun
}
drop departement_code territory municipality level_randomization territory_excluded sample 

save "Intermediate\results_02", replace


** 2014: generate municipality-level outcomes

use "Original\results_eur14", clear

* merge with database listing municipalities in the sample; drop excluded territories and municipalities not in the sample

merge m:1 municipality_code using "Intermediate\sample_municipalities"
drop if _merge ~= 3 | sample ~= 1
drop _merge

* define municipality-level variables by summing over the municipality's precincts

foreach var of varlist nb_registered_eur14 - nb_vec_eur14{
so municipality_code
by municipality_code: egen `var'_mun = total(`var')
}

* keep 1 obs per municipality in territories where randomization was conducted at municipality level

so municipality_code 
by municipality_code: gen id = _n
drop if id ~= 1 & level_randomization ~= 1
drop id
foreach var of varlist nb_registered_eur14 - nb_vec_eur14{
replace `var' = . if level_randomization ~= 1
}
replace precinct_code = "" if level_randomization ~= 1

* define all variables at the level of randomization

foreach var of varlist nb_registered_eur14 - nb_vec_eur14{
rename `var' `var'_an
replace `var'_an = `var'_mun if level_randomization ~= 1
drop `var'_mun
}
drop departement_code territory municipality level_randomization territory_excluded sample 

save "Intermediate\results_14", replace


****************************************
*** PART III *** Work on activists' reports: identify reports which indicate the precinct covered; compute the number of doors knocked in each precinct
****************************************

use "Original\activists_reports", clear

** merge with sample database; drop reports for precincts / municipalities not in the sample

merge m:1 departement_code territory municipality precinct using "Intermediate\sample"
drop if _merge ~= 3 | sample ~= 1
drop _merge


** identify reports which indicate the precinct covered (resp. the municipality, in territories randomized at the municipality level)
/* All reports successfully merged on territory x municipality x precinct verify this condition*/

gen report_indicating_precinct = ((level_randomization == 0 & municipality ~= "") | (level_randomization == 1 & precinct ~= ""))
label var report_indicating_precinct "door-to-door report indicating the precinct covered"


** compute aggregate number of doors knocked in each precinct (resp. municipality): in total, before first round, and between the 2 rounds

gen nb_doors_knocked_1 = nb_doors_knocked if report_date ~= . & report_date <= 19105
gen nb_doors_knocked_2 = nb_doors_knocked if report_date ~= . & report_date > 19105

so departement_code territory municipality
by departement_code territory municipality: egen temp0 = total(nb_doors_knocked)
by departement_code territory municipality: egen temp0_1 = total(nb_doors_knocked_1)
by departement_code territory municipality: egen temp0_2 = total(nb_doors_knocked_2)
by departement_code territory municipality: gen id0 =_n
so departement_code territory municipality precinct
by departement_code territory municipality precinct: egen temp1 = total(nb_doors_knocked)
by departement_code territory municipality precinct: egen temp1_1 = total(nb_doors_knocked_1)
by departement_code territory municipality precinct: egen temp1_2 = total(nb_doors_knocked_2)
by departement_code territory municipality precinct: gen id1 =_n
gen total_nb_doors_knocked = temp0 if level_randomization == 0
replace total_nb_doors_knocked = temp1 if level_randomization == 1
gen total_nb_doors_knocked_1 = temp0_1 if level_randomization == 0
replace total_nb_doors_knocked_1 = temp1_1 if level_randomization == 1
gen total_nb_doors_knocked_2 = temp0_2 if level_randomization == 0
replace total_nb_doors_knocked_2 = temp1_2 if level_randomization == 1
label var total_nb_doors_knocked "total # doors knocked in precinct"
label var total_nb_doors_knocked_1 "total # doors knocked in precinct before 1st round"
label var total_nb_doors_knocked_2 "total # doors knocked in precinct between 1st and 2nd round"


** keep 1 observation per precinct (resp. municipality)

keep if (level_randomization == 0 & id0 == 1) | (level_randomization == 1 & id1 == 1)
drop temp0 temp1 id0 id1

keep departement_code territory municipality precinct total_nb_doors_knocked total_nb_doors_knocked_1 total_nb_doors_knocked_2 report_indicating_precinct

save "Intermediate\activists_reports_v2", replace


****************************************
*** PART IV *** Work on postelectoral survey: obtain descriptive statistics and identify territories in which at least one respondent mentions that she or her unit used the list of allocated precincts
****************************************

use "Original\activists_survey", clear

** drop survey respondents who report that they did not participate in the door-to-door canvassing campaign

drop if canvassing_frequency == 4


** obtain descriptive statistics (Table 1)

foreach var in age role_campaign role_ps canvassing_before canvassing_training canvassing_frequency area_type priority_precincts_coverage toolkits_use canvassing_relative_time sympathisers_team sympathisers_recruitment canvassing_assessment canvassing_assessment_reason campaign_support_assessment campaign_website_assessment{
replace `var' = -99 if `var' == .
ta `var'
}


** drop observations for which departement or territory is missing

keep if departement_code ~= . & territory ~= ""


** merge with database listing territories in the sample; drop excluded territories

merge m:1 departement_code territory using "Intermediate\sample_territories"
drop if _merge ~= 3 | sample ~= 1
drop _merge


** identify territories in which at least one respondent mentions that she or her unit used the list of allocated precincts

gen temp = (priority_precincts_coverage == 2)
so departement_code territory
by departement_code territory: egen used_list_allocated_precincts = max(temp)
label var used_list_allocated_precincts "terr. in which at least 1 resp. said her unit used the list of alloc. precincts"
drop temp


** keep 1 observation per territory

so departement_code territory
by departement_code territory: gen id = _n
keep if id == 1
drop id

keep departement_code territory used_list_allocated_precincts
so departement_code territory

save "Intermediate\activists_survey_v2", replace


****************************************
*** PART V *** Work on Insee census data: generate sociodemographic variables used as controls and for balance checks
****************************************

** population 2011

use "Original\insee_population_11", clear

* compute share of men and of people aged 0-14, 15-29, 30-44, 45-59, 60-74, and over 75

gen share_men = p11_poph / p11_pop
label var share_men "% men in the municipality"
foreach var in 1529 3044 4559 6074 {
gen share_`var' = p11_pop`var' / p11_pop
}
gen share_75p = (p11_pop7589 + p11_pop90p) / p11_pop
gen share_0014 = 1 - (share_1529 + share_3044 + share_4559 + share_6074 + share_75p)
order share_0014, before(share_1529)
label var share_0014 "% population younger than 14 in the municipality"   
label var share_1529 "% population aged 15 to 29 in the municipality"
label var share_3044 "% population aged 30 to 44 in the municipality"
label var share_4559 "% population aged 45 to 59 in the municipality"
label var share_6074 "% population aged 60 to 74 in the municipality"
label var share_75p "% population older than 75 in the municipality"

rename p11_pop population
label var population "population size of the municipality"

keep municipality_code_noarr population share_*
label var municipality_code_noarr "municipality identifier without arrondissements in Paris / Lyon / Marseille"

save "Intermediate\insee_population_11_v2", replace


** activity 2011

use "Original\insee_activity_11", clear

* compute share of working population and share of unemployed (among working population)

gen share_working_pop = p11_act1564 / p11_pop1564
gen share_unemployed = p11_chom1564 / p11_act1564
replace share_working_pop = 0 if p11_pop1564 == 0
replace share_unemployed = 0 if p11_act1564 == 0
label var share_working_pop "% working population in the municipality"
label var share_unemployed "% unemployed workers in the municipality"

keep municipality_code_noarr share_*
label var municipality_code_noarr "municipality identifier without arrondissements in Paris / Lyon / Marseille"

save "Intermediate\insee_activity_11_v2", replace


** income 2011

use "Original\insee_income_11", clear

rename rfucq211 median_income
label var median_income "median income in the municipality"

keep municipality_code_noarr median_income
label var municipality_code_noarr "municipality identifier without arrondissements in Paris / Lyon / Marseille"

save "Intermediate\insee_income_11_v2", replace


** population 2006

use "Original\insee_population_06", clear

* compute share of men and of people aged 0-14, 15-29, 30-44, 45-59, 60-74, and over 75

gen share_men_lag = p06_poph / p06_pop
label var share_men_lag "% men in the municipality, lagged"
foreach var in 1529 3044 4559 6074 75p{
gen share_`var'_lag = p06_pop`var' / p06_pop
}
gen share_0014_lag = 1 - (share_1529_lag + share_3044_lag + share_4559_lag + share_6074_lag + share_75p_lag)
order share_0014_lag, before(share_1529_lag)
label var share_0014_lag "% population younger than 14 in the municipality, lagged"   
label var share_1529_lag "% population aged 15 to 29 in the municipality, lagged"
label var share_3044_lag "% population aged 30 to 44 in the municipality, lagged"
label var share_4559_lag "% population aged 45 to 59 in the municipality, lagged"
label var share_6074_lag "% population aged 60 to 74 in the municipality, lagged"
label var share_75p_lag "% population older than 75 in the municipality, lagged"

rename p06_pop population_lag
label var population_lag "population size of the municipality, lagged"

keep municipality_code_noarr population_lag share_*
label var municipality_code_noarr "municipality identifier without arrondissements in Paris / Lyon / Marseille"

save "Intermediate\insee_population_06_v2", replace


** activity 2006

use "Original\insee_activity_06", clear

* compute share of working population and share of unemployed (among working population)

gen share_working_pop_lag = p06_act1564 / p06_pop1564
gen share_unemployed_lag = p06_chom1564 / p06_act1564
replace share_working_pop_lag = 0 if p06_pop1564 == 0
replace share_unemployed_lag = 0 if p06_act1564 == 0
label var share_working_pop_lag "% working population in the municipality, lagged"
label var share_unemployed_lag "% unemployed workers in the municipality, lagged"

keep municipality_code_noarr share_*
label var municipality_code_noarr "municipality identifier without arrondissements in Paris / Lyon / Marseille"

save "Intermediate\insee_activity_06_v2", replace


****************************************
*** PART VI *** Merge randomization database with other databases
****************************************

use "Intermediate\randomization_precinct&municipality", clear

** drop excluded territories

keep if sample == 1
drop sample territory_excluded


** merge with electoral results

* 2012 results

merge 1:1 municipality_code precinct_code using "Intermediate\results_12"
drop if _merge == 2
gen merge_results12 = (_merge == 3)
drop _merge
label var merge_results12 "successful merge with 2012 results"

* 2007 results

merge 1:1 municipality_code precinct_code using "Intermediate\results_07"
drop if _merge == 2
gen merge_results07 = (_merge == 3)
drop _merge
label var merge_results07 "successful merge with 2007 results"

* 2002 results

merge 1:1 municipality_code precinct_code using "Intermediate\results_02"
drop if _merge == 2
gen merge_results02 = (_merge == 3)
drop _merge
label var merge_results02 "successful merge with 2002 results"

* 2014 results

merge 1:1 municipality_code precinct_code using "Intermediate\results_14"
drop if _merge == 2
gen merge_results14 = (_merge == 3)
drop _merge
label var merge_results14 "successful merge with 2014 results"


** merge with activists' reports

merge 1:1 departement_code territory municipality precinct using "Intermediate\activists_reports_v2"
drop _merge

* identify territories in which at least one report indicates the precinct (resp. municipality) covered, in territories randomized at the precinct (resp. municipality) level

so departement_code territory
by departement_code territory: egen territory_in_crit1 = max(report_indicating_precinct)
label variable territory_in_crit1 "territory used list of allocated precincts based on activists' reports"
drop report_indicating_precinct


** merge with postelectoral survey

merge m:1 departement_code territory using "Intermediate\activists_survey_v2"
drop _merge

* identify territories in which at least one respondent mentions that she or her unit used the list of allocated precincts

gen territory_in_crit2 = (used_list_allocated_precincts == 1)
label variable territory_in_crit2 "territory used list of allocated precincts based on postelectoral survey"
drop used_list_allocated_precincts


** identify territories which used the list of allocated precincts based on the first or second criterion

gen territory_in = (territory_in_crit1 == 1 | territory_in_crit2 == 1)
label variable territory_in "territory used list of allocated precincts"


** merge with Insee census data

gen municipality_code_noarr = municipality_code
replace municipality_code_noarr = 13055 if (municipality_code_noarr >= 13201 & municipality_code_noarr <= 13216)
replace municipality_code_noarr = 69123 if (municipality_code_noarr >= 69381 & municipality_code_noarr <= 69389)
replace municipality_code_noarr = 75056 if (municipality_code_noarr >= 75101 & municipality_code_noarr <= 75120)
label var municipality_code_noarr "municipality identifier without arrondissements in Paris / Lyon / Marseille"

merge m:1 municipality_code_noarr using "Intermediate\insee_population_11_v2"
drop if _merge == 2
drop _merge
merge m:1 municipality_code_noarr using "Intermediate\insee_activity_11_v2"
drop if _merge == 2
drop _merge
merge m:1 municipality_code_noarr using "Intermediate\insee_income_11_v2"
drop if _merge == 2
drop _merge
merge m:1 municipality_code_noarr using "Intermediate\insee_population_06_v2"
drop if _merge == 2
drop _merge
merge m:1 municipality_code_noarr using "Intermediate\insee_activity_06_v2"
drop if _merge == 2
drop _merge
drop municipality_code_noarr


** define outcome and baseline variables

* turnout

foreach round in pr12t1 pr12t2 pr07t1 pr07t2 pr02t1 pr02t2 leg12t1 leg12t2 eur14 {
gen prop_turnout_`round'_an = nb_voters_`round'_an / nb_registered_`round'_an
label var prop_turnout_`round'_an "`round': % voters who participated"
}

* vote shares

foreach round in t1 t2 {
gen prop_hollande_pr12`round'_an = nb_hollande_pr12`round'_an / nb_expressedvotes_pr12`round'_an
label var prop_hollande_pr12`round'_an "pr12`round': % votes, candidate Hollande"
gen prop_hollande_pr12`round'b_an = nb_hollande_pr12`round'_an / nb_registered_pr12`round'_an
label var prop_hollande_pr12`round'b_an "pr12`round': % votes (/ reg. citizens), candidate Hollande"
gen prop_royal_pr07`round'_an = nb_royal_pr07`round'_an / nb_expressedvotes_pr07`round'_an
label var prop_royal_pr07`round'_an "pr07`round': % votes, candidate Royal"
gen prop_royal_pr07`round'b_an = nb_royal_pr07`round'_an / nb_registered_pr07`round'_an
label var prop_royal_pr07`round'b_an "pr07`round': % votes (/ reg. citizens), candidate Royal"
gen prop_ps_leg12`round'_an = nb_soc_leg12`round'_an / nb_expressedvotes_leg12`round'_an
label var prop_ps_leg12`round'_an "leg12`round': % votes, candidates of pol. orientation 'Socialiste'"
gen prop_ps_leg12`round'b_an = nb_soc_leg12`round'_an / nb_registered_leg12`round'_an
label var prop_ps_leg12`round'b_an "leg12`round': % votes (/ reg. citizens), candidates of pol. orientation 'Socialiste'"
}

gen prop_jospin_pr02t1_an = nb_jospin_pr02t1_an / nb_expressedvotes_pr02t1_an
label var prop_jospin_pr02t1_an "pr02t1: % votes, candidate Jospin"
gen prop_jospin_pr02t2_an = (nb_jospin_pr02t1_an + nb_besancenot_pr02t1_an + nb_chevenement_pr02t1_an + nb_gluckstein_pr02t1_an + nb_hue_pr02t1_an + nb_laguiller_pr02t1_an + nb_mamere_pr02t1_an + nb_taubira_pr02t1_an) / nb_expressedvotes_pr02t1_an
label var prop_jospin_pr02t2_an "pr02t2: % votes (reconstructed), candidate Jospin"
gen prop_ps_eur14_an = nb_ug_eur14_an / nb_expressedvotes_eur14_an
label var prop_ps_eur14_an "eur14: % votes, lists of pol. orientation 'Union de la Gauche'"
gen prop_ps_eur14b_an = nb_ug_eur14_an / nb_registered_eur14_an
label var prop_ps_eur14b_an "eur14: % votes (/ reg. citizens), lists of pol. orientation 'Union de la Gauche'"
gen prop_farleft_pr12t1_an = (nb_poutou_pr12t1_an + nb_arthaud_pr12t1_an) / nb_expressedvotes_pr12t1_an
label var prop_farleft_pr12t1_an "pr12t1: % votes, far-left candidates"
gen prop_otherleft_pr12t1_an = (nb_joly_pr12t1_an + nb_melenchon_pr12t1_an) / nb_expressedvotes_pr12t1_an
label var prop_otherleft_pr12t1_an "pr12t1: % votes, left candidates other than Hollande"
gen prop_center_pr12t1_an = (nb_bayrou_pr12t1_an) / nb_expressedvotes_pr12t1_an
label var prop_center_pr12t1_an "pr12t1: % votes, center candidates"
gen prop_right_pr12t1_an = (nb_sarkozy_pr12t1_an + nb_dupontaignan_pr12t1_an) / nb_expressedvotes_pr12t1_an
label var prop_right_pr12t1_an "pr12t1: % votes, right candidates"
gen prop_farright_pr12t1_an = (nb_lepen_pr12t1_an) / nb_expressedvotes_pr12t1_an
label var prop_farright_pr12t1_an "pr12t1: % votes, far-right candidates"
gen prop_farleft_pr07t1_an = (nb_besancenot_pr07t1_an + nb_bove_pr07t1_an + nb_laguiller_pr07t1_an + nb_schivardi_pr07t1_an) / nb_expressedvotes_pr07t1_an
label var prop_farleft_pr07t1_an "pr07t1: % votes, far-left candidates"
gen prop_otherleft_pr07t1_an = (nb_buffet_pr07t1_an + nb_voynet_pr07t1_an) / nb_expressedvotes_pr07t1_an
label var prop_otherleft_pr07t1_an "pr07t1: % votes, left candidates other than Royal"
gen prop_center_pr07t1_an = (nb_bayrou_pr07t1_an) / nb_expressedvotes_pr07t1_an
label var prop_center_pr07t1_an "pr07t1: % votes, center candidates"
gen prop_right_pr07t1_an = (nb_sarkozy_pr07t1_an + nb_nihous_pr07t1_an + nb_devilliers_pr07t1_an) / nb_expressedvotes_pr07t1_an
label var prop_right_pr07t1_an "pr07t1: % votes, right candidates"
gen prop_farright_pr07t1_an = (nb_lepen_pr07t1_an) / nb_expressedvotes_pr07t1_an
label var prop_farright_pr07t1_an "pr07t1: % votes, far-right candidates"

* averages over 1st and 2nd round

foreach var in turnout_pr12 turnout_pr07 turnout_pr02 turnout_leg12 hollande_pr12 royal_pr07 jospin_pr02 {
gen prop_`var't12_an = (prop_`var't1_an + prop_`var't2_an) / 2
}
label var prop_turnout_pr12t12_an "pr12, average t1/t2: % voters who participated"
label var prop_turnout_pr07t12_an "pr07, average t1/t2: % voters who participated"
label var prop_turnout_pr02t12_an "pr02, average t1/t2: % voters who participated"
label var prop_turnout_leg12t12_an "leg12, average t1/t2: % voters who participated"
label var prop_hollande_pr12t12_an "pr12, average t1/t2: % votes, candidate Hollande"
label var prop_royal_pr07t12_an "pr07, average t1/t2: % votes, candidate Royal"
label var prop_jospin_pr02t12_an "pr02, average t1/t2: % votes, candidate Jospin"
foreach var in hollande_pr12 royal_pr07 {
gen prop_`var't12b_an = (prop_`var't1b_an + prop_`var't2b_an) / 2
}
label var prop_hollande_pr12t12b_an "pr12, average t1/t2: % votes (/ reg. citizens), candidate Hollande"
label var prop_royal_pr07t12b_an "pr07, average t1/t2: % votes (/ reg. citizens), candidate Royal"

drop nb_voters_pr12t1_an - nb_hollande_pr12t1_an nb_registered_pr12t2_an - nb_sarkozy_pr12t2_an nb_registered_leg12t1_an - nb_vec_leg12t1_an nb_registered_leg12t2_an - nb_vec_leg12t2_an nb_voters_pr07t1_an - nb_devilliers_pr07t1_an nb_registered_pr07t2_an - nb_sarkozy_pr07t2_an nb_registered_pr02t1_an - nb_taubira_pr02t1_an nb_registered_pr02t2_an - nb_lepen_pr02t2_an nb_registered_eur14_an - nb_vec_eur14_an


** define other variables used repeatedly in the analysis

* five-year changes of the census variables

foreach var in population share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_75p share_working_pop share_unemployed{
gen `var'_delta = `var' - `var'_lag
}
label var population_delta "population size of the municipality, five-year change"
label var share_men_delta "% men in the municipality, five-year change"
label var share_0014_delta "% population younger than 14 in the municipality, five-year change"   
label var share_1529_delta "% population aged 15 to 29 in the municipality, five-year change"
label var share_3044_delta "% population aged 30 to 44 in the municipality, five-year change"
label var share_4559_delta "% population aged 45 to 59 in the municipality, five-year change"
label var share_6074_delta "% population aged 60 to 74 in the municipality, five-year change"
label var share_75p_delta "% population older than 75 in the municipality, five-year change"
label var share_working_pop_delta "% working population in the municipality, five-year change"
label var share_unemployed_delta "% unemployed workers in the municipality, five-year change"

* prop_leftabstention at the level of observation

gen prop_leftabstention_an = prop_leftabstention if level_randomization == 1 
replace prop_leftabstention_an = prop_leftabstention_mun if level_randomization == 0
label var prop_leftabstention_an "% left abstention, level of observation (from 2007 pres, round 2)"
drop prop_leftabstention_mun prop_leftabstention

* stratum identifier across territories

so departement_code territory stratum
by departement_code territory stratum: gen temp = _n
replace temp = 0 if temp ~= 1
gen stratum_identifier = 1
replace stratum_identifier = stratum_identifier[_n-1] + temp if _n ~= 1
drop temp
label var stratum_identifier "stratum identifier"

* region code and region dummies

gen region_code = 1 if departement_code == 971 | departement_code == 972 | departement_code == 973 | departement_code == 974 | departement_code == 976
replace region_code = 11 if departement_code == 75 | departement_code == 77 | departement_code == 78 | departement_code == 91 | departement_code == 92 | departement_code == 93 | departement_code == 94 | departement_code == 95 
replace region_code = 21 if departement_code == 8 | departement_code == 10 | departement_code == 51 | departement_code == 52
replace region_code = 22 if departement_code == 2 | departement_code == 60 | departement_code == 80 
replace region_code = 23 if departement_code == 27 | departement_code == 76
replace region_code = 24 if departement_code == 18 | departement_code == 28 | departement_code == 36 | departement_code == 37 | departement_code == 41 | departement_code == 45
replace region_code = 25 if departement_code == 14 | departement_code == 50 | departement_code == 61
replace region_code = 26 if departement_code == 21 | departement_code == 58 | departement_code == 71 | departement_code == 89
replace region_code = 31 if departement_code == 59 | departement_code == 62
replace region_code = 41 if departement_code == 54 | departement_code == 55 | departement_code == 57 | departement_code == 88 
replace region_code = 42 if departement_code == 67 | departement_code == 68
replace region_code = 43 if departement_code == 25 | departement_code == 39 | departement_code == 70 | departement_code == 90
replace region_code = 52 if departement_code == 44 | departement_code == 49 | departement_code == 53 | departement_code == 72 | departement_code == 85
replace region_code = 53 if departement_code == 22 | departement_code == 29 | departement_code == 35 | departement_code == 56 
replace region_code = 54 if departement_code == 16 | departement_code == 17 | departement_code == 79 | departement_code == 86
replace region_code = 72 if departement_code == 24 | departement_code == 33 | departement_code == 40 | departement_code == 47 | departement_code == 64
replace region_code = 73 if departement_code == 9 | departement_code == 12 | departement_code == 31 | departement_code == 32 | departement_code == 46 | departement_code == 65 | departement_code == 81 | departement_code == 82
replace region_code = 74 if departement_code == 19 | departement_code == 23 | departement_code == 87
replace region_code = 82 if departement_code == 1 | departement_code == 7 | departement_code == 26 | departement_code == 38 | departement_code == 42 | departement_code == 69 | departement_code == 73 | departement_code == 74
replace region_code = 83 if departement_code == 3 | departement_code == 15 | departement_code == 43 | departement_code == 63
replace region_code = 91 if departement_code == 11 | departement_code == 30 | departement_code == 34 | departement_code == 48 | departement_code == 66
replace region_code = 93 if departement_code == 4 | departement_code == 5 | departement_code == 6 | departement_code == 13 | departement_code == 83 | departement_code == 84
replace region_code = 94 if departement_code == 20
label var region_code "region identifier"

so region_code
ta region_code, gen(region_id)

save "Analysis\analysis", replace
