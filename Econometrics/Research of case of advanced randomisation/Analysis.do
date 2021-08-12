clear all
set matsize 11000
set maxvar 20000

** Name: Analysis.do
** Date Created: 10/23/2017 by Vincent Pons (vpons@hbs.edu)
** Last Updated: 

* Data In: [analysis & randomization_precinct&municipality & activists_reports]

* Purpose of do-file: Generate tables and figures of paper and online appendix

* Organization :
/* 
*** PART I *** RESULTS IN THE MAIN TEXT
- PART I.A - Randomization check and first stage
- PART I.B - Impact on presidential elections
- PART I.C - Impact on following elections
- PART I.D - Placebo: Impact on 2007 presidential elections
- PART I.E - Figures reported in the text

*** PART II *** RESULTS IN THE APPENDIX
- PART II.A - All territories, whether or not they used the randomization lists (Appendix A)
- PART II.B - Territories characterized using only the 1st or the 2nd criterion (Appendix B)
- PART II.C - First stratum of each territory, or minimal sample (Appendix C)
- PART II.D - Clustered standard errors (Appendix D)
- PART II.E - Trimming precincts with the largest number of registered citizens (Appendix E)
- PART II.F - Using the change in the dependent variable as outcome (Appendix F)
- PART II.G - Treatment impact heterogeneity along PO (prop_leftabstention_an) (Appendix G)
- PART II.H - Seemingly unrelated regressions (Appendix H)
- PART II.I - Impact on the difference between Hollande's vote share (expressed as a fraction of registered voters) and voter turnout (Appendix I)
*/


* Setting file path

cd "${data}"

****************************************
****************************************
*** PART I *** RESULTS IN THE MAIN TEXT
****************************************
****************************************

use "Analysis\analysis", clear

keep if territory_in == 1

****************************************
*** PART I.A *** Randomization check and First Stage
****************************************

****************************************
*** Randomization check (Table 2)
****************************************

gen var=""
for any diff p control_mean control_sd treatment_mean treatment_sd N: gen X=.

#delimit;
local vars= 36;

for any level_randomization nb_registered_pr12t1_an prop_leftabstention_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 region_id23 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_75p share_working_pop share_unemployed median_income   
\ num 1/`vars': 
replace var="X" if _n==Y \ 
ivreg2 X treatment if merge_results12 == 1, robust \ 
replace N=e(N) if _n==Y \ 
mat beta=e(b) \ 
replace diff=beta[1,1] if _n==Y \ 
test treatment=0 \ 
replace p=r(p) if _n==Y \ 
sum X if treatment==0 & merge_results12 == 1\ 
replace control_mean=r(mean) if _n==Y \ 
replace control_sd=r(sd) if _n==Y \ 
sum X if treatment==1 & merge_results12 == 1 \ 
replace treatment_mean=r(mean) if _n==Y \ 
replace treatment_sd=r(sd) if _n==Y;
for var diff-treatment_sd: replace X=round(X, 0.001);
outsheet var control_mean control_sd treatment_mean treatment_sd p N if _n<=`vars' using "$paper\Table2_SummaryStatistics_1", replace;

local vars= 4;

for any prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an   
\ num 1/`vars': 
replace var="X" if _n==Y \ 
ivreg2 X treatment if merge_results12 == 1 & merge_results07 == 1, robust \ 
replace N=e(N) if _n==Y \ 
mat beta=e(b) \ 
replace diff=beta[1,1] if _n==Y \ 
test treatment=0 \ 
replace p=r(p) if _n==Y \ 
sum X if treatment==0 & merge_results12 == 1 & merge_results07 == 1\ 
replace control_mean=r(mean) if _n==Y \ 
replace control_sd=r(sd) if _n==Y \ 
sum X if treatment==1 & merge_results12 == 1 & merge_results07 == 1 \ 
replace treatment_mean=r(mean) if _n==Y \ 
replace treatment_sd=r(sd) if _n==Y;
for var diff-treatment_sd: replace X=round(X, 0.001);
outsheet var control_mean control_sd treatment_mean treatment_sd p N if _n<=`vars' using "$paper\Table2_SummaryStatistics_2", replace;

#delimit cr

drop var diff p control_mean control_sd treatment_mean treatment_sd N

* test for the joint significance of all characteristics

reg treatment level_randomization nb_registered_pr12t1_an prop_leftabstention_an prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_working_pop share_unemployed median_income if merge_results12 == 1 & merge_results07 == 1
test level_randomization nb_registered_pr12t1_an prop_leftabstention_an prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_working_pop share_unemployed median_income


****************************************
*** First stage, including controls (Table 3)
****************************************

global controls = "nb_registered_pr12t1_an population share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_unemployed share_working_pop population_delta share_men_delta share_0014_delta share_1529_delta share_3044_delta share_4559_delta share_6074_delta share_unemployed_delta share_working_pop_delta"

su allocated if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 allocated treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table3_FirstStage.out", replace nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su allocated if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 allocated ${controls} prop_turnout_pr07t1_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table3_FirstStage.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_turnout_pr07t2_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table3_FirstStage.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_turnout_pr07t12_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table3_FirstStage.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_royal_pr07t1_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table3_FirstStage.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_royal_pr07t2_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table3_FirstStage.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_royal_pr07t12_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table3_FirstStage.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)


****************************************
*** PART I.B *** Impact on presidential elections
****************************************

*** Impact on voter turnout (Table 4)

* ITT

su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_B.out", replace nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table4_ImpactTurnout_B.out", append nonotes noni dec(4)


*** Impact on Hollande's vote share (Table 5)

* ITT

su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table5_ImpactPSShare_B.out", append nonotes noni dec(4)


*** Impact on all parties' vote shares (Table 6)

* ITT

su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_farleft_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_farleft_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_farleft_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_farleft_pr12t1_an treatment prop_farleft_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_otherleft_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_otherleft_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_otherleft_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_otherleft_pr12t1_an treatment prop_otherleft_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_center_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_center_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_center_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_center_pr12t1_an treatment prop_center_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_right_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_right_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_right_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_right_pr12t1_an treatment prop_right_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_farright_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_farright_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_farright_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_farright_pr12t1_an treatment prop_farright_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_B.out", append nonotes noni dec(4)

xtivreg2 prop_farleft_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_B.out", append nonotes noni dec(4)
xtivreg2 prop_farleft_pr12t1_an (allocated = treatment) prop_farleft_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_B.out", append nonotes noni dec(4)

xtivreg2 prop_otherleft_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_B.out", append nonotes noni dec(4)
xtivreg2 prop_otherleft_pr12t1_an (allocated = treatment) prop_otherleft_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_B.out", append nonotes noni dec(4)

xtivreg2 prop_center_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_B.out", append nonotes noni dec(4)
xtivreg2 prop_center_pr12t1_an (allocated = treatment) prop_center_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_B.out", append nonotes noni dec(4)

xtivreg2 prop_right_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_B.out", append nonotes noni dec(4)
xtivreg2 prop_right_pr12t1_an (allocated = treatment) prop_right_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_B.out", append nonotes noni dec(4)

xtivreg2 prop_farright_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_B.out", append nonotes noni dec(4)
xtivreg2 prop_farright_pr12t1_an (allocated = treatment) prop_farright_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table6_ImpactOtherShares_B.out", append nonotes noni dec(4)


****************************************
*** PART I.C *** Impact on following elections
****************************************

* create constituency fixed effects
/* Constituency fixed effects are included for parliamentary elections outcomes, to account for differences in the number and identity of competing candidates 
across constituencies, in strata spanning multiple constituencies. When none of the stratum in which a constituency is represented spans multiple constituencies,
that constituency's fixed effect is dropped from the regression.*/

ta constituency_leg12 if merge_results12 == 1 & merge_results07 == 1, gen(const_id)


*** Impact on voter turnout (Table 7)

* ITT

su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table7_ImpactTurnout_Follow_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table7_ImpactTurnout_Follow_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_leg12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_leg12t1_an treatment prop_turnout_pr07t1_an ${controls} const_id* prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table7_ImpactTurnout_Follow_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_leg12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_leg12t2_an treatment prop_turnout_pr07t2_an ${controls} const_id* prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table7_ImpactTurnout_Follow_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_eur14_an if treatment == 0 & merge_results14 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_eur14_an treatment prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results14 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table7_ImpactTurnout_Follow_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table7_ImpactTurnout_Follow_B.out", replace nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table7_ImpactTurnout_Follow_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_leg12t1_an (allocated = treatment) prop_turnout_pr07t1_an ${controls} const_id* prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table7_ImpactTurnout_Follow_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_leg12t2_an (allocated = treatment) prop_turnout_pr07t2_an ${controls} const_id* prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table7_ImpactTurnout_Follow_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_eur14_an (allocated = treatment) prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results14 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table7_ImpactTurnout_Follow_B.out", append nonotes noni dec(4)


*** Impact on vote shares of PS candidates (expressed as a fraction of expressed votes) (Table 8, columns 1 - 5)

* ITT

su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare_Follow_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare_Follow_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_ps_leg12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_ps_leg12t1_an treatment prop_royal_pr07t1_an ${controls} const_id* prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare_Follow_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_ps_leg12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_ps_leg12t2_an treatment prop_royal_pr07t2_an ${controls} const_id* prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare_Follow_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_ps_eur14_an if treatment == 0 & merge_results14 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_ps_eur14_an treatment prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results14 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare_Follow_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare_Follow_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare_Follow_B.out", append nonotes noni dec(4)

xtivreg2 prop_ps_leg12t1_an (allocated = treatment) prop_royal_pr07t1_an ${controls} const_id* prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare_Follow_B.out", append nonotes noni dec(4)
xtivreg2 prop_ps_leg12t2_an (allocated = treatment) prop_royal_pr07t2_an ${controls} const_id* prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare_Follow_B.out", append nonotes noni dec(4)

xtivreg2 prop_ps_eur14_an (allocated = treatment) prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results14 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare_Follow_B.out", append nonotes noni dec(4)


*** Impact on vote shares of PS candidates (expressed as a fraction of registered voters) (Table 8, columns 6 - 10)

* ITT

su prop_hollande_pr12t1b_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1b_an treatment prop_royal_pr07t1b_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare2_Follow_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t2b_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2b_an treatment prop_royal_pr07t2b_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare2_Follow_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_ps_leg12t1b_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_ps_leg12t1b_an treatment prop_royal_pr07t1b_an ${controls} const_id* prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare2_Follow_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_ps_leg12t2b_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_ps_leg12t2b_an treatment prop_royal_pr07t2b_an ${controls} const_id* prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare2_Follow_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_ps_eur14b_an if treatment == 0 & merge_results14 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_ps_eur14b_an treatment prop_royal_pr07t1b_an ${controls} prop_leftabstention_an if merge_results14 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare2_Follow_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1b_an (allocated = treatment) prop_royal_pr07t1b_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare2_Follow_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2b_an (allocated = treatment) prop_royal_pr07t2b_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare2_Follow_B.out", append nonotes noni dec(4)

xtivreg2 prop_ps_leg12t1b_an (allocated = treatment) prop_royal_pr07t1b_an ${controls} const_id* prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare2_Follow_B.out", append nonotes noni dec(4)
xtivreg2 prop_ps_leg12t2b_an (allocated = treatment) prop_royal_pr07t2b_an ${controls} const_id* prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare2_Follow_B.out", append nonotes noni dec(4)

xtivreg2 prop_ps_eur14b_an (allocated = treatment) prop_royal_pr07t1b_an ${controls} prop_leftabstention_an if merge_results14 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table8_ImpactPSShare2_Follow_B.out", append nonotes noni dec(4)

drop const_id*


****************************************
*** PART I.D *** Placebo: Impact on 2007 presidential elections
****************************************

*** Impact on voter turnout in 2007 (Table 9)

* ITT

global controls = "nb_registered_pr07t1_an population_lag share_men_lag share_0014_lag share_1529_lag share_3044_lag share_4559_lag share_6074_lag share_unemployed_lag share_working_pop_lag"

su prop_turnout_pr07t1_an if treatment == 0 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr07t1_an treatment if merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr07t1_an if treatment == 0 & merge_results07 == 1 & merge_results02 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr07t1_an treatment prop_turnout_pr02t1_an if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr07t1_an treatment prop_turnout_pr02t1_an ${controls} if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr07t2_an if treatment == 0 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr07t2_an treatment if merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr07t2_an if treatment == 0 & merge_results07 == 1 & merge_results02 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr07t2_an treatment prop_turnout_pr02t2_an if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr07t2_an treatment prop_turnout_pr02t2_an ${controls} if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr07t12_an if treatment == 0 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr07t12_an treatment if merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr07t12_an if treatment == 0 & merge_results07 == 1 & merge_results02 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr07t12_an treatment prop_turnout_pr02t12_an if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr07t12_an treatment prop_turnout_pr02t12_an ${controls} if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_turnout_pr07t1_an (allocated = treatment) if merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_B.out", replace nonotes noni dec(4)
xtivreg2 prop_turnout_pr07t1_an (allocated = treatment) prop_turnout_pr02t1_an if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr07t1_an (allocated = treatment) prop_turnout_pr02t1_an ${controls} if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr07t2_an (allocated = treatment) if merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr07t2_an (allocated = treatment) prop_turnout_pr02t2_an if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr07t2_an (allocated = treatment) prop_turnout_pr02t2_an ${controls} if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr07t12_an (allocated = treatment) if merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr07t12_an (allocated = treatment) prop_turnout_pr02t12_an if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr07t12_an (allocated = treatment) prop_turnout_pr02t12_an ${controls} if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table9_ImpactTurnout_Placebo_B.out", append nonotes noni dec(4)


*** Impact on Royal's vote share in 2007 (Table 10)

* ITT

su prop_royal_pr07t1_an if treatment == 0 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_royal_pr07t1_an treatment if merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_royal_pr07t1_an if treatment == 0 & merge_results07 == 1 & merge_results02 == 1
local mean_control=r(mean)
xtivreg2 prop_royal_pr07t1_an treatment prop_jospin_pr02t1_an if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_royal_pr07t1_an treatment prop_jospin_pr02t1_an ${controls} if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_royal_pr07t2_an if treatment == 0 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_royal_pr07t2_an treatment if merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_royal_pr07t2_an if treatment == 0 & merge_results07 == 1 & merge_results02 == 1
local mean_control=r(mean)
xtivreg2 prop_royal_pr07t2_an treatment prop_jospin_pr02t2_an if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_royal_pr07t2_an treatment prop_jospin_pr02t2_an ${controls} if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_royal_pr07t12_an if treatment == 0 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_royal_pr07t12_an treatment if merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_royal_pr07t12_an if treatment == 0 & merge_results07 == 1 & merge_results02 == 1
local mean_control=r(mean)
xtivreg2 prop_royal_pr07t12_an treatment prop_jospin_pr02t12_an if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_royal_pr07t12_an treatment prop_jospin_pr02t12_an ${controls} if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_royal_pr07t1_an (allocated = treatment) if merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_B.out", replace nonotes noni dec(4)
xtivreg2 prop_royal_pr07t1_an (allocated = treatment) prop_jospin_pr02t1_an if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_B.out", append nonotes noni dec(4)
xtivreg2 prop_royal_pr07t1_an (allocated = treatment) prop_jospin_pr02t1_an ${controls} if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_B.out", append nonotes noni dec(4)

xtivreg2 prop_royal_pr07t2_an (allocated = treatment) if merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_B.out", append nonotes noni dec(4)
xtivreg2 prop_royal_pr07t2_an (allocated = treatment) prop_jospin_pr02t2_an if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_B.out", append nonotes noni dec(4)
xtivreg2 prop_royal_pr07t2_an (allocated = treatment) prop_jospin_pr02t2_an ${controls} if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_B.out", append nonotes noni dec(4)

xtivreg2 prop_royal_pr07t12_an (allocated = treatment) if merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_B.out", append nonotes noni dec(4)
xtivreg2 prop_royal_pr07t12_an (allocated = treatment) prop_jospin_pr02t12_an if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_B.out", append nonotes noni dec(4)
xtivreg2 prop_royal_pr07t12_an (allocated = treatment) prop_jospin_pr02t12_an ${controls} if merge_results07 == 1 & merge_results02 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$paper\Table10_ImpactPSShare_Placebo_B.out", append nonotes noni dec(4)


****************************************
*** PART I.E *** Figures reported in the text
****************************************

*** Number of included and excluded territories

use "Intermediate\randomization_precinct&municipality", clear

so departement_code territory
by departement_code territory: gen id = _n

** number of territories included in the randomization
count if id == 1 & sample == 1
* 3260

** number of territories excluded
count if id == 1 & territory_excluded == 1
drop id
* 279
/* In territories counting a unique municipality, that municipality had to be allocated to the canvassers in any
case (so that they could participate in the door-to-door campaign); this territory was thus not included in the randomization*/


*** Sample statistics on territories which used the list of allocated precincts

use "Analysis\analysis", clear

** number of territories which used the list of allocated precincts based on the first or second criterion
so departement_code territory
by departement_code territory: gen id = _n
count if id == 1 & territory_in == 1
drop id
* 791

** number of registered citizens in these territories (estimated by the time of the 2011 primaries)

gen temp = nb_registered_prim if level_randomization == 1
replace temp = nb_registered_mun if level_randomization == 0
egen temp2 = total(temp) if territory_in == 1
ta temp2
drop temp temp2
* 5024256

** number of strata in these territories
so departement_code territory stratum
by departement_code territory stratum: gen id = _n
count if territory_in == 1 & id == 1
drop id
* 966

** number of precincts in these territories
count if territory_in == 1
* 4674

** treatment assignment in these territories
ta treatment if territory_in == 1
* 3,748 of the precincts were randomly assigned to the treatment group and 926 to the control group

** number of precincts allocated to canvassers
ta allocated if treatment == 1 & territory_in == 1
* 2139

** distribution of the number of strata included in the randomization
so departement_code territory
by departement_code territory: gen id = _n
ta nb_strata_included if territory_in == 1 & id == 1
drop id
* 1 stratum in 693 territories; 2 strata in 58 territories; 3 or more strata in the remaining territories


*** Statistics based on the reports database

** Plot daily number of doors knocked (Figure 4)

use "Original\activists_reports", clear

* replace one erroneous value for report date (indicated in 2010); drop if report_date is missing

su report_date
replace report_date = . if report_date == 18613

* compute aggregate number of doors knocked per day

so report_date
by report_date: egen total_nb_doors_knocked = total(nb_doors_knocked)
by report_date: gen id = _n
keep if id == 1

keep report_date total_nb_doors_knocked

* add missing dates, with 0 number of doors knocked on these dates

so report_date
gen temp = report_date - report_date[_n-1]
ta temp
foreach i in 1 2 3 4 17{
expand `i' if temp == `i'
}
so report_date
by report_date: gen id = _n
replace total_nb_doors_knocked = 0 if id < temp & temp ~= .
forvalues i=1(1)16{
replace report_date = report_date[_n-1] + 1 if id == `i' & id < temp & temp ~= .
}

drop if report_date == .
drop temp id

outsheet using "$paper\Figure4", replace


** Compute aggregate number of doors knocked in each territory before first round and between the 2 rounds

use "Original\activists_reports", clear

gen nb_doors_knocked_1 = nb_doors_knocked if report_date ~= . & report_date <= 19105
gen nb_doors_knocked_2 = nb_doors_knocked if report_date ~= . & report_date > 19105

so departement_code territory
by departement_code territory: egen total_nb_doors_knocked_1_ter = total(nb_doors_knocked_1)
by departement_code territory: egen total_nb_doors_knocked_2_ter = total(nb_doors_knocked_2)
label var total_nb_doors_knocked_1_ter "total # doors knocked in precinct before 1st round"
label var total_nb_doors_knocked_2_ter "total # doors knocked in precinct between 1st and 2nd round"

by departement_code territory: gen id = _n
keep if id == 1

keep departement_code territory total_nb_doors_knocked_1_ter total_nb_doors_knocked_2_ter

save "Intermediate\activists_reports_ter", replace


*** 1st and 2nd round differential intensity multipliers

use "Analysis\analysis", clear

** compute total number of registered citizens in allocated / non-allocated / control precincts

gen temp = nb_registered_prim if level_randomization == 1
replace temp = nb_registered_mun if level_randomization == 0
egen temp1 = total(temp) if territory_in == 1 & treatment == 1 & allocated == 1
egen n_allocated = max(temp1)
egen temp2 = total(temp) if territory_in == 1 & treatment == 1 & allocated == 0
egen n_nonallocated = max(temp2)
egen temp3 = total(temp) if territory_in == 1 & treatment == 0
egen n_control = max(temp3)
ta n_allocated
* 2486941
ta n_nonallocated
* 1613156
ta n_control
* 924159
drop temp*

** compute total number of doors knocked in allocated / non-allocated / control precincts

egen temp1 = total(total_nb_doors_knocked) if territory_in == 1 & treatment == 1 & allocated == 1
egen temp_allocated = max(temp1)
egen temp2 = total(total_nb_doors_knocked) if territory_in == 1 & treatment == 1 & allocated == 0
egen temp_nonallocated = max(temp2)
egen temp3 = total(total_nb_doors_knocked) if territory_in == 1 & treatment == 0
egen temp_control = max(temp3)
gen knocked_allocated = temp_allocated / (temp_allocated + temp_nonallocated + temp_control)
gen knocked_nonallocated = temp_nonallocated / (temp_allocated + temp_nonallocated + temp_control)
gen knocked_control = temp_control / (temp_allocated + temp_nonallocated + temp_control)
ta knocked_allocated
* .7251436
ta knocked_nonallocated
* .1414127
ta knocked_control
* .1334437
drop temp*

** compute 2nd round multiplier
gen multiplier_round2 = 1 / n_allocated * 1 / ((knocked_allocated + knocked_nonallocated)/(n_allocated + n_nonallocated) - knocked_control / n_control)
ta multiplier_round2
* 6.005491

** compute total number of doors knocked before 1st round in allocated / non-allocated / control precincts

egen temp1 = total(total_nb_doors_knocked_1) if territory_in == 1 & treatment == 1 & allocated == 1
egen temp_allocated_1 = max(temp1)
egen temp2 = total(total_nb_doors_knocked_1) if territory_in == 1 & treatment == 1 & allocated == 0
egen temp_nonallocated_1 = max(temp2)
egen temp3 = total(total_nb_doors_knocked_1) if territory_in == 1 & treatment == 0
egen temp_control_1 = max(temp3)
gen knocked_allocated_1 = temp_allocated_1 / (temp_allocated_1 + temp_nonallocated_1 + temp_control_1)
gen knocked_nonallocated_1 = temp_nonallocated_1 / (temp_allocated_1 + temp_nonallocated_1 + temp_control_1)
gen knocked_control_1 = temp_control_1 / (temp_allocated_1 + temp_nonallocated_1 + temp_control_1)
ta knocked_allocated_1
* .7299473
ta knocked_nonallocated_1
* .1401382
ta knocked_control_1
* .1299144
drop temp*

** compute fraction of doors knocked before 1st round

merge m:1 departement_code territory using "Intermediate\activists_reports_ter"

so departement_code territory
by departement_code territory: gen id = _n
egen temp1 = total(total_nb_doors_knocked_1_ter) if id == 1 & territory_in == 1
egen temp2 = total(total_nb_doors_knocked_2_ter) if id == 1 & territory_in == 1
gen knocked_round1 = temp1 / (temp1 + temp2)
ta knocked_round1
* .7648827


** compute 1st round multiplier
gen multiplier_round1 = 1 / (n_allocated * knocked_round1) * 1 / ((knocked_allocated_1 + knocked_nonallocated_1)/(n_allocated + n_nonallocated) - knocked_control_1 / n_control)
ta multiplier_round1
* 7.338609


*** Verification that the characteristics of precincts in territories which do not satisfy the verification criteria do not systematically differ across treatment and control

use "Analysis\analysis", clear

gen territory_out = 1 - territory_in

foreach var in level_randomization nb_registered_pr12t1_an prop_leftabstention_an population region_id1 region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 region_id23 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_working_pop share_unemployed median_income prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an{
gen `var'_t = `var' * treatment
}

ivreg2 territory_out treatment level_randomization nb_registered_pr12t1_an prop_leftabstention_an population region_id1 region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_75p share_working_pop share_unemployed median_income prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an level_randomization_t nb_registered_pr12t1_an_t prop_leftabstention_an_t population_t region_id1_t region_id2_t region_id3_t region_id4_t region_id5_t region_id6_t region_id7_t region_id8_t region_id9_t region_id10_t region_id11_t region_id12_t region_id13_t region_id14_t region_id15_t region_id16_t region_id17_t region_id18_t region_id19_t region_id20_t region_id21_t region_id22_t share_men_t share_0014_t share_1529_t share_3044_t share_4559_t share_6074_t share_working_pop_t share_unemployed_t median_income_t prop_turnout_pr07t1_an_t prop_turnout_pr07t2_an_t prop_royal_pr07t1_an_t prop_royal_pr07t2_an_t if merge_results12 == 1 & merge_results07 == 1, robust
test treatment level_randomization_t nb_registered_pr12t1_an_t prop_leftabstention_an_t population_t region_id1_t region_id2_t region_id3_t region_id4_t region_id5_t region_id6_t region_id7_t region_id8_t region_id9_t region_id10_t region_id11_t region_id12_t region_id13_t region_id14_t region_id15_t region_id16_t region_id17_t region_id18_t region_id19_t region_id20_t region_id21_t region_id22_t share_men_t share_0014_t share_1529_t share_3044_t share_4559_t share_6074_t share_working_pop_t share_unemployed_t median_income_t


****************************************
****************************************
*** PART II *** RESULTS IN THE APPENDIX
****************************************
****************************************


****************************************
*** PART II.A *** All territories, whether or not they used the randomization lists (Appendix A)
****************************************

use "Analysis\analysis", clear


*** Randomization check (Table A1)

gen var=""
for any diff p control_mean control_sd treatment_mean treatment_sd N: gen X=.

#delimit;
local vars= 37;

for any level_randomization nb_registered_pr12t1_an prop_leftabstention_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 region_id23 region_id1 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_75p share_working_pop share_unemployed median_income   

\ num 1/`vars': 
replace var="X" if _n==Y \ 
ivreg2 X treatment if merge_results12 == 1, robust \ 
replace N=e(N) if _n==Y \ 
mat beta=e(b) \ 
replace diff=beta[1,1] if _n==Y \ 
test treatment=0 \ 
replace p=r(p) if _n==Y \ 
sum X if treatment==0 & merge_results12 == 1\ 
replace control_mean=r(mean) if _n==Y \ 
replace control_sd=r(sd) if _n==Y \ 
sum X if treatment==1 & merge_results12 == 1 \ 
replace treatment_mean=r(mean) if _n==Y \ 
replace treatment_sd=r(sd) if _n==Y;
for var diff-treatment_sd: replace X=round(X, 0.001);
outsheet var control_mean control_sd treatment_mean treatment_sd p N if _n<=`vars' using "$appendix\TableA1_SummaryStatistics_All_1",replace;

local vars= 4;

for any prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an   
\ num 1/`vars': 
replace var="X" if _n==Y \ 
ivreg2 X treatment if merge_results12 == 1 & merge_results07 == 1, robust \ 
replace N=e(N) if _n==Y \ 
mat beta=e(b) \ 
replace diff=beta[1,1] if _n==Y \ 
test treatment=0 \ 
replace p=r(p) if _n==Y \ 
sum X if treatment==0 & merge_results12 == 1 & merge_results07 == 1\ 
replace control_mean=r(mean) if _n==Y \ 
replace control_sd=r(sd) if _n==Y \ 
sum X if treatment==1 & merge_results12 == 1 & merge_results07 == 1 \ 
replace treatment_mean=r(mean) if _n==Y \ 
replace treatment_sd=r(sd) if _n==Y;
for var diff-treatment_sd: replace X=round(X, 0.001);
outsheet var control_mean control_sd treatment_mean treatment_sd p N if _n<=`vars' using "$appendix\TableA1_SummaryStatistics_All_2",replace;

#delimit cr

drop var diff p control_mean control_sd treatment_mean treatment_sd N

* test for the joint significance of all characteristics

reg treatment level_randomization nb_registered_pr12t1_an prop_leftabstention_an prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_working_pop share_unemployed median_income if merge_results12 == 1 & merge_results07 == 1
test level_randomization nb_registered_pr12t1_an prop_leftabstention_an prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_working_pop share_unemployed median_income


*** Impact on voter turnout (Table A2)

* ITT

global controls = "nb_registered_pr12t1_an population share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_unemployed share_working_pop population_delta share_men_delta share_0014_delta share_1529_delta share_3044_delta share_4559_delta share_6074_delta share_unemployed_delta share_working_pop_delta"

su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_B.out", replace nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA2_ImpactTurnout_All_B.out", append nonotes noni dec(4)


*** Impact on Hollande's vote share (Table A3)

* ITT

su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableA3_ImpactPSShare_All_B.out", append nonotes noni dec(4)


****************************************
*** PART II.B *** Territories characterized using only the 1st or the 2nd criterion (Appendix B)
****************************************

*** Randomization check (territories which used the randomization lists, based on reports: first criterion) (Table B1)

use "Analysis\analysis", clear

keep if territory_in_crit1 == 1

gen var=""
for any diff p control_mean control_sd treatment_mean treatment_sd N: gen X=.

#delimit;
local vars= 36;

for any level_randomization nb_registered_pr12t1_an prop_leftabstention_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 region_id23 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_75p share_working_pop share_unemployed median_income   

\ num 1/`vars': 
replace var="X" if _n==Y \ 
ivreg2 X treatment if merge_results12 == 1, robust \ 
replace N=e(N) if _n==Y \ 
mat beta=e(b) \ 
replace diff=beta[1,1] if _n==Y \ 
test treatment=0 \ 
replace p=r(p) if _n==Y \ 
sum X if treatment==0 & merge_results12 == 1\ 
replace control_mean=r(mean) if _n==Y \ 
replace control_sd=r(sd) if _n==Y \ 
sum X if treatment==1 & merge_results12 == 1 \ 
replace treatment_mean=r(mean) if _n==Y \ 
replace treatment_sd=r(sd) if _n==Y;
for var diff-treatment_sd: replace X=round(X, 0.001);
outsheet var control_mean control_sd treatment_mean treatment_sd p N if _n<=`vars' using "$appendix\TableB1_SummaryStatistics_Crit1_1",replace;

local vars= 4;

for any prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an   
\ num 1/`vars': 
replace var="X" if _n==Y \ 
ivreg2 X treatment if merge_results12 == 1 & merge_results07 == 1, robust \ 
replace N=e(N) if _n==Y \ 
mat beta=e(b) \ 
replace diff=beta[1,1] if _n==Y \ 
test treatment=0 \ 
replace p=r(p) if _n==Y \ 
sum X if treatment==0 & merge_results12 == 1 & merge_results07 == 1\ 
replace control_mean=r(mean) if _n==Y \ 
replace control_sd=r(sd) if _n==Y \ 
sum X if treatment==1 & merge_results12 == 1 & merge_results07 == 1 \ 
replace treatment_mean=r(mean) if _n==Y \ 
replace treatment_sd=r(sd) if _n==Y;
for var diff-treatment_sd: replace X=round(X, 0.001);
outsheet var control_mean control_sd treatment_mean treatment_sd p N if _n<=`vars' using "$appendix\TableB1_SummaryStatistics_Crit1_2",replace;

#delimit cr

drop var diff p control_mean control_sd treatment_mean treatment_sd N

* test for the joint significance of all characteristics

reg treatment level_randomization nb_registered_pr12t1_an prop_leftabstention_an prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_working_pop share_unemployed median_income if merge_results12 == 1 & merge_results07 == 1
test level_randomization nb_registered_pr12t1_an prop_leftabstention_an prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_working_pop share_unemployed median_income


*** Impact on voter turnout (territories which used the randomization lists, based on reports: first criterion) (Table B2)

* ITT

global controls = "nb_registered_pr12t1_an population share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_unemployed share_working_pop population_delta share_men_delta share_0014_delta share_1529_delta share_3044_delta share_4559_delta share_6074_delta share_unemployed_delta share_working_pop_delta"

su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_B.out", replace nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB2_ImpactTurnout_Crit1_B.out", append nonotes noni dec(4)

*** Impact on Hollande's vote share (territories which used the randomization lists, based on reports: first criterion) (Table B3)

* ITT

su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB3_ImpactPSShare_Crit1_B.out", append nonotes noni dec(4)


*** Randomization check (territories which used the randomization lists, based on survey: second criterion) (Table B4)

use "Analysis\analysis", clear

keep if territory_in_crit2 == 1

gen var=""
for any diff p control_mean control_sd treatment_mean treatment_sd N: gen X=.

#delimit;
local vars= 35;

for any level_randomization nb_registered_pr12t1_an prop_leftabstention_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_75p share_working_pop share_unemployed median_income   

\ num 1/`vars': 
replace var="X" if _n==Y \ 
ivreg2 X treatment if merge_results12 == 1, robust \ 
replace N=e(N) if _n==Y \ 
mat beta=e(b) \ 
replace diff=beta[1,1] if _n==Y \ 
test treatment=0 \ 
replace p=r(p) if _n==Y \ 
sum X if treatment==0 & merge_results12 == 1\ 
replace control_mean=r(mean) if _n==Y \ 
replace control_sd=r(sd) if _n==Y \ 
sum X if treatment==1 & merge_results12 == 1 \ 
replace treatment_mean=r(mean) if _n==Y \ 
replace treatment_sd=r(sd) if _n==Y;
for var diff-treatment_sd: replace X=round(X, 0.001);
outsheet var control_mean control_sd treatment_mean treatment_sd p N if _n<=`vars' using "$appendix\TableB4_SummaryStatistics_Crit2_1",replace;

local vars= 4;

for any prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an   
\ num 1/`vars': 
replace var="X" if _n==Y \ 
ivreg2 X treatment if merge_results12 == 1 & merge_results07 == 1, robust \ 
replace N=e(N) if _n==Y \ 
mat beta=e(b) \ 
replace diff=beta[1,1] if _n==Y \ 
test treatment=0 \ 
replace p=r(p) if _n==Y \ 
sum X if treatment==0 & merge_results12 == 1 & merge_results07 == 1\ 
replace control_mean=r(mean) if _n==Y \ 
replace control_sd=r(sd) if _n==Y \ 
sum X if treatment==1 & merge_results12 == 1 & merge_results07 == 1 \ 
replace treatment_mean=r(mean) if _n==Y \ 
replace treatment_sd=r(sd) if _n==Y;
for var diff-treatment_sd: replace X=round(X, 0.001);
outsheet var control_mean control_sd treatment_mean treatment_sd p N if _n<=`vars' using "$appendix\TableB4_SummaryStatistics_Crit2_2",replace;

#delimit cr

drop var diff p control_mean control_sd treatment_mean treatment_sd N

* test for the joint significance of all characteristics

reg treatment level_randomization nb_registered_pr12t1_an prop_leftabstention_an prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_75p share_working_pop share_unemployed median_income if merge_results12 == 1 & merge_results07 == 1
test level_randomization nb_registered_pr12t1_an prop_leftabstention_an prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_75p share_working_pop share_unemployed median_income


*** Impact on voter turnout (territories which used the randomization lists, based on survey: second criterion) (Table B5)

* ITT

global controls = "nb_registered_pr12t1_an population share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_unemployed share_working_pop population_delta share_men_delta share_0014_delta share_1529_delta share_3044_delta share_4559_delta share_6074_delta share_unemployed_delta share_working_pop_delta"

su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_B.out", replace nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB5_ImpactTurnout_Crit2_B.out", append nonotes noni dec(4)


*** Impact on Hollande's vote share (territories which used the randomization lists, based on survey: second criterion) (Table B6)

* ITT

su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableB6_ImpactPSShare_Crit2_B.out", append nonotes noni dec(4)


****************************************
*** PART II.C *** First stratum of each territory, or minimal sample (Appendix C)
****************************************

*** Randomization check (first stratum of each territory) (Table C1)

use "Analysis\analysis", clear

keep if territory_in == 1 & stratum == 1

gen var=""
for any diff p control_mean control_sd treatment_mean treatment_sd N: gen X=.

#delimit;
local vars= 36;

for any level_randomization nb_registered_pr12t1_an prop_leftabstention_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 region_id23 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_75p share_working_pop share_unemployed median_income   

\ num 1/`vars': 
replace var="X" if _n==Y \ 
ivreg2 X treatment if merge_results12 == 1, robust \ 
replace N=e(N) if _n==Y \ 
mat beta=e(b) \ 
replace diff=beta[1,1] if _n==Y \ 
test treatment=0 \ 
replace p=r(p) if _n==Y \ 
sum X if treatment==0 & merge_results12 == 1\ 
replace control_mean=r(mean) if _n==Y \ 
replace control_sd=r(sd) if _n==Y \ 
sum X if treatment==1 & merge_results12 == 1 \ 
replace treatment_mean=r(mean) if _n==Y \ 
replace treatment_sd=r(sd) if _n==Y;
for var diff-treatment_sd: replace X=round(X, 0.001);
outsheet var control_mean control_sd treatment_mean treatment_sd p N if _n<=`vars' using "$appendix\TableC1_SummaryStatistics_First_1",replace;

local vars= 4;

for any prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an   
\ num 1/`vars': 
replace var="X" if _n==Y \ 
ivreg2 X treatment if merge_results12 == 1 & merge_results07 == 1, robust \ 
replace N=e(N) if _n==Y \ 
mat beta=e(b) \ 
replace diff=beta[1,1] if _n==Y \ 
test treatment=0 \ 
replace p=r(p) if _n==Y \ 
sum X if treatment==0 & merge_results12 == 1 & merge_results07 == 1\ 
replace control_mean=r(mean) if _n==Y \ 
replace control_sd=r(sd) if _n==Y \ 
sum X if treatment==1 & merge_results12 == 1 & merge_results07 == 1 \ 
replace treatment_mean=r(mean) if _n==Y \ 
replace treatment_sd=r(sd) if _n==Y;
for var diff-treatment_sd: replace X=round(X, 0.001);
outsheet var control_mean control_sd treatment_mean treatment_sd p N if _n<=`vars' using "$appendix\TableC1_SummaryStatistics_First_2",replace;

#delimit cr

drop var diff p control_mean control_sd treatment_mean treatment_sd N

* test for the joint significance of all characteristics

reg treatment level_randomization nb_registered_pr12t1_an prop_leftabstention_an prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_working_pop share_unemployed median_income if merge_results12 == 1 & merge_results07 == 1
test level_randomization nb_registered_pr12t1_an prop_leftabstention_an prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_working_pop share_unemployed median_income


*** First stage (first stratum of each territory) (Table C2)

global controls = "nb_registered_pr12t1_an population share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_unemployed share_working_pop population_delta share_men_delta share_0014_delta share_1529_delta share_3044_delta share_4559_delta share_6074_delta share_unemployed_delta share_working_pop_delta"

su allocated if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 allocated treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC2_FirstStage_First.out", replace nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su allocated if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 allocated ${controls} prop_turnout_pr07t1_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC2_FirstStage_First.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_turnout_pr07t2_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC2_FirstStage_First.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_turnout_pr07t12_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC2_FirstStage_First.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_royal_pr07t1_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC2_FirstStage_First.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_royal_pr07t2_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC2_FirstStage_First.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_royal_pr07t12_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC2_FirstStage_First.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)


*** Impact on voter turnout (first stratum of each territory) (Table C3)

* ITT

su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_B.out", replace nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC3_ImpactTurnout_First_B.out", append nonotes noni dec(4)


*** Impact on Hollande's vote share (first stratum of each territory) (Table C4)

* ITT

su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC4_ImpactPSShare_First_B.out", append nonotes noni dec(4)


*** Randomization check (minimal sample) (Table C5)

use "Analysis\analysis", clear

keep if territory_in == 1 & smallest_set_strata == 1

gen var=""
for any diff p control_mean control_sd treatment_mean treatment_sd N: gen X=.

#delimit;
local vars= 36;

for any level_randomization nb_registered_pr12t1_an prop_leftabstention_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 region_id23 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_75p share_working_pop share_unemployed median_income   
\ num 1/`vars': 
replace var="X" if _n==Y \ 
ivreg2 X treatment if merge_results12 == 1, robust \ 
replace N=e(N) if _n==Y \ 
mat beta=e(b) \ 
replace diff=beta[1,1] if _n==Y \ 
test treatment=0 \ 
replace p=r(p) if _n==Y \ 
sum X if treatment==0 & merge_results12 == 1\ 
replace control_mean=r(mean) if _n==Y \ 
replace control_sd=r(sd) if _n==Y \ 
sum X if treatment==1 & merge_results12 == 1 \ 
replace treatment_mean=r(mean) if _n==Y \ 
replace treatment_sd=r(sd) if _n==Y;
for var diff-treatment_sd: replace X=round(X, 0.001);
outsheet var control_mean control_sd treatment_mean treatment_sd p N if _n<=`vars' using "$appendix\TableC5_SummaryStatistics_Min_1",replace;

local vars= 4;

for any prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an   
\ num 1/`vars': 
replace var="X" if _n==Y \ 
ivreg2 X treatment if merge_results12 == 1 & merge_results07 == 1, robust \ 
replace N=e(N) if _n==Y \ 
mat beta=e(b) \ 
replace diff=beta[1,1] if _n==Y \ 
test treatment=0 \ 
replace p=r(p) if _n==Y \ 
sum X if treatment==0 & merge_results12 == 1 & merge_results07 == 1\ 
replace control_mean=r(mean) if _n==Y \ 
replace control_sd=r(sd) if _n==Y \ 
sum X if treatment==1 & merge_results12 == 1 & merge_results07 == 1 \ 
replace treatment_mean=r(mean) if _n==Y \ 
replace treatment_sd=r(sd) if _n==Y;
for var diff-treatment_sd: replace X=round(X, 0.001);
outsheet var control_mean control_sd treatment_mean treatment_sd p N if _n<=`vars' using "$appendix\TableC5_SummaryStatistics_Min_2",replace;

#delimit cr

drop var diff p control_mean control_sd treatment_mean treatment_sd N

* test for the joint significance of all characteristics

reg treatment level_randomization nb_registered_pr12t1_an prop_leftabstention_an prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_working_pop share_unemployed median_income if merge_results12 == 1 & merge_results07 == 1
test level_randomization nb_registered_pr12t1_an prop_leftabstention_an prop_turnout_pr07t1_an prop_turnout_pr07t2_an prop_royal_pr07t1_an prop_royal_pr07t2_an population region_id2 region_id3 region_id4 region_id5 region_id6 region_id7 region_id8 region_id9 region_id10 region_id11 region_id12 region_id13 region_id14 region_id15 region_id16 region_id17 region_id18 region_id19 region_id20 region_id21 region_id22 share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_working_pop share_unemployed median_income


*** First stage (minimal sample) (Table C6)

global controls = "nb_registered_pr12t1_an population share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_unemployed share_working_pop population_delta share_men_delta share_0014_delta share_1529_delta share_3044_delta share_4559_delta share_6074_delta share_unemployed_delta share_working_pop_delta"

su allocated if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 allocated treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC6_FirstStage_Min.out", replace nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su allocated if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 allocated ${controls} prop_turnout_pr07t1_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC6_FirstStage_Min.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_turnout_pr07t2_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC6_FirstStage_Min.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_turnout_pr07t12_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC6_FirstStage_Min.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_royal_pr07t1_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC6_FirstStage_Min.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_royal_pr07t2_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC6_FirstStage_Min.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 allocated ${controls} prop_royal_pr07t12_an treatment prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC6_FirstStage_Min.out", append nonotes noni nolabel addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)


*** Impact on voter turnout (minimal sample) (Table C7)

* ITT

su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_B.out", replace nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC7_ImpactTurnout_Min_B.out", append nonotes noni dec(4)


*** Impact on Hollande's vote share (minimal sample) (Table C8)

* ITT

su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableC8_ImpactPSShare_Min_B.out", append nonotes noni dec(4)


****************************************
*** PART II.D *** Clustered standard errors (Appendix D)
****************************************

use "Analysis\analysis", clear

keep if territory_in == 1

* define territory's identifier

so departement_code territory
by departement_code territory: gen temp = _n
replace temp = 0 if temp ~= 1
gen territory_identifier = 1
replace territory_identifier = territory_identifier[_n-1] + temp if _n ~= 1
drop temp
label var territory_identifier "territory identifier"


*** Impact on voter turnout (regular cluster robust standard errors at the level of the territory) (Table D1)

* ITT

global controls = "nb_registered_pr12t1_an population share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_unemployed share_working_pop population_delta share_men_delta share_0014_delta share_1529_delta share_3044_delta share_4559_delta share_6074_delta share_unemployed_delta share_working_pop_delta"

su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_B.out", replace nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD1_ImpactTurnout_Clust1_B.out", append nonotes noni dec(4)


*** Impact on Hollande's vote share (regular cluster robust standard errors at the level of the territory) (Table D2)

* ITT

su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(territory_identifier)
quietly outreg2 using "$appendix\TableD2_ImpactPSShare_Clust1_B.out", append nonotes noni dec(4)


*** Impact on voter turnout (regular cluster robust standard errors at the level of the département) (Table D3)

* ITT

su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_B.out", replace nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD3_ImpactTurnout_Clust2_B.out", append nonotes noni dec(4)


*** Impact on Hollande's vote share (regular cluster robust standard errors at the level of the département) (Table D4)

* ITT

su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust cl(departement_code)
quietly outreg2 using "$appendix\TableD4_ImpactPSShare_Clust2_B.out", append nonotes noni dec(4)


*** Wild cluster bootstrap or pairs cluster bootstrap at the level of the departement or the region (Tables D5, D6, D7, D8)

/* The code below takes time to run and should be run using a server. Some variables may need to be renamed and their names shortened in order for all 
replications to go through. Since clustse does not admit the option "if", I drop observations not used in the regressions and first run the regressions
without controls, and second the regressions with controls. I use the wild cluster bootstrap procedure with 5,000 bootstrap iterations and the pairs cluster 
bootstrap procedure with 10,000 bootstrap iterations. (I could only use 5,000 replications with the wild cluster boostrap procedure due to matsize limit.)
*/

** Regressions without controls

keep if merge_results12 == 1

ta stratum_identifier, gen(stratum_id)

* Wild cluster bootstrap at the level of the departement (Table D5, columns 1, 4, 7)

log using "$appendix\TableD5_ImpactTurnout_WildCluster1_A.log", replace
clustse regress prop_turnout_pr12t1_an treatment stratum_id*, cluster(departement_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_turnout_pr12t2_an treatment stratum_id*, cluster(departement_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_turnout_pr12t12_an treatment stratum_id*, cluster(departement_code) method(wild) reps(5000) seed(13915183)
log close

log using "$appendix\TableD5_ImpactPSShare_WildCluster1_B.log", replace
clustse regress prop_hollande_pr12t1_an treatment stratum_id*, cluster(departement_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_hollande_pr12t2_an treatment stratum_id*, cluster(departement_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_hollande_pr12t12_an treatment stratum_id*, cluster(departement_code) method(wild) reps(5000) seed(13915183)
log close

* Wild cluster bootstrap at the level of the region (Table D6, columns 1, 4, 7)

log using "$appendix\TableD6_ImpactTurnout_WildCluster2_A.log", replace
clustse regress prop_turnout_pr12t1_an treatment stratum_id*, cluster(region_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_turnout_pr12t2_an treatment stratum_id*, cluster(region_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_turnout_pr12t12_an treatment stratum_id*, cluster(region_code) method(wild) reps(5000) seed(13915183)
log close

log using "$appendix\TableD6_ImpactPSShare_WildCluster2_B.log", replace
clustse regress prop_hollande_pr12t1_an treatment stratum_id*, cluster(region_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_hollande_pr12t2_an treatment stratum_id*, cluster(region_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_hollande_pr12t12_an treatment stratum_id*, cluster(region_code) method(wild) reps(5000) seed(13915183)
log close 

* Pairs cluster bootstrap at the level of the departement (Table D7, columns 1, 4, 7)

log using "$appendix\TableD7_ImpactTurnout_PairsCluster1_A.log", replace
clustse regress prop_turnout_pr12t1_an treatment stratum_id*, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_turnout_pr12t2_an treatment stratum_id*, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_turnout_pr12t12_an treatment stratum_id*, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
log close

log using "$appendix\TableD7_ImpactPSShare_PairsCluster1_B.log", replace
clustse regress prop_hollande_pr12t1_an treatment stratum_id*, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_hollande_pr12t2_an treatment stratum_id*, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_hollande_pr12t12_an treatment stratum_id*, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
log close 

* Pairs cluster bootstrap at the level of the region (Table D8, columns 1, 4, 7)

log using "$appendix\TableD8_ImpactTurnout_PairsCluster2_A.log", replace
clustse regress prop_turnout_pr12t1_an treatment stratum_id*, cluster(region_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_turnout_pr12t2_an treatment stratum_id*, cluster(region_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_turnout_pr12t12_an treatment stratum_id*, cluster(region_code) method(pairs) reps(10000) seed(13915183)
log close

log using "$appendix\TableD8_ImpactPSShare_PairsCluster2_B.log", replace
clustse regress prop_hollande_pr12t1_an treatment stratum_id*, cluster(region_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_hollande_pr12t2_an treatment stratum_id*, cluster(region_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_hollande_pr12t12_an treatment stratum_id*, cluster(region_code) method(pairs) reps(10000) seed(13915183)
log close


** Regressions with controls

keep if merge_results12 == 1 & merge_results07 == 1

drop stratum_id1 - stratum_id733
ta stratum_identifier , gen(stratum_id)

* Wild cluster bootstrap at the level of the departement (Table D5, columns 2, 3, 5, 6, 8, 9)

log using "$appendix\TableD5_ImpactTurnout_WildCluster1_Abis.log", replace
clustse regress prop_turnout_pr12t1_an treatment stratum_id* prop_turnout_pr07t1_an prop_leftabstention_an, cluster(departement_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_turnout_pr12t1_an treatment stratum_id* prop_turnout_pr07t1_an prop_leftabstention_an ${controls}, cluster(departement_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_turnout_pr12t2_an treatment stratum_id* prop_turnout_pr07t2_an prop_leftabstention_an, cluster(departement_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_turnout_pr12t2_an treatment stratum_id* prop_turnout_pr07t2_an prop_leftabstention_an ${controls}, cluster(departement_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_turnout_pr12t12_an treatment stratum_id* prop_turnout_pr07t12_an prop_leftabstention_an, cluster(departement_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_turnout_pr12t12_an treatment stratum_id* prop_turnout_pr07t12_an prop_leftabstention_an ${controls}, cluster(departement_code) method(wild) reps(5000) seed(13915183)
log close

log using "$appendix\TableD5_ImpactPSShare_WildCluster1_Bbis.log", replace
clustse regress prop_hollande_pr12t1_an treatment stratum_id* prop_royal_pr07t1_an prop_leftabstention_an, cluster(departement_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_hollande_pr12t1_an treatment stratum_id* prop_royal_pr07t1_an prop_leftabstention_an ${controls}, cluster(departement_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_hollande_pr12t2_an treatment stratum_id* prop_royal_pr07t2_an prop_leftabstention_an, cluster(departement_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_hollande_pr12t2_an treatment stratum_id* prop_royal_pr07t2_an prop_leftabstention_an ${controls}, cluster(departement_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_hollande_pr12t12_an treatment stratum_id* prop_royal_pr07t12_an prop_leftabstention_an, cluster(departement_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_hollande_pr12t12_an treatment stratum_id* prop_royal_pr07t12_an prop_leftabstention_an ${controls}, cluster(departement_code) method(wild) reps(5000) seed(13915183)
log close

* Wild cluster bootstrap at the level of the region (Table D6, columns 2, 3, 5, 6, 8, 9)

log using "$appendix\TableD6_ImpactTurnout_WildCluster2_Abis.log", replace
clustse regress prop_turnout_pr12t1_an treatment stratum_id* prop_turnout_pr07t1_an prop_leftabstention_an, cluster(region_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_turnout_pr12t1_an treatment stratum_id* prop_turnout_pr07t1_an prop_leftabstention_an ${controls}, cluster(region_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_turnout_pr12t2_an treatment stratum_id* prop_turnout_pr07t2_an prop_leftabstention_an, cluster(region_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_turnout_pr12t2_an treatment stratum_id* prop_turnout_pr07t2_an prop_leftabstention_an ${controls}, cluster(region_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_turnout_pr12t12_an treatment stratum_id* prop_turnout_pr07t12_an prop_leftabstention_an, cluster(region_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_turnout_pr12t12_an treatment stratum_id* prop_turnout_pr07t12_an prop_leftabstention_an ${controls}, cluster(region_code) method(wild) reps(5000) seed(13915183)
log close

log using "$appendix\TableD6_ImpactPSShare_WildCluster2_Bbis.log", replace
clustse regress prop_hollande_pr12t1_an treatment stratum_id* prop_royal_pr07t1_an prop_leftabstention_an, cluster(region_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_hollande_pr12t1_an treatment stratum_id* prop_royal_pr07t1_an prop_leftabstention_an ${controls}, cluster(region_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_hollande_pr12t2_an treatment stratum_id* prop_royal_pr07t2_an prop_leftabstention_an, cluster(region_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_hollande_pr12t2_an treatment stratum_id* prop_royal_pr07t2_an prop_leftabstention_an ${controls}, cluster(region_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_hollande_pr12t12_an treatment stratum_id* prop_royal_pr07t12_an prop_leftabstention_an, cluster(region_code) method(wild) reps(5000) seed(13915183)
clustse regress prop_hollande_pr12t12_an treatment stratum_id* prop_royal_pr07t12_an prop_leftabstention_an ${controls}, cluster(region_code) method(wild) reps(5000) seed(13915183)
log close

* Pairs cluster bootstrap at the level of the departement (Table D7, columns 2, 3, 5, 6, 8, 9)

log using "$appendix\TableD7_ImpactTurnout_PairsCluster1_Abis.log", replace
clustse regress prop_turnout_pr12t1_an treatment stratum_id* prop_turnout_pr07t1_an prop_leftabstention_an, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_turnout_pr12t1_an treatment stratum_id* prop_turnout_pr07t1_an prop_leftabstention_an ${controls}, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_turnout_pr12t2_an treatment stratum_id* prop_turnout_pr07t2_an prop_leftabstention_an, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_turnout_pr12t2_an treatment stratum_id* prop_turnout_pr07t2_an prop_leftabstention_an ${controls}, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_turnout_pr12t12_an treatment stratum_id* prop_turnout_pr07t12_an prop_leftabstention_an, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_turnout_pr12t12_an treatment stratum_id* prop_turnout_pr07t12_an prop_leftabstention_an ${controls}, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
log close

log using "$appendix\TableD7_ImpactPSShare_PairsCluster1_Bbis.log", replace
clustse regress prop_hollande_pr12t1_an treatment stratum_id* prop_royal_pr07t1_an prop_leftabstention_an, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_hollande_pr12t1_an treatment stratum_id* prop_royal_pr07t1_an prop_leftabstention_an ${controls}, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_hollande_pr12t2_an treatment stratum_id* prop_royal_pr07t2_an prop_leftabstention_an, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_hollande_pr12t2_an treatment stratum_id* prop_royal_pr07t2_an prop_leftabstention_an ${controls}, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_hollande_pr12t12_an treatment stratum_id* prop_royal_pr07t12_an prop_leftabstention_an, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_hollande_pr12t12_an treatment stratum_id* prop_royal_pr07t12_an prop_leftabstention_an ${controls}, cluster(departement_code) method(pairs) reps(10000) seed(13915183)
log close

* Pairs cluster bootstrap at the level of the region (Table D8, columns 2, 3, 5, 6, 8, 9)

log using "$appendix\TableD8_ImpactTurnout_PairsCluster2_Abis.log", replace
clustse regress prop_turnout_pr12t1_an treatment stratum_id* prop_turnout_pr07t1_an prop_leftabstention_an, cluster(region_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_turnout_pr12t1_an treatment stratum_id* prop_turnout_pr07t1_an prop_leftabstention_an ${controls}, cluster(region_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_turnout_pr12t2_an treatment stratum_id* prop_turnout_pr07t2_an prop_leftabstention_an, cluster(region_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_turnout_pr12t2_an treatment stratum_id* prop_turnout_pr07t2_an prop_leftabstention_an ${controls}, cluster(region_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_turnout_pr12t12_an treatment stratum_id* prop_turnout_pr07t12_an prop_leftabstention_an, cluster(region_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_turnout_pr12t12_an treatment stratum_id* prop_turnout_pr07t12_an prop_leftabstention_an ${controls}, cluster(region_code) method(pairs) reps(10000) seed(13915183)
log close

log using "$appendix\TableD8_ImpactPSShare_PairsCluster2_Bbis.log", replace
clustse regress prop_hollande_pr12t1_an treatment stratum_id* prop_royal_pr07t1_an prop_leftabstention_an, cluster(region_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_hollande_pr12t1_an treatment stratum_id* prop_royal_pr07t1_an prop_leftabstention_an ${controls}, cluster(region_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_hollande_pr12t2_an treatment stratum_id* prop_royal_pr07t2_an prop_leftabstention_an, cluster(region_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_hollande_pr12t2_an treatment stratum_id* prop_royal_pr07t2_an prop_leftabstention_an ${controls}, cluster(region_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_hollande_pr12t12_an treatment stratum_id* prop_royal_pr07t12_an prop_leftabstention_an, cluster(region_code) method(pairs) reps(10000) seed(13915183)
clustse regress prop_hollande_pr12t12_an treatment stratum_id* prop_royal_pr07t12_an prop_leftabstention_an ${controls}, cluster(region_code) method(pairs) reps(10000) seed(13915183)
log close

* Reseting Matsize (change due to clustse command)
set matsize 11000

****************************************
*** PART II.E *** Trimming precincts with the largest number of registered citizens (Appendix E)
****************************************

use "Analysis\analysis", clear

keep if territory_in == 1

* define the 95th and 90th percentile of nb_registered_pr12t1_an

su nb_registered_pr12t1_an if merge_results12 == 1, detail
gen nb_registered_pr12t1_an_95p = r(p95)
gen nb_registered_pr12t1_an_90p = r(p90)
label var nb_registered_pr12t1_an_95p "pr12t1: # registered voters, 95th percentile"
label var nb_registered_pr12t1_an_90p "pr12t1: # registered voters, 90th percentile"


*** Impact on voter turnout, trimming the 5% precincts with the largest number of registered citizens (Table E1)

* ITT

global controls = "nb_registered_pr12t1_an population share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_unemployed share_working_pop population_delta share_men_delta share_0014_delta share_1529_delta share_3044_delta share_4559_delta share_6074_delta share_unemployed_delta share_working_pop_delta"

su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_B.out", replace nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE1_ImpactTurnout_Trim95_B.out", append nonotes noni dec(4)


*** Impact on Hollande's vote share, trimming the 5% precincts with the largest number of registered citizens (Table E2)

* ITT

su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_95p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE2_ImpactPSShare_Trim95_B.out", append nonotes noni dec(4)


*** Impact on voter turnout, trimming the 10% precincts with the largest number of registered citizens (Table E3)

* ITT

su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t1_an treatment prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t2_an treatment prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t12_an treatment prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_B.out", replace nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated = treatment) prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated = treatment) prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated = treatment) prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE3_ImpactTurnout_Trim90_B.out", append nonotes noni dec(4)


*** Impact on Hollande's vote share, trimming the 10% precincts with the largest number of registered citizens (Table E4)

* ITT

su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t1_an treatment prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t2_an treatment prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t12_an treatment prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated = treatment) prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated = treatment) prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) if merge_results12 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated = treatment) prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1 & nb_registered_pr12t1_an <= nb_registered_pr12t1_an_90p, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableE4_ImpactPSShare_Trim90_B.out", append nonotes noni dec(4)


****************************************
*** PART II.F *** Using the change in the dependent variable as outcome (Appendix F)
****************************************

use "Analysis\analysis", clear

keep if territory_in == 1

* define the change in turnout or in the PS candidate's vote share between 2007 and 2012

foreach round in t1 t2 t12 {
gen diff_prop_turnout_`round'_an = prop_turnout_pr12`round'_an - prop_turnout_pr07`round'_an
gen diff_prop_hollande_`round'_an = prop_hollande_pr12`round'_an - prop_royal_pr07`round'_an
}
label var diff_prop_turnout_t1_an "pr12 - pr07, t1: % voters who participated"
label var diff_prop_turnout_t2_an "pr12 - pr07, t2: % voters who participated"
label var diff_prop_turnout_t12_an "pr12 - pr07, average t1/t2: % voters who participated"
label var diff_prop_hollande_t1_an "pr12 - pr07, t1: % votes, PS candidate"
label var diff_prop_hollande_t2_an "pr12 - pr07, t2: % votes, PS candidate"
label var diff_prop_hollande_t12_an "pr12 - pr07, average t1/t2: % votes, PS candidate"


*** Impact on the difference between turnout at the 2012 and 2007 presidential elections (Table F1)

* ITT

global controls = "nb_registered_pr12t1_an population_delta share_men_delta share_0014_delta share_1529_delta share_3044_delta share_4559_delta share_6074_delta share_unemployed_delta share_working_pop_delta"

su diff_prop_turnout_t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 diff_prop_turnout_t1_an treatment if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF1_ImpactTurnout_Diff_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 diff_prop_turnout_t1_an treatment ${controls} if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF1_ImpactTurnout_Diff_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su diff_prop_turnout_t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 diff_prop_turnout_t2_an treatment if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF1_ImpactTurnout_Diff_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 diff_prop_turnout_t2_an treatment ${controls} if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF1_ImpactTurnout_Diff_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su diff_prop_turnout_t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 diff_prop_turnout_t12_an treatment if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF1_ImpactTurnout_Diff_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 diff_prop_turnout_t12_an treatment ${controls} if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF1_ImpactTurnout_Diff_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 diff_prop_turnout_t1_an (allocated = treatment) if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF1_ImpactTurnout_Diff_B.out", replace nonotes noni dec(4)
xtivreg2 diff_prop_turnout_t1_an (allocated = treatment) ${controls} if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF1_ImpactTurnout_Diff_B.out", append nonotes noni dec(4)

xtivreg2 diff_prop_turnout_t2_an (allocated = treatment) if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF1_ImpactTurnout_Diff_B.out", append nonotes noni dec(4)
xtivreg2 diff_prop_turnout_t2_an (allocated = treatment) ${controls} if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF1_ImpactTurnout_Diff_B.out", append nonotes noni dec(4)

xtivreg2 diff_prop_turnout_t12_an (allocated = treatment) if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF1_ImpactTurnout_Diff_B.out", append nonotes noni dec(4)
xtivreg2 diff_prop_turnout_t12_an (allocated = treatment) ${controls} if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF1_ImpactTurnout_Diff_B.out", append nonotes noni dec(4)


*** Impact on the difference between Hollande and Royal's vote share in 2012 and 2007 (Table F2)

* ITT

su diff_prop_hollande_t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 diff_prop_hollande_t1_an treatment if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF2_ImpactPSShare_Diff_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 diff_prop_hollande_t1_an treatment ${controls} if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF2_ImpactPSShare_Diff_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su diff_prop_hollande_t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 diff_prop_hollande_t2_an treatment if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF2_ImpactPSShare_Diff_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 diff_prop_hollande_t2_an treatment ${controls} if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF2_ImpactPSShare_Diff_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su diff_prop_hollande_t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 diff_prop_hollande_t12_an treatment if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF2_ImpactPSShare_Diff_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 diff_prop_hollande_t12_an treatment ${controls} if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF2_ImpactPSShare_Diff_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 diff_prop_hollande_t1_an (allocated = treatment) if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF2_ImpactPSShare_Diff_B.out", replace nonotes noni dec(4)
xtivreg2 diff_prop_hollande_t1_an (allocated = treatment) ${controls} if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF2_ImpactPSShare_Diff_B.out", append nonotes noni dec(4)

xtivreg2 diff_prop_hollande_t2_an (allocated = treatment) if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF2_ImpactPSShare_Diff_B.out", append nonotes noni dec(4)
xtivreg2 diff_prop_hollande_t2_an (allocated = treatment) ${controls} if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF2_ImpactPSShare_Diff_B.out", append nonotes noni dec(4)

xtivreg2 diff_prop_hollande_t12_an (allocated = treatment) if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF2_ImpactPSShare_Diff_B.out", append nonotes noni dec(4)
xtivreg2 diff_prop_hollande_t12_an (allocated = treatment) ${controls} if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableF2_ImpactPSShare_Diff_B.out", append nonotes noni dec(4)


****************************************
*** PART II.G *** Treatment impact heterogeneity along PO (prop_leftabstention_an) (Appendix G)
****************************************

use "Analysis\analysis", clear

keep if territory_in == 1

* define interactions between treatment / allocated and prop_leftabstention_an

gen treatment_po = treatment * prop_leftabstention_an
gen allocated_po = allocated * prop_leftabstention_an
label var treatment_po "treatment interacted with PO"
label var allocated_po "allocated interacted with PO"

su prop_leftabstention_an if merge_results12 == 1, detail
gen high_po = (prop_leftabstention_an >= r(p50)) & prop_leftabstention_an ~= .
gen treatment_highpo = treatment * high_po
gen allocated_highpo = allocated * high_po
gen treatment_lowpo = treatment * (1 - high_po)
gen allocated_lowpo = allocated * (1 - high_po)
label var high_po "dummy indicating a precinct with high PO"
label var treatment_highpo "treatment interacted with high PO"
label var allocated_highpo "allocated interacted with high PO"
label var treatment_lowpo "treatment interacted with low PO"
label var allocated_lowpo "allocated interacted with low PO"


*** Impact on voter turnout, differentiated for high vs. low PO precincts (Table G1)

global controls = "nb_registered_pr12t1_an population share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_unemployed share_working_pop population_delta share_men_delta share_0014_delta share_1529_delta share_3044_delta share_4559_delta share_6074_delta share_unemployed_delta share_working_pop_delta"

su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment_lowpo treatment_highpo high_po if merge_results12 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_A.out", replace nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment_lowpo treatment_highpo prop_turnout_pr07t1_an high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t1_an treatment_lowpo treatment_highpo prop_turnout_pr07t1_an ${controls} high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment_lowpo treatment_highpo high_po if merge_results12 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment_lowpo treatment_highpo prop_turnout_pr07t2_an high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t2_an treatment_lowpo treatment_highpo prop_turnout_pr07t2_an ${controls} high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment_lowpo treatment_highpo high_po if merge_results12 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment_lowpo treatment_highpo prop_turnout_pr07t12_an high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t12_an treatment_lowpo treatment_highpo prop_turnout_pr07t12_an ${controls} high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_turnout_pr12t1_an high_po (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) if merge_results12 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_B.out", replace nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t1_an (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) prop_turnout_pr07t1_an high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t1_an (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) prop_turnout_pr07t1_an ${controls} high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)

xtivreg2 prop_turnout_pr12t2_an high_po (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) if merge_results12 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t2_an (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) prop_turnout_pr07t2_an high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t2_an (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) prop_turnout_pr07t2_an ${controls} high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)

xtivreg2 prop_turnout_pr12t12_an high_po (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) if merge_results12 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t12_an (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) prop_turnout_pr07t12_an high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t12_an (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) prop_turnout_pr07t12_an ${controls} high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG1_ImpactTurnout_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)


*** Impact on Hollande's vote share, differentiated for high vs. low PO precincts (Table G2)

* ITT

su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment_lowpo treatment_highpo high_po if merge_results12 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_A.out", replace nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment_lowpo treatment_highpo prop_royal_pr07t1_an high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t1_an treatment_lowpo treatment_highpo prop_royal_pr07t1_an ${controls} high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment_lowpo treatment_highpo high_po if merge_results12 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment_lowpo treatment_highpo prop_royal_pr07t2_an high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t2_an treatment_lowpo treatment_highpo prop_royal_pr07t2_an ${controls} high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment_lowpo treatment_highpo high_po if merge_results12 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment_lowpo treatment_highpo prop_royal_pr07t12_an high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t12_an treatment_lowpo treatment_highpo prop_royal_pr07t12_an ${controls} high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom treatment_highpo - treatment_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_A.out", append nonotes noni addstat("control mean", `mean_control', "estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1_an high_po (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) if merge_results12 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_B.out", replace nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t1_an (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) prop_royal_pr07t1_an high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t1_an (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) prop_royal_pr07t1_an ${controls} high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)

xtivreg2 prop_hollande_pr12t2_an high_po (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) if merge_results12 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t2_an (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) prop_royal_pr07t2_an high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t2_an (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) prop_royal_pr07t2_an ${controls} high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)

xtivreg2 prop_hollande_pr12t12_an high_po (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) if merge_results12 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t12_an (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) prop_royal_pr07t12_an high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t12_an (allocated_lowpo allocated_highpo = treatment_lowpo treatment_highpo) prop_royal_pr07t12_an ${controls} high_po prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
lincom allocated_highpo - allocated_lowpo
local estimate = r(estimate)
local se = r(se)
quietly outreg2 using "$appendix\TableG2_ImpactPSShare_Het1_B.out", append nonotes noni addstat("estimate", `estimate', "se", `se') dec(4) adec(4) rdec(3)


*** Impact on voter turnout, interacting treatment with PO (Table G3)

* ITT

su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment treatment_po prop_leftabstention_an if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t1_an treatment treatment_po prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t1_an treatment treatment_po prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment treatment_po prop_leftabstention_an if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t2_an treatment treatment_po prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t2_an treatment treatment_po prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment treatment_po prop_leftabstention_an if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_turnout_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_turnout_pr12t12_an treatment treatment_po prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_turnout_pr12t12_an treatment treatment_po prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_turnout_pr12t1_an prop_leftabstention_an (allocated allocated_po = treatment treatment_po) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_B.out", replace nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated allocated_po = treatment treatment_po) prop_turnout_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t1_an (allocated allocated_po = treatment treatment_po) prop_turnout_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t2_an prop_leftabstention_an (allocated allocated_po = treatment treatment_po) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated allocated_po = treatment treatment_po) prop_turnout_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t2_an (allocated allocated_po = treatment treatment_po) prop_turnout_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_B.out", append nonotes noni dec(4)

xtivreg2 prop_turnout_pr12t12_an prop_leftabstention_an (allocated allocated_po = treatment treatment_po) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated allocated_po = treatment treatment_po) prop_turnout_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_B.out", append nonotes noni dec(4)
xtivreg2 prop_turnout_pr12t12_an (allocated allocated_po = treatment treatment_po) prop_turnout_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG3_ImpactTurnout_Het2_B.out", append nonotes noni dec(4)


*** Impact on Hollande's vote share, interacting treatment with PO (Table G4)

* ITT

su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment treatment_po prop_leftabstention_an if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t1_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t1_an treatment treatment_po prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t1_an treatment treatment_po prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment treatment_po prop_leftabstention_an if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t2_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t2_an treatment treatment_po prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t2_an treatment treatment_po prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment treatment_po prop_leftabstention_an if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_pr12t12_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_pr12t12_an treatment treatment_po prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_pr12t12_an treatment treatment_po prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_pr12t1_an prop_leftabstention_an (allocated allocated_po = treatment treatment_po) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated allocated_po = treatment treatment_po) prop_royal_pr07t1_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t1_an (allocated allocated_po = treatment treatment_po) prop_royal_pr07t1_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t2_an prop_leftabstention_an (allocated allocated_po = treatment treatment_po) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated allocated_po = treatment treatment_po) prop_royal_pr07t2_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t2_an (allocated allocated_po = treatment treatment_po) prop_royal_pr07t2_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_pr12t12_an prop_leftabstention_an (allocated allocated_po = treatment treatment_po) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated allocated_po = treatment treatment_po) prop_royal_pr07t12_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_pr12t12_an (allocated allocated_po = treatment treatment_po) prop_royal_pr07t12_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableG4_ImpactPSShare_Het2_B.out", append nonotes noni dec(4)


****************************************
*** PART II.H *** Seemingly unrelated regressions (Appendix H)
****************************************

use "Analysis\analysis", clear
keep if territory_in == 1

global controls = "nb_registered_pr12t1_an population share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_unemployed share_working_pop population_delta share_men_delta share_0014_delta share_1529_delta share_3044_delta share_4559_delta share_6074_delta share_unemployed_delta share_working_pop_delta"

gen id = _n

expand 2
bys id: gen n = _n

* define outcomes

gen turnout_hollande_pr12t1 = prop_turnout_pr12t1_an if n == 1
replace turnout_hollande_pr12t1 = prop_hollande_pr12t1b_an if n == 2
gen turnout_hollande_pr12t2 = prop_turnout_pr12t2_an if n == 1
replace turnout_hollande_pr12t2 = prop_hollande_pr12t2b_an if n == 2
gen turnout_hollande_pr12 = prop_turnout_pr12t12_an if n == 1
replace turnout_hollande_pr12 = prop_hollande_pr12t12b_an if n == 2

gen droite_farleft_pr12t1 = prop_right_pr12t1_an if n == 1
replace droite_farleft_pr12t1 = prop_farleft_pr12t1_an if n == 2
gen droite_otherleft_pr12t1 = prop_right_pr12t1_an if n == 1
replace droite_otherleft_pr12t1 = prop_otherleft_pr12t1_an if n == 2
gen droite_centre_pr12t1 = prop_right_pr12t1_an if n == 1
replace droite_centre_pr12t1 = prop_center_pr12t1_an if n == 2
gen droite_exdroite_pr12t1 = prop_right_pr12t1_an if n == 1
replace droite_exdroite_pr12t1 = prop_farright_pr12t1_an if n == 2

* define controls

gen prop_turnout_pr07t1_an_1 = prop_turnout_pr07t1_an if n == 1
replace prop_turnout_pr07t1_an_1 = 0 if n == 2
gen prop_royal_pr07t1b_an_2 = prop_royal_pr07t1b_an if n == 2
replace prop_royal_pr07t1b_an_2 = 0 if n == 1

gen prop_turnout_pr07t2_an_1 = prop_turnout_pr07t2_an if n == 1
replace prop_turnout_pr07t2_an_1 = 0 if n == 2
gen prop_royal_pr07t2b_an_2 = prop_royal_pr07t2b_an if n == 2
replace prop_royal_pr07t2b_an_2 = 0 if n == 1

gen prop_turnout_pr07t12_an_1 = prop_turnout_pr07t12_an if n == 1
replace prop_turnout_pr07t12_an_1 = 0 if n == 2
gen prop_royal_pr07t12b_an_2 = prop_royal_pr07t12b_an if n == 2
replace prop_royal_pr07t12b_an_2 = 0 if n == 1

gen prop_right_pr07t1_an_1 = prop_right_pr07t1_an if n == 1
replace prop_right_pr07t1_an_1 = 0 if n == 2
gen prop_farleft_pr07t1_an_2 = prop_farleft_pr07t1_an if n == 2
replace prop_farleft_pr07t1_an_2 = 0 if n == 1
gen prop_otherleft_pr07t1_an_2 = prop_otherleft_pr07t1_an if n == 2
replace prop_otherleft_pr07t1_an_2 = 0 if n == 1
gen prop_center_pr07t1_an_2 = prop_center_pr07t1_an if n == 2
replace prop_center_pr07t1_an_2 = 0 if n == 1
gen prop_farright_pr07t1_an_2 = prop_farright_pr07t1_an if n == 2
replace prop_farright_pr07t1_an_2 = 0 if n == 1

foreach var of varlist treatment nb_registered_pr12t1_an population share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_unemployed share_working_pop population_delta share_men_delta share_0014_delta share_1529_delta share_3044_delta share_4559_delta share_6074_delta share_unemployed_delta share_working_pop_delta prop_leftabstention_an{
gen `var'_1 = `var' if n == 1
replace `var'_1 = 0 if n == 2
gen `var'_2 = `var' if n == 2
replace `var'_2 = 0 if n == 1
}

ta stratum_identifier if merge_results12 == 1, gen(stratum_id)

forvalues i=1(1)733{
gen stratum_id`i'_1 = stratum_id`i' if n == 1
replace stratum_id`i'_1 = 0 if n == 2
gen stratum_id`i'_2 = stratum_id`i' if n == 2
replace stratum_id`i'_2 = 0 if n == 1
}

global controls = "nb_registered_pr12t1_an_1 population_1 share_men_1 share_0014_1 share_1529_1 share_3044_1 share_4559_1 share_6074_1 share_unemployed_1 share_working_pop_1 population_delta_1 share_men_delta_1 share_0014_delta_1 share_1529_delta_1 share_3044_delta_1 share_4559_delta_1 share_6074_delta_1 share_unemployed_delta_1 share_working_pop_delta_1 nb_registered_pr12t1_an_2 population_2 share_men_2 share_0014_2 share_1529_2 share_3044_2 share_4559_2 share_6074_2 share_unemployed_2 share_working_pop_2 population_delta_2 share_men_delta_2 share_0014_delta_2 share_1529_delta_2 share_3044_delta_2 share_4559_delta_2 share_6074_delta_2 share_unemployed_delta_2 share_working_pop_delta_2"


*** Comparison turnout / hollande (Table H1)

ivreg2 turnout_hollande_pr12t1 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 n if merge_results12 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
local ratio = _b[treatment_1] / _b[treatment_2]
quietly outreg2 using "$appendix\TableH1_ImpactComparison_TurnoutVote.out", replace nonotes noni nor2 addstat("ratio", `ratio', "pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)
ivreg2 turnout_hollande_pr12t1 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 prop_leftabstention_an_* prop_turnout_pr07t1_an_1 prop_royal_pr07t1b_an_2 n if merge_results12 == 1 & merge_results07 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
local ratio = _b[treatment_1] / _b[treatment_2]
quietly outreg2 using "$appendix\TableH1_ImpactComparison_TurnoutVote.out", append nonotes noni nor2 addstat("ratio", `ratio', "pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)
ivreg2 turnout_hollande_pr12t1 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 prop_leftabstention_an_* prop_turnout_pr07t1_an_1 prop_royal_pr07t1b_an_2 n ${controls} if merge_results12 == 1 & merge_results07 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
local ratio = _b[treatment_1] / _b[treatment_2]
quietly outreg2 using "$appendix\TableH1_ImpactComparison_TurnoutVote.out", append nonotes noni nor2 addstat("ratio", `ratio', "pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)

ivreg2 turnout_hollande_pr12t2 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 n if merge_results12 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
local ratio = _b[treatment_1] / _b[treatment_2]
quietly outreg2 using "$appendix\TableH1_ImpactComparison_TurnoutVote.out", append nonotes noni nor2 addstat("ratio", `ratio', "pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)
ivreg2 turnout_hollande_pr12t2 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 prop_leftabstention_an_* prop_turnout_pr07t2_an_1 prop_royal_pr07t2b_an_2 n if merge_results12 == 1 & merge_results07 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
local ratio = _b[treatment_1] / _b[treatment_2]
quietly outreg2 using "$appendix\TableH1_ImpactComparison_TurnoutVote.out", append nonotes noni nor2 addstat("ratio", `ratio', "pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)
ivreg2 turnout_hollande_pr12t2 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 prop_leftabstention_an_* prop_turnout_pr07t2_an_1 prop_royal_pr07t2b_an_2 n ${controls} if merge_results12 == 1 & merge_results07 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
local ratio = _b[treatment_1] / _b[treatment_2]
quietly outreg2 using "$appendix\TableH1_ImpactComparison_TurnoutVote.out", append nonotes noni nor2 addstat("ratio", `ratio', "pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)

ivreg2 turnout_hollande_pr12 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 n if merge_results12 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
local ratio = _b[treatment_1] / _b[treatment_2]
quietly outreg2 using "$appendix\TableH1_ImpactComparison_TurnoutVote.out", append nonotes noni nor2 addstat("ratio", `ratio', "pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)
ivreg2 turnout_hollande_pr12 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 prop_leftabstention_an_* prop_turnout_pr07t12_an_1 prop_royal_pr07t12b_an_2 n if merge_results12 == 1 & merge_results07 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
local ratio = _b[treatment_1] / _b[treatment_2]
quietly outreg2 using "$appendix\TableH1_ImpactComparison_TurnoutVote.out", append nonotes noni nor2 addstat("ratio", `ratio', "pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)
ivreg2 turnout_hollande_pr12 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 prop_leftabstention_an_* prop_turnout_pr07t12_an_1 prop_royal_pr07t12b_an_2 n ${controls} if merge_results12 == 1 & merge_results07 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
local ratio = _b[treatment_1] / _b[treatment_2]
quietly outreg2 using "$appendix\TableH1_ImpactComparison_TurnoutVote.out", append nonotes noni nor2 addstat("ratio", `ratio', "pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)


*** Comparison impact on other parties (Table H2)

ivreg2 droite_farleft_pr12t1 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 n if merge_results12 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
quietly outreg2 using "$appendix\TableH2_ImpactComparison_OtherShares.out", replace nonotes noni nor2 addstat("pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)
ivreg2 droite_farleft_pr12t1 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 prop_leftabstention_an_* prop_right_pr07t1_an_1 prop_farleft_pr07t1_an_2 n ${controls} if merge_results12 == 1 & merge_results07 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
quietly outreg2 using "$appendix\TableH2_ImpactComparison_OtherShares.out", append nonotes noni nor2 addstat("pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)

ivreg2 droite_otherleft_pr12t1 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 n if merge_results12 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
quietly outreg2 using "$appendix\TableH2_ImpactComparison_OtherShares.out", append nonotes noni nor2 addstat("pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)
ivreg2 droite_otherleft_pr12t1 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 prop_leftabstention_an_* prop_right_pr07t1_an_1 prop_otherleft_pr07t1_an_2 n ${controls} if merge_results12 == 1 & merge_results07 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
quietly outreg2 using "$appendix\TableH2_ImpactComparison_OtherShares.out", append nonotes noni nor2 addstat("pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)

ivreg2 droite_centre_pr12t1 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 n if merge_results12 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
quietly outreg2 using "$appendix\TableH2_ImpactComparison_OtherShares.out", append nonotes noni nor2 addstat("pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)
ivreg2 droite_centre_pr12t1 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 prop_leftabstention_an_* prop_right_pr07t1_an_1 prop_center_pr07t1_an_2 n ${controls} if merge_results12 == 1 & merge_results07 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
quietly outreg2 using "$appendix\TableH2_ImpactComparison_OtherShares.out", append nonotes noni nor2 addstat("pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)

ivreg2 droite_exdroite_pr12t1 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 n if merge_results12 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
quietly outreg2 using "$appendix\TableH2_ImpactComparison_OtherShares.out", append nonotes noni nor2 addstat("pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)
ivreg2 droite_exdroite_pr12t1 treatment_1 treatment_2 stratum_id*_1 stratum_id*_2 prop_leftabstention_an_* prop_right_pr07t1_an_1 prop_farright_pr07t1_an_2 n ${controls} if merge_results12 == 1 & merge_results07 == 1, cl(id) small
test treatment_1 = treatment_2
local pvalue = r(p)
local fstatistic = r(F)
quietly outreg2 using "$appendix\TableH2_ImpactComparison_OtherShares.out", append nonotes noni nor2 addstat("pvalue", `pvalue', "F-statistic", `fstatistic') dec(4) adec(3)


****************************************
*** PART II.I *** Impact on the difference between Hollande's vote share (expressed as a fraction of registered voters) and voter turnout (Appendix I, Table I1)
****************************************

use "Analysis\analysis", clear

keep if territory_in == 1

* define vote share (as fraction of registered voters) net of turnout

foreach round in t1 t2 t12 {
gen prop_hollande_net_pr12`round'b_an = prop_hollande_pr12`round'b_an - prop_turnout_pr12`round'_an
gen prop_royal_net_pr07`round'b_an = prop_royal_pr07`round'b_an - prop_turnout_pr07`round'_an
}
label var prop_hollande_net_pr12t1b_an "pr12t1: % votes (/ reg. cit.), Hollande - % voters who participated"
label var prop_hollande_net_pr12t2b_an "pr12t2: % votes (/ reg. cit.), Hollande - % voters who participated"
label var prop_hollande_net_pr12t12b_an "pr12, average t1/t2: % votes (/ reg. cit.), Hollande - % voters who participated"
label var prop_royal_net_pr07t1b_an "pr07t1: % votes (/ reg. cit.), Royal - % voters who participated"
label var prop_royal_net_pr07t2b_an "pr07t1: % votes (/ reg. cit.), Royal - % voters who participated"
label var prop_royal_net_pr07t12b_an "pr07, average t1/t2: % votes (/ reg. cit.), Royal - % voters who participated"

* ITT

global controls = "nb_registered_pr12t1_an population share_men share_0014 share_1529 share_3044 share_4559 share_6074 share_unemployed share_working_pop population_delta share_men_delta share_0014_delta share_1529_delta share_3044_delta share_4559_delta share_6074_delta share_unemployed_delta share_working_pop_delta"

su prop_hollande_net_pr12t1b_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_net_pr12t1b_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_A.out", replace nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_net_pr12t1b_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_net_pr12t1b_an treatment prop_royal_net_pr07t1b_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_net_pr12t1b_an treatment prop_royal_net_pr07t1b_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_net_pr12t2b_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_net_pr12t2b_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_net_pr12t2b_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_net_pr12t2b_an treatment prop_royal_net_pr07t2b_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_net_pr12t2b_an treatment prop_royal_net_pr07t2b_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

su prop_hollande_net_pr12t12b_an if treatment == 0 & merge_results12 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_net_pr12t12b_an treatment if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
su prop_hollande_net_pr12t12b_an if treatment == 0 & merge_results12 == 1 & merge_results07 == 1
local mean_control=r(mean)
xtivreg2 prop_hollande_net_pr12t12b_an treatment prop_royal_net_pr07t12b_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)
xtivreg2 prop_hollande_net_pr12t12b_an treatment prop_royal_net_pr07t12b_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_A.out", append nonotes noni addstat("control mean", `mean_control') dec(4) adec(4) rdec(3)

* IV

xtivreg2 prop_hollande_net_pr12t1b_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_B.out", replace nonotes noni dec(4)
xtivreg2 prop_hollande_net_pr12t1b_an (allocated = treatment) prop_royal_net_pr07t1b_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_net_pr12t1b_an (allocated = treatment) prop_royal_net_pr07t1b_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_net_pr12t2b_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_net_pr12t2b_an (allocated = treatment) prop_royal_net_pr07t2b_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_net_pr12t2b_an (allocated = treatment) prop_royal_net_pr07t2b_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_B.out", append nonotes noni dec(4)

xtivreg2 prop_hollande_net_pr12t12b_an (allocated = treatment) if merge_results12 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_net_pr12t12b_an (allocated = treatment) prop_royal_net_pr07t12b_an prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_B.out", append nonotes noni dec(4)
xtivreg2 prop_hollande_net_pr12t12b_an (allocated = treatment) prop_royal_net_pr07t12b_an ${controls} prop_leftabstention_an if merge_results12 == 1 & merge_results07 == 1, i(stratum_identifier) fe robust
quietly outreg2 using "$appendix\TableI1_ImpactDiff_PSShareTurnout_B.out", append nonotes noni dec(4)
