// BOTH MODELS ARE WITH INDUSTRY AND EDUC SPECIFICATION (IN BOTH CASSES, WE ANALYSE POOLED CHARACTERISTICS - 
// WE DON'T DIFFER such AREAS as (catering and sales) & (transportation), (insurance) & (credit and finance) 
//MODEL1 - WITHOUT SPECIFICATION OF STATUS, I.E. THE REASON FOR BEING UNEMPLOYED

//THE MODEL NOT ONLY PRINTS THE RESULTS, IT WRITES A SEPARATE XLS-FILE WITH THE MODEL-TABLE

use "Project_2_data_after_stset.dta", clear 


drop if status == .
drop if napravl6==.
drop if serv6==.
drop if age_n>65 
replace age = age_n
drop if gender == .
drop if gender == 1

// generate variables for married
drop if semejnoe ==.

gen married = 1 if semejnoe ==1
replace married = 0 if semejnoe !=1
drop if obrzw == .
drop if stav_n == . 

drop if ivdiwen ==.
//define the education: 

// we have chosen 2 as baseline because 2nd is the biggest
// !!NB THE README!
// EDUC1 == 1 if the education is 9 grades

// EDUC3==1 if the education is PTU
// EDUC4 ==1 if the education is college
// EDUC5==1 if the education is higher (uni and upper)
 
gen EDUC1 = 1 if obrzw == 1
replace EDUC1 =0 if obrzw!=1
gen EDUC3 = 1 if obrzw == 3
replace EDUC3 = 0 if obrzw!=3
gen EDUC4 = 1 if obrzw == 4
replace EDUC4 = 0 if obrzw!=4
gen EDUC5 = 1 if inlist(obrzw, 5, 6,7)
replace EDUC5=0 if EDUC5==.

// specification for the industry of last engagement: 

gen industry = otrasl_s
drop if industry == .
// industry1 for those in industry, construction, light industry, other material branches
gen industry1 = 1 if industry == 1 | industry == 18| industry == 10 |industry ==4
replace industry1 = 0 if industry1==.
// industry2 if agriculture

gen industry2 = 1 if industry == 2
replace industry2 = 0 if industry2==.
// industry3 if transport sales and catering
gen industry3 = 1 if industry == 3 | industry == 5
replace industry3=0 if industry3==.

//industry4: communal services utilities 
gen industry4 = 1 if industry==6 | industry ==7
replace industry4=0 if industry4==.

//industry5: management
gen industry5 = 1 if industry ==8
replace industry5=0 if industry5==.
//industry6: science education 
gen industry6 = 1 if industry==9 | industry==13
replace industry6=0 if industry6==.
//industry7: healthcare social protection culture
gen industry7 = 1 if industry==11 | industry==12
replace industry7=0 if industry7==.
//industry8: insurance credit finance
gen industry8=1 if industry==15| industry==16
replace industry8=0 if industry8==.
//otherwise, defence



// survivor function (Kaplan-Meyer non-parametric estimation)
sts graph
// hazard function
sts graph, hazard 

sts graph, by(serv6)
//stcurve, at(gender=0) at(gender=1) hazard 


// regress on gender and level of education using WEIBULL distribution
// (variables are called differently in the data, you should carefully construct them)
streg serv6 age o_stav stav_n married ivdiwen EDUC1 EDUC3 EDUC4 EDUC5 industry1 industry2 industry3 industry4 industry5 industry6 industry7 industry8, d(w)  
//streg [varlist], d(w), here coefficients are positive 

streg serv6 age o_stav stav_n married ivdiwen EDUC1 EDUC3 EDUC4 EDUC5 industry1 industry2 industry3 industry4 industry5 industry6 industry7 industry8, d(w) nohr 
outreg2 using output_file.xls, append ctitle(Weibull2)
//- regresija, where coefficients are both negative and 
stcurve, at (serv6=1) at (serv6=0) hazard




// To see the values of coefficients, use option "nohr":

 // already defined
stcox serv6 age o_stav stav_n married ivdiwen EDUC1 EDUC3 EDUC4 EDUC5 industry1 industry2 industry3 industry4 industry5 industry6 industry7 industry8, basehc(coxhaz) nohr
outreg2 using output_file.xls, append ctitle(Cox2)
drop coxhaz 
stcox serv6 age o_stav stav_n married ivdiwen EDUC1 EDUC3 EDUC4 EDUC5 industry1 industry2 industry3 industry4 industry5 industry6 industry7 industry8 obrzw, basehc(coxhaz) nohr

stcurve, at (serv6=1) at (serv6=0) hazard
//here for us it's more attractive to use obrzw instead of EDUCs, 
// since the split by EDUC1 will contain all oðers not having these degree 
drop coxhaz // already defined





/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//MODEL2 - WITH SPECIFICATION OF STATUS, I.E. THE REASON FOR BEING UNEMPLOYED 




use "Project_2_data_after_stset.dta", clear 
// remove the observations not containing the vital variables for regression:


drop if status == .
drop if napravl6==.
drop if serv6==.
drop if age_n>65 
replace age = age_n
drop if gender == .
drop if gender == 1

// generate variables for married
drop if semejnoe ==.

gen married = 1 if semejnoe ==1
replace married = 0 if semejnoe !=1
drop if obrzw == .
drop if stav_n == . 

drop if ivdiwen ==.
//define the education: 

// we have chosen 2 as baseline because 2nd is the biggest
// !!NB THE README!
// EDUC1 == 1 if the education is 9 grades

// EDUC3==1 if the education is PTU
// EDUC4 ==1 if the education is college
// EDUC5==1 if the education is higher (uni and upper)
 
gen EDUC1 = 1 if obrzw == 1
replace EDUC1 =0 if obrzw!=1
gen EDUC3 = 1 if obrzw == 3
replace EDUC3 = 0 if obrzw!=3
gen EDUC4 = 1 if obrzw == 4
replace EDUC4 = 0 if obrzw!=4
gen EDUC5 = 1 if inlist(obrzw, 5, 6,7)
replace EDUC5=0 if EDUC5==.

// specification for reason of being unemployed: 



// as the number of those who just 'lost job' is predominant, we have introduced the following variables: 
// red - for being redundant
// LTerm - long-term not employed:
// NW  - never worked before
// thus making those just lost job the baseline sample in this sence
gen red = 1 if status == 1
replace red = 0 if status !=1
gen LTerm = 1 if status == 3
replace LTerm= 0 if status != 3
gen NW = 1 if status == 4
replace NW = 0 if status !=4

// specification for the industry of last engagement: 

gen industry = otrasl_s
drop if industry == .
// industry1 for those in industry, construction, light industry, other material branches
gen industry1 = 1 if industry == 1 | industry == 18| industry == 10 |industry ==4
replace industry1 = 0 if industry1==.
// industry2 if agriculture

gen industry2 = 1 if industry == 2
replace industry2 = 0 if industry2==.
// industry3 if transport sales and catering
gen industry3 = 1 if industry == 3 | industry == 5
replace industry3=0 if industry3==.

//industry4: communal services utilities 
gen industry4 = 1 if industry==6 | industry ==7
replace industry4=0 if industry4==.

//industry5: management
gen industry5 = 1 if industry ==8
replace industry5=0 if industry5==.
//industry6: science education 
gen industry6 = 1 if industry==9 | industry==13
replace industry6=0 if industry6==.
//industry7: healthcare social protection culture
gen industry7 = 1 if industry==11 | industry==12
replace industry7=0 if industry7==.
//industry8: insurance credit finance
gen industry8=1 if industry==15| industry==16
replace industry8=0 if industry8==.
//otherwise, defence



// survivor function (Kaplan-Meyer non-parametric estimation)
sts graph
// hazard function
sts graph, hazard 

sts graph, by(serv6)
//stcurve, at(gender=0) at(gender=1) hazard 


// regress on gender and level of education using WEIBULL distribution
// (variables are called differently in the data, you should carefully construct them)
streg serv6 age o_stav stav_n married ivdiwen EDUC1 EDUC3 EDUC4 EDUC5 red LTerm NW industry1 industry2 industry3 industry4 industry5 industry6 industry7 industry8, d(w)  
//streg [varlist], d(w), here coefficients are positive 

streg serv6 age o_stav stav_n married ivdiwen EDUC1 EDUC3 EDUC4 EDUC5 red LTerm NW industry1 industry2 industry3 industry4 industry5 industry6 industry7 industry8, d(w) nohr 
outreg2 using output_file.xls, append ctitle(Weibull2)
stcurve, at (red=1) at (red=0) hazard
//- regresija, where coefficients are both negative and 




// To see the values of coefficients, use option "nohr":

 // already defined
stcox serv6 age o_stav stav_n married ivdiwen EDUC1 EDUC3 EDUC4 EDUC5 red LTerm NW industry1 industry2 industry3 industry4 industry5 industry6 industry7 industry8, basehc(coxhaz) nohr
outreg2 using output_file.xls, append ctitle(Cox2)
stcurve, at (red=1) at (red=0) hazard
drop coxhaz 
stcox serv6 age o_stav stav_n married ivdiwen EDUC1 EDUC3 EDUC4 EDUC5 red LTerm NW industry1 industry2 industry3 industry4 industry5 industry6 industry7 industry8 obrzw, basehc(coxhaz) nohr
stcurve, at (obrzw=1) at (obrzw=2) at(obrzw=3) at (obrzw=4) at (obrzw=5) hazard

//here for us it's more attractive to use obrzw instead of EDUCs, 
// since the split by EDUC1 will contain all oðers not having these degree 

drop coxhaz // already defined
