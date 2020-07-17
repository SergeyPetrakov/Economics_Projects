use Workingfile2015.dta, 
keep  if ixprfstd != .
replace ixhmtrds = 0 if ixhmtrds == . 
replace ixlngdtr = 0 if ixlngdtr == .
// keep if ixhmtrds!=.
// keep if ixlngdtr!=.
drop if (ixwagelm == .) & (ixwrknow ==1) 
replace ixwagelm = 0 if ixwagelm == .
keep if (ixskills!=97)& (ixskills!=98) | (ixskills!=0)
gen age = 2015 - ixbirthy
keep if (age<=65)&(age>=16)
keep if ixwagelm < 70000
gen age2 = age*age 	//following Katya's âge quadratic dependency 
gen EDUC1=.
replace EDUC1=1 if ixhiedul>2 & ixhiedul<=6 //technical professional education (proletariat)
replace EDUC1=0 if ixhiedul<=2
replace EDUC1=0 if ixhiedul>6 & ixhiedul<15
gen EDUC2=.
replace EDUC2=1 if ixhiedul==10 | ixhiedul==11
replace EDUC2=0 if ixhiedul<10
replace EDUC2=0 if ixhiedul>11 & ixhiedul<15
gen EDUC3=.
replace EDUC3=1 if ixhiedul==8 | ixhiedul==12 |ixhiedul==13 | ixhiedul==14
replace EDUC3=0 if ixhiedul<8 | ixhiedul==10 | ixhiedul==11
gen rus = 1 if (ixnation ==1)
replace rus = 0 if (ixnation !=1) // dummy for russian nationalitygen trained if ixprfstd
gen lnwage = ln(ixwagelm)
// keep if ixwagelm!=.
gen skills = ixskills
gen real_train_days = ixhmtrds
gen avg_train_hours_per_day = ixlngdtr
gen arbeit = 1 if (ixwrknow == 1)
replace arbeit = 0 if (ixwrknow !=1) // dummy for 'employed'
gen fem = 1 if ixgender==2 //refiguring sex variable
replace fem = 0 if ixgender == 1 // 0 if man
// gen real_inc_hh = TINCM_RX
gen non_sal_inc = TINCM_RX - tprvcarx - tgovcarx
gen rel_inc = TINCM_RX //- ixwagelm
gen disab = 1 if ixdisabl == 1
replace disab = 0 if ixdisabl == 2
gen gov = 1 if ixentgov==1
replace gov = 0 if ixentgov==2 | ixentgov==.
gen wrkgov = arbeit*gov
gen tot_hh = ixlngdtr*ixhmtrds
gen trained = 1 if ixprfstd == 1
gen kids = 1 if ixkids == 1
replace kids = 0 if ixkids != .
gen wage = ixwagelm
gen fact_wage = ixwagelm if ixwagelm!=0
replace trained = 0 if ixprfstd == 2 // variable denoting if respondent has trained in general 
gen fact_avg_hs = avg_train_hours_per_day if avg_train_hours_per_day !=0
gen fact_tr_days = real_train_days if real_train_days !=0
summarize fact_wage wage  fact_avg_hs avg_train_hours_per_day fact_tr_days real_train_days skills age trained fem EDUC1 EDUC2 EDUC3 
heckman lnwage wrkgov trained  disab age age2 EDUC1 EDUC2 EDUC3 fem, twostep select(arbeit =  age age2 EDUC1 EDUC2 EDUC3 disab fem non_sal_inc )rhosigma
