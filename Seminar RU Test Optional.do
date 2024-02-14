******************************************************
// Project Name: Seminar in Action Research
// Test-Optional Evaluation for Rutgers-New Brunswick
// Jason Schweitzer
// preamble: this file pulls an institutional data set and runs a series of analyses

******************************************************

clear all
estimates clear
set matsize 1000  // Indicates number of variables to be used
version 18     // Declares the Stata version do file was written in

set scheme Rutgers2 //use Rutgers color pallette, defaults to s2color

use students_noRUID.dta

// label variables
label variable ACTSCORE "ACT Score"
label variable HSGPA "High School GPA"
label variable SAT_M_NEW "SAT Math / 10"
label variable SAT_V_NEW "SAT ERW / 10"
label variable FYGPA "First Year GPA"
label variable FALL1 "First Term Enrolled"
label variable FALL1_STATUS "First Term Status"
label variable FALL2 "First-to-Second Year Retention"
label variable FEMALE "Female"
label variable ETHNICITY "Ethnicity"
label variable ETHNICITYII "Race/Ethnicity"
label variable URM_IND "Underrepresented Minority"
label variable AFR_AM "African American"
label variable HISPANIC "Hispanic"
label variable ASIAN "Asian"
label variable OTHER_MISS "Other/Missing"
label variable STEM_IND "STEM Major"
label variable FIRST_GEN "First Generation Student"
label variable PELL_IND "Pell Recipient"

label variable EN_CONV_GPA "English HS GPA"
label variable MT_CONV_GPA "Math HS GPA"
label variable SS_CONV_GPA "Social Studies HS GPA"
label variable NS_CONV_GPA "Science HS GPA"

label variable PREDICTED_M "Academic Success Math"
label variable PREDICTED_V "Academic Success Verbal"

label variable GPA_INDEX "Test Index"
label variable SR_OPT_INDEX "OPT Index"
label variable BEST_INDEX "Best Index"

label variable YR "Cohort Year"
label variable TEST_OPT "Test Optional Status"

generate nCollege = real(COLLEGE_OF_APPLICATION)
format nCollege %02.0f

destring RUTGERS_ID, replace // Converted YR to numeric
destring YR, replace // Converted YR to numeric

// label COLLEGE_OF_APPLICATION variable values
label define college_cd 01 "SAS" 07 "MGSA" 11 "SEBS" 14 "SOE" 25 "NURS-NK" 30 "PHARM" 33 "RBS-NB" 77 "NURS-NB"
label values nCollege college_cd

label variable nCollege "College of Application"

// label FEMALE variable values
label define gender 0 "Male" 1 "Female"
label values FEMALE gender

// label URM_IND variable values
label define URM 0 "Not URM" 1 "URM"
label values URM_IND URM

// label FIRST_GEN variable values
label define FIRSTGEN 0 "Not First Gen" 1 "First Gen"
label values FIRST_GEN FIRSTGEN

// label PELL_IND variable values
label define PELLIND 0 "Not Pell Recipient" 1 "Pell Recipient"
label values PELL_IND PELLIND

// label STEM variable values
label define STEM 0 "Not STEM" 1 "STEM Major"
label values STEM_IND STEM

// label TEST_OPT variable values
label define TO 0 "Test Takers" 1 "Test Optional"
label values TEST_OPT TO

gen byte POST = 0
replace POST = 1 if YR > 2020

gen PCT_COMP = FY_COMP_HOURS/FY_ATT_HOURS
label variable PCT_COMP "% First Year Credits Completed"


replace SAT_M_NEW = . if SAT_M_NEW == 0
replace SAT_V_NEW = . if SAT_V_NEW == 0
replace PCT_COMP = 1 if PCT_COMP > 1 & PCT_COMP != .  // 4 students had over 100% due to bringing in an EXCH course

replace EN_CONV_GPA = . if EN_CONV_GPA == 0
replace MT_CONV_GPA = . if MT_CONV_GPA == 0
replace SS_CONV_GPA = . if SS_CONV_GPA == 0
replace NS_CONV_GPA = . if NS_CONV_GPA == 0

encode RESIDENCY, gen(nRESIDENCY)
des RESIDENCY nRESIDENCY
label list nRESIDENCY

label variable nRESIDENCY "Residency"

encode CEEB, gen(nCEEB)
des CEEB nCEEB
label list nCEEB
label variable nCEEB "High School CEEB Code"

sort nCEEB STUD_NUM
by nCEEB: gen CEEB_NUM = _N // Create variable to count how many students from each high school for analysis inclusion/exclusion



xtile NH_MEDFAMINC_DEC = A_NH_MEDFAMINC_PTILE, nq(10)
label variable NH_MEDFAMINC_DEC "Income Decile of Neighborhood"


gen SAT_TOTAL = SAT_MATH + SAT_VERBAL
label variable SAT_TOTAL "SAT Composite Score"

// create standardized regression coefficients with center

center HSGPA SAT_MATH SAT_VERBAL SAT_TOTAL, standardize prefix(Z_)

// Generate BEST_INDEX by Year Across All Years -- GPA_INDEX Pre-2020, BEST_INDEX after

gen BEST_INDEX_ALL = BEST_INDEX
replace BEST_INDEX_ALL = GPA_INDEX if YR < 2020
tabstat BEST_INDEX_ALL, by(YR)

tabstat FYGPA, by(YR)
tabstat FYGPA if inlist(YR, 2017, 2018, 2021, 2022), by(YR)

tabstat SAT_TOTAL if inlist(YR, 2017, 2018, 2021, 2022), by(YR)

hist PCT_COMP

summarize
describe

save students.dta, replace

replace Z_SAT_VERBAL = 0 if Z_SAT_VERBAL == .
replace Z_SAT_MATH = 0 if Z_SAT_MATH == .
replace Z_SAT_TOTAL = 0 if Z_SAT_TOTAL == .

table (YR) (TEST_OPT), statistic(mean Z_SAT_TOTAL)
	
***** Series of First-Year GPA Regressions  Excluding Fall 2019 and Fall 2020 cohorts due to generous RU grading policy as a result of COVID disruptions

estimates clear

regress FYGPA Z_SAT_TOTAL TEST_OPT i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust)
estimates store Model1

regress FYGPA Z_HSGPA i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust)
estimates store Model2

regress FYGPA Z_SAT_TOTAL TEST_OPT Z_HSGPA i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust)
estimates store Model3

regress FYGPA Z_SAT_TOTAL TEST_OPT Z_HSGPA URM_IND PELL_IND FEMALE FIRST_GEN ib2.nRESIDENCY i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust) beta
estimates store Model4

regress FYGPA Z_SAT_TOTAL TEST_OPT Z_HSGPA URM_IND##PELL_IND##FEMALE FIRST_GEN ib2.nRESIDENCY i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust) beta
estimates store Model5

// nCEEB 1383 is South Brunswick High School, n = 726
regress FYGPA Z_SAT_TOTAL TEST_OPT Z_HSGPA URM_IN##PELL_IND##FEMALE FIRST_GEN ib2.nRESIDENCY i.YR i.nCollege ib1383.nCEEB if inlist(YR, 2017, 2018, 2021, 2022) ///
& RESIDENCY != "INTL" & CEEB_NUM > 30, vce(robust) beta
estimates store Model6

* Regression Table using esttab
esttab Model* using "Regression_Output_FYGPA.csv" , replace order(_cons) ///
	mtitle label stats(rss df_r r2 F N) ///
	cells(b (fmt(3) star) se(par fmt(3))) varwidth(40) ///
	title("Panel A: First-Year College GPA")  

*****

***** Series of First Year Credits Completed (%) Regressions  Excluding Fall 2019 and Fall 2020 cohorts due to generous RU grading policy as a result of COVID disruptions

estimates clear

regress PCT_COMP Z_SAT_TOTAL TEST_OPT i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust)
estimates store Model1

regress PCT_COMP Z_HSGPA i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust)
estimates store Model2

regress PCT_COMP Z_SAT_TOTAL TEST_OPT Z_HSGPA i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust)
estimates store Model3

regress PCT_COMP Z_SAT_TOTAL TEST_OPT Z_HSGPA URM_IND PELL_IND FEMALE FIRST_GEN ib2.nRESIDENCY i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust) beta
estimates store Model4

regress PCT_COMP Z_SAT_TOTAL TEST_OPT Z_HSGPA URM_IND##PELL_IND##FEMALE FIRST_GEN ib2.nRESIDENCY i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust) beta
estimates store Model5

// nCEEB 1383 is South Brunswick High School, n = 726
regress PCT_COMP Z_SAT_TOTAL TEST_OPT Z_HSGPA URM_IND##PELL_IND##FEMALE FIRST_GEN ib2.nRESIDENCY i.YR i.nCollege ib1383.nCEEB if inlist(YR, 2017, 2018, 2021, 2022) ///
& RESIDENCY != "INTL" & CEEB_NUM > 30, vce(robust) beta
estimates store Model6

* Regression Table using esttab
esttab Model* using "Regression_Output_PCT_COMP.csv" , replace order(_cons) ///
	mtitle label stats(rss df_r r2 F N) ///
	cells(b (fmt(3) star) se(par fmt(3))) varwidth(40) ///
	title("Panel B: Percentage of Completed First-Year Credits")  

*****

***** Series of First-to-Second Year Retention Regressions  Excluding Fall 2019 and Fall 2020 cohorts due to generous RU grading policy as a result of COVID disruptions

estimates clear

logistic FALL2 Z_SAT_TOTAL TEST_OPT i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust)
estimates store Model1

logistic FALL2 Z_HSGPA i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust)
estimates store Model2

logistic FALL2 Z_SAT_TOTAL TEST_OPT Z_HSGPA i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust)
estimates store Model3

logistic FALL2 Z_SAT_TOTAL TEST_OPT Z_HSGPA URM_IND PELL_IND FEMALE FIRST_GEN ib2.nRESIDENCY i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust)
estimates store Model4

logistic FALL2 Z_SAT_TOTAL TEST_OPT Z_HSGPA URM_IND##PELL_IND##FEMALE FIRST_GEN ib2.nRESIDENCY i.YR i.nCollege if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", vce(robust)
estimates store Model5

// nCEEB 1383 is South Brunswick High School, n = 726
logistic FALL2 Z_SAT_TOTAL TEST_OPT Z_HSGPA URM_IND##PELL_IND##FEMALE FIRST_GEN ib2.nRESIDENCY i.YR i.nCollege ib1383.nCEEB if inlist(YR, 2017, 2018, 2021, 2022) ///
& RESIDENCY != "INTL" & CEEB_NUM > 30, vce(robust)
estimates store Model6

* Regression Table using esttab
esttab Model* using "Regression_Output_FALL2.csv" , replace order(_cons) ///
	mtitle label stats(rss df_r r2 F N) ///
	cells(b (fmt(3) star) se(par fmt(3))) varwidth(40) ///
	title("Panel C: First-to-Second Year Retention")  

*****


tabstat SAT_COMBINED_NEW, by(YR)
tabstat SAT_MATH, by(YR)
tabstat SAT_VERBAL, by(YR)
tabstat SAT_TOTAL, by(YR)
tabstat FYGPA, by(YR)
tabstat HSGPA, by(YR)
tabstat BEST_INDEX, by(YR)
tabstat GPA_INDEX, by(YR)
tabstat SR_OPT_INDEX, by(YR)

tabstat PELL_IND, by(YR)
tabstat TEST_OPT, by(YR)
tabstat URM_IND, by(YR)
tabstat FEMALE, by(YR)
tabstat FIRST_GEN, by(YR)
tabstat nRESIDENCY, by(YR)

graph bar FYGPA, over(YR) blabel(bar, format(%4.3f)) ytitle("Average First-Year GPA") ///
b1title("Overall Incoming First-Year Cohort") ///
name(BarFYGPA, replace)

graph box FYGPA, over(YR) name(Box2, replace)

twoway (hist SAT_TOTAL if inlist(YR, 2017, 2018), frequency bins(10) color("204 0 51") lcolor(black)) ///
(hist SAT_TOTAL if inlist(YR, 2021, 2022), frequency bins(10) color("95 106 114 %80") lcolor(black)), ///
ylabels(, format(%12.0fc)) ///
legend(order(1 "Class of 2017-2018" 2 "Class of 2021-2022")) ///
name(histSATbyGroups2, replace)

* Descriptive Table 1
dtable FYGPA FALL2 PCT_COMP HSGPA i.(URM_IND PELL_IND FEMALE FIRST_GEN nRESIDENCY) if inlist(YR, 2017, 2018, 2021, 2022) & RESIDENCY != "INTL", by(TEST_OPT, nototal) ///
title(Table 1) ///
export("table1", as(xlsx) sheet(Sheet1, replace) cell(A1) replace) 

*  Binscatter graphs

local ytitle: variable label FYGPA 
local xtitle: variable label HSGPA

binscatter FYGPA HSGPA, ytitle(`ytitle') xtitle(`xtitle') ///
name(binscatter1, replace)
graph export BinScatter1.png, replace  //  png format for easy viewing

local ytitle: variable label FYGPA 
local xtitle: variable label SAT_TOTAL

binscatter FYGPA SAT_TOTAL, controls(HSGPA URM_IND FEMALE FIRST_GEN) ytitle(`ytitle') xtitle(`xtitle') ///
name(binscatter2, replace)
graph export BinScatter2.png, replace  //  png format for easy viewing

local ytitle: variable label FYGPA 
local xtitle: variable label HSGPA

binscatter FYGPA HSGPA, controls(SAT_TOTAL URM_IND FEMALE FIRST_GEN) ytitle(`ytitle') xtitle(`xtitle') ///
name(binscatter3, replace)
graph export BinScatter3.png, replace  //  png format for easy viewing

binscatter FYGPA SAT_TOTAL, by(URM_IND)

binscatter FYGPA SAT_TOTAL, by(URM_IND) absorb(HSGPA) legend(lab(1 Not URM Students) lab(2 URM Students))


sort UNIQUEID
gen STUD_NUM = _n

order STUD_NUM

drop UNIQUEID RUTGERS_ID

save students_noRUID, replace

export excel using "cohort_stata.xlsx", firstrow(variables) replace