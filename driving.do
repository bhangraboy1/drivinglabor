* This is the STATA Code for the Driving in Saudi Arabia article
*
*
* New Comment
* New Comment

// Define Directory for Files to be written to

clear all
global datapath "/Users/ashbelur/Documents/ash belur/BIGPROJECTS/phd/STATA/drivinglabor/"
cd "$datapath"

import delimited GOLDIN.csv
save goldin, replace
use goldin

// Rename input colums to useful names
rename v1 date
rename v2 country
rename v3 flpp
rename v4 gdpp

// Move Columns to Rows
reshape wide flpp gdpp, i(date) j(country) string

// Fix Issue with AFG as first datapoint is a string
replace gdppAFG = "." if gdppAFG == "nan"
destring gdppAFG, replace

// Clean up the data
destring, replace

generate year2  = substr(date, 1, 4)
generate month2 = substr(date, 6, 2)
generate day2   = substr(date, 9, 2) 

destring year2,  gen(year)
destring month2, gen(month)
destring day2,   gen(day) 
generate dates = mdy(month, day, year)
format dates %tdCCYY

generate int year3 = yofd(dates)

// Years to Treatment
generate time = year3 - 2019

// Creating a lagged variable to align treatment periods
replace flppQAT = flppQAT[_n+1]
replace gdppQAT = gdppQAT[_n+1]
replace flppAFG = flppAFG[_n+2]
replace gdppAFG = gdppAFG[_n+2]

// Non-Negative time variable
generate timerel=year3-1990

drop date
drop day2 month2 year2
drop day month year
drop dates

generate D=0
replace D=1 if time>=0
generate D0=0
replace D0=1 if time==0
generate D1=0
replace D1=1 if time==1
generate D2=0
replace D2=1 if time==2
generate D3=0
replace D3=1 if time==3
generate D4=0
replace D4=1 if time==4

// Drop all irrelevant countries except SAU AFG QAT2
local countries ARE IRN IRQ MAR
foreach v of local countries {
	drop flpp`v'
	drop gdpp`v'
}

// drop extra year due to shift in data
drop if year3==2024

reshape long gdpp flpp, i(time) j(country_id) string

order time timerel

label variable D0 "Treatment Year"
label variable D1 "T+1"
label variable D2 "T+2"
label variable D3 "T+3"
label variable D4 "T+4"

label variable gdpp "GDP per Capita"
label variable timerel "Time"
sort country_id time

encode country_id, gen(country_id_numeric)

label variable country_id_numeric "Country Fixed Effects"

// Graph of the FLFPR for both countries
graph twoway (line flpp time if country_id=="SAU", color(black)) ||  (line flpp time if country_id=="QAT", color(black) lwidth(thin)) || (line flpp time if country_id=="AFG", color(black) lpattern(dash) lwidth(thin)), ///
xline(0, lpattern(dash) lwidth(medium)) xtitle("Years from Treatment") ytitle("FLFPR") legend(label(1 "Saudi Arabia") label(2 "Qatar") label (3 "Afghanistan"))
graph export "figure1.jpg", replace

// Try some OLS Linear Regression Models - this is not reported
// Too Many multi-collinear variables
reg flpp time ib(28).timerel gdpp, noconstant
reg flpp country_id_numeric D gdpp, noconstant

// Interuppted Time Series (ITS) Model for each of the two countries
reg flpp country_id_numeric time D0 D1 D2 D3 D4 gdpp, noconstant cluster(country_id_numeric)
// reg flpp time D0 D1 D2 D3 D4 gdpp if country_id=="SAU" 
// reg flpp time D0 D1 D2 D3 D4 gdpp if country_id=="QAT2" 

// Implement the Country Fixed Effect Model
// Correct Way to estimate Fixed Effects with de-meaning
xtset country_id_numeric

xtreg flpp D gdpp timerel, fe vce(cluster country_id_numeric)
esttab using "table2.tex", replace nogaps star(* 0.10 ** 0.05 *** 0.001) ///
cells("b(fmt(2) star) se(par fmt(2))") ///
collabels("Event Study" "SE") ///
coeflabels (gdpp "GDP per Capita") ///
booktabs nonumbers nomtitle label

xtreg flpp D0 D1 D2 D3 D4 gdpp timerel, fe vce(cluster country_id_numeric)
esttab using "table1.tex", replace nogaps keep(D0 D1 D2 D3 D4 gdpp) star(* 0.10 ** 0.05 *** 0.01) ///
cells("b(fmt(2) star) se(par fmt(2))") ///
collabels("Event Study" "SE") ///
coeflabels (gdpp "GDP per Capita") ///
booktabs nonumbers nomtitle label

preserve

parmest, label norestore
keep if inlist(label, "Treatment Year", "T+1", "T+2", "T+3", "T+4")

local CONF95 = 1.96
generate lower_ci = estimate - `CONF95' * stderr
generate upper_ci = estimate + `CONF95' * stderr
generate order = _n

graph twoway (rcap upper_ci lower_ci order, vertical color(black)) ///  Draws the vertical error bars
             (scatter estimate order, msize(medium) mcolor(black)), /// Draws the coefficient points
             xtitle(Years from Treatment) ///
             ytitle(Treatment Effect (%)) ///
             xlabel(1 "T+0" 2 "T+1" 3 "T+2" 4 "T+3" 5 "T+4") /// Manually labels the x-axis points
			 legend(off)
			 
restore 
			 
* Filter data to only SAU and QAT before the loop if not done yet in main script
* keep if country_id == "QAT" | country_id == "SAU" 

* Define the countries you want to loop through
local countries QAT AFG SAU 

* Clear previous estimates stores if any
estimates clear

foreach var in `countries' {
    * Run the ITS regression for each country separately
    * Note: D0-D4 are the POST dummies for QAT. They are all zero for SAU.
    * The 'timerel' variable is the continuous time trend.
    
    * Use standard simple quotes for the if condition
    reg flpp timerel D0 D1 D2 D3 D4 gdpp if country_id == "`var'"
    
    * Store the results with a meaningful name for esttab - THIS LINE MUST BE ACTIVE
    est sto model_`var'
}

* Combine stored models into a single table
esttab  model_QAT model_AFG model_SAU using "table3.tex", replace star(* 0.10 ** 0.05 *** 0.01) ///
    booktabs nonumber label ///
    cells("b(fmt(2) star) se(par fmt(2))") ///
	mtitles("Qatar"  "Afghanistan" "Saudi Arabia") ///
    collabels("Est" "SE" "Est" "SE") ///
    keep(timerel D0 D1 D2 D3 D4 gdpp) ///
    coeflabels(timerel "Time Trend" gdpp "GDP per Capita")

preserve
	
parmest, label norestore
keep if inlist(label, "Treatment Year", "T+1", "T+2", "T+3", "T+4")

local CONF95 = 1.96
generate lower_ci = estimate - `CONF95' * stderr
generate upper_ci = estimate + `CONF95' * stderr
generate order = _n

// Graph of Confidence Intervals for Annual Treatment Effect for Saudi Arabia
graph twoway (rcap upper_ci lower_ci order, vertical color(black)) ///  Draws the vertical error bars
             (scatter estimate order, msize(medium) mcolor(black)), /// Draws the coefficient points
             xtitle(Years from Treatment) ///
             ytitle(Treatment Effect (%)) ///
             xlabel(1 "T+0" 2 "T+1" 3 "T+2" 4 "T+3" 5 "T+4") /// Manually labels the x-axis points
			 legend(off)
graph export "figure2.jpg", replace

restore



