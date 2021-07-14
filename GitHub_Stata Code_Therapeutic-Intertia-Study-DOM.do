
* Therapeutic Inertia Study
     
*------------------------------------------------------------------------------*
*****   TIME TO 1ST LINE THERPAY     ******
*------------------------------------------------------------------------------*

*** Descriptive table ***
sum age_index, d
gen age 	= 0
replace age = 1 if age_index >= 50 & age_index < 60
replace age = 2 if age_index >= 60
tab age 

tab sex_f

tab ethnicity
encode ethnicity, gen(ethn) 
gen ethnic 		= 0
replace ethnic = 1 if ethn == 4
replace ethnic = 2 if ethn == 3
replace ethnic = 4 if ethn == 2
replace ethnic = 3 if ethn == 1
tab ethnic

tab imd2007_5

sum incident_hba1c, d

gen hba_perct = (incident_hba1c / 10.929) + 2.15
sum hba_perct, d 

replace cvdmmgrp  	= 2 if cvdmmgrp    == 3
tab cvdmmgrp
replace noncvdmmgrp = 2 if noncvdmmgrp == 3
tab noncvdmmgrp 

tab indexyr  // year t2dm diagnosis
sum indexyr, d 

gen t2d 	= 0
replace t2d = 1 if indexyr >= 2005 & indexyr < 2010
replace t2d = 2 if indexyr >= 2010 & indexyr < 2015
replace t2d = 3 if indexyr >= 2015
tab t2d 

* Calculate the difference between T2DM diagnosis and HbA1c measurement in the study
gen diff_t2_hba1c =  incident_hba1c_dt - index_dt
gen years_diff 	  = diff_t2_hba1c / 365.24
sum years_diff, d
 
***  Potential factors affecting time to 1st line therapy - Standardised Survival ****
gen fu_yrs=fu_time1/365.24
summarize fu_yrs

stset fu_yrs, f(line1 == 1) id(patid)
stptime, title(Person-years)

stci
sts graph
stpm2 i.age i.sex_f  i.ethnic i.imd2007_5   i.cvdmmgrp i.noncvdmmgrp incident_hba1c indexyr, scale(hazard) df(3) eform 


*** Time to 1st line therapy from when HbA1c measurement ***

/*follow-up*/
gen out1     = 1 if initiation_dt !=.      // event or not (missing)
replace out1 = 0 if missing(initiation_dt)  

gen end1 	 = initiation_dt if out1 == 1      // event of 1st line date
replace end1 = lastfu_dt if out1 == 0       // right cenosring using last follow-up date


*--------------------------------------------------------------------------------*

******** CHECK IF INTENSIFICATION IS OVER LAST FOLLLOW-UP **********************
gen over 	 = initiation_dt
replace over = 999 if initiation_dt > lastfu_dt & initiation_dt!=.

*****  0 was over
*--------------------------------------------------------------------------------*

gen t1 	   = end1-incident_hba1c_dt         // time from hba1c recording to 1st line or no prescription
replace t1 = t1/365.24        // years 
order t1, after(out1)

format end1 %td

stset t1, failure(out1 == 1)
sort t1
distplot t1   
// 708 were given prescription  on the day of at Hba1c recording - this is excluded from stset - take into account, add a tiny amount of time 
replace t1 = 1/365.24 if t1 == 0  // 1 day after 

stset t1, failure(out1 == 1)
tab out1, m 
sts graph, failure
sum t1, d

stci

* https://pclambert.net/software/stpm2_standsurv/standardized_survival_centiles/
* Create dummy variables 
tab age,         gen(ag)
tab sex_f,       gen(sxf)
tab ethnic,      gen(eth)
tab imd2007_5,   gen(imd)
tab cvdmmgrp, 	 gen(cvdmmm)
tab noncvdmmgrp, gen(noncvmm)

stpm2 ag2 ag3 sxf2 eth2 eth3 eth4 imd2 imd3 imd4 imd5 cvdmmm2 cvdmmm3 noncvmm2 noncvmm3 incident_hba1c indexyr, scale(hazard) df(3) eform 

range tt 0 18 100


* * Predictions
standsurv, timevar(tt) atvar(ms_all)  ci 

line ms_all tt, sort  yline(0.50, lpattern(shortdash) lcolor(black)) ylabel(0.0(0.1)1,angle(h) format(%3.1f))

* flip the graph over - failure 
line ms_all tt, sort  yline(0.50, lpattern(shortdash) lcolor(black)) ylabel(0.0(0.1)1,angle(h) format(%3.1f)) ysc(reverse)


/*plot standardized survival curve*/ 
twoway  (rarea ms_all_lci ms_all_uci tt, color(blue%25) lwidth(none)) /*
*/        (line ms_all tt, sort lcolor(blue))  /*
*/        , yline(0.50, lpattern(shortdash) lcolor(black)) ylabel(0.0(0.1)1,angle(h) format(%3.1f))  ytitle("Therapeutic inertia (95% CI)") /*
*/        xtitle("Years since first HbA1c>=6.5%") title("HbA1c>=6.5%") xlabel(0(1)18, grid angle(horizontal) labsize(small))   plotregion(lstyle(none))  graphregion(fcolor(white)) legend(pos(1) ring(0) col(1))  


* Centile
standsurv,  timevar(tt) atvar(med_all)  ci centile(50)
list med_all* in 1   // 1.44 (1.41, 1.47)

drop _centvals


***** AGE ****

* Predictions
standsurv, at1(ag1 1 ag2 0 ag3 0) at2(ag1 0 ag2 1 ag3 0) at3(ag1 0 ag2 0 ag3 1) timevar(tt) atvar(ms_ag1 ms_ag2 ms_ag3)  ci

line ms_ag1 ms_ag2 ms_ag3 tt, sort yline(0.5, lpattern(dash) lcolor(black))

twoway  (line ms_ag1 ms_ag2 ms_ag3 tt, sort) ///
, yline(0.5, lpattern(dash) lcolor(black)) ///
        yline(0.5, lpattern(dash) lcolor(black)) ///
       xtitle("Years since HbA1c") ///
        ytitle("S(t)", angle(h)) ///
        ylabel(0(0.2)1, format(%3.1f) angle(h)) ysc(reverse)

		
* Centile 
standsurv, at1(ag1 1 ag2 0 ag3 0) at2(ag1 0 ag2 1 ag3 0) at3(ag1 0 ag2 0 ag3 1) timevar(tt) atvar(med_ag1 med_ag2 med_ag3)  ci centile(50)

list med_ag1* in 1   //0.98
list med_ag2* in 1    // 1.05
list med_ag3* in 1    // 1.71

drop _centvals


***** Men and Women *******

* Predictions
standsurv, at1(sxf1 1 sxf2 0 ) at2(sxf1 0 sxf2 1) timevar(tt) atvar(ms_male ms_female)  ci
line ms_male ms_female tt, sort yline(0.5, lpattern(dash) lcolor(black))

* Centile 
standsurv, at1(sxf1 1 sxf2 0 ) at2(sxf1 0 sxf2 1)timevar(tt) atvar(med_male med_female)  ci centile(50)
list med_male* in 1   // 1.45
list med_female* in 1    // 1.43
drop _centvals


*****  Ethnicity ******
standsurv, 	at1(eth1 1 eth2 0 eth3 0 eth4 0) /*
*/			at2(eth1 0 eth2 1 eth3 0 eth4 0) /*
*/			at3(eth1 0 eth2 0 eth3 1 eth4 0) /*
*/			at4(eth1 0 eth2 0 eth3 0 eth4 1)  timevar(tt)  atvar(ms_white ms_sa ms_black ms_other)  ci

/*plot standardized survival curve*/ 
twoway        (line ms_white tt, sort lcolor(dknavy)) /*
*/        (line ms_sa  tt, sort lcolor(cranberry)) /*
*/        (line ms_black  tt, sort lcolor(dkgreen)) /*
*/        (line ms_other  tt, sort lcolor(orange)) , yline(0.5, lpattern(dash) lcolor(black)) ysc(reverse)


* Centile 
standsurv, 	at1(eth1 1 eth2 0 eth3 0 eth4 0) /*
*/			at2(eth1 0 eth2 1 eth3 0 eth4 0) /*
*/			at3(eth1 0 eth2 0 eth3 1 eth4 0) /*
*/			at4(eth1 0 eth2 0 eth3 0 eth4 1)  timevar(tt)  atvar(med_white med_sa med_black med_other)  ci centile(50)

list med_white* in 1   // 1.43
list med_sa* in 1    // 1.41
list med_black* in 1    // 1.85
list med_other* in 1    // 1.75

drop _centvals



*** IMD
standsurv, 	at1(imd1 1 imd2 0 imd3 0 imd4 0 imd5 0) /*
*/			at2(imd1 0 imd2 1 imd3 0 imd4 0 imd5 0) /*
*/			at3(imd1 0 imd2 0 imd3 1 imd4 0 imd5 0) /*
*/			at4(imd1 0 imd2 0 imd3 0 imd4 1 imd5 0) /* 
*/			at5(imd1 0 imd2 0 imd3 0 imd4 0 imd5 1) timevar(tt)  atvar(ms_imd1 ms_imd2 ms_imd3 ms_imd4 ms_imd5) ci

line ms_imd1 ms_imd2 ms_imd3 ms_imd4 ms_imd5 tt, sort yline(0.5, lpattern(dash) lcolor(black))

		
* Centile 
standsurv, 	at1(imd1 1 imd2 0 imd3 0 imd4 0 imd5 0) /*
*/			at2(imd1 0 imd2 1 imd3 0 imd4 0 imd5 0) /*
*/			at3(imd1 0 imd2 0 imd3 1 imd4 0 imd5 0) /*
*/			at4(imd1 0 imd2 0 imd3 0 imd4 1 imd5 0) /* 
*/			at5(imd1 0 imd2 0 imd3 0 imd4 0 imd5 1) timevar(tt)  atvar(med_imd1 med_imd2 med_imd3 med_imd4 med_imd5) ci centile(50)

list med_imd1* in 1   // 1.56
list med_imd2* in 1   // 1.60
list med_imd3* in 1   // 1.48
list med_imd4* in 1   // 1.30
list med_imd5* in 1   // 1.22

drop _centvals


****** By CVDMM  *******

standsurv, 	at1(cvdmmm1 1 cvdmmm2 0 cvdmmm3 0) /*
*/			at2(cvdmmm1 0 cvdmmm2 1 cvdmmm3 0) /*
*/			at3(cvdmmm1 0 cvdmmm2 0 cvdmmm3 1)  timevar(tt) atvar(ms_cvdmm1 ms_cvdmm2 ms_cvdmm3) ci

line ms_cvdmm1 ms_cvdmm2 ms_cvdmm3 tt, sort yline(0.5, lpattern(dash) lcolor(black))

* Centile 
standsurv, 	at1(cvdmmm1 1 cvdmmm2 0 cvdmmm3 0) /*
*/			at2(cvdmmm1 0 cvdmmm2 1 cvdmmm3 0) /*
*/			at3(cvdmmm1 0 cvdmmm2 0 cvdmmm3 1)  timevar(tt) atvar(med_cvdmm1 med_cvdmm2 med_cvdmm3) ci centile(50)

list med_cvdmm1* in 1   // 1.42
list med_cvdmm2* in 1   // 1.52
list med_cvdmm3* in 1   // 1.76

drop _centvals


******* By NONCVDMM  ******
standsurv, 	at1(noncvmm1 1 noncvmm2 0 noncvmm3 0) /*
*/			at2(noncvmm1 0 noncvmm2 1 noncvmm3 0) /*
*/			at3(noncvmm1 0 noncvmm2 0 noncvmm3 1)  timevar(tt) atvar(ms_noncvdmm1 ms_noncvdmm2 ms_noncvdmm3) ci

line ms_noncvdmm1 ms_noncvdmm2 ms_noncvdmm3 tt, sort yline(0.5, lpattern(dash) lcolor(black))


* Centile 
standsurv, 	at1(noncvmm1 1 noncvmm2 0 noncvmm3 0) /*
*/			at2(noncvmm1 0 noncvmm2 1 noncvmm3 0) /*
*/			at3(noncvmm1 0 noncvmm2 0 noncvmm3 1)  timevar(tt) atvar(med_noncvdmm1 med_noncvdmm2 med_noncvdmm3) ci centile(50)

list med_noncvdmm1* in 1   // 1.44
list med_noncvdmm2* in 1   // 1.40
list med_noncvdmm3* in 1   // 1.61

 
*** Repeat the above code for >=7.0% data ***
use "substudy_HbA1c_70_survival.dta", clear


 
* Full cohort Table 1 characteristics - repeat above 
use "analysis1.dta", clear

replace index_CVDMM = 2 if index_CVDMM >=3
replace index_nonCVDMM = 2 if index_nonCVDMM >=3
tab index_CVDMM
tab index_nonCVDMM

sum incident_HbA1c65, d
gen hba_perct = (incident_HbA1c65 / 10.929) + 2.15
sum hba_perct, d 
* missing 25,423


*------------ GRAPHS - FOREST PLOTS --------------*
 
use "forestplot65.dta", clear
 
label var adjustedhr95ci "Adjusted HR (95% CI)"
label var factor "Factors"
 
 foreach var of varlist  hr lci uci {
	gen ln`var' = ln(`var')
}

gen seln = (lnuci - lnlci) / 3.92


metan lnhr seln, eform nooverall lcols(factor) rcols( adjustedhr95ci ) nostats  nowt nohet by(group)   force  textsize(110) ///
favours("Delayed treatment" #   "Early treatment" )   nosubgroup ///
graphregion(fcolor(white)) pointopt(mcolor(orange) msize(small) mlcolor( black) mlstyle(forground) msymbol(d) ) ///
diamopt( lcolor( cranberry)  lstyle(grid))    ///
      boxsca(2) xsize(13) ysize(14) astext(50) xlab(0.60, 0.70, 0.80, 0.90, 1.0, 1.1, 1.2, 1.3)  notable
	  
* Repeat using 7.0% data  	  
use "forestplot70.dta", clear
  
 
*------------------------------------------------------------------------------*
*****   LINE OF THERAPY    ******
*------------------------------------------------------------------------------*

use "glptherapy.dta", clear 

tab line

drop Insulin_Type 
gen yr_med   = year(initiation_dt)
drop if line == 0 | line > 4
sort yr_med

**** FIGURE - Line of therapy 2000 to 2018 *****
* Heat map use column percentage 
tab Category line , col

**** FIGURE - Line of therapy 2000 to 2013 *****
tab Category line if  yr_med < 2014, col

**** FIGURE - Line of therapy 2014 to 2018 *****
tab Category line if  yr_med >= 2014, col

*-------------------------------------------------

** TIME TO PRESCRIPTION / INTENSIFICATION **

***** SURVIVAL ANALYSIS *****

use "analysis1.dta", clear
merge 1:1 patid using "glptherapy_wide.dta"
drop Category0 initiation_dt0 index_dt0 Category5- initiation_dt9 _merge

* Include everyone in the study (N=120,409)

 **** OVERALL TIME TO EACH LINE OF THERAPY  ****

* Start time = T2DM diagnosis date == index_dt 
* End time = line of therapy, death, the last date of linkage to HES, or end of study on December 31, 2018  == lastfu_dt
* Event = the line of therapy 


* Event if had therapy (failure) 
gen line1 = 1 if initiation_dt1 !=.
gen line2 = 1 if initiation_dt2 !=.
gen line3 = 1 if initiation_dt3 !=.
gen line4 = 1 if initiation_dt4 !=. 


** 1ST LINE **
tab line1, m 
* 87,264 had an intensification after T2DM diagnosis 
* 33,145 had no intensification - so do not have a 2nd, 3rd or 4th line - these should also be included in the analysis 
* use lastfu_dt for these patients as right censoring will be the same 


/*follow-up*/
gen out1     = 1 if initiation_dt1 !=.      // event or not (missing)
replace out1 = 0 if missing(initiation_dt1)  
gen end1 = initiation_dt1 if out1 == 1      // event of 1st line date
replace end1 = lastfu_dt if out1 == 0       // right cenosring using last follow-up date


*--------------------------------------------------------------------------------*

******** CHECK IF INTENSIFICATION IS OVER LAST FOLLLOW-UP **********************
gen over     = initiation_dt1
replace over = 999 if initiation_dt1 > lastfu_dt & initiation_dt1!=.
***** 3 WERE OVER - DUE TO DATA BEING LINKED TO OTHER DATASETS OUTSIDE OF CPRD -  PUT AS RIGHT CENSORING 0 - USE LAST FOLLOW-UP AND PUT AS DATE MISSING 

sort over

replace end1  = lastfu_dt  if over == 999   // replace to last follow-up
replace out1  = 0  if over == 999     // right censored
replace line1 =.  if over == 999
replace initiation_dt1 =.  if over == 999   // date is now missing in this case 

*--------------------------------------------------------------------------------*

gen t1     = end1 - index_dt         // time from t2dm to 1st line or no prescription 
replace t1 = t1/365.24        // years 
order t1, after(out1)

format end1 %td

stset t1, failure(out1 == 1)
sort t1
distplot t1   
// 22,403 were given prescription  on the day of T2DM - this is excluded from stset - take into account, add a tiny amount of time 
replace t1 = 1/365.24 if t1 == 0  // 1 day after 

stset t1, failure(out1 == 1) id(patid)
tab out1, m 
 
sts graph, failure risktable
stpm2, scale(h) df(3)
predict hr, h per(1000)  // per 1000
line hr _t, sort   // HR instaneous rate of recieveing first-line 

stptime, title(Person-years)
 

stci
* Use kaplan meier to graphically present the results - probability of getting first-line prescription  - report failure 
qui stci
local p50 = r(p50)
sts graph, failure ci yline(0.50, lpattern(shortdash) lcolor(black)) ylab(0(0.1)1) ///
xlab(0(1)18, grid) graphregion(fcolor(white)) title("Time to 1st line prescription")  xline(`p50', lpattern(shortdash) lcolor(black)) xtitle("Time from T2DM diagnosis, y") ytitle("Proportion") legend(pos(5) ring(0) col(1)) plotopts( lstyle(medthick) lpattern(solid) lcolor(blue)) 


/////////////////////////////////////////////


** Time to 2nd line therapy after receiving  1st line **

** 2ND LINE **
tab line2, m 
* 43,300 had an intensification after 1st line therapy
* 77,109 had no 2ND LINE intensification - so either ended at line1 or had no prescription 
* use lastfu_dt for these patients as right censoring will be the same 


/*follow-up*/
gen out2     = 1 if initiation_dt2 !=.      // event
replace out2 = 0 if missing(initiation_dt2)  // no event 
replace out2 = . if missing(initiation_dt1)  // only include those who had a 1st line therapy as we want to see how many move onto 2nd line after their first-line 

gen end2     = initiation_dt2 if out2 == 1      // event of 2nd line - date
replace end2 = lastfu_dt if out2 == 0       // right censoring using last follow-up - date

*--------------------------------------------------------------------------------*

******** CHECK IF INTENSIFICATION IS OVER LAST FOLLLOW-UP **********************
gen over2     = initiation_dt2
replace over2 = 999 if initiation_dt2 > lastfu_dt & initiation_dt2!=.
***** 1 WAS OVER - DUE TO DATA BEING LINKED TO OTHER DATASETS OUTSIDE OF CPRD  -  PUT AS RIGHT CENSORING 0 - USE LAST FOLLOW-UP

sort over2

replace end2  = lastfu_dt  if over2 == 999
replace out2  = 0  if over2 == 999
replace line2 = .  if over2==999
replace initiation_dt2 = .  if over2 == 999

*--------------------------------------------------------------------------------*


gen t2     = end2 - initiation_dt1        // time from 1st to 2nd line or last follow-up 
replace t2 = t2/365.24               // years 
order t2, after(out2)

stset t2, failure(out2 == 1)
sort t2
distplot t2

// 3,095 were given prescription  on the same day as 1st line - this is excluded from stset - take into account, add a tiny amount of time 
replace t2 = 1/365.24 if t2 == 0  // 1 day after 

stset t2, failure(out2 == 1) id(patid)
tab out2, m 
 
sts graph, failure risktable
stpm2, scale(h) df(3)
predict hr2, h per(1000)  // per 1000
line hr2 _t, sort   // HR instaneous rate of receiveing second-line 

stptime, title(Person-years)

stci

qui stci
local p50 = r(p50)
sts graph, failure ci yline(0.50, lpattern(shortdash) lcolor(black)) ylab(0(0.1)1) ///
xlab(0(1)18, grid) graphregion(fcolor(white)) title("Time to 2nd line prescription")  xline(`p50', lpattern(shortdash) lcolor(black)) xtitle("Time from 1st line prescription, y") ytitle("Proportion") legend(pos(5) ring(0) col(1)) plotopts( lstyle(medthick) lpattern(solid) lcolor(blue))

* MEDIAN 4.07 (4.01, 4.13)years 

/////////////////////////////////////////////

** Time to 3RD line therapy after receiving  2ND line **

** 3RD LINE **
tab line3, m 
* 19,797 had an intensification after 2nd line therapy
* 100,612 had no 3RD LINE intensification - so either ended at line1, line2 or had no prescription 
* use lastfu_dt for these patients as right censoring will be the same 


/*follow-up*/
gen out3     = 1 if initiation_dt3 !=.      // event
replace out3 = 0 if missing(initiation_dt3)  // no event 
replace out3 = . if missing(initiation_dt2)  // only include those who had a 2nd line therapy as we want to see how many move onto 3rd line after their 2nd 
replace out3 = . if missing(initiation_dt1) 
tab out3, m

gen end3     = initiation_dt3 if out3 == 1      // event of 3rd line date
replace end3 = lastfu_dt if out3 == 0      // right cenosring using last follow-up date


*--------------------------------------------------------------------------------*

******** CHECK IF INTENSIFICATION IS OVER LAST FOLLLOW-UP **********************
gen over3     = initiation_dt3
replace over3 = 999 if initiation_dt3 > lastfu_dt & initiation_dt3!=.
***** 2 WAS OVER - DUE TO DATA BEING LINKED TO OTHER DATASETS OUTSIDE OF CPRD  -  PUT AS RIGHT CENSORING 0 - USE LAST FOLLOW-UP 

sort over3

replace end3  = lastfu_dt  if over3 == 999
replace out3  = 0  if over3 == 999
replace line3 = .  if over3==999
replace initiation_dt3 = .  if over3 == 999

*--------------------------------------------------------------------------------*

gen t3     = end3 - initiation_dt2        // time from 2nd line to 3rd line or last follow-up 
replace t3 = t3/365.24               // years 
order t3, after(out3)

stset t3, failure(out3 == 1)
sort t3
distplot t3

// 433 were given prescription  on the day of T2DM/2nd intensification - this is excluded from stset - take into account, add a tiny amount of time 
replace t3 = 1/365.24 if t3 == 0  // 1 day after 

stset t3, failure(out3 == 1) id(patid)
tab out3, m 
 
sts graph, failure risktable
stpm2, scale(h) df(3)
predict hr3, h per(1000)  // per 1000
line hr3 _t, sort   // HR instaneous rate of recieveing second-line 

stptime, title(Person-years)

stci

qui stci
local p50 = r(p50)
sts graph, failure ci yline(0.50, lpattern(shortdash) lcolor(black)) ylab(0(0.1)1) ///
xlab(0(1)18, grid) graphregion(fcolor(white)) title("Time 3rd line prescription")  xline(`p50', lpattern(shortdash) lcolor(black)) xtitle("Time from 2nd line prescription, y") ytitle("Proportion") legend(pos(5) ring(0) col(1)) plotopts( lstyle(medthick) lpattern(solid) lcolor(blue))

** Median 4.57 (4.47, 4.67)

/////////////////////////////////////////////

** Time to 4th line therapy after receiving  3rd line **

** 4th LINE **
tab line4, m 
* 8,245 had a 4th line intensification after 3rd line therapy
* 112,164 had no 4th LINE intensification - so either ended at line1, line2, line3 or had no prescription 
* use lastfu_dt for these patients as right censoring will be the same 


/*follow-up*/
gen out4     = 1 if initiation_dt4 !=.      // event
replace out4 = 0 if missing(initiation_dt4)  // no event 
replace out4 = . if missing(initiation_dt3)  // only include those who had a 3rd line therapy as we want to see how many move onto 4th line 
tab out4, m

gen end4     = initiation_dt4 if out4 == 1      // event of 4th line date
replace end4 = lastfu_dt if out4 == 0       // right cenosring using last follow-up date

*--------------------------------------------------------------------------------*

******** CHECK IF INTENSIFICATION IS OVER LAST FOLLLOW-UP **********************
gen over4     = initiation_dt4
replace over4 = 999 if initiation_dt4 > lastfu_dt & initiation_dt4!=.
***** 0 WAS OVER

*--------------------------------------------------------------------------------*


gen t4     = end4 - initiation_dt3        // time from 3rd to 4th line or last follow-up 
replace t4 = t4/365.24                // years 
order t4, after(out4)


stset t4, failure(out4 == 1)
sort t4
distplot t4
// 132 were given prescription  on the day of T2DM - this is excluded from stset - take into account, add a tiny amount of time 
replace t4 = 1/365.24 if t4 == 0  // 1 day after 


stset t4, failure(out4 == 1) id(patid)
tab out4, m 
 
sts graph, failure risktable
stpm2, scale(h) df(3)
predict hr4, h per(1000)  // per 1000
line hr4 _t, sort   // HR instaneous rate of recieveing second-line 

stptime, title(Person-years)

stci

qui stci
local p50 = r(p50)
sts graph, failure ci yline(0.50, lpattern(shortdash) lcolor(black)) ylab(0(0.1)1) ///
xlab(0(1)18, grid) graphregion(fcolor(white)) title("Time to 4th line prescription")  xline(`p50', lpattern(shortdash) lcolor(black)) xtitle("Time from 3rd line prescription, y") ytitle("Proportion") legend(pos(5) ring(0) col(1)) plotopts( lstyle(medthick) lpattern(solid) lcolor(blue))

* Median 4.70 (4.55, 4.86) years

*** GRAPH *** 
*cd "\\uol.le.ac.uk\root\staff\home\y\yc244\My Documents\1 Epidemiologist\3 Projects\6 Novo Nordisk\3 Data\Figures and Tables\2 Time to treatments"

*graph combine  Prescribed_1st_line.gph Prescribed_2nd_line.gph  Prescribed_3rd_line.gph  Prescribed_4th_line.gph   , ycommon col(1)

// x=5, y=20, scale factor 1.5

////////////////////////////////////////////////////////////////////////////////////////// 


**** Time to a POTENTIAL INTENSIFICATION EVENT after T2DM diagnosis  ****

 
****** CVD event, macrovascular event, and CVD comorbidity are similar but just used different coding - use the earliest **********

* keep incident_macrovas_dt incident_CVDevent_dt incident_CVDmm_dt  total_cv index_dt patid   
* sort incident_macrovas_dt
* some are the same dates, and some are different - take the lowest one from these FIRST MACROVASCULAR EVENT

gen cv1 = incident_macrovas_dt 
gen cv2 = incident_CVDevent_dt 
gen cv3 = incident_CVDmm_dt 

egen min=rowmin(cv*)

format cv1 cv2 cv3  %td

format min %td
order index_dt, after(min)

rename min incident_mincv_dt

*--------------------------------------------------------------------------------*


gen cv_out=1 if incident_mincv_dt!=.  // event 
replace cv_out=0 if missing(incident_mincv_dt)  // censor did not have event 
sort cv_out
tab cv_out
* 30,121 had cvd event 

gen end_cv = incident_mincv_dt if cv_out==1 // event macrovascular event 
replace end_cv = lastfu_dt if cv_out==0 // right cenosoring using last follow-up if not had the event************************


*--------------------------------------------------------------------------------*
*************   CHECK IF EVENT IS OVER LAST FOLLLOW-UP  ************************
gen overmac     = incident_mincv_dt
replace overmac = 999 if incident_mincv_dt > lastfu_dt & incident_mincv_dt!=.
***** 9,011 WAS OVER - DUE TO DATA BEING LINKED TO OTHER DATASETS OUTSIDE OF CPRD  -  PUT AS RIGHT CENSORING 0 - USE LAST FOLLOW-UP 

sort overmac

replace end_cv = lastfu_dt  if overmac == 999
replace cv_out = 0  if overmac == 999
replace incident_mincv_dt = .  if overmac == 999

*--------------------------------------------------------------------------------*

format end_cv %td

gen t1_cv = end_cv - index_dt      // time from t2dm to event
replace t1_cv = t1_cv/365.24          // years 

stset t1_cv, f(cv_out==1)
sort t1_cv
distplot t1_cv 
// 67 DID NOT HAVE AN EVENT AND LAST DAY OF FOLLOW-UP was the day of T2DM - this is excluded from stset - take into account, add a tiny amount of time 
replace t1_cv = 1/365.24 if t1_cv == 0  // 1 day after 

stset t1_cv, f(cv_out==1) id(patid)
stci

stptime, title(Person-years)

qui stci
local p50 = r(p50)
sts graph, failure ci yline(0.50, lpattern(shortdash) lcolor(black)) ylab(0(0.1)1) ///
xlab(0(1)18, grid) graphregion(fcolor(white)) title("Time to macrovascular event")  xline(`p50', lpattern(shortdash) lcolor(black)) xtitle("Time from T2DM diagnosis, y") ytitle("Proportion") legend(pos(5) ring(0) col(1)) plotopts( lstyle(medthick) lpattern(solid) lcolor(cranberry))


* Median 14.52 (14.28, 14.79) years

* N=120,409
* Event = 30,121

tab incident_CVDMM_cat cv_out, m col

order incident_CVDevent_dt, after(incident_CVDmm_dt)
order incident_macrovas_dt, after(incident_CVDmm_dt)


** NON-CARDIOVASCULAR **

gen noncv_out=1 if incident_nonCVDmm_dt!=.
replace noncv_out=0 if missing(incident_nonCVDmm_dt)  // censor
sort noncv_out
tab noncv_out

gen end_noncv = incident_nonCVDmm_dt if noncv_out==1 // event noncvascular event 
replace end_noncv = lastfu_dt if noncv_out==0 // right cenosoring using last follow-up if not had the event************************


*--------------------------------------------------------------------------------*

*************   CHECK IF EVENT IS OVER LAST FOLLLOW-UP  ************************
gen overncv     = incident_nonCVDmm_dt
replace overncv = 999 if incident_nonCVDmm_dt > lastfu_dt & incident_nonCVDmm_dt!=.
***** 0 WAS OVER 

*--------------------------------------------------------------------------------*

gen t1_noncv     = end_noncv-index_dt      // time from t2dm to event
replace t1_noncv = t1_noncv/365.24          // years 

format end_noncv %td

stset t1_noncv, f(noncv_out==1)
sort t1_noncv
distplot t1_noncv 
// 67 last day same as T2DM - this is excluded from stset - take into account, add a tiny amount of time 
replace t1_noncv = 1/365.24 if t1_noncv == 0  // 1 day after 

stset t1_noncv, f(noncv_out==1) id(patid)
stci

stptime, title(Person-years)

qui stci
local p50 = r(p50)
sts graph, failure ci yline(0.50, lpattern(shortdash) lcolor(black)) ylab(0(0.1)1) ///
xlab(0(1)18, grid) graphregion(fcolor(white)) title("Time to first non-cardiovascular event")  xline(`p50', lpattern(shortdash) lcolor(black)) xtitle("Time from T2DM diagnosis, y") ytitle("Proportion") legend(pos(5) ring(0) col(1)) plotopts( lstyle(medthick) lpattern(solid) lcolor(cranberry))

* Median 12.76 (12.52, 12.92) years

* N=120,409
* Event = 31,503

tab incident_nonCVDMM_cat if noncv_out==1


************************************************************************************

**** Time to next line therapy after 'POTENTIAL INTENSIFICATION EVENT'  ****

** FROM MACROVASCULAR TO INTENSIFICATION ** 
       	   
order  initiation_dt1 initiation_dt2 initiation_dt3 initiation_dt4   , after(_t0)

sort initiation_dt1 

order incident_mincv_dt, after (initiation_dt4)
  
gen minc1 = initiation_dt1 if  initiation_dt1 > incident_mincv_dt  // the line has to be greater than the event - for it be 'intensified'
gen minc2 = initiation_dt2 if  initiation_dt2 > incident_mincv_dt
gen minc3 = initiation_dt3 if  initiation_dt3 > incident_mincv_dt
gen minc4 = initiation_dt4 if  initiation_dt4 > incident_mincv_dt
format minc1 minc2 minc3 minc4  %td

egen minline_macrov = rowmin(minc*)     // use the smallest time to next line intensification
format minline_macrov %td


gen marcov_nxt_out     = 1 if  minline_macrov != .  // event if next line of therapy (can be any 1,2,3,4)
replace marcov_nxt_out = 0 if missing(minline_macrov) //  no next line therapy 
replace marcov_nxt_out = . if missing(incident_mincv_dt)  // only include those who had macrovascular event
tab marcov_nxt_out, m


gen end_macrov_nxtline     = minline_macrov if marcov_nxt_out == 1 // event microvascular event 
replace end_macrov_nxtline = lastfu_dt if marcov_nxt_out == 0 // right cenosoring using last follow-up if not had the intensification****



*--------------------------------------------------------------------------------*
*************   CHECK IF INTENS EVENT IS OVER LAST FOLLLOW-UP  ************************
gen overmac_int     = incident_mincv_dt
replace overmac_int = 999 if incident_mincv_dt > lastfu_dt & incident_mincv_dt!=.
***** 0 WAS OVER -ALREADY DONE FROM BEFORE 
*--------------------------------------------------------------------------------*


format end_macrov_nxtline  %td

gen t1_macrov_nxtline     = end_macrov_nxtline - incident_mincv_dt      // time from microv to intensification
replace t1_macrov_nxtline = t1_macrov_nxtline/365.24          // years 

stset t1_macrov_nxtline, f(marcov_nxt_out==1)
sort t1_macrov_nxtline 
distplot t1_macrov_nxtline 

order lastfu_dt, after(incident_mincv_dt)
order death, after(lastfu_dt)


// 490 had macrovascular event the same day as last follow-up- - take into account, add a tiny amount of time 
replace t1_macrov_nxtline = 1/365.24 if t1_macrov_nxtline == 0  // 1 day after 


stset t1_macrov_nxtline, f(marcov_nxt_out==1) id(patid)
sts graph, failure risktable
stci

stptime, title(Person-years)

qui stci
local p50 = r(p50)
sts graph, failure ci yline(0.50, lpattern(shortdash) lcolor(black)) ylab(0(0.1)1) ///
xlab(0(1)18, grid) graphregion(fcolor(white)) title("Time to next line intensification")  xline(`p50', lpattern(shortdash) lcolor(black)) xtitle("Time from macrovascular event, y") ytitle("Proportion") legend(pos(5) ring(0) col(1)) plotopts( lstyle(medthick) lpattern(solid) lcolor(blue))

* N=30,121
* Event=10,098


* Median 5.46 (5.30, 5.61) years 

 
 
** FROM NON-CARDIOVASCULAR TO INTENSIFICATION ** 
	   
order  initiation_dt1 initiation_dt2 initiation_dt3 initiation_dt4   , after(_t0)

sort initiation_dt1 

order incident_nonCVDmm_dt, after (initiation_dt4)
  
gen minn1 = initiation_dt1 if  initiation_dt1 > incident_nonCVDmm_dt  // the line has to be greater than the event - for it be 'intensified'
gen minn2 = initiation_dt2 if  initiation_dt2 > incident_nonCVDmm_dt
gen minn3 = initiation_dt3 if  initiation_dt3 > incident_nonCVDmm_dt
gen minn4 = initiation_dt4 if  initiation_dt4 > incident_nonCVDmm_dt
format minn1 minn2 minn3 minn4  %td

egen minline_noncv = rowmin(minn*)     // use the smallest time to next line intensification
format minline_noncv %td

       
gen noncv_nxt_out     = 1 if  minline_noncv != .  // event if next line of therapy (can be any 1,2,3,4)
replace noncv_nxt_out = 0 if missing(minline_noncv) //  no next line therapy 
replace noncv_nxt_out = . if missing(incident_nonCVDmm_dt)  // only include those who had microvascular event
tab noncv_nxt_out, m

      
gen end_noncv_nxtline     = minline_noncv if noncv_nxt_out == 1 // event microvascular event 
replace end_noncv_nxtline = lastfu_dt if noncv_nxt_out == 0 // right cenosoring using last follow-up if not had the intensification****


*--------------------------------------------------------------------------------*
*************   CHECK IF INTENS EVENT IS OVER LAST FOLLLOW-UP  ************************
gen overnc_int     = end_noncv_nxtline
replace overnc_int = 999 if end_noncv_nxtline > lastfu_dt & end_noncv_nxtline!=.
***** 0 WAS OVER -ALREADY DONE FROM BEFORE 
*--------------------------------------------------------------------------------*


format end_noncv_nxtline  %td

gen t1_noncv_nxtline     = end_noncv_nxtline - incident_nonCVDmm_dt      // time from microv to intensification
replace t1_noncv_nxtline = t1_noncv_nxtline/365.24                  // years 

stset t1_noncv_nxtline, f(noncv_nxt_out==1)
sort t1_noncv_nxtline 
distplot t1_noncv_nxtline 

order lastfu_dt, after(incident_nonCVDmm_dt)
order death, after(lastfu_dt)


// 154 had non-cardiovascular event / died the same day as last follow-up- - take into account, add a tiny amount of time - right censorerd - so they are not excluded from the study 
replace t1_noncv_nxtline = 1/365.24 if t1_noncv_nxtline == 0  // 1 day after 


stset t1_noncv_nxtline, f(noncv_nxt_out==1) id(patid)
sts graph, failure risktable
stci

stptime, title(Person-years)

qui stci
local p50 = r(p50)
sts graph, failure ci yline(0.50, lpattern(shortdash) lcolor(black)) ylab(0(0.1)1) ///
xlab(0(1)18, grid) graphregion(fcolor(white)) title("Time to intensification (n=31,503)")  xline(`p50', lpattern(shortdash) lcolor(black)) xtitle("Time from non-cardiovascular event, y") ytitle("Proportion") legend(pos(5) ring(0) col(1)) plotopts( lstyle(medthick) lpattern(solid) lcolor(blue))
 
* N=31,503
* event=9,515

* Median 6.84 (6.63, 7.08) years 
 
**********************************************
 * Combine graphs 
graph combine Macrovascular_event.gph Macrovascular_event_Intensification.gph, ycommon row(1)
graph combine Non-cardiovascular_event.gph Non-cardiovascular_event_Intensification.gph, ycommon row(1)

 ** END OF ANALYSIS ** 
 
 
 

