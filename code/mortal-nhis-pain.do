capture log close
log using nhis-pain.txt, replace text

//  program:    nhis-pain.do
//  task:		produce estimates of pain trends
//  author:     sam harper \ 2024-06-06


* load NHIS data
use "nhis_00020.dta", clear


* set survey design
svyset psu [pweight=perweight], strata(strata) vce(linearized) ///
	singleunit(missing)
	
* recode gender
gen male = (sex==1) if (sex!=7 & sex!=9)
tab sex male, mis

* age >=25
gen age25 = (age>=25) if (age!=997 & age!=999)
tab age25, mis

gen age2574 = age if (age>=25 & age<75 & age!=997 & age!=999)

* recode to age groups
recode age (0/14 = 1 "<15") (15/24 = 2 "15-24") (25/34 = 3 "25-34") ///
  (35/44 = 4 "35-44") (45/54 = 5 "45-54") (55/64 = 6 "55-64") ///
  (65/85 = 7 "65+") (997 999 = .), gen(age8)
tab age8

* groups over 25
recode age2574 (25/34 = 1 "25-34") ///
  (35/44 = 2 "35-44") (45/54 = 3 "45-54") (55/64 = 4 "55-64") ///
  (65/85 = 5 "65-74"), gen(age25cat)

* generate working-age population
recode age8 (3/6 = 1) (7 = 0) (1 2 = .), gen(wa)
label var wa "Working age?"
label values wa ny
	
* recode education
recode educ (0 = .) (102/116 = 1 "<HS") (201 202 302 303 = 2 "HS") ///
  (301 = 3 "Some coll") (400/505 = 4 "Univ") ///
  (997/999 = .), gen(educ4)
tab educ educ4, mis

recode educ4 (4 = 0 "No") (1/3 = 1 "Yes"), gen(nouni)
label var nouni "Less than university education?"

label define ny 0 "No" 1 "Yes", modify
label values nouni ny
tab nouni, mis

recode educ (102/116 = 1 "<HS") (201 = 2 "HS Grad") (202 302 303 = 3 "GED/Assoc") (301 = 4 "Some college") (400 = 5 "BA") (501/505 = 6 ">BA") (0 997/999 = .), gen(educ6)
tab educ educ6


* recode race and ethnicity
gen nhw = 1 if racea==100 & hispyn==1
replace  nhw=0 if (racea>100 & racea<990) | hispyn==2


label var nhw  "non-Hispanic white?"
label values nhw ny
tab nhw

gen nhb = 1 if racea==200 & hispyn==1
replace  nhb=0 if (racea !=100 & racea>200 & racea<990) | hispyn==2


label var nhb  "non-Hispanic black?"
label values nhb ny
tab nhb

* collapse to 3-year averages
egen year3 = cut(year), at(2001(3)2024) icodes
bysort year3: sum(year)
replace year3 = . if year3 == 6



* recode back pain
tab lbpain3mo
tab lbpain3mo, nol
recode lbpain3mo (0 7/9=.) (1=0 no) (2=1 yes), gen(lbpain)
label var lbpain "Low back pain past 3 months?"
label values lbpain ny

* estimate prevalence in each year
svy linearized : logit lbpain i.year

* marginal probabilities
margins year, vce(unconditional)

* change from base year
margins r.year, vce(unconditional)


* recode neck pain
tab neckpain3mo
tab neckpain3mo, nol
recode neckpain3mo (0 7/9=.) (1=0 no) (2=1 yes), gen(npain)
label var npain "Neck pain past 3 months?"
label values npain ny


* recode face pain
tab facepain3mo
tab facepain3mo, nol
recode facepain3mo (0 7/9=.) (1=0 no) (2=1 yes), gen(fpain)
label var fpain "Facial pain past 3 months?"
label values fpain ny

* estimate prevalence in each year
svy linearized : logit npain i.year

* marginal probabilities
margins year, vce(unconditional)

* any pain
gen anypain = 1 if (lbpain==1 | npain==1 | fpain==1)
replace anypain = 0 if (lbpain==0 & npain==0 & fpain==0)
label var anypain "Any back/neck/facial pain past 3 months?"
label values anypain ny

* non-Hispanic white adults
gen nhwadults = (age25==1 & nhw==1)

recode ahopeless (6/9 = .) (0 = 1 "None") ///
  (1/2 = 2 "Little/Some") ///
  (3/4 = 3 "Most/All"), gen(hope30d)
label var hope30d "Past month hopelessness"

recode hope30d (1 = 0) (2/3 = 1), gen(anyhopeless)
label var anyhopeless "Any hopelessness past 30d"
label values anyhopeless ny

gen hopeless = (hope30d==3)
label var hopeless "Hopeless most/all time past month"

* estimate prevalence in each year, by gender

foreach var of varlist lbpain npain fpain anypain hopeless {
  qui svy linearized, subpop(nhwadults): logit `var' male##nouni##year3
  margins male#nouni#year3, nofvlabel post
  
  *write marginal estimates to a dataset
  preserve
  tempfile `var'_m
  parmest, saving(``var'_m', replace)
  use ``var'_m', clear
  gen male=substr(parm,1,1)
  gen hash=strpos(parm,"#")
  gen univ=substr(parm,hash+1,1)
  gen yp=strpos(parm,"year")
  gen year=substr(parm,yp-2,1)
  destring male, replace
  destring univ, replace
  destring year, replace
  drop hash yp
  outsheet using `var'_m.csv, comma replace
  restore
}


log close
exit

