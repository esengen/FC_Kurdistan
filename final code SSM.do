*** Created by: Sinem Esengen
** Last updated: May 30,2025
clear
set more off
capture log close 

//Download the data either from MICS website or IPUMS.

//Set working directory
cd "XXX"
local dir_out "XXXX"
use "XXX.dta"

//Start a log file
log using"XXXXX", append

//survey design
svyset PSU [pw = wmweight], strata(stratum) singleunit(scaled) 
svydescribe 

// 1- analytic sample : currently married women and women with daughters in Kurdistan

//number of daughters total
egen number_daug= rowtotal(CM5B CM7B)
label variable number_daug "number_daug"
tabulate number_daug, missing
summ number_daug
svy: mean number_daug

//kurdistan with Kerkuk  *correct

generate kurdistan=Survey 
lab list Survey
replace kurdistan=0 if Survey==1 
replace kurdistan=1 if Survey==2 | HH71 ==14
label define kurdistan 1"kurdistan wih kerkuk" 0"other"
label values kurdistan kurdistan
svy: tab  kurdistan

//support for continuation in kurdistan
svy: tab   FG22 if kurdistan ==1

// marital status  
codebook MA6,detail
tabulate MA6, missing 
generate marital_status=MA6 
tab marital_status, missing 
label variable marital_status "marital_status"
tabulate marital_status, missing
label values marital_status MA6
svy: tab marital_status, missing 
tab marital_status kurdistan
svy: tab MA5

//CLEAN : married or lived with a men before
tabulate MA7, missing 
generate ever_cohabit=.
replace ever_cohabit =0 if MA7==. 
replace ever_cohabit =1 if MA7==1 | MA7==2
label variable ever_cohabit "ever_cohabit"
tabulate ever_cohabit, missing
tab ever_cohabit kurdistan if number_daug!=0

generate ever_married=0
replace ever_married =1 if marital_status!=. | ever_cohabit!=0
tab ever_married kurdistan if number_daug!=0

**ever divorced or seperated
generate ever_divorced=0
replace ever_divorced =1 if ever_cohabit==1 & (marital_status==2 |  marital_status==3)
tab ever_divorced kurdistan

		
//CLEAN :CURRENTLY MARRIED
*NOTE: No spousal information if not currently married.///

**currently married *correct

tab MA1,m
generate currently_married=.
replace currently_married =1 if MA1==1 
tab currently_married, missing 
tab currently_married kurdistan if number_daug!=0,m

tab currently_married if kurdistan==1 & number_daug!=0


//CLEAN MA2:AGE OF PARTNER 
codebook MA2,detail
summ MA2
tab MA2,m
generate age_partner=MA2 
tab age_partner, missing 

 label list MA2 //99coded as no response and 98 as DK
replace age_partner=. if MA2==99  | MA2==98

label variable age_partner "age_partner"
tabulate age_partner, missing
label values age_partner MA2
tab age_partner,m

// 64 missing on partners age in the sample
tab age_partner  if currently_married==1 & kurdistan==1 & number_daug!=0,m

//CLEAN OUTCOME - ANY DAUG FG

tab FG10A number_daug,m
gen n_circ_daug = .
replace n_circ_daug=. if FG10A==99
replace n_circ_daug=0 if FG10A==0 
replace n_circ_daug=1 if FG10A==1
replace n_circ_daug=2 if FG10A==2 
replace n_circ_daug=3 if FG10A==3 
replace n_circ_daug=4 if FG10A==4 
replace n_circ_daug=5 if FG10A==5 
replace n_circ_daug=6 if FG10A==6 
replace n_circ_daug=7 if FG10A==7 
replace n_circ_daug=8 if FG10A==8 
replace n_circ_daug=9 if FG10A==9 
replace n_circ_daug=10 if FG10A==10 
replace n_circ_daug=11 if FG10A==11
replace n_circ_daug=12 if FG10A==12
label variable n_circ_daug "number of circumsized daug"

generate any_daug_fgm=.
tab FG10A number_daug 
replace any_daug_fgm=1 if FG10A>0 & FG10A<10
replace any_daug_fgm=0 if FG10A==0
label define any_daug_fgm 1"yes" 0"no"
label values any_daug_fgm any_daug_fgm
tab any_daug_fgm,m

// 954 missing on outcome var in the sample
tab any_daug_fgm if currently_married==1 & kurdistan==1 & number_daug!=0 & age_partner !=.,m
g missing_on_outcome_mysample =0
replace missing_on_outcome_mysample=1 if any_daug_fgm==. &( currently_married==1 & kurdistan==1 & number_daug!=0 & age_partner !=. )
* missing women coded as No

//FG1 and FG2
tab FG1 
tab FG2
lab list FG1
lab list FG2
generate daug_fg_missing_No=.
label variable daug_fg_missing_No "missings on daughter's status coded as No"
replace daug_fg_missing_No=1 if n_circ_daug>0 & n_circ_daug<10
replace daug_fg_missing_No=0 if n_circ_daug==0
replace daug_fg_missing_No=0 if missing_on_outcome_mysample==1
replace daug_fg_missing_No=. 
label define daug_fg_missing_No 1"yes" 0"no"
label values daug_fg_missing_No any_daug_fgm
tab daug_fg_missing_No   

**age at FG for women
generate age_circ=FG7 
replace age_circ=. if FG7>25
label values age_circ FG7
tab age_circ,m
summ age_circ,detail


//sub population
generate mysample=0
replace mysample=1 if (currently_married==1 & number_daug!=0 & kurdistan==1 & age_partner!=. & any_daug_fgm!=.) 
tab mysample, missing 
label variable mysample "currently married, and has daughters, kurdistan"
svy, subpop(mysample): tab any_daug_fgm

//LOOK FOR THE WOMEN WHO ARE NOT ASKED FGM QUESTION  *correct
tab HH71 if FG1==2 & mysample==1    // never HEARD OF circumcision =54
tab HH71 if FG2==2  & mysample==1    // never heard of genital mutilation =0
tab HH71 if FG2==1  & mysample==1    //  heard of genital mutilation =54
tab HH71 if FG3==2 &mysample==1 

//those 54 heard of FGM heard of genital cutting
tab FG2 FG1 if FG1==2 & mysample==1


// AGE WOMEN 5 YEAR COHORTS
g agew_5=.
replace agew_5 = 1 if agew >=15 & agew <=19
replace agew_5 = 2 if agew >=20 & agew <=24
replace agew_5 = 3 if agew >=25 & agew <=29
replace agew_5 = 4 if agew >=30 & agew <=34
replace agew_5 = 5 if agew >=35 & agew <=39
replace agew_5 = 6 if agew >=40 & agew <=44
replace agew_5 = 7 if agew >=45 & agew <=49
				label define agew_5 1"[15,19]" 2"[20,24]" 3 "[25,29]" 4"[30,34]" 5"[35,39]" 6"[40,44]" 7 "[45,49]"
				label values agew_5 agew_5
tab agew_5,m

g agew_10=.
replace agew_10 = 1 if agew >=15 & agew <=24
replace agew_10 = 2 if agew >=25 & agew <=34
replace agew_10 = 3 if agew >=35 & agew <=44
replace agew_10 = 4 if agew >=45 & agew <=49
				label define agew_10 1"[15,24]" 2"[25,34]" 3 "[35,44]" 4"[45+]" 
				label values agew_10 agew_10
tab agew_10,m


*************************************
//feminist theory varaibles cleaning
*************************************
//CLEAN age af first marrige (wagem)    

codebook wagem,detail
tabulate wagem, missing 
generate age_at_1st_marriage=wagem 
label variable age_at_1st_marriage "age_at_1st_marriage"
tabulate age_at_1st_marriage, missing
label values age_at_1st_marriage wagem
tab age_at_1st_marriage,m
svy, subpop(mysample): mean age_at_1st_marriage

g age_at_1st_marriage_cl=age_at_1st_marriage
g age_at_1st_marriage_dist=age_at_1st_marriage
g age_at_1st_marriage_srt=age_at_1st_marriage

*age categorical
g cat_age_at_1st_marriage=age_at_1st_marriage
replace cat_age_at_1st_marriage=1 if age_at_1st_marriage<=12
replace cat_age_at_1st_marriage=1 if age_at_1st_marriage<=17 & age_at_1st_marriage >12
replace cat_age_at_1st_marriage=2 if age_at_1st_marriage<=23 & age_at_1st_marriage >=18
replace cat_age_at_1st_marriage=3 if age_at_1st_marriage<=29 & age_at_1st_marriage >=24
replace cat_age_at_1st_marriage=4 if age_at_1st_marriage<=50 & age_at_1st_marriage >=30
label define cat_age_at_1st_marriage  1"(less than 18)" 2" [18 - 23] " 3"[24 and 29]" 4"(above 30)"
label values cat_age_at_1st_marriage cat_age_at_1st_marriage
tab cat_age_at_1st_marriage,m
svy, subpop(mysample): tab cat_age_at_1st_marriage


 ***dummy out age 
 
 g marr_age_1=0
 replace marr_age_1=1 if age_at_1st_marriage<=17
				label define marr_age_1 1"less than 18 " 0"0"
				label values marr_age_1 marr_age_1
tab marr_age_1 if mysample==1


  g marr_age_2=0
replace marr_age_2=1 if age_at_1st_marriage<=23 & age_at_1st_marriage >=18
				label define marr_age_2 1"[18 - 23]" 0"0"
				label values marr_age_2 marr_age_2
tab marr_age_2 if mysample==1



    g marr_age_3=0
replace marr_age_3=1 if age_at_1st_marriage<=29 & age_at_1st_marriage >=24
				label define marr_age_3 1"[24 and 29] " 0"0"
				label values marr_age_3 marr_age_3
tab marr_age_3 if mysample==1


 g marr_age_4=0
 replace marr_age_4=1 if age_at_1st_marriage<=50 & age_at_1st_marriage >=30
				label define marr_age_4 1"(above 30) " 0"0"
				label values marr_age_4 marr_age_4
tab marr_age_4 if mysample==1


//Spousal age difference & young husband  

*recode spousal age difference

generate current_spousal_age_diffr= age_partner - WB2

tabstat current_spousal_age_diffr, stat(mean, sd, min, max, N)
tab current_spousal_age_diffr if mysample==1

summ current_spousal_age_diffr 
summ age_at_1st_marriage 
tab current_spousal_age_diffr,m

			
** spousal age difference categorical
g spousal_age_diffr_4y=.
replace spousal_age_diffr_4y=0 if current_spousal_age_diffr>=-4 &  current_spousal_age_diffr<=4

replace spousal_age_diffr_4y=1 if current_spousal_age_diffr<-4

replace spousal_age_diffr_4y=2 if current_spousal_age_diffr>4 & current_spousal_age_diffr <=14

replace spousal_age_diffr_4y=3 if current_spousal_age_diffr>14 & current_spousal_age_diffr < 48

tab spousal_age_diffr_4y
				label define spousal_age_diffr_new_4y  0"(within +-4 years apart of each other [-4,+4])" 				1"(more than 4 years younger husband [-5,-22])"				2"husband older by 5-14 years [5,14]" 				3"husband older by 15 or more years [15,47]"			
				label values spousal_age_diffr_4y spousal_age_diffr_new_4y
 svy, subpop(mysample): tab spousal_age_diffr_4y

 
 ***dummy out sp age diffr *correct
tab current_spousal_age_diffr if mysample ==1

 g spousal_age_diffr_younger=0
 replace spousal_age_diffr_younger=1 if current_spousal_age_diffr<=-5
				label define spousal_age_diffr_younger 1"younger husband by 5 or more [-22,-5] " 0"0"
				label values spousal_age_diffr_younger spousal_age_diffr_younger4
tab spousal_age_diffr_younger if mysample==1
g spousal_age_younger_cl=spousal_age_diffr_younger
g spousal_age_younger_dist=spousal_age_diffr_younger
g spousal_age_younger_srt=spousal_age_diffr_younger

  g spousal_age_diffr_4years=0
replace spousal_age_diffr_4years=1 if current_spousal_age_diffr<=4 & current_spousal_age_diffr>=-4
				label define spousal_age_diffr_4years 1"within 4 years[-4,4] " 0"0"
				label values spousal_age_diffr_4years spousal_age_diffr_4years
tab spousal_age_diffr_4years if mysample==1

				list spousal_age_diffr_4years  if id==52884
g spousal_age_4years_cl=spousal_age_diffr_4years
g spousal_age_4years_dist=spousal_age_diffr_4years
g spousal_age_4years_srt=spousal_age_diffr_4years

    g spousal_age_diffr_mid=0
replace spousal_age_diffr_mid=1 if current_spousal_age_diffr>=5 & current_spousal_age_diffr<=14
				label define spousal_age_diffr_mid 1"[5,14] " 0"0"
				label values spousal_age_diffr_mid spousal_age_diffr_mid
tab spousal_age_diffr_mid if mysample==1

g spousal_age_mid_cl=spousal_age_diffr_mid
g spousal_age_mid_dist=spousal_age_diffr_mid
g spousal_age_mid_srt=spousal_age_diffr_mid

 g spousal_age_diffr_older=0
 replace spousal_age_diffr_older=1 if current_spousal_age_diffr>14
				label define spousal_age_diffr_older 1"older[14,] " 0"0"
				label values spousal_age_diffr_older spousal_age_diffr_older
tab spousal_age_diffr_older if mysample==1

				list spousal_age_diffr_younger  if id== 13660   
				
g spousal_age_older_cl=spousal_age_diffr_older
g spousal_age_older_dist=spousal_age_diffr_older
g spousal_age_older_srt=spousal_age_diffr_older
			

 ***dummy out sp age diffr *correct

 g spousal_age_diffr_younger2=0
 replace spousal_age_diffr_younger2=1 if spousal_age_diffr_2==0 
				label define spousal_age_diffr_younger2 1"younger husband by 2 or more  " 0"0"
				label values spousal_age_diffr_younger2 spousal_age_diffr_younger2
tab spousal_age_diffr_younger2 if mysample==1


  g spousal_age_diffr_2years=0
replace spousal_age_diffr_2years=1 if spousal_age_diffr_2==1 
				label define spousal_age_diffr_2years 1"within 2 years " 0"0"
				label values spousal_age_diffr_2years spousal_age_diffr_2years
tab spousal_age_diffr_2years if mysample==1

				list spousal_age_diffr_4years  if id==52884


    g spousal_age_diffr_mid2=0
replace spousal_age_diffr_mid2=1 if spousal_age_diffr_2==2
				label define spousal_age_diffr_mid2 1"[older by 3-10] " 0"0"
				label values spousal_age_diffr_mid2 spousal_age_diffr_mid2
tab spousal_age_diffr_mid2 if mysample==1


 g spousal_age_diffr_older2=0
 replace spousal_age_diffr_older2=1 if spousal_age_diffr_2==3
				label define spousal_age_diffr_older2 1"older[10 +] " 0"0"
				label values spousal_age_diffr_older2 spousal_age_diffr_older2
tab spousal_age_diffr_older2 if mysample==1

				list spousal_age_diffr_younger  if id== 13660   

 ***dummy out sp age diffr *correct
tab current_spousal_age_diffr if mysample ==1

 g spousal_age_diffr_young10=0
 replace spousal_age_diffr_young10=1 if current_spousal_age_diffr<=-5
				label define spousal_age_diffr_young10 1"younger husband by 5 or more [-22,-5] " 0"0"
				label values spousal_age_diffr_young10 spousal_age_diffr_young10
tab spousal_age_diffr_young10 if mysample==1

  g spousal_age_diffr_4years10=0
replace spousal_age_diffr_4years10=1 if current_spousal_age_diffr<=4 & current_spousal_age_diffr>=-4
				label define spousal_age_diffr_4years10 1"within 4 years[-4,4] " 0"0"
				label values spousal_age_diffr_4years10 spousal_age_diffr_4years10
tab spousal_age_diffr_4years10 if mysample==1

				list spousal_age_diffr_4years10  if id==52884

    g spousal_age_diffr_mid10=0
replace spousal_age_diffr_mid10=1 if current_spousal_age_diffr>=5 & current_spousal_age_diffr<=10
				label define spousal_age_diffr_mid10 1"[5,10] " 0"0"
				label values spousal_age_diffr_mid10 spousal_age_diffr_mid10
tab spousal_age_diffr_mid if mysample==1


 g spousal_age_diffr_older10=0
 replace spousal_age_diffr_older10=1 if current_spousal_age_diffr>10
				label define spousal_age_diffr_older10 1"older[10,] " 0"0"
				label values spousal_age_diffr_older10 spousal_age_diffr_older10
tab spousal_age_diffr_older10 if mysample==1

				list spousal_age_diffr_older10  if id== 13660   
			
 
//husband related

//CLEAN MA12:  husband related to you
codebook MA9A,detail
tabulate MA9A, missing 
generate husband_related=0
replace husband_related=1 if MA9A==1 
label define husband_related  1"Yes" 0"No"
label values husband_related husband_related
tab husband_related, missing 
//replace husband_related=. if MA9A==9 
 svy, subpop(mysample): tab husband_related
 
 
g husband_related_cl=husband_related
g husband_related_dist=husband_related
g husband_related_srt=husband_related
			
//polygyny


//CLEAN MA3: husband have other wives    *correct
codebook MA3,detail
tabulate MA3, missing 
generate polygyny=. 

replace polygyny=1 if MA3==1
replace polygyny=0 if MA3==2

label define polygyny 1"yes" 0"no"
label values polygyny polygyny
tabulate polygyny, missing
tab polygyny if mysample==1
 svy, subpop(mysample): tab polygyny


g polygyny_cl=polygyny
g polygyny_dist=polygyny
g polygyny_srt=polygyny


// Intimate Part6ner violence justification (ipv)

				//CLEAN DV1A DV1B DV1C DV1D DV1E DV1E DV1F DV1G
				tab DV1A, missing    		//* there are no missings but DK
				generate beats1=DV1A 
				replace beats1=. if DV1A==9 
				replace beats1=1 if DV1A==1 
				replace beats1=0 if DV1A==2 
			
				tab beats1 //1237 DK
				
				list id if beats1==8
				//2048 ==1
				 // 6713 ==1
						// 53675 ==8
						// 54108 ==8
						//55168==0
						//55124 ==0
						
				egen miss_group = group(beats1)
				sort miss_group
				tab miss_group 
				list miss_group id in -10/l
				
				 tab miss_group if id==53675
				  tab miss_group if id==54108
				  
				 tab miss_group if id==2048
				  tab miss_group if id==6713
				  
				by miss_group: gen index = _n - 1

				list index if id==5648
				list index if id==26881
				list index if id==42927
				list index if id==38037

/* 
			mis group	ID
55190. |        3    5648 | 1232
55191. |        3   26881 | 1233
55192. |        3   42927 | 1234
55193. |        3   38037 | 1235
55194. |        3   28230 

*/

replace beats1 = mod(index, 2) if beats1 == 8
drop miss_group index
label variable beats1 "goes out w.o. telling"
label define beat1 1"yes" 0"no"
label values beats1 beat1
tab beats1,m

 tab beats1 if id==5648
 tab beats1 if id==26881
 tab beats1 if id==42927
 tab beats1 if id==38037

 tab DV1B, missing 
				generate beats2=DV1B 
				replace beats2=. if DV1B==9 
				replace beats2=1 if DV1B==1 
				replace beats2=0 if DV1B==2 
				egen miss_group = group(beats2)
sort miss_group
				by miss_group: gen index = _n - 1
replace beats2 = mod(index, 2) if beats2 == 8
drop miss_group index
				label variable beats2 "neglects children"
				label define beat2 1"yes" 0"no"
				label values beats2 beat2

				tab DV1C, missing
				generate beats3=DV1C 
				replace beats3=. if DV1C==9 
				replace beats3=1 if DV1C==1 
				replace beats3=0 if DV1C==2 
				egen miss_group = group(beats3)
sort miss_group
				by miss_group: gen index = _n - 1
replace beats3 = mod(index, 2) if beats3 == 8
drop miss_group index
				label variable beats3 "argues w husband"
				label define beat3 1"yes" 0"no"
				label values beats3 beat3
				tab beats3,m

				tab DV1D, missing 
				generate beats4=DV1D 
				replace beats4=. if DV1D==9 
				replace beats4=1 if DV1D==1 
				replace beats4=0 if DV1D==2 
				egen miss_group = group(beats4)
sort miss_group
				by miss_group: gen index = _n - 1
replace beats4 = mod(index, 2) if beats4 == 8
drop miss_group index

				label variable beats4 "refuse sex"
				label define beat4 1"yes" 0"no"
				label values beats4 beat4


				tab DV1E, missing 
				generate beats5=DV1E 
				replace beats5=0 if DV1E==9 
				replace beats5=1 if DV1E==1 
				replace beats5=0 if DV1E==2 
				egen miss_group = group(beats5)
sort miss_group
				by miss_group: gen index = _n - 1
replace beats5 = mod(index, 2) if beats5 == 8
drop miss_group index
				label variable beats5 "burns food"
				label define beat5 1"yes" 0"no"
				label values beats5 beat5


				tab DV1F, missing 
				generate beats6=DV1F 
				replace beats6=. if DV1F==9 
				replace beats6=1 if DV1F==1 
				replace beats6=0 if DV1F==2 
				egen miss_group = group(beats6)
sort miss_group
				by miss_group: gen index = _n - 1
replace beats6 = mod(index, 2) if beats6 == 8
drop miss_group index

				label variable beats6 "felt dirty"
				label define beat6 1"yes" 0"no"
				label values beats6 beat6


				tab DV1G, missing
				generate beats7=DV1G 
				replace beats7=. if DV1G==9 
				replace beats7=1 if DV1G==1 
				replace beats7=0 if DV1G==2 
				egen miss_group = group(beats7)
sort miss_group
				by miss_group: gen index = _n - 1
replace beats7 = mod(index, 2) if beats7 == 8
drop miss_group index

				label variable beats7 "hh secrets exposed"
				label define beat7 1"yes" 0"no"
				label values beats7 beat7

				tab beats1 if mysample==1,m
				tab beats2 if mysample==1,m
				tab beats3 if mysample==1,m
				tab beats4 if mysample==1,m
				tab beats5  if mysample==1,m
				tab beats6 if mysample==1,m
				tab beats7 if mysample==1,m


				**************
				//recode beating
				***************
				generate beating_justified=.
				replace beating_justified=1 if beats1==1
				replace beating_justified=2 if beats2==1
				replace beating_justified=3 if beats3==1
				replace beating_justified=4 if beats4==1
				replace beating_justified=5 if beats5==1
				replace beating_justified=6 if beats6==1
				replace beating_justified=7 if beats7==1
				tab beating_justified

				generate beating_justified_ever=0
				replace beating_justified_ever=1 if beats1==1 |beats2==1 |beats3==1  |beats4==1 |beats5==1 |beats6==1 |beats7==1

				label define beating_justified_ever 1"yes" 0"no" 
				label values beating_justified_ever beating_justified_ever
				tab beating_justified_ever,m


				**************
				//recode beating index  
				***************
				generate IPV_index=.
				replace IPV_index=(beats1+beats2+beats3+beats4+beats5+beats6+beats7)
				replace IPV_index=(beats1+beats2+beats3+beats4+beats5+beats6+beats7)/7
egen meanindex = rowmean(beats1 beats2 beats3 beats4 beats5 beats6 beats7)
//histogram meanindex, percent
pwcorr beats1 beats2 beats3 beats4 beats5 beats6 beats7
alpha beats1 beats2 beats3 beats4 beats5 beats6 beats7, item
				tab IPV_index if mysample==1
				tab IPV_index,m

				svy, subpop(mysample): mean IPV_index


g IPV_index_cl=IPV_index
g IPV_index_dist=IPV_index
g IPV_index_srt=IPV_index


//CLEAN DV1A DV1B DV1C DV1D DV1E DV1E DV1F DV1G
				tab DV1A, missing    		
				generate beats1dk=DV1A 
				replace beats1dk=. if DV1A==9 
				replace beats1dk=. if DV1A==8 
				replace beats1dk=1 if DV1A==1 
				replace beats1dk=0 if DV1A==2 
						label variable beats1dk "goes out w.o. telling"
				label define beats1dk 1"yes" 0"no"
				label values beats1dk beats1dk
				tab beats1dk,m
 
				tab DV1B, missing 
				generate beats2dk=DV1B 
				replace beats2dk=. if DV1B==9 
								replace beats2dk=. if DV1B==8
				replace beats2dk=1 if DV1B==1 
				replace beats2dk=0 if DV1B==2 
							label variable beats2dk "neglects children"
				label define beats2dk 1"yes" 0"no"
				label values beats2dk beats2dk
tab beats2dk,m

				tab DV1C, missing
				generate beats3dk=DV1C 
				replace beats3dk=. if DV1C==9 
							replace beats3dk=. if DV1C==8
				replace beats3dk=1 if DV1C==1 
				replace beats3dk=0 if DV1C==2 
				
				label variable beats3dk "argues w husband"
				label define beats3dk 1"yes" 0"no"
				label values beats3dk beats3dk
				tab beats3dk,m

				tab DV1D, missing 
				generate beats4dk=DV1D 
				replace beats4dk=. if DV1D==9 
								replace beats4dk=. if DV1D==8
				replace beats4dk=1 if DV1D==1 
				replace beats4dk=0 if DV1D==2 
			
				label variable beats4dk "refuse sex"
				label define beats4dk 1"yes" 0"no"
				label values beats4dk beats4dk


				tab DV1E, missing 
				generate beats5dk=DV1E 
				replace beats5dk=0 if DV1E==9 
								replace beats5dk=0 if DV1E==8
				replace beats5dk=1 if DV1E==1 
				replace beats5dk=0 if DV1E==2 
				
				label variable beats5dk "burns food"
				label define beats5dk 1"yes" 0"no"
				label values beats5dk beats5dk


				tab DV1F, missing 
				generate beats6dk=DV1F 
				replace beats6dk=. if DV1F==9 
						replace beats6dk=. if DV1F==8 
				replace beats6dk=1 if DV1F==1 
				replace beats6dk=0 if DV1F==2 


				label variable beats6dk "felt dirty"
				label define beats6dk 1"yes" 0"no"
				label values beats6dk beats6dk


				tab DV1G, missing
				generate beats7dk=DV1G 
				replace beats7dk=. if DV1G==9 
						replace beats7dk=. if DV1G==8
				replace beats7dk=1 if DV1G==1 
				replace beats7dk=0 if DV1G==2 
				
				label variable beats7dk "hh secrets exposed"
				label define beat7dk 1"yes" 0"no"
				label values beats7dk beat7dk

				tab beats1dk if mysample==1,m
				tab beats2dk if mysample==1,m
				tab beats3dk if mysample==1,m
				tab beats4dk if mysample==1,m
				tab beats5dk  if mysample==1,m
				tab beats6dk if mysample==1,m
				tab beats7dk if mysample==1,m


				**************
				//recode beating
				***************
				generate beating_just_dk=.
				replace beating_just_dk=1 if beats1dk==1
				replace beating_just_dk=2 if beats2dk==1
				replace beating_just_dk=3 if beats3dk==1
				replace beating_just_dk=4 if beats4dk==1
				replace beating_just_dk=5 if beats5dk==1
				replace beating_just_dk=6 if beats6dk==1
				replace beating_just_dk=7 if beats7dk==1
				tab beating_just_dk

				generate beating_just_ev_dk=0
				replace beating_just_ev_dk=1 if beats1dk==1 |beats2dk==1 |beats3dk==1  |beats4dk==1 |beats5dk==1 |beats6dk==1 |beats7dk==1

				label define beating_just_ev_dk 1"yes" 0"no" 
				label values beating_just_ev_dk beating_just_ev_dk
				tab beating_just_ev_dk,m


				**************
				//recode beating index  *correct!
				***************
				generate IPV_indexdk=.
				replace IPV_indexdk=(beats1dk+beats2dk+beats3dk+beats4dk+beats5dk+beats6dk+beats7dk)
				replace IPV_indexdk=(beats1dk+beats2dk+beats3dk+beats4dk+beats5dk+beats6dk+beats7dk)/7
//egen meanindex = rowmean(beats1dk beats2dk beats3dk beats4dk beats5dk beats6dk beats7dk)
//histogram meanindex, percent
pwcorr beats1 beats2 beats3 beats4 beats5 beats6 beats7
alpha beats1 beats2 beats3 beats4 beats5 beats6 beats7, item
				tab IPV_indexdk if mysample==1
				tab IPV_indexdk,m

				svy, subpop(mysample): mean IPV_indexdk
	
				
******************************
// MODERNIZATION CONTROLS 
******************************

************************
//urban
*************************
tab HH6
lab list HH6
	generate urban=.
replace urban=1 if HH6==1
replace urban=0 if HH6==2
tab urban

svy, subpop(mysample): mean urban


//wealth  

//wealth index for the full sample weighted is normal./ for the kurdish sample it is distorted
svy: tab windex
svy, subpop(mysample): tab windex5
	//it is not about Kerkuk's inclusion in gerenal for kurdish sample wealth index is distorted
	gen kurdishsample=.
	replace kurdishsample=1 if Survey==2
	 replace kurdishsample=0 if Survey==1
	svy, subpop(kurdishsample): tab windex5


tab windex5
summ wscore,detail
svy: tab windex5 if mysample==1

svy: mean wscore


summ wscore if mysample==1

//education 		


codebook welevel,detail
tabulate welevel, missing 
generate educ=welevel 
tab educ, missing 
replace educ=. if welevel==9
replace educ=1 if welevel==4 //no formal education coded as none
label variable educ "women's education'"
tabulate educ, missing
label values educ welevel
tab educ,m
svy, subpop(mysample): tab educ


codebook ED4A,detail
tabulate ED4A, missing 
generate educ1=ED4A 
label define educ1 1"None" 2"primary" 3"Intermediate+" 
label values educ1 educ1
tab educ1, missing 

replace educ1=1 if ED4A==. |ED4A==0 |ED4A==7 |ED4A==8 //no formal education coded as none
replace educ1=2 if ED4A==1
replace educ1=3 if ED4A==2 |ED4A==3 |ED4A==4 |ED4A==5 |ED4A==6
label variable educ1 "women's education'"
tabulate educ1, missing
svy, subpop(mysample): tab educ1

tab educ1 if mysample==1
// should i report descriptives with the svy or without?

***dummy out educ1
  g educ_none=0
 replace educ_none=1 if educ1==1
 
 g educ_pri=0
 replace educ_pri=1 if educ1==2
	
  g educ_second=0
 replace educ_second=1 if educ1==3
 
tab educ_none
tab educ_pri
tab educ_second

************************************************************************************
// SCT : COMMUNITY VARIABLES
************************************************************************************

****************
// WSCORE community
****************
// community wealth score
summ wscore 
by HH1, sort: egen comm_wscore_cluster = mean(wscore)
replace comm_wscore_cluster=comm_wscore_cluster*100

//mean wealth for region for district
by HH72R, sort:egen comm_wscore_district = mean(wscore)
summ comm_wscore_district


//mean wscore for stratum
by stratum, sort:egen comm_wscore_stratum = mean(wscore)
summ comm_wscore_stratum


*****************************
//WINDEX 			*correct
*****************************
// community wealth index

tab windex5 
by HH1, sort: egen comm_windex_cluster = mean(windex5)
tab comm_windex_cluster


//mean wealth for region for district
by HH72R, sort:egen comm_windex_district = mean(windex5)
tab comm_windex_district

			//control
			sum windex5 if HH72R==91   //mean=3.21
			tab comm_windex_district if HH72R ==91


//mean wealth for region for stratum
by stratum, sort:egen comm_windex_stratum = mean(windex5)
tab comm_windex_stratum


			
**************************
//education COMMUNITY      
*************************
//cluster education
by HH1, sort: egen comm_educ_0_cluster = mean(educ1==1) 
replace comm_educ_0_cluster=comm_educ_0_cluster*100

by HH1, sort: egen comm_educ_1_cluster = mean(educ1==2) 
replace comm_educ_1_cluster=comm_educ_1_cluster*100

by HH1, sort: egen comm_educ_2_cluster = mean(educ1==3) 
replace comm_educ_2_cluster=comm_educ_2_cluster*100

summ comm_educ_0_cluster
tab comm_educ_0_cluster

summ comm_educ_1_cluster
tab comm_educ_1_cluster

summ comm_educ_2_cluster
tab comm_educ_2_cluster

// community welevel district

tab educ1,m

by HH72R, sort: egen comm_educ_0_district = mean(educ_none) 
//replace educ_none_district=educ_none_district*100
summ educ_none if HH72R ==91   //mean .125
tab comm_educ_0_district if HH72R ==91   //mean .125

by HH72R, sort: egen comm_educ_1_district = mean(educ_pri) 
//replace educ_primary_district=educ_primary_district*100
summ educ_pri if HH72R ==91   //mean .411
tab comm_educ_1_district if HH72R ==91   //mean .411

by HH72R, sort: egen comm_educ_2_district = mean(educ_second) 
//replace educ_secondary_district=educ_secondary_district*100
summ educ_second if HH72R ==91   //mean .463
tab comm_educ_2_district if HH72R ==91   //mean .463

summ comm_educ_0_district
tab comm_educ_0_district

summ comm_educ_1_district, detail
tab comm_educ_1_district 

summ comm_educ_2_district
tab comm_educ_2_district


by HH72R, sort: egen comm_educ_1and2 = mean(educ1>1) 
replace comm_educ_1and2=comm_educ_1and2*100

//stratum

//mean educ pri for stratum
by stratum, sort:egen comm_educ_0_stratum = mean(educ_none)
summ comm_educ_0_stratum

by stratum, sort:egen comm_educ_1_stratum = mean(educ_pri)
summ comm_educ_1_stratum

//mean educ sec for stratum
by stratum, sort:egen comm_educ_2_stratum = mean(educ_second)
summ comm_educ_2_stratum


************************************************************************************
// FC PREVALANCE COMMUNITY 				
************************************************************************************

**************
//CLEAN FG3 = respondent fc
***************
codebook FG3,detail
tabulate FG3, missing 
lab list FG3
generate respondent_FG=.
tab respondent_FG, missing 
replace respondent_FG=. if FG3==9  //MISSING
replace respondent_FG=1 if FG3==1  //no response coded as no
replace respondent_FG=0 if FG3==2  

label variable respondent_FG "respondent cut"
tabulate respondent_FG, missing
label values respondent_FG FG3
tab respondent_FG,m


// community respondent FG

//percent women cut for region for clusters
by HH1, sort:egen comm_FG_cluster = mean(respondent_FG)
replace comm_FG_cluster=comm_FG_cluster*100
//histogram percent_FG_cluster if mysample1==1

//percent women cut for region for district
by HH72R, sort:egen comm_FG_district = mean(respondent_FG)
//histogram percent_FG_district if mysample1==1
replace comm_FG_district=comm_FG_district*100
summ comm_FG_district
svy, subpop(mysample): mean comm_FG_district

	//control
			summ respondent_FG if HH72R==91   //mean=0.0046
			tab comm_FG_district if HH72R ==91


//mean FGM for stratum
by stratum, sort:egen comm_FG_stratum = mean(respondent_FG)
summ comm_FG_stratum


************************************************************************************
// IPV COMMUNITY 
************************************************************************************

//mean IPV for region for cluster
by HH1, sort:egen comm_IPV_index_cluster = mean(IPV_index)
replace comm_IPV_index_cluster=comm_IPV_index_cluster*100
//histogram IPV_index_cluster if mysample1==1
svy, subpop(mysample): mean comm_IPV_index_cluster


			
//mean IPV for region for district
by HH72R, sort:egen comm_IPV_index_district = mean(IPV_index)
//histogram IPV_index_district if mysample1==1
replace comm_IPV_index_district=comm_IPV_index_district*100
tab comm_IPV_index_district
bysort HH72R: summarize IPV_index 
	//control
			summ IPV_index if HH72R==91   //mean=0.447
			tab comm_IPV_index_district if HH72R ==91

//mean IPV for stratum
by stratum, sort:egen comm_IPV_index_stratum = mean(IPV_index)
summ comm_IPV_index_stratum
			
//number of daughters total

tabulate number_daug, missing
svy, subpop(mysample): mean number_daug
 mean number_daug

************************************************************************************
// 				DESCRIPTIVES
************************************************************************************
	svyset PSU [pw = denorm_wmweight], strata(stratum) singleunit(scaled) 
svydescribe  if e(sample) 
 svy: mean kurdistan  

//outcome
tab any_daug_fgm if mysample==1
mean any_daug_fgm [pw=denorm_wmweight] if mysample==1
mean any_daug_fgm if mysample==1
 svy: mean any_daug_fgm   if mysample==1
svy, subpop(mysample): mean any_daug_fgm
svy: tab any_daug_fgm if mysample==1

*feminist
svy: mean age_at_1st_marriage if mysample==1
 mean age_at_1st_marriage if mysample==1

svy:tab spousal_age_4years_dist if mysample==1
tab spousal_age_4years_dist if mysample==1
svy: tab spousal_age_younger_dist if mysample==1
 tab spousal_age_younger_dist if mysample==1
svy: tab spousal_age_mid_dist if mysample==1
 tab spousal_age_mid_dist if mysample==1
svy: tab spousal_age_older_dist if mysample==1
 tab spousal_age_older_dist if mysample==1


tab husband_related,m
tab husband_related if mysample==1

summ number_sons_alive,detail
summ ratio_son_daug,detail
summ prop_son_alive,detail

tab polygyny if mysample==1

summ number_daug if mysample==1
summ current_spousal_age_diffr,detail
tab spousal_age_diffr_4y

svy: tab agew_5 if mysample==1
tab agew_5 if mysample==1

*modernization
svy:tab urban if mysample==1
summ wscore if mysample==1
tab windex5 if mysample==1
tab educ1 if mysample==1
tab welevel,m // non formal educ is coded to none educ


*community

tab respondent_FG if mysample==1
 by HH71, sort:tab respondent_FG,m
  by HH72R, sort:tab respondent_FG,m

    by HH72R, sort:tab respondent_FG,m

  
tab IPV_index if mysample==1
 by HH71, sort:tab IPV_index if mysample==1
  by HH72R, sort:tab IPV_index,m
 
 ****DESCRIPTIVES 
 
 svy, subpop(mysample): tab any_daug_fgm
  svy: tab any_daug_fgm if mysample==1
 tab any_daug_fgm if mysample==1
 
 svy, subpop(mysample): mean age_at_1st_marriage
 summ age_at_1st_marriage if mysample==1
 svy, subpop(mysample): tab spousal_age_diffr_4y
 svy, subpop(mysample): tab husband_related
 svy, subpop(mysample): tab polygyny
 svy, subpop(mysample): tab urban
 svy, subpop(mysample): tab windex5
 svy, subpop(mysample): tab educ1
 svy, subpop(mysample): tab respondent_FG
 svy, subpop(mysample): tab IPV_index
 
 svy, subpop(mysample): mean number_daug
 svy, subpop(mysample): tab agew_5
 summ agew if mysample==1
 
 
*****************CENTERING****************************
 *centered at cluster district and st levels for expo analysis

//centering at cluster
xtcenter age_at_1st_marriage_cl, i(HH72R) replace
xtcenter spousal_age_younger_cl, i(HH72R) interaction  replace
xtcenter spousal_age_4years_cl, i(HH72R) interaction  replace
xtcenter spousal_age_mid_cl, i(HH72R) interaction  replace
xtcenter spousal_age_older_cl, i(HH72R) interaction  replace
xtcenter husband_related_cl , i(HH72R) interaction  replace
xtcenter polygyny_cl , i(HH72R) interaction  replace
xtcenter IPV_index_cl , i(HH72R)   replace

//centering at sratum
xtcenter age_at_1st_marriage_srt, i(HH72R) replace
xtcenter spousal_age_younger_srt, i(HH72R) interaction  replace
xtcenter spousal_age_4years_srt, i(HH72R) interaction  replace
xtcenter spousal_age_mid_srt, i(HH72R) interaction  replace
xtcenter spousal_age_older_srt, i(HH72R) interaction  replace
xtcenter husband_related_srt , i(HH72R) interaction  replace
xtcenter polygyny_srt , i(HH72R) interaction  replace
xtcenter IPV_index_srt , i(HH72R)   replace

//centering at district
xtcenter age_at_1st_marriage_dist, i(HH72R) replace
xtcenter spousal_age_younger_dist, i(HH72R) interaction  replace
xtcenter spousal_age_4years_dist, i(HH72R) interaction  replace
xtcenter spousal_age_mid_dist, i(HH72R) interaction  replace
xtcenter spousal_age_older_dist, i(HH72R) interaction  replace
xtcenter husband_related_dist , i(HH72R) interaction  replace
xtcenter polygyny_dist , i(HH72R) interaction  replace
xtcenter IPV_index_dist , i(HH72R)   replace


xtcenter spousal_age_diffr_young10, i(HH72R) interaction  replace
xtcenter spousal_age_diffr_4years10, i(HH72R) interaction  replace
xtcenter spousal_age_diffr_mid10, i(HH72R) interaction  replace
xtcenter spousal_age_diffr_older10 , i(HH72R) interaction  replace
xtcenter IPV_indexdk , i(HH72R)   replace


///global feminist models
global feminist_dist age_at_1st_marriage_dist_zti spousal_age_younger_dist_zti spousal_age_older_dist_zti     spousal_age_mid_dist_zti  husband_related_dist_zti  polygyny_dist_zti  IPV_index_dist_zti  wscore  i.educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug i.agew_10 
global feminist_cl age_at_1st_marriage_cl_zti spousal_age_younger_cl_zti spousal_age_older_cl_zti     spousal_age_mid_cl_zti  husband_related_cl_zti  polygyny_cl_zti  IPV_index_cl_zti  wscore  i.educ1 comm_FG_district comm_IPV_index_cluster  comm_wscore_cluster comm_educ_1_cluster comm_educ_2_cluster number_daug i.agew_10 
global feminist_srt age_at_1st_marriage_srt_zti spousal_age_younger_srt_zti spousal_age_older_srt_zti     spousal_age_mid_srt_zti  husband_related_srt_zti  polygyny_srt_zti  IPV_index_srt_zti  wscore  i.educ1 comm_FG_stratum comm_IPV_index_stratum  comm_wscore_stratum comm_educ_1_stratum comm_educ_2_stratum number_daug i.agew_10 

///global feminist model
global feminist_dist age_at_1st_marriage_dist_zti spousal_age_younger_dist_zti spousal_age_older_dist_zti     spousal_age_mid_dist_zti  husband_related_dist_zti  polygyny_dist_zti  IPV_index_dist_zti urban wscore  i.educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug i.agew_5
global feminist_cl age_at_1st_marriage_cl_zti spousal_age_younger_cl_zti spousal_age_older_cl_zti     spousal_age_mid_cl_zti  husband_related_cl_zti  polygyny_cl_zti  IPV_index_cl_zti urban wscore  i.educ1 comm_FG_district comm_IPV_index_cluster  comm_wscore_cluster comm_educ_1_cluster comm_educ_2_cluster number_daug i.agew_5 
global feminist_srt age_at_1st_marriage_srt_zti spousal_age_younger_srt_zti spousal_age_older_srt_zti     spousal_age_mid_srt_zti  husband_related_srt_zti  polygyny_srt_zti  IPV_index_srt_zti urban  wscore  i.educ1 comm_FG_stratum comm_IPV_index_stratum  comm_wscore_stratum comm_educ_1_stratum comm_educ_2_stratum number_daug i.agew_5 


************************************************************************************
//diagnostics
************************************************************************************
//drop if mysample==0
//cross-tabulations between the binary outcome and all the categorical predictors

tab any_daug_fgm spousal_age_diffr_4y if mysample==1, row col chi 
tab any_daug_fgm husband_related if mysample==1, row col chi
tab any_daug_fgm urban if mysample==1, row col chi
tab any_daug_fgm educ if mysample==1, row col chi
tab any_daug_fgm polygyny if mysample==1, row col chi

//multicollinearity

pwcorr    any_daug_fgm  age_at_1st_marriage_dist spousal_age_younger_dist spousal_age_older_dist     spousal_age_mid_dist  husband_related_dist  polygyny_dist  IPV_index_dist urban wscore  educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug agew_5

//condition indeces
coldiag2 any_daug_fgm  age_at_1st_marriage spousal_age_diffr_4y  husband_related number_sons_alive polygyny  number_daug comm_IPV_index_district urban wscore educ comm_wscore_district comm_FG_district IPV_index   


//outliers & influence 

egen id2 = seq(), from(1)
melogit any_daug_fgm $feminist_dist  if mysample==1 [pw=aw1]|| HH72R:,
predict deviance_dist, deviance
scatter deviance_dist id if ( deviance_dist>2  | deviance_dist<-2) & mysample ==1
tab id if (deviance_dist>2  | deviance_dist<-2) & mysample ==1

melogit any_daug_fgm $feminist_cl  if mysample==1 [pw=aw1]|| HH1:,
predict deviance_cl, deviance
scatter deviance_cl id if ( deviance_cl>2  | deviance_cl<-2) & mysample ==1
tab id if (deviance_cl>2  | deviance_cl<-2) & mysample ==1

melogit any_daug_fgm $feminist_srt  if mysample==1 [pw=wom_weight]|| stratum:,
predict deviance_srt, deviance
scatter deviance_srt id if ( deviance_srt>2  | deviance_srt<-2) & mysample ==1
tab id if (deviance_srt>2  | deviance_srt<-2) & mysample ==1

//decision for final model and weights
  *unweighted
melogit any_daug_fgm $feminist_dist if mysample==1 || HH72R:,
melogit any_daug_fgm $feminist_cl if mysample==1 || HH1:,
melogit any_daug_fgm $feminist_srt if mysample==1 || stratum:,

 *denormalized weighted

melogit any_daug_fgm $feminist_dist if mysample==1 [pw=denorm_wmweight]|| HH72R:,
melogit any_daug_fgm $feminist_cl if mysample==1 [pw=denorm_wmweight]|| HH1:,
melogit any_daug_fgm $feminist_srt if mysample==1 [pw=denorm_wmweight]|| stratum:,

 *carla(2009) weights

melogit any_daug_fgm $feminist_dist if mysample==1 [pw=bw1]|| HH72R:,
melogit any_daug_fgm $feminist_cl if mysample==1 [pw=aw1]|| HH1:,
melogit any_daug_fgm $feminist_srt if mysample==1 [pw=aw1]|| stratum:,

**FINAL MODEL IN THE PAPER
melogit any_daug_fgm $feminist_dist if mysample==1 || HH72R:,


//final model without outliers
melogit any_daug_fgm $feminist_dist  if mysample==1 || HH72R:,
predict deviance_final, deviance
scatter deviance_final id if ( deviance_final>2  | deviance_final<-2) & mysample ==1
//final model without outliers
melogit any_daug_fgm $feminist_dist  if (mysample==1 & (deviance_final<2  & deviance_final>-2))  || HH72R:,


//women who stopped_fc
tab n_circ_daug,m
tab n_circ_daug FG10A,m
tab FG10A
tab n_circ_daug number_daug,m

describe n_circ_daug 
describe number_daug 

tab  number_daug
tab number_daug FG10A 

gen n_daug_circ_minus = 0 - n_circ_daug
tab n_daug_circ_minus ,m 


egen n_noncirc= rowtotal(number_daug n_daug_circ_minus)
tab n_noncirc 
tab FG10A if n_noncirc==-1
tab number_daug if number_daug < n_circ_daug,m

gen stopped_fc=.
replace stopped_fc=1 if n_circ_daug<number_daug
replace stopped_fc=0 if n_circ_daug==0
replace stopped_fc=2 if n_circ_daug>=number_daug
tab  stopped_fc if mysample==1

gen circumsized_all=.
replace circumsized_all=1 if n_circ_daug==number_daug
replace circumsized_all=1 if n_circ_daug>number_daug
replace circumsized_all=0 if n_circ_daug==0
tab  circumsized_all if mysample==1

* model with women who stopped_fc
meologit stopped_fc $feminist_dist  if mysample==1  || HH72R:,
melogit circumsized_all $feminist_dist  if mysample==1  || HH72R:,

global feminist_dist age_at_1st_marriage_dist_zti spousal_age_younger_dist_zti spousal_age_older_dist_zti     spousal_age_mid_dist_zti  husband_related_dist_zti  polygyny_dist_zti  IPV_index_dist_zti urban wscore  i.educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug i.agew_10 


 ********************** nested models ************************************* 

//baseline model
melogit any_daug_fgm  if mysample==1 || HH72R: ,or
  estat ic 
estat icc

//modernization model 
  melogit any_daug_fgm  urban wscore i.educ1 number_daug i.agew_5  if mysample==1|| HH72R: 
  estat ic 
estat icc

// M+ sct
  melogit any_daug_fgm urban wscore  i.educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug i.agew_5  if mysample==1 || HH72R: 

//feminist model 

  melogit any_daug_fgm age_at_1st_marriage_dist_zti spousal_age_younger_dist_zti spousal_age_older_dist_zti     spousal_age_mid_dist_zti  husband_related_dist_zti  polygyny_dist_zti  IPV_index_dist_zti urban wscore  i.educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug i.agew_5   if mysample==1   || HH72R:
  melogit any_daug_fgm age_at_1st_marriage_dist_zti spousal_age_younger_dist_zti spousal_age_older_dist_zti     spousal_age_mid_dist_zti  husband_related_dist_zti  polygyny_dist_zti  IPV_index_dist_zti urban wscore  i.educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug i.agew_5   if mysample==1   || HH72R: || HH1: || HH2:
  estat icc
  
egen id_hh = group(HH1 HH2)
   logit any_daug_fgm age_at_1st_marriage_dist_zti spousal_age_younger_dist_zti spousal_age_older_dist_zti     spousal_age_mid_dist_zti  husband_related_dist_zti  polygyny_dist_zti  IPV_index_dist_zti urban wscore  i.educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug i.agew_5   if mysample==1,  vce(cluster id_hh)
  melogit any_daug_fgm age_at_1st_marriage_dist_zti spousal_age_younger_dist_zti spousal_age_older_dist_zti     spousal_age_mid_dist_zti  husband_related_dist_zti  polygyny_dist_zti  IPV_index_dist_zti urban wscore  i.educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug i.agew_5 HH13  if mysample==1   || HH72R: 

//sub population

generate mysample2=0
replace mysample2=1 if (currently_married==1 & number_daug!=0 & kurdistan==1 & age_partner!=. ) 
tab mysample2, missing 
label variable mysample2 "currently married, and has daughters, kurdistan"
tabulate mysample2 any_daug_fgm, missing

g no_fg_954=.
replace no_fg_954=0 if (mysample2==1&any_daug_fgm==.)
replace no_fg_954=1 if FG10A>=1 & FG10A!=.
replace no_fg_954=0 if FG10A==0 
tab no_fg_954 if mysample2==1

melogit no_fg_954 age_at_1st_marriage_dist_zti spousal_age_younger_dist_zti spousal_age_older_dist_zti     spousal_age_mid_dist_zti  husband_related_dist_zti  polygyny_dist_zti  IPV_index_dist_zti urban wscore  i.educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug i.agew_10   if mysample==1 || HH72R: 


//stopped and all circumsized
meologit stopped_fc age_at_1st_marriage_dist_zti spousal_age_younger_dist_zti spousal_age_older_dist_zti     spousal_age_mid_dist_zti  husband_related_dist_zti  polygyny_dist_zti  IPV_index_dist_zti urban wscore  i.educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug i.agew_10   if mysample==1 || HH72R: 
melogit circumsized_all age_at_1st_marriage_dist_zti spousal_age_younger_dist_zti spousal_age_older_dist_zti     spousal_age_mid_dist_zti  husband_related_dist_zti  polygyny_dist_zti  IPV_index_dist_zti urban wscore  i.educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug i.agew_10   if mysample==1 || HH72R: 


// sp age diff 10
melogit any_daug_fgm age_at_1st_marriage_dist_zti spousal_age_diffr_young10_zti spousal_age_diffr_older10_zti     spousal_age_diffr_mid10_zti  husband_related_dist_zti  polygyny_dist_zti  IPV_index_dist_zti urban wscore  i.educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug i.agew_10   if mysample==1 || HH72R: 
		
//ipv index dk dropped
melogit any_daug_fgm age_at_1st_marriage_dist_zti spousal_age_younger_dist_zti spousal_age_older_dist_zti     spousal_age_mid_dist_zti  husband_related_dist_zti  polygyny_dist_zti  IPV_indexdk_zti urban wscore  i.educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug i.agew_10   if mysample==1 || HH72R: 

/// outcome 954 missing are coded alternating

g sampletr=0
replace sampletr=1 if currently_married==1 & kurdistan==1 & number_daug!=0 & age_partner !=.
tab sampletr,m

drop if sampletr==0
tab any_daug_fgm,m

	tab any_daug_fgm, missing  
				generate outcome=any_daug_fgm 
				replace outcome=1 if any_daug_fgm==1 
				replace outcome=0 if any_daug_fgm==0 
			
				tab outcome,m //1237 DK
				
				list id if outcome==8
				//2048 ==1
				 // 6713 ==1
						// 53675 ==8
						// 54108 ==8
						//55168==0
						//55124 ==0
						
				egen miss_group = group(outcome)
				sort miss_group
				tab miss_group 
				list miss_group id in -10/l
				
				 tab miss_group if id==53675
				  tab miss_group if id==54108
				  
				 tab miss_group if id==2048
				  tab miss_group if id==6713
				  
				by miss_group: gen index = _n - 1

				list index if id==5648
				list index if id==26881
				list index if id==42927
				list index if id==38037

/* 
			mis group	ID
55190. |        3    5648 | 1232
55191. |        3   26881 | 1233
55192. |        3   42927 | 1234
55193. |        3   38037 | 1235
55194. |        3   28230 

*/

replace outcome = mod(index, 2) if outcome == .

drop miss_group index

				label variable outcome "goes out w.o. telling"
				label define outcome 1"yes" 0"no"
				label values outcome outcome
				tab outcome,m

melogit outcome age_at_1st_marriage_dist_zti spousal_age_younger_dist_zti spousal_age_older_dist_zti     spousal_age_mid_dist_zti  husband_related_dist_zti  polygyny_dist_zti  IPV_index_dist_zti urban wscore  i.educ1 comm_FG_district comm_IPV_index_district  comm_wscore_district comm_educ_1_district comm_educ_2_district number_daug i.agew_10   if mysample==1 || HH72R: 


**Final model presented
melogit any_daug_fgm $feminist_dist if mysample==1 || HH72R:,