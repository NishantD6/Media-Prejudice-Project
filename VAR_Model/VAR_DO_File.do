clear all
use "/Users/nishant/Desktop/1-RESEARCH/Data&Code/Z-Final DataSets/VARData.dta", clear // import the file here

 //You cannot xtset a string variable. We must make a numeric variable from our string variable and xtset that. One alternative is

encode make, gen(mk)


gen date1 = ym(Year, month)

gen lnsales = log(sales)
gen lnpercep = log(percep)
//gen lncomp = log(CompCont)
gen lnad = log(ad)

gen dlnsales = lnsales - lnsales[_n-1]

gen dlnpercep = lnpercep - lnpercep[_n-1]

gen dlnad = lnad - lnad[_n-1]

gen dlncomp = lncomp - lncomp[_n-1]

gen ddlnsales = dlnsales - dlnsales[_n-1]

gen ddlnpercep = dlnpercep - dlnpercep[_n-1]

gen dddlnpercep = ddlnpercep - ddlnpercep[_n-1]

gen ddlnad = dlnad - dlnad[_n-1]

gen dddlnad = ddlnad - ddlnad[_n-1]



xtset make date, monthly

keep if make == 6

gen dlnadY = lnad - lnad[_n-12]
gen ddlnadY = lnad - lnad[_n-1]




//AIC criterion to determine nmber of lags





//Dicky fuller Null: The series is not stationary. Alternative is that it is stationary.
varsoc lnsales 
dfuller lnsales, trend lag(1) 
//

varsoc lnsales 
dfuller lnsales, trend lag(1)


varsoc lnpercep

dfuller lnpercep, trend lag(2)
//

varsoc dlnpercep
dfuller dlnpercep, trend lag(1)

varsoc ddlnpercep
dfuller ddlnpercep, trend lag(2)

varsoc dddlnpercep
dfuller dddlnpercep, trend lag(2)

varsoc ddlnadY
dfuller ddlnadY, trend lag(2)
//

varsoc lnad
dfuller dlnad, trend lag(3)

varsoc ddlnad
dfuller ddlnad, trend lag(3)

varsoc ddlnad
dfuller ddlnad, trend lag(3)

varsoc dddlnad
dfuller dddlnad, trend lag(3)


varsoc lncomp
dfuller lncomp, trend lag(3)

varsoc dlncomp
dfuller dlncomp, trend lag(3)

//CONTROL FOR SEASONALITY

varsoc  lnpercep lnadY lnsales
// DICKEN & HANSENS - Sustained spending and response 


//Cointigration Test, null is that their is no cointigration.

xtcointtest pedroni lnpercep lnad lnsales

// Running the VAR Model
varbasic  lnsales lnad  dlnpercep, lags(1)
// Less than 0.05 means independent granger causes dependent. More means we do not know if it granger causes.
vargranger

varstable

 irf table irf, impulse(lnpercep) response(lnsales)
 
varlmar
varnorm, jbera
varstable


vec dlnpercep ddlnadY lnsales

//Diagnostic tests
veclmar //autocorrelation test
vecnorm, jbera //normality test - the null is that the errors are not normally distributied
vecstable // stability test

irf create vec, set(vecintro, replace) step(24)

irf graph oirf, impulse(dlnpercep) response(lnsales) yline(0)

irf create irf, impulse(lnpercep) response(lnsales)



irf table irf, impulse(lnad) response(lnsales)
irf table irf, impulse(lnpercep) response(lnsales)
irf table irf, impulse(adspend) response(percep)



// AB GMM Method

xtabond lsales lpercep lnad, vce(robust) lags(2)
estat abond

xtreg lnsales lnad lnpercep, re

xtreg lnsales lnad lnpercep, fe


clear all
use "/Users/nishant/Desktop/1-RESEARCH/Data&Code/Z-Final DataSets/YouGov Data/YouGov/000_Master_us_sector_7_.dta", clear

gsem (buzz_score impression_score quality_score value_score satisfaction_score recommend_score consider_score likelybuy_score<- Percep)
predict percep
