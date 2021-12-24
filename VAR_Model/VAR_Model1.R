# R code : Does Media Prejuidce affect Brands? 
# "Emperical Demonstration of VAR model"
# 7th July 2020
# By Nishant Das
# MRM Program
# IESE Business School

# Import Packages
library(vars)
library(dplyr)
library(tseries)
library(forecast)
library(astsa)
library(zoo)

# Import Data File
data <- read.csv('/Users/nishant/Desktop/1-RESEARCH/Data&Code/Z-Final DataSets/VAR_data.csv')
# Clean Data File & Select Brand
data <-data%>%
  arrange(date)%>%
  filter(make=='infiniti')
  
# Make Data into Panel Data
data <- ts(data, start= c(2015,1 ), frequency = 12)
#Obtain Each Variable
sales    = (data[,'sales'])%>%log()
percep   = (data[,'percep'])%>%log()
ad       = (data[,'ad']+1)%>%log()
# Augmented Dickey Fuller Test
adf.test(sales)
adf.test(percep)
adf.test(ad)
# Fix for Stationarity 
sales    =  sales%>%diff(lag=1)
percep   =  percep%>%diff(lag=1)
ad       =  ad%>%diff(lag=1)

sales =  sales[-1]
percep  = percep[-1]
ad      = ad[-1]

## AIC 
a = data.frame(sales, percep, ad)
VARselect(a)

var= VAR(a, p=9)

irf<-irf(var, impulse = NULL, response = NULL, n.ahead = 10,
         ortho = FALSE, cumulative = FALSE, boot = TRUE, ci = 0.95,
         runs = 200, seed = NULL)

# Obtaining the Data
percep_to_sales = irf[[1]][[2]][,1]
percep_upper = irf$Upper[[2]][,1]
percep_lower = irf$Lower[[2]][,1]
x <- data.frame(percep_to_sales,percep_upper,percep_lower)
View(x)
plot(irf)

# Obtaining the Data
ad_to_sales = irf[[1]][[3]][,1]
ad_upper = irf$Upper[[3]][,1]
ad_lower = irf$Lower[[3]][,1]
y <- data.frame(ad_to_sales,ad_upper,ad_lower)
View(y)



saveRDS(irf, 'acura.RData')
saveRDS(irf, 'audi.RData')
saveRDS(irf, 'bmw.RData')
saveRDS(irf, 'buick.RData')
saveRDS(irf, 'cadillac.RData')
saveRDS(irf, 'chevrolet1.RData')
saveRDS(irf, 'chrysler.RData')
saveRDS(irf, 'dodge.RData')
saveRDS(irf, 'fiat.RData')
saveRDS(irf, 'ford.RData')
saveRDS(irf, 'gmc.RData')
saveRDS(irf, 'honda.RData')
saveRDS(irf, 'hyundai.RData')
saveRDS(irf, 'infiniti.RData')
saveRDS(irf, 'jaguar.RData')
saveRDS(irf, 'kia.RData')
saveRDS(irf, 'lincoln.RData')
saveRDS(irf, 'mazda.RData')
saveRDS(irf, 'mercedes.RData')
saveRDS(irf, 'mitshubishi.RData')
saveRDS(irf, 'nissan.RData')
saveRDS(irf, 'proche.RData')
saveRDS(irf, 'subaru.RData')
saveRDS(irf, 'tesla.RData')
saveRDS(irf, 'toyota1.RData')
saveRDS(irf, 'volkswagen1.RData')
saveRDS(irf, 'volvo.RData')


saveRDS(irf, 'acura.RData')
saveRDS(irf, 'audi.RData')
saveRDS(irf, 'bmw.RData')
saveRDS(irf, 'buick.RData')
saveRDS(irf, 'cadillac.RData')
saveRDS(irf, 'chevrolet.RData')
saveRDS(irf, 'chrysler.RData')
saveRDS(irf, 'dodge.RData')
saveRDS(irf, 'fiat.RData')
saveRDS(irf, 'ford.RData')
saveRDS(irf, 'gmc.RData')
saveRDS(irf, 'honda.RData')
saveRDS(irf, 'hyundai.RData')
saveRDS(irf, 'infiniti.RData')
saveRDS(irf, 'jaguar.RData')
saveRDS(irf, 'kia.RData')
saveRDS(irf, 'lincoln.RData')
saveRDS(irf, 'mazda.RData')
saveRDS(irf, 'mercedes.RData')
saveRDS(irf, 'mitshubishi.RData')
saveRDS(irf, 'nissan.RData')
saveRDS(irf, 'proche.RData')
saveRDS(irf, 'subaru.RData')
saveRDS(irf, 'tesla.RData')
saveRDS(irf, 'toyota.RData')
saveRDS(irf, 'volvo.RData')

irf =readRDS('fiat.RData')







acura = data.frame(percep_to_sales,upper,lower)
audi = data.frame(percep_to_sales,upper,lower)
bmw= data.frame(percep_to_sales,upper,lower)
buick = data.frame(percep_to_sales,upper,lower)
cadillac = data.frame(percep_to_sales,upper,lower)
chevrolet = data.frame(percep_to_sales,upper,lower)
dodge	 = data.frame(percep_to_sales,upper,lower)
ford = data.frame(percep_to_sales,upper,lower )
#honda=data.frame(percep_to_sales,upper,lower )
hyundai = data.frame(percep_to_sales,upper,lower )
infiniti =data.frame(percep_to_sales,upper,lower)
jaguar = data.frame(percep_to_sales,upper,lower)
kia = data.frame(percep_to_sales,upper,lower)
lexus = data.frame(percep_to_sales,upper,lower)
lincoln =data.frame(percep_to_sales,upper,lower)
mazda = data.frame(percep_to_sales,upper,lower)
mercedes = data.frame(percep_to_sales,upper,lower)
#mitsubishi = data.frame(percep_to_sales,upper,lower)
nissan = data.frame(percep_to_sales,upper,lower)
porsche=data.frame(percep_to_sales,upper,lower)
subaru=data.frame(percep_to_sales,upper,lower)
tesla=data.frame(percep_to_sales,upper,lower)
toyota=data.frame(percep_to_sales,upper,lower)
volkswagen=data.frame(percep_to_sales,upper,lower)
volvo= data.frame(percep_to_sales,upper,lower)


write.csv(irf_df, "irf2.csv")


irf_df = rbind(acura, audi, bmw, buick, cadillac, chevrolet, dodge, ford, hyundai, infiniti, 
           jaguar, kia, lexus, lincoln, mazda, mercedes, mitsubishi, nissan, porsche, subaru, toyota, volkswagen, volvo)


# Obtaining the Data
ad_to_sales = irf[[1]][[3]][,1]
ad_upper = irf$Upper[[3]][,1]
ad_lower = irf$Lower[[3]][,1]

data.frame(ad_to_sales,ad_upper,ad_lower)

aacura = data.frame(ad_to_sales,ad_upper,ad_lower)
aaudi = data.frame(ad_to_sales,ad_upper,ad_lower)
abmw= data.frame(ad_to_sales,ad_upper,ad_lower)
abuick = data.frame(ad_to_sales,ad_upper,ad_lower)
acadillac = data.frame(ad_to_sales,ad_upper,ad_lower)
achevrolet = data.frame(ad_to_sales,ad_upper,ad_lower)
adodge	 = data.frame(ad_to_sales,ad_upper,ad_lower)
aford = data.frame(ad_to_sales,ad_upper,ad_lower)
#ahonda= data.frame(ad_to_sales,ad_upper,ad_lower)
ahyundai = data.frame(ad_to_sales,ad_upper,ad_lower)
ainfiniti =data.frame(ad_to_sales,ad_upper,ad_lower)
ajaguar = data.frame(ad_to_sales,ad_upper,ad_lower)
akia = data.frame(ad_to_sales,ad_upper,ad_lower)
alexus = data.frame(ad_to_sales,ad_upper,ad_lower)
alincoln =data.frame(ad_to_sales,ad_upper,ad_lower)
amazda = data.frame(ad_to_sales,ad_upper,ad_lower)
amercedes = data.frame(ad_to_sales,ad_upper,ad_lower)
#amitsubishi = data.frame(ad_to_sales,ad_upper,ad_lower)
anissan = data.frame(ad_to_sales,ad_upper,ad_lower)
aporsche=data.frame(ad_to_sales,ad_upper,ad_lower)
asubaru=data.frame(ad_to_sales,ad_upper,ad_lower)
atesla=data.frame(ad_to_sales,ad_upper,ad_lower)
atoyota=data.frame(ad_to_sales,ad_upper,ad_lower)
avolkswagen=data.frame(ad_to_sales,ad_upper,ad_lower)
avolvo= data.frame(ad_to_sales,ad_upper,ad_lower)

airf_df = rbind(aacura, aaudi, abmw, abuick, acadillac, achevrolet, adodge, aford, ahyundai, ainfiniti, 
               ajaguar, akia, alexus, alincoln, amazda, amercedes, anissan, aporsche, asubaru, atoyota, avolkswagen, avolvo)

write.csv(airf_df, "airf2.csv")
