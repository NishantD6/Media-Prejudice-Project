library(readxl)
library(dplyr)
library(lubridate)
Add_15 <- read_excel("Desktop/1-RESEARCH/Data&Code/Nielsen Data/Nielsen Ad Intel International Erasmus University Report.xlsx", 
                                                                       sheet = "United States 2015 - 2017")

Add18 <- read_excel("Desktop/1-RESEARCH/Data&Code/Nielsen Data/Nielsen Ad Intel International Erasmus University Report.xlsx", 
                                                                       sheet = "United States 2018 - 2019")

glimpse(Add_15)
glimpse(Add18)

ad_df <- bind_rows(Add_15, Add18)

glimpse(ad_df)

addf<- ad_df%>%
  select(Country, 
    LocalAdvertise= 'Local Advertiser',
    LocalProduct='Local Product',
    GlobalCategoryMinor= 'Global Category Minor',
    LocalCategoryMajor='Local Category Major',
    LocalCategoryMid= 'Local Category Mid',
    LocalCategoryMinor = 'Local Category Minor',
    MediaType = 'Media Type',
    UserDecive='User Device',
    AdType='Ad Type',
    MediaChannel ='Media Channel',
    Year, Month,
    Spend_Local = 'Spend Local',
    Spend_Euro = "Spend Euro",
    LocalDiscounted = 'Local Discounted',
    EuroDiscounted = 'Euro Discounted'

  )

glimpse(addf)

addf$Country = as.factor(addf$Country)
addf$LocalAdvertise = as.factor(addf$LocalAdvertise)
addf$LocalProduct = as.factor(addf$LocalProduct)
addf$GlobalCategoryMinor = as.factor(addf$GlobalCategoryMinor)
addf$LocalCategoryMajor = as.factor(addf$LocalCategoryMajor)
addf$LocalCategoryMid = as.factor(addf$LocalCategoryMid)
addf$LocalCategoryMinor = as.factor(addf$LocalCategoryMinor)
addf$MediaType = as.factor(addf$MediaType)
addf$UserDecive = as.factor(addf$UserDecive)
addf$AdType  = as.factor(addf$AdType)
addf$MediaChannel  = as.factor(addf$MediaChannel)
addf$Year  = as.factor(addf$Year)
addf$Month  = as.factor(addf$Month)


addf$LocalAdvertise <- recode(addf$LocalAdvertise,
  "ACURA AUTOMOTIVE DIV"= 'acura',
  'ACURA DIVISION' = 'acura',
  "AUDI OF AMERICA INC-DA"="audi",
  'BMW OF NORTH AMERICA LLC' = 'bmw',
  'BUICK MOTOR DIVISION' = 'buick',
  'CADILLAC MOTOR CO' = 'cadillac',
  'CADILLAC AUTOMOTIVE DIV' = 'cadillac',
  'CHEVROLET MOTOR DIVISION-DA' = 'chevrolet',
  'CHRYSLER GROUP LLC' = 'chrysler',
  'DODGE CAR-TRUCK DIVISION' = 'dodge',
  'FIAT AUTO USA INC' = 'fiat',
  'FORD MOTOR CO-DA' = 'ford',
  'GENERAL MOTORS CO' = 'general_motors',
  'GMC TRUCK DIVISION' = 'gmc',
  'HONDA MOTOR CO LTD' = 'honda',
  'HYUNDAI MOTOR AMERICA-DA' = 'hyundai',
  'INFINITI DIVISION' = 'infiniti',
  'JAGUAR CARS LTD' = 'jaguar',
  'KIA MOTORS AMERICA INC' = 'kia',
  'KIA MOTORS CORP' = 'kia',
  'LEXUS DIVISION' = 'lexus',
  'LINCOLN MOTOR CO' = 'lincoln',
  'MAZDA NORTH AMERICAN OPERATIONS' = 'mazda',
  'MERCEDES-BENZ USA LLC-DA' = 'mercedes',
  'MITSUBISHI MOTORS NORTH AMERICA INC' = 'mitsubishi',
  'NISSAN MOTOR CO LTD' = 'nissan',
  'NISSAN NORTH AMERICA INC' = 'nissan',
  'PORSCHE CARS NORTH AMERICA INC' = 'porsche',
  'RAM DIVISION' = 'ram',
  'SUBARU OF AMERICA INC' = 'subaru',
  'TESLA INC' = 'tesla',
  'TOYOTA MOTOR CORP' = 'toyota',
  'TOYOTA MOTOR SALES USA INC-DA' = 'toyota',
  'VOLKSWAGEN AG' = 'volkswagen',
  'VOLKSWAGEN OF AMERICA INC' = 'volkswagen',
  'VOLVO CARS OF NORTH AMERICA LLC' = 'volvo',
)

addf$YearMonth <- paste(addf$Year, "-", addf$Month, "-1")

addf$Date <- ymd(addf$YearMonth)

glimpse(addf)

addf1<- addf%>%
  select(Make = LocalAdvertise, Spend_Local, LocalDiscounted, Date)%>%
  filter(Make %in% c('acura', 'audi', 'bmw', 'buick', 'cadillac','chevrolet', 'chrysler', 'dodge', 'fiat', 'ford', 'general motors', 'gmc', 'honda', 'hyundai',
                     'infiniti', 'jaguar', 'kia', 'lexus', 'lincoln', 'mazda', 'mercedes', 'mitsubishi', 'nissan', 'porsche', 'tesla',
                     'ram', 'subaru', 'toyota', 'volkswagen', 'volvo'))



adfw <- addf1%>%
  #filter(Make=='acura', Date == '2015-01-01')%>%
  group_by(Make, Date)%>%
  na.omit()%>%
  summarise(LocalAd = sum(Spend_Local), LocalDiscountedAd = sum(LocalDiscounted))

glimpse(adfw)

View(adfw%>%
       group_by(Make)%>%
       summarise(n()))


write.csv(x = adfw  ,  file="/Users/nishant/Desktop/1-RESEARCH/Data&Code/Z-Final DataSets/AdSpend/AdSpend.csv")


View(addf1%>%
  select(Make)%>%
  group_by(Make)%>%
  summarise(n()))

View(addf%>%
       select(LocalAdvertise)%>%
       group_by(LocalAdvertise)%>%
       summarise(n()))


