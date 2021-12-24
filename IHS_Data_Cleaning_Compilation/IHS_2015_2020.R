library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
df <- read_csv("/Users/nishant/Desktop/1-RESEARCH/Data&Code/IHS/IHS Erasmus_2015_2020.csv")
glimpse(df)
df%>%
  select(Group='Make Group', Make)%>%
  group_by(Group, Make)%>%
  summarise(n())%>%
  View
glimpse(df1)
df1<- df%>%
  select(CountryName=`Country-Name`,
         LastAvailableDate=`Last Available Date`,
         MakeGroup = `Make Group`,
         Make,
         ModelGroup=`Model Group`,
         Model, 
         SubGroup=`Sub Model`,
         Version,
         Trim,
         GenerationYear=`Generation Year`,
         FeulType=`Fuel Type`,
         PriceLocal=`Price (local)`,
         "01-01-2015"=`2015/01`,
         "01-02-2015"=`2015/02`,
         "01-03-2015"=`2015/03`,
         "01-04-2015"=`2015/04`,
         "01-05-2015"=`2015/05`,
         "01-06-2015"=`2015/06`,
         "01-07-2015"=`2015/07`,
         "01-08-2015"=`2015/08`,
         "01-09-2015"=`2015/09`,
         "01-10-2015"=`2015/10`,
         "01-11-2015"=`2015/11`,
         "01-12-2015"=`2015/12`,
         "01-01-2016"=`2016/01`,
         "01-02-2016"=`2016/02`,
         "01-03-2016"=`2016/03`,
         "01-04-2016"=`2016/04`,
         "01-05-2016"=`2016/05`,
         "01-06-2016"=`2016/06`,
         "01-07-2016"=`2016/07`,
         "01-08-2016"=`2016/08`,
         "01-09-2016"=`2016/09`,
         "01-10-2016"=`2016/10`,
         "01-11-2016"=`2016/11`,
         "01-12-2016"=`2016/12`,
         "01-01-2017"=`2017/01`,
         "01-02-2017"=`2017/02`,
         "01-03-2017"=`2017/03`,
         "01-04-2017"=`2017/04`,
         "01-05-2017"=`2017/05`,
         "01-06-2017"=`2017/06`,
         "01-07-2017"=`2017/07`,
         "01-08-2017"=`2017/08`,
         "01-09-2017"=`2017/09`,
         "01-10-2017"=`2017/10`,
         "01-11-2017"=`2017/11`,
         "01-12-2017"=`2017/12`,
         "01-01-2018"=`2018/01`,
         "01-02-2018"=`2018/02`,
         "01-03-2018"=`2018/03`,
         "01-04-2018"=`2018/04`,
         "01-05-2018"=`2018/05`,
         "01-06-2018"=`2018/06`,
         "01-07-2018"=`2018/07`,
         "01-08-2018"=`2018/08`,
         "01-09-2018"=`2018/09`,
         "01-10-2018"=`2018/10`,
         "01-11-2018"=`2018/11`,
         "01-12-2018"=`2018/12`,
         "01-01-2019"=`2019/01`,
         "01-02-2019"=`2019/02`,
         "01-03-2019"=`2019/03`,
         "01-04-2019"=`2019/04`,
         "01-05-2019"=`2019/05`,
         "01-06-2019"=`2019/06`,
         "01-07-2019"=`2019/07`,
         "01-08-2019"=`2019/08`,
         "01-09-2019"=`2019/09`,
         "01-10-2019"=`2019/10`,
         "01-11-2019"=`2019/11`,
         "01-12-2019"=`2019/12`,
         "01-01-2020"=`2020/01`,
         "01-02-2020"=`2020/02`,)%>%
  filter(CountryName=="USA")

df2 = gather(df1, Date, NumSold,"01-01-2015":"01-02-2020" )
View(df2)


df2$Date=as.Date(df2$Date, format= "%d-%m-%Y")

df3<-df2%>%
  group_by(MakeGroup, Date)%>%
  #filter(MakeGroup=='Alpina')%>%
  summarise(numSold=sum(NumSold))

glimpse(df2)




df2%>%
  group_by(MakeGroup, Date)%>%
  filter( Date<"2020-01-01")%>%
  summarise(N_sold_per_Brand_Month=sum(NumSold))%>%
  ggplot(aes(x=Date, y=N_sold_per_Brand_Month))+
  geom_line(se = F)+
  facet_wrap(~MakeGroup)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Number of Cars MAKE SOLD per Month from 2015 to 2020")

df2$Make=as.factor(df2$Make)

sold <-df3%>%
  select(Make= MakeGroup, Date, numSold)%>%
  mutate(month=month(Date), Year=year(Date))

glimpse(sold)

View(sold)
glimpse(complaints1)


complaints1 <- complaints%>%
  select(Make=MFR_NAME, FAILDATE)%>%
  mutate(month=month(FAILDATE), Year=year(FAILDATE))%>%
  group_by(Make, Year,month)%>%
  summarise(num_complaints=n())%>%
  ungroup()

new_dataset1 <- complaints1%>%
  right_join(sold, by=c("Make","Year", "month"))

View(new_dataset1)
glimpse(new_dataset1)  
View(complaints1)

# correct dataset
cdf<-new_dataset1%>%
  na.omit()%>%
  #filter(Make=="BMW" |Make=="Daimler", Year=="2015", month=="1"|month=="2")%>%
  #filter(Year=="2016", month=="1")%>%
  filter(numSold!=0, num_complaints!=0)%>%
  group_by(Date)%>%
  mutate(BCbyBS=num_complaints/numSold)%>%
  ungroup()%>%
  group_by(Year, month)%>%
  mutate(sBCbyBS=sum(BCbyBS),MSCC=BCbyBS*100/sBCbyBS)%>%
  filter(Make =="Aston Martin Finance Gr.")%>%
  select(MSCC)%>%
  ndiffs(MSCC)
  #filter(Year=="2018", month=="2")

cdf%>%
  ggplot(aes(x=Date, y= MSCC))+
  geom_line(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Sales adjusted Monthly Market Share of Complaints 2015 to 2020")
  
glimpse(cdf)

cdf%>%
  ungroup()%>%
  group_by(Year, month)%>%
  mutate(total_sales=sum(numSold), total_complaints=sum(num_complaints), complaintShare=100*num_complaints/total_complaints)%>%
  ungroup()%>%
  #filter(Year=="2016", month=="1")%>%
  ggplot(aes(x=Date, y= complaintShare))+
  geom_line(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Monthly Market Share of Complaints 2015 to 2020")

cdf%>%
  ungroup()%>%
  #group_by(Year, month)%>%
  #mutate(total_sales=sum(numSold), total_complaints=sum(num_complaints), complaintShare=100*num_complaints/total_complaints)%>%
  ungroup()%>%
  filter(Year=="2016", Make=="Toyota Group")%>%
  ggplot(aes(x=Date, y= num_complaints))+
  geom_line(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Total Complaints per day 2015 to 2020")


new_dataset1%>%
  ggplot(aes(x=Date, y= num_complaints))+
  geom_smooth(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Number of Cars MAKE SOLD per Month from 2015 to 2020")

View(new_dataset1)
new_dataset1%>%
  filter(Make!="Suzuki")%>%
  filter(Year<2020)%>%
  na.omit()%>%
  mutate(CSRatio = num_complaints/numSold*100)%>%
  group_by(Make)%>%
  summarise(CSRatioMean=mean(CSRatio))

new_dataset1%>%
  filter(Make!="Suzuki", 	Make!='Aston Martin Finance Gr.', Make!="McLaren Group")%>%
  filter(Year<2020)%>%
  na.omit()%>%
  mutate(CSRatio = num_complaints/numSold*100)%>%
  ggplot(aes(x=Date, y= CSRatio))+
  geom_smooth(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Number of Complaints to Cars Sold per Month from 2015 to 2020")

new_dataset1%>%
  filter(Make!="Suzuki", 	Make!='Aston Martin Finance Gr.', Make!="McLaren Group")%>%
  filter(Year<2020)%>%
  na.omit()%>%
  mutate(CSRatio = num_complaints/numSold*100)%>%
  group_by(month, Year)%>%
  mutate(totalCSRatio=sum(CSRatio))%>%
  ungroup()%>%
  mutate(complaintShare=CSRatio/totalCSRatio*100)%>%
  #filter( month==1, Year==2018)%>%
  #mutate(TotalMonthlyComplaints=sum(num_complaints), TotalMonthlySales=sum(numSold), ComplaintShare=CSRatio/(TotalMonthlyComplaints/TotalMonthlySales))%>%
  ggplot(aes(x=Date, y= complaintShare))+
  geom_smooth(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Sales Adjusted Share of Complaints for Cars Sold per Month from 2015 to 2020")


new_dataset1%>%
  filter(Make!="Suzuki", 	Make!='Aston Martin Finance Gr.', Make!="McLaren Group")%>%
  filter(Year<2020)%>%
  na.omit()%>%
  group_by(month, Year)%>%
  mutate(CSRatio = num_complaints/numSold*100)%>%
  #mutate(totalComplaints=sum(num_complaints), totalSold=sum(numSold))%>%
  ungroup()%>%
  group_by(Make, month, Year)%>%
  #mutate(complaints = sum(num_complaints), complaintShare=complaints/totalComplaints, salesProportion=numSold/totalSold, adjComplaintShare=complaintShare*salesProportion*10000)%>%
  #filter( month==1, Year==2018)%>%
  mutate(TotalMonthlyComplaints=sum(num_complaints), TotalMonthlySales=sum(numSold), ComplaintShare=CSRatio/(TotalMonthlyComplaints/TotalMonthlySales))%>%
  ggplot(aes(x=Date, y= ComplaintShare))+
  geom_smooth(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Share of Complaints for Cars Sold per Month from 2015 to 2020")
  
# Complaints Market Share = Number of Complaints/Total Number of Complaints in that month
new_dataset1%>%
  filter(Make!="Suzuki", 	Make!='Aston Martin Finance Gr.', Make!="McLaren Group")%>%
  filter(Year<2020)%>%
  na.omit()%>%
  #mutate(CSRatio = num_complaints/numSold*100)%>%
  group_by(month, Year)%>%
  mutate(totalComplaints=sum(num_complaints), totalSold=sum(numSold))%>%
  ungroup()%>%
  group_by(Make, month, Year)%>%
  mutate(complaints = sum(num_complaints), complaintShare=complaints/totalComplaints, salesProportion=numSold/totalSold, adjComplaintShare=complaintShare*salesProportion*10000)%>%
  #filter( month==1, Year==2018)%>%
  #mutate(TotalMonthlyComplaints=sum(num_complaints), TotalMonthlySales=sum(numSold), ComplaintShare=CSRatio/(TotalMonthlyComplaints/TotalMonthlySales))%>%
  ggplot(aes(x=Date, y= complaintShare))+
  geom_smooth(se = F)+
  facet_wrap(~Make)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Share of Complaints for Cars Sold per Month from 2015 to 2020")
  

  
