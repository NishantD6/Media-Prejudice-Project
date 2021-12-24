library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
df <- read_csv("/Users/nishant/Desktop/1-RESEARCH/Data&Code/NHSTA/Old:Misc/clean_dfw.CSV")
df <- tbl_df(df)
glimpse(df)

df$MFR_NAME <- as.factor(df$MFR_NAME)
df$MAKETXT <- as.factor(df$MAKETXT)
df$MODELTXT <- as.factor(df$MODELTXT)
df$CRASH <- as.factor(df$CRASH)
df$FIRE <- as.factor(df$FIRE)
df$INJURED <- as.factor(df$INJURED)
df$DEATHS <- as.factor(df$DEATHS)
df$CITY <- as.factor(df$CITY)
df$COMPDESC <- as.factor(df$COMPDESC)
df$STATE <- as.factor(df$STATE)
df$POLICE_RPT_YN  <- as.factor(df$POLICE_RPT_YN)
df$FUEL_SYS <- as.factor(df$FUEL_SYS)
df$FUEL_TYPE <- as.factor(df$FUEL_TYPE)
df$VEHICLES_TOWED_YN <- as.factor(df$VEHICLES_TOWED_YN)
df$MEDICAL_ATTN <- as.factor(df$MEDICAL_ATTN)
df$NUM_CYLS <- as.factor(df$NUM_CYLS)
df$REPAIRED_YN <- as.factor(df$REPAIRED_YN)

complaints<- df%>%
  group_by(ODINO) %>% 
  slice(1)%>%
  ungroup()%>%
  filter(FAILDATE>='2015-01-01')

glimpse(complaints)

complaints1 <- complaints%>%
  select(Make=MAKETXT, FAILDATE)%>%
  mutate(month=month(FAILDATE), Year=year(FAILDATE))%>%
  group_by(Make, Year, month)%>%
  summarise(num_complaints=n())%>%
  ungroup()



write.csv(complaints1, "/Users/nishant/Desktop/1-RESEARCH/Data&Code/Z-Final DataSets/Complaints Data/Complaints.csv")

glimpse(complaints1)




# NUMBER OF COMPLAINTS PER MAKE GROUP YEAR FROM 2010 TO 2020
df1%>%
  select(MFR_NAME, FAILDATE)%>%
  mutate(month=month(FAILDATE), year = year(FAILDATE))%>%
  group_by(MFR_NAME, year)%>%
  summarise(count= n())%>%
  ggplot(aes(y=count, x=year))+
  geom_smooth(se = F)+
  facet_wrap(~MFR_NAME)

# NUMBER OF COMPLAINTS PER GROUP PER DAY FROM 2010 TO 2020
complaints%>%
  select(MFR_NAME, FAILDATE)%>%
  group_by(MFR_NAME, FAILDATE)%>%
  summarise(count= n())%>%
  ggplot(aes(y=count, x=FAILDATE))+
  geom_line(se = F)+
  facet_wrap(~MFR_NAME)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Number of Compaints per GROUP per Day from 2010 to 2020")

# NUMBER OF COMPLAINTS PER MAKE PER DAY FROM 2010 TO 2020
complaints%>%
  select(MAKETXT, FAILDATE)%>%
  #filter(MAKETXT=="VOLKSWAGEN")%>%
  mutate(month=month(FAILDATE), year = year(FAILDATE))%>%
  group_by(MAKETXT, FAILDATE)%>%
  summarise(count= n())%>%
  ggplot(aes(y=count, x=FAILDATE))+
  geom_col(se = F)+
  #facet_wrap(~MAKETXT)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Number of VOLKSWAGEN per Day from 2010 to 2020")

# NUMBER OF COMPLAINTS PER MAKE PER Month FROM 2010 TO 2020
complaints%>%
  select(MAKETXT, FAILDATE)%>%
  #filter(MAKETXT=="VOLKSWAGEN")%>%
  mutate(month = as.Date(cut(FAILDATE, breaks = "month")))%>%
  #mutate(month=month(FAILDATE), year = year(FAILDATE))%>%
  group_by(MAKETXT, month)%>%
  summarise(count= n())%>%
  ggplot(aes(y=count, x=month))+
  stat_summary(fun.y=sum, geom='bar')+
  #geom_bar(se = F)+
  facet_wrap(~MAKETXT)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Number of Complaints per month from 2010 to 2020")


# NUMBER OF COMPLAINTS PER Group PER Month FROM 2010 TO 2020
complaints%>%
  select(MFR_NAME, FAILDATE)%>%
  #filter(MAKETXT=="VOLKSWAGEN")%>%
  mutate(month = as.Date(cut(FAILDATE, breaks = "month")))%>%
  #mutate(month=month(FAILDATE), year = year(FAILDATE))%>%
  group_by(MFR_NAME, month)%>%
  summarise(count= n())%>%
  ggplot(aes(y=count, x=month))+
  stat_summary(fun.y=sum, geom='bar')+
  #geom_bar(se = F)+
  facet_wrap(~MFR_NAME)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle("Number of Complaints per MFR month from 2010 to 2020")

ggplot(data = log,
       aes(Month, Quantity)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar") + # or "line"
  scale_x_date(
    labels = date_format("%Y-%m"),
    breaks = "1 month") # custom x-axis labels

df1%>%
  select(MFR_NAME, MAKETXT, FAILDATE)%>%
  mutate(month=month(FAILDATE), year = year(FAILDATE))%>%
  group_by(MAKETXT, year)%>%
  filter(year<2020e)%>%
  summarise(count= n())%>%
  ggplot(aes(y=count, x=year,col=MFR_NAME))+
  geom_smooth(se = F)


# Number of Complaints per State
df1%>%
  select(STATE)%>%
  group_by(STATE)%>%
  summarise(ComplaintsPerState=n())%>%
  ggplot(aes(x= STATE, y=ComplaintsPerState))+
  geom_col()+
  ggtitle("Number of Compaints per State")
