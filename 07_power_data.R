
######File to read in monthly power generation data 
# 1.  read in ENTSO-E data for hourly generation by technology (MW) to calculate emissions
#     lines -  15 - 105
# 2.  read in ENTSO-E demand data
#     lines - 115 - 175
# 3.  read in India data
#     lines 180-185
# 4.  read in us data
#     lines - 190 - 240



#path to files with ENTSO-E data, downloaded via SFTP (https://entsoe.zendesk.com/hc/en-us/articles/115000173266-Overview-of-data-download-options-on-Transparency-Platform)
#files first need to be converted to UTF-8 (e.g. via notepad++ or similar editors)
path <- "ENTSO-E_data/"
#file name, same as folder on SFTP server with "AggregatedGenerationPerType"
filen <- "_AggregatedGenerationPerType.csv"

#25 eu countries
eu <- c("Italy","United Kingdom" , "Slovenia" , "Hungary"  ,"Germany"  ,"Belgium","Latvia","Finland", #8
               "Sweden","Czech Republic","Ireland", "Spain" ,"Slovakia", "Netherlands"  ,"Romania","Estonia","Poland" , #9
              "Denmark","France" ,"Greece"  ,  "Portugal" ,"Bulgaria","Cyprus","Lithuania","Austria" ) #8
# missing Malta, Croatia (only joined in 2013?), Luxembourg
#plus 6 more countries, other area names refer to CTA or BZN

oth_cty <- c("Montenegro", "Bosnia Herzegovina" , "Serbia" , "Switzerland" ,"North Macedonia"  , "Norway"  )

fossils <- c("Fossil Gas","Fossil Hard coal","Fossil Oil","Fossil Brown coal/Lignite",
             "Fossil Peat","Fossil Coal-derived gas","Fossil Oil shale")

# emission intensity of different fossils, values chosen from various sources:
# https://www.ipcc.ch/site/assets/uploads/2018/02/ipcc_wg3_ar5_chapter7.pdf
# https://www.iea.org/reports/chinas-emissions-trading-scheme
# https://de.wikipedia.org/wiki/Kraftwerk_Neurath
int<-data.frame(ProductionType = fossils,
                int = c(0.42,0.85,0.89,1.1,0.9,0.7,0.89))
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep")

#initialize the matrix of data to read in
mat <- data.frame(AreaName = rep(c(eu,oth_cty),each = length(fossils)), ProductionType = rep(fossils,length(c(eu,oth_cty))))

# calculate the daily average for the first 9 months
for(i in seq(1,length(months))){
  #read in ENTSO-E data, downloaded via SFTP (https://entsoe.zendesk.com/hc/en-us/articles/115000173266-Overview-of-data-download-options-on-Transparency-Platform)
  #files first need to be converted to UTF-8 (e.g. via notepad++ or similar editors)
  last <- fread(paste0(path,"2019_",i,filen))
  this <- fread(paste0(path,"2020_",i,filen))
  eight <- fread(paste0(path,"2018_",i,filen))
  this <- this %>% filter(AreaTypeCode=="CTY",ProductionType %in% fossils)
  last <- last %>% filter(AreaTypeCode=="CTY",ProductionType %in% fossils)
  eight <- eight %>% filter(AreaTypeCode=="CTY",ProductionType %in% fossils)
  this$date <- as.Date(paste0(this$Year,":",this$Month,":",this$Day),tryFormats=c("%Y:%m:%d"))
  matt <- this %>% group_by(AreaName,ProductionType) %>% summarise(this=mean(ActualGenerationOutput,na.rm = T))
  matl <- last %>% group_by(AreaName,ProductionType) %>% summarise(last=mean(ActualGenerationOutput,na.rm = T)) 
  mate <- eight %>% group_by(AreaName,ProductionType) %>% summarise(eight=mean(ActualGenerationOutput,na.rm = T)) 
  colnames(matt)[3] <- paste0("av2020_",i)
  colnames(matl)[3] <- paste0("av2019_",i)
  colnames(mate)[3] <- paste0("av2018_",i)
  mat <- full_join(mat,matt)
  mat <- full_join(mat,matl)
  mat <- full_join(mat,mate)
}
rm(last)
rm(this)
rm(eight)

# Average for Jan1-September30 10 in each year, considering leap year 
mat <- mat %>% mutate(av18 = (31*av2018_1+28*av2018_2+31*av2018_3+30*av2018_4+31*av2018_5+30*av2018_6+31*av2018_7+31*av2018_8+30*av2018_9)/(31+28+31+30+31+30+31+31+30))
mat <- mat %>% mutate(av19 = (31*av2019_1+28*av2019_2+31*av2019_3+30*av2019_4+31*av2019_5+30*av2019_6+31*av2019_7+31*av2019_8+30*av2019_9)/(31+28+31+30+31+30+31+31+30))
mat <- mat %>% mutate(av20 = (31*av2020_1+29*av2020_2+31*av2020_3+30*av2020_4+31*av2020_5+30*av2020_6+31*av2020_7+31*av2020_8+30*av2020_9)/(31+29+31+30+31+30+31+31+30))
#tidy data
mat <- mat %>% gather(period,value,-AreaName,-ProductionType)

# hourly averaged emissions of the power system
emi <- full_join(mat,int) %>% mutate(value=int*value) %>% select(-int)
emi$ProductionType <- paste0("Emissions-",emi$ProductionType)

#hourly averaged month-wise total fossil generation:
fos <- mat %>% group_by(AreaName,period) %>% summarise(value=sum(value,na.rm=T)) %>% ungroup()
fos$ProductionType <- "Fossil total"
#hourly averaged month-wise total fossil emissions 
fosemi <- emi %>% group_by(AreaName,period) %>% summarise(value=sum(value,na.rm=T)) %>% ungroup()
fosemi$ProductionType <- "Emissions-Fossil total"

#combine data by variables
mat <- rbind(mat,emi,fos,fosemi)  
  
# Hourly-averaged monthly generation and emission for (total) Eu25 countries and (total) ENTSO-E countries
eu25 <- mat %>% filter(AreaName %in% eu) %>% group_by(ProductionType,period) %>% summarise(value=sum(value,na.rm = T)) %>% ungroup()
eu25$AreaName <- "EU25"
entsoe <- mat %>% group_by(ProductionType,period) %>% summarise(value=sum(value,na.rm = T)) %>% ungroup()
entsoe$AreaName <- "ENTSOE"

#combine data by regions
mat <- rbind(mat,eu25,entsoe)

#check - total Fossil generation in Germany in GWh for first 9 months
mat %>% filter(AreaName=="Germany",ProductionType=="Fossil total",period %in% c("av19","av20")) %>%
  mutate(value=value*24*((31+29+31+30+31+30+31+31+30)/1000))

#Calculate reduction of averaged hourly emissions per month and country
red <- mat %>% filter(ProductionType=="Emissions-Fossil total") %>% spread(period,value) %>%
   mutate(jan=av2020_1/av2019_1,feb=av2020_2/av2019_2,mar=av2020_3/av2019_3,apr=av2020_4/av2019_4,may=av2020_5/av2019_5,
             jun=av2020_6/av2019_6,jul=av2020_7/av2019_7,aug=av2020_8/av2019_8,sep=av2020_9/av2019_9) %>% select(-av2020_1,-av2020_2,-av2020_3,-av2020_4,-av2020_5,-av2020_6,-av2020_7,-av2020_8,-av2020_9,
                                               -av2019_1,-av2019_2,-av2019_3,-av2019_4,-av2019_5,-av2019_6,-av2019_7,-av2019_8,-av2019_9,-av19,-av20,
                                               -av2018_1,-av2018_2,-av2018_3,-av2018_4,-av2018_5,-av2018_6,-av2018_7,-av2018_8,-av2018_9) %>% 
  gather(period,value,-ProductionType,-AreaName) # %>% arrange(value)

# save file for use in correction of daily data
save(red,file="red_entso-e.RData")

#show largest emitters, based in 2019
mat %>% filter(ProductionType=="Emissions-Fossil total",period=="av19") %>% arrange(desc(value))

#average emission and generation in 2020 per type
mat %>% filter(period == "av20") %>% group_by(ProductionType) %>% 
  summarise(value=sum(value,na.rm = T))

#average emissions and generation in 2019 per type
mat %>% filter(period == "av19") %>% group_by(ProductionType) %>% 
  summarise(value=sum(value,na.rm = T))



###### demand data

#path to files with ENTSO-E data, downloaded via SFTP (https://entsoe.zendesk.com/hc/en-us/articles/115000173266-Overview-of-data-download-options-on-Transparency-Platform)
#files first need to be converted to UTF-8 (e.g. via notepad++ or similar editors)
path <- "ENTSO-E_data/"
#file name, same as folder on SFTP server with "ActualTotalLoad"
filen <- "_ActualTotalLoad.csv"

#initialize the matrix of data to read in
matd <- data.frame(AreaName = c(eu,oth_cty))

#for first 9 months
for(i in seq(1,length(months))){
  #read in ENTSO-E data, downloaded via SFTP (https://entsoe.zendesk.com/hc/en-us/articles/115000173266-Overview-of-data-download-options-on-Transparency-Platform)
  #files first need to be converted to UTF-8 (e.g. via notepad++ or similar editors)
  last <- fread(paste0(path,"2019_",i,filen))
  this <- fread(paste0(path,"2020_",i,filen))
  this <- this %>% filter(AreaTypeCode=="CTY")
  last <- last %>% filter(AreaTypeCode=="CTY")
  this$date <- as.Date(paste0(this$Year,":",this$Month,":",this$Day),tryFormats=c("%Y:%m:%d"))
  matt <- this %>% group_by(AreaName) %>% summarise(this=mean(TotalLoadValue,na.rm = T))
  matl <- last %>% group_by(AreaName) %>% summarise(last=mean(TotalLoadValue,na.rm = T)) 
  colnames(matt)[2] <- paste0("av2020_",i)
  colnames(matl)[2] <- paste0("av2019_",i)
  matd <- full_join(matd,matt)
  matd <- full_join(matd,matl)
}
rm(last)
rm(this)

#add average for Jan1-June20 in each year
matd <- matd %>% mutate(av19 = (31*av2019_1+28*av2019_2+31*av2019_3+30*av2019_4+31*av2019_5+30*av2019_6+31*av2019_7+31*av2019_8+30*av2019_9)/(31+28+31+30+31+30+31+31+30))
matd <- matd %>% mutate(av20 = (31*av2020_1+29*av2020_2+31*av2020_3+30*av2020_4+31*av2020_5+30*av2020_6+31*av2020_7+31*av2020_8+30*av2020_9)/(31+29+31+30+31+30+31+31+30))
#tidy data
matd <- matd %>% gather(period,value,-AreaName)


#add eu25 total, and total of ENTSO-E countries
eu25 <- matd %>% filter(AreaName %in% eu) %>% group_by(period) %>% summarise(value=sum(value,na.rm = T)) %>% ungroup()
eu25$AreaName <- "EU25"
entsoe <- matd %>% group_by(period) %>% summarise(value=sum(value,na.rm = T)) %>% ungroup()
entsoe$AreaName <- "ENTSOE"

#combine data by regions
matd <- rbind(matd,eu25,entsoe)

#calculate monthly emission reductions for correction of daily data
red_dem <- matd %>%  spread(period,value) %>%
  mutate(jan=av2020_1/av2019_1,feb=av2020_2/av2019_2,mar=av2020_3/av2019_3,apr=av2020_4/av2019_4,may=av2020_5/av2019_5,
         jun=av2020_6/av2019_6,jul=av2020_7/av2019_7,aug=av2020_8/av2019_8,sep=av2020_9/av2019_9) %>% select(-av2020_1,-av2020_2,-av2020_3,-av2020_4,-av2020_5,-av2020_6,-av2020_7,-av2020_8,-av2020_9,
                                           -av2019_1,-av2019_2,-av2019_3,-av2019_4,-av2019_5,-av2019_6,-av2019_7,-av2019_8,-av2019_9,-av19,-av20) %>% 
  gather(period,value,-AreaName) # %>% arrange(value)




#india, see excel "other_data/India-power-carbon.xlsx", B26-G27, from carbontracker.in (using the "range" and "summary statistics" options)
ind <- data.frame(period = c("jan","feb","mar","apr","may","jun","jul","aug","sep"),
                  demand= c(1.015318624,	1.056459342,	0.896263615,	0.749329552,	0.838157807,	0.898184694, 0.962183479,0.980841165,1.049581604),#2020 demand indexed to respective 2019 demands
                  value = c(0.983587784,	1.02972808,	0.84147143,	0.684500414,	0.768187467,	0.815120789, 0.944599196,0.960064043,1.073892311))#2020 emissions indexed to respective 2019 emissions


#us
#read in EIA generation data
# https://www.eia.gov/opendata/qb.php?category=3390105
# https://www.eia.gov/opendata/qb.php?category=3390105&sdid=EBA.US48-ALL.NG.COL.H
# https://www.eia.gov/opendata/qb.php?category=3390105&sdid=EBA.US48-ALL.NG.NG.H
# https://www.eia.gov/opendata/qb.php?category=3390105&sdid=EBA.US48-ALL.NG.OIL.H
##https://www.eia.gov/opendata/qb.php?category=3389935&sdid=EBA.US48-ALL.D.H
#replace space between date and hour column with "," in a text editor like notepad++
usc <- read.csv("other_data/Net_generation_from_coal_for_United_States_Lower_48_(region)_hourly_-_UTC_time.csv",skip = 5,col.names=c("date","hour","coal"))
usg <- read.csv("other_data/Net_generation_from_natural_gas_for_United_States_Lower_48_(region)_hourly_-_UTC_time.csv",skip = 5,col.names=c("date","hour","gas"))
uso <- read.csv("other_data/Net_generation_from_petroleum_for_United_States_Lower_48_(region)_hourly_-_UTC_time.csv",skip = 5,col.names=c("date","hour","oil"))
usd <- read.csv("other_data/Demand_for_United_States_Lower_48_(region)_hourly_-_UTC_time.csv",skip = 5, col.names=c("date","hour","demand")) 
  #filter(!date %in% c("06/21/2020","06/22/2020","06/23/2020"))
us <- full_join(usc,usg)
us <- full_join(us,uso)
us <- full_join(us,usd)
us <- us %>% mutate(emi = 0.85*coal+0.42*gas+0.89*oil)
us$date <- as.Date(us$date,tryFormats = c("%m/%d/%Y"))

#data frame to store indexed values for US
usm <- data.frame(period = c("jan","feb","mar","apr","may","jun","jul","aug","sep"),
                  value = rep(0,9),demand = rep(0,9)) # value: indexed average daily emission per month 
                                                      # demand: indexed average daily demand per month
#calculate indexed monthly values
usm[1,2] <- sum(us[(us$date >"2019-12-31" & us$date <"2020-02-01"),]$emi)/sum(us[(us$date >"2018-12-31" & us$date <"2019-02-01"),]$emi)
usm[1,3] <- sum(us[(us$date >"2019-12-31" & us$date <"2020-02-01"),]$demand)/sum(us[(us$date >"2018-12-31" & us$date <"2019-02-01"),]$demand)
usm[2,2] <- sum(us[(us$date >"2020-01-31" & us$date <"2020-03-01"),]$emi)/sum(us[(us$date >"2019-01-31" & us$date <"2019-03-01"),]$emi)*28/29#to account for leap year
usm[2,3] <- sum(us[(us$date >"2020-01-31" & us$date <"2020-03-01"),]$demand)/sum(us[(us$date >"2019-01-31" & us$date <"2019-03-01"),]$demand)*28/29 #to account for leap year
usm[3,2] <- sum(us[(us$date >"2020-02-29" & us$date <"2020-04-01"),]$emi)/sum(us[(us$date >"2019-02-28" & us$date <"2019-04-01"),]$emi)
usm[3,3] <- sum(us[(us$date >"2020-02-29" & us$date <"2020-04-01"),]$demand)/sum(us[(us$date >"2019-02-28" & us$date <"2019-04-01"),]$demand)
usm[4,2] <- sum(us[(us$date >"2020-03-31" & us$date <"2020-05-01"),]$emi)/sum(us[(us$date >"2019-03-31" & us$date <"2019-05-01"),]$emi)
usm[4,3] <- sum(us[(us$date >"2020-03-31" & us$date <"2020-05-01"),]$demand)/sum(us[(us$date >"2019-03-31" & us$date <"2019-05-01"),]$demand)
usm[5,2] <- sum(us[(us$date >"2020-04-30" & us$date <"2020-06-01"),]$emi)/sum(us[(us$date >"2019-04-30" & us$date <"2019-06-01"),]$emi)
usm[5,3] <- sum(us[(us$date >"2020-04-30" & us$date <"2020-06-01"),]$demand)/sum(us[(us$date >"2019-04-30" & us$date <"2019-06-01"),]$demand)
usm[6,2] <- sum(us[(us$date >"2020-05-31" & us$date <"2020-07-01"),]$emi,na.rm=T)/sum(us[(us$date >"2019-05-31" & us$date <"2019-07-01"),]$emi)
usm[6,3] <- sum(us[(us$date >"2020-05-31" & us$date <"2020-07-01"),]$demand)/sum(us[(us$date >"2019-05-31" & us$date <"2019-07-01"),]$demand)
usm[7,2] <- sum(us[(us$date >"2020-06-30" & us$date <"2020-08-01"),]$emi,na.rm=T)/sum(us[(us$date >"2019-06-30" & us$date <"2019-08-01"),]$emi)
usm[7,3] <- sum(us[(us$date >"2020-06-30" & us$date <"2020-08-01"),]$demand)/sum(us[(us$date >"2019-06-30" & us$date <"2019-08-01"),]$demand)
usm[8,2] <- sum(us[(us$date >"2020-07-31" & us$date <"2020-09-01"),]$emi,na.rm=T)/sum(us[(us$date >"2019-07-31" & us$date <"2019-09-01"),]$emi)
usm[8,3] <- sum(us[(us$date >"2020-07-31" & us$date <"2020-09-01"),]$demand)/sum(us[(us$date >"2019-07-31" & us$date <"2019-09-01"),]$demand)
usm[9,2] <- sum(us[(us$date >"2020-08-31" & us$date <"2020-10-01"),]$emi,na.rm=T)/sum(us[(us$date >"2019-08-31" & us$date <"2019-10-01"),]$emi)
usm[9,3] <- sum(us[(us$date >"2020-08-31" & us$date <"2020-10-01"),]$demand)/sum(us[(us$date >"2019-08-31" & us$date <"2019-10-01"),]$demand)


#data frame to store indexed generation numbers for US coal and gas power generation
uscg <- data.frame(month = c("jan","feb","mar","apr","may","jun","jul","aug","sep"),
                   gas = rep(0,9),coal = rep(0,9))
uscg[1,2] <- sum(us[(us$date >"2019-12-31" & us$date <"2020-02-01"),]$gas)/sum(us[(us$date >"2018-12-31" & us$date <"2019-02-01"),]$gas)
uscg[1,3] <- sum(us[(us$date >"2019-12-31" & us$date <"2020-02-01"),]$coal)/sum(us[(us$date >"2018-12-31" & us$date <"2019-02-01"),]$coal)
uscg[2,2] <- sum(us[(us$date >"2020-01-31" & us$date <"2020-03-01"),]$gas)/sum(us[(us$date >"2019-01-31" & us$date <"2019-03-01"),]$gas)*28/29#to account for leap year
uscg[2,3] <- sum(us[(us$date >"2020-01-31" & us$date <"2020-03-01"),]$coal)/sum(us[(us$date >"2019-01-31" & us$date <"2019-03-01"),]$coal)*28/29#to account for leap year
uscg[3,2] <- sum(us[(us$date >"2020-02-29" & us$date <"2020-04-01"),]$gas)/sum(us[(us$date >"2019-02-28" & us$date <"2019-04-01"),]$gas)
uscg[3,3] <- sum(us[(us$date >"2020-02-29" & us$date <"2020-04-01"),]$coal)/sum(us[(us$date >"2019-02-28" & us$date <"2019-04-01"),]$coal)
uscg[4,2] <- sum(us[(us$date >"2020-03-31" & us$date <"2020-05-01"),]$gas)/sum(us[(us$date >"2019-03-31" & us$date <"2019-05-01"),]$gas)
uscg[4,3] <- sum(us[(us$date >"2020-03-31" & us$date <"2020-05-01"),]$coal)/sum(us[(us$date >"2019-03-31" & us$date <"2019-05-01"),]$coal)
uscg[5,2] <- sum(us[(us$date >"2020-04-30" & us$date <"2020-06-01"),]$gas)/sum(us[(us$date >"2019-04-30" & us$date <"2019-06-01"),]$gas)
uscg[5,3] <- sum(us[(us$date >"2020-04-30" & us$date <"2020-06-01"),]$coal)/sum(us[(us$date >"2019-04-30" & us$date <"2019-06-01"),]$coal)
uscg[6,2] <- sum(us[(us$date >"2020-05-31" & us$date <"2020-07-01"),]$gas,na.rm=T)/sum(us[(us$date >"2019-05-31" & us$date <"2019-07-01"),]$gas)
uscg[6,3] <- sum(us[(us$date >"2020-05-31" & us$date <"2020-07-01"),]$coal)/sum(us[(us$date >"2019-05-31" & us$date <"2019-07-01"),]$coal,na.rm=T)
uscg[7,2] <- sum(us[(us$date >"2020-06-30" & us$date <"2020-08-01"),]$gas,na.rm=T)/sum(us[(us$date >"2019-06-30" & us$date <"2019-08-01"),]$gas)
uscg[7,3] <- sum(us[(us$date >"2020-06-30" & us$date <"2020-08-01"),]$coal)/sum(us[(us$date >"2019-06-30" & us$date <"2019-08-01"),]$coal)
uscg[8,2] <- sum(us[(us$date >"2020-07-31" & us$date <"2020-09-01"),]$gas,na.rm=T)/sum(us[(us$date >"2019-07-31" & us$date <"2019-09-01"),]$gas)
uscg[8,3] <- sum(us[(us$date >"2020-07-31" & us$date <"2020-09-01"),]$coal)/sum(us[(us$date >"2019-07-31" & us$date <"2019-09-01"),]$coal)
uscg[9,2] <- sum(us[(us$date >"2020-08-31" & us$date <"2020-10-01"),]$gas,na.rm=T)/sum(us[(us$date >"2019-08-31" & us$date <"2019-10-01"),]$gas)
uscg[9,3] <- sum(us[(us$date >"2020-08-31" & us$date <"2020-10-01"),]$coal)/sum(us[(us$date >"2019-08-31" & us$date <"2019-10-01"),]$coal)


## scatter plot for demand reduction vs. emission reduction
red_dem <- red_dem %>% rename(demand=value)
red_dem <- full_join(red_dem,red %>% select(-ProductionType))
big_eu <- c("Germany","Poland","Italy","United Kingdom","Spain","Czech Republic","Netherlands")#,"Bulgaria","Greece","France")
ggplot()+
  geom_abline(slope=1)+
   # geom_point(dat=df_to_write,aes(x=demand,y=value,color=AreaName,shape=period))+
  geom_point(dat=red_dem[red_dem$AreaName %in% big_eu & red_dem$period != "av18",],aes(x=demand,y=value,color=AreaName,shape=period))+
  geom_point(dat=usm,aes(x=demand,y=value,shape=period),color="black")+
  geom_point(dat=ind,aes(x=demand,y=value,shape=period),color="red")+
  coord_cartesian(xlim=c(0.5,1.3),ylim=c(0.5,1.3)) + 
  theme_bw() +
  scale_shape_manual(values=c(1:9))+
  ylab("Power sector emissions (2019 = 1)")+
  xlab("Demand (2019=1)")
ggsave("Fig2.png")
ggsave("Fig2.pdf",width=6,height=5)

df_to_write <- bind_rows(red_dem[red_dem$AreaName %in% big_eu& red_dem$period != "av18",],usm %>% mutate(AreaName="USA"),ind %>% mutate(AreaName="IND"))
write.table(x = "*Data shown in Figure 2 (see paper for data sources); 
*'AreaName' denotes the countries (showing 7 largest European emitters plus India and USA)
*'period' denotes the month (Jan-Sep)
*'demand' denotes the demand in 2020 relative to the same month in 2019
*'value' denotes the CO2 emissions in 2020 relative to the same month in 2019",file = "Fig2.csv",row.names = F,quote = F)
write.table(df_to_write,file = "Fig2.csv",row.names = F,quote = F,append = T,sep=",")
