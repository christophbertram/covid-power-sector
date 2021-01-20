# file with daily emission model
# 1. creates a set of scenarios for daily emissions by country and sector for 2020, based on confinement index methodology from Le Quere et al. 2020
#    The set is ultimately stored in dt_all, with following variables:
#       $ REGION_CODE - 144 countries as in Le Quere et al.  based on ISO 3 codes, plus "GLOBAL" and "EUandUK" for world and EU 27+UK data respectively
#       $ variable  - 6 different sectors (power, industry, surface transport, public, residential, aviation), plus total CO2
#       $ date - date in format yyyy-mm-dd
#       $ scenario - 28 scenarios: "constant" = 2019 average daily emissions
#                      + 27 scenarios, existing from 9 different "specification" (see below), each with a low, medium and high sensitivity to capture uncertainty
#                        "S" versions ("S"cenario): three specifications correspond to the original paper scenarios: emi_s1_low/med/high, emis_s2_low/med/high and emis_s3_low/med/high
#                             these have different assumptions on evolution of confinement index: 1: all countries back to zero by mid-June, 2: all countries extend confinement until end-of July, 3: as 2, but with CI=1 for the rest of year after July
#                        "N" versions ("N"ew): three specifications based on original scenario 3, with changed reduction matrix: emi_n1_lmh, emi_n2_lmh, emi_n3_lmh
#                             these 3 have different assumptions on elasticity of power emissiosn in confinement index 2 and 3: 1: low elasticity, 2: high elasticity, 3: medium elasticity
#                        "B" versions ("B"est guess): as "N" versions, but corrected for India, European and US power data corresponding to real data
#                              as "n", existing for all three variants, although only the high sensitivity (emi_b3_low/medium/high) case is used in paper, as this matched best observed data.
# 2. after line 375, the data is used for a couple of plots and for calculating the numbers cited in the paper
#


#temporal scope: all days in 2020 and 2021
y20_21 <- as.Date(seq(18262,18262+365+365),origin="1970-01-01")

# https://www.icos-cp.eu/gcp-covid19 Data file 1
# Emissions per day by sector (and total)
lq <- read.csv2("other_data/LeQuere.csv")
lq <- lq %>% gather(variable, value,-REGION_ID,-REGION_CODE,-REGION_NAME) %>% filter(REGION_CODE!="None")
lq$REGION_ID <- NULL
lq %>% filter(REGION_CODE=="DEU")

map <- lq %>% select(REGION_CODE,REGION_NAME)

# https://www.icos-cp.eu/gcp-covid19 Data file 5
# Scenario 1. CI until 11 June. Thereafter, deconfinement begins, i.e., CI=0
ci <- read.csv2("other_data/Confinement_index_11June2020.csv")
ci <- ci %>% gather(REGION_NAME,ci,-Date)
ci$date <- as.Date(ci$Date,tryFormats = c("%d.%m.%Y"))
ci$Date <- NULL
ci$REGION_NAME <- gsub(ci$REGION_NAME,pattern = "\\.",replacement = " ")
ci$REGION_NAME <- as.factor(ci$REGION_NAME)

# creating 3 different confinement index scenarios, 
# corresponding to the 3 scenarios mentioned in Le Quere et al.

# Scenario 2. Extension of confinement - Confinement indexes (on 11 June) are extended until end of July, after which CI=0
ci2 <- ci
tmp <- ci2[ci2$date=="2020-06-11",]
for(i in y20_21[y20_21>"2020-06-11" & y20_21<"2020-08-01"]){
  tmp$date <- as.Date(i,origin="1970-01-01")
  ci2 <- rbind(ci2,tmp)
}


# Scenario 3. CI after end of July remains 1 until end of year 
ci3 <- ci2
for(i in y20_21[y20_21>"2020-07-31" & y20_21<"2021-01-01"]){
  tmp$date <- as.Date(i,origin="1970-01-01")
  tmp$ci <- 1
  ci3 <- rbind(ci3,tmp)
}

ci <- ci %>% rename(ci1 = ci)
ci2 <- ci2 %>% rename(ci2 = ci)
ci3 <- ci3 %>% rename(ci3 = ci)


for(original in c(TRUE,FALSE)){# True: original specifications for S1-S3,
  #False: new specifications, N1-N3 are only variants of S3 to test power sensitivity

if(isTRUE(original)){ #True: original specifications for S1-S3, 
  ci <- full_join(ci,ci2)
  ci <- full_join(ci,ci3)
} else {#False: new specification, N1-N3 are only variants of S3 to test power sensitivity
  
  #countries without confinement index - get CI 1 after 11 June until end of year
  no_data_lockdown <- setdiff(unique(lq$REGION_NAME),unique(ci$REGION_NAME))
  no_data_lockdown <- setdiff(no_data_lockdown,c("EU and UK","Global"))
  no_data_lockdown <- data.frame(REGION_NAME=no_data_lockdown,
                                 date=rep(y20_21[y20_21>"2020-06-11" & y20_21<"2021-01-01"],each=length(no_data_lockdown)),
                                 ci3=1)
  
  ci <- ci3 %>% rename(ci1 = ci3)
  ci2 <- ci3 %>% rename(ci2 = ci3)
  ci <- full_join(ci,ci2)
  ci <- full_join(ci,ci3)
  ci <- full_join(ci,no_data_lockdown)
  
   # first date when a country goes into confinement
  df <- data.frame(region=NULL,date=NULL)
  for ( i in unique(ci$REGION_NAME)){
  tmp <- data.frame(region=i,date=min(ci[ci$ci3>0 & ci$REGION_NAME==i,]$date,na.rm = T))
  df <- rbind(df,tmp)
  }

# anytime after first confinement, CI cannot fall to zero 
  for (i in unique(df$region)){
   for (j in y20_21[y20_21>df[df$region==i,]$date & y20_21<"2020-08-01"]){
    if (ci[ci$REGION_NAME==i & ci$date==as.Date(j),]$ci3==0)
      ci[ci$REGION_NAME==i & ci$date==as.Date(j),]$ci3 <- 1
    
    }
  }
  
}
  

dat <- lq

  dat %>% filter(REGION_CODE %in% c("GIB"))
  # get rid of Gibraltar, anyway only 0
  dat <- dat %>% filter(REGION_CODE != "GIB")
  
  #temporal scope: 2020 and 2021
  #y20_21 <- as.Date(seq(18262,18262+365+365),origin="1970-01-01")
  dt <- merge(dat,y20_21)
  colnames(dt)[5] <- "date"
  # dt <- dat %>% add_column(date=y20_21[1])
  # for (i in seq(2,731)){
  #   dt <- rbind(dt, dat %>% add_column(date=y20_21[i]))
  # }
  # 
  dt$value <- as.numeric(dt$value)
  
  #countries without CI
  setdiff(unique(dt$REGION_NAME),unique(ci$REGION_NAME))
  
  dt <- left_join(dt,ci)
  
  

  if(!isTRUE(original)){
  #special case China Industry and Power sector:
  # as these two sectors seem to be close to normal levels again, change ci to 0 for those on 1 out of 2 days for April -July:
  dates_1of2 <- as.Date(2*seq(0,57),origin="2020-04-09")
  # and for 2 out of 3 days for August - December
  dates_2of3 <- c(as.Date(3*seq(0,51),origin="2020-08-01"),as.Date(3*seq(0,51)+1,origin="2020-08-01"))
  dates_CHN <- c(dates_1of2,dates_2of3)
  #new assumption: all days from April onward are normal
  dates_CHN <- c(as.Date(seq(0,153+114),origin="2020-04-09"))
  dt[dt$REGION_CODE=="CHN" & dt$variable %in% c("POWER","INDUSTRY") & dt$date %in% dates_CHN,]$ci1 <- 0
  dt[dt$REGION_CODE=="CHN" & dt$variable %in% c("POWER","INDUSTRY") & dt$date %in% dates_CHN,]$ci2 <- 0
  dt[dt$REGION_CODE=="CHN" & dt$variable %in% c("POWER","INDUSTRY") & dt$date %in% dates_CHN,]$ci3 <- 0
  }
  
 
  # Accounting for range of reductions (low, med, high - called specifications) in different sectors
  # reduction matrix CI 0                 CI1                  CI2                     CI3 (from Table 2, main paper)
  # power - industry - surface transport - public - residential - aviation
  if(isTRUE(original)){   
  mat1 <- data.frame(variable=rep(unique(dt$variable)[seq(2,7)],4),ci1=c(rep(0,6),rep(1,6),rep(2,6),rep(3,6)),
                     med1=c(0,0,0,0,0,0,  0,-10,-10,-5,0,-20, -5,-15,-40,-22.5,0,-75, -15,-35,-50,-32.5,5,-75),
                     low1=c(0,0,0,0,0,0,  0,0,0,0,0,0,        0,0,-35,-5,-5,-55,         -5,-25,-40,-15,0,-60),
                     high1=c(0,0,0,0,0,0, 0,-20,-20,-10,0,-50, -15,-35,-45,-40,5,-95, -25,-45,-65,-50,10,-90))
  
  mat2 <- data.frame(variable=rep(unique(dt$variable)[seq(2,7)],4),ci2=c(rep(0,6),rep(1,6),rep(2,6),rep(3,6)),
                     med2=c(0,0,0,0,0,0,  0,-10,-10,-5,0,-20, -5,-15,-40,-22.5,0,-75, -15,-35,-50,-32.5,5,-75),
                     low2=c(0,0,0,0,0,0,  0,0,0,0,0,0,        0,0,-35,-5,-5,-55,         -5,-25,-40,-15,0,-60),
                     high2=c(0,0,0,0,0,0, 0,-20,-20,-10,0,-50, -15,-35,-45,-40,5,-95, -25,-45,-65,-50,10,-90))
  
  mat3 <- data.frame(variable=rep(unique(dt$variable)[seq(2,7)],4),ci3=c(rep(0,6),rep(1,6),rep(2,6),rep(3,6)),
                     med3=c(0,0,0,0,0,0,  0,-10,-10,-5,0,-20, -5,-15,-40,-22.5,0,-75, -15,-35,-50,-32.5,5,-75),
                     low3=c(0,0,0,0,0,0,  0,0,0,0,0,0,        0,0,-35,-5,-5,-55,         -5,-25,-40,-15,0,-60),
                     high3=c(0,0,0,0,0,0, 0,-20,-20,-10,0,-50, -15,-35,-45,-40,5,-95, -25,-45,-65,-50,10,-90))
} else {#alternative setting for ci3 : adjusting low specification for CI1 for all sectors, and power for all CIs, and res for low
# ci1 and ci2 are for varying power assumption in CI2 and CI3, to see which settings best lead to fit for India, US and DEU  
  mat1 <- data.frame(variable=rep(unique(dt$variable)[seq(2,7)],4),ci1=c(rep(0,6),rep(1,6),rep(2,6),rep(3,6)),
                     med1=c(0,0,0,0,0,0,  -7.5,-7.5,-10,-5,0,-45,   -10,-15,-40,-22.5,0,-75, -15,-35,-50,-32.5,5,-75),
                     low1=c(0,0,0,0,0,0,  -2.5,-2.5,0,-2.5,5,-30,    -5,0,-35,-5,-5,-55,      -10,-25,-40,-15,0,-60),
                     high1=c(0,0,0,0,0,0, -12.5,-12.5,-20,-10,0,-60,   -20,-35,-45,-40,5,-95,   -20,-45,-65,-50,10,-90))
  
  mat2 <- data.frame(variable=rep(unique(dt$variable)[seq(2,7)],4),ci2=c(rep(0,6),rep(1,6),rep(2,6),rep(3,6)),
                     med2=c(0,0,0,0,0,0,  -7.5,-7.5,-10,-5,0,-45,   -20,-15,-40,-22.5,0,-75, -25,-35,-50,-32.5,5,-75),
                     low2=c(0,0,0,0,0,0,  -2.5,-2.5,0,-2.5,5,-30,    -15,0,-35,-5,-5,-55,     -20,-25,-40,-15,0,-60),
                     high2=c(0,0,0,0,0,0, -12.5,-12.5,-20,-10,0,-60,   -30,-35,-45,-40,5,-95,  -30,-45,-65,-50,10,-90))
  
  mat3 <- data.frame(variable=rep(unique(dt$variable)[seq(2,7)],4),ci3=c(rep(0,6),rep(1,6),rep(2,6),rep(3,6)),
                     med3=c(0,0,0,0,0,0,  -7.5,-7.5,-10,-5,0,-45,  -15,-15,-40,-22.5,0,-75, -20,-35,-50,-32.5,5,-75),
                     low3=c(0,0,0,0,0,0,  -2.5,-2.5,0,-2.5,5,-30,   -10,0,-35,-5,-5,-55,      -15,-25,-40,-15,0,-60),
                     high3=c(0,0,0,0,0,0, -12.5,-12.5,-20,-10,0,-60, -25,-35,-45,-40,5,-95,   -25,-45,-65,-50,10,-90))
}
  
  dt <- left_join(dt,mat1)
  dt <- left_join(dt,mat2)
  dt <- left_join(dt,mat3)
  dt[is.na(dt$med1),]$med1 <- 0
  dt[is.na(dt$med2),]$med2 <- 0
  dt[is.na(dt$med3),]$med3 <- 0
  dt[is.na(dt$low1),]$low1 <- 0
  dt[is.na(dt$low2),]$low2 <- 0
  dt[is.na(dt$low3),]$low3 <- 0
  dt[is.na(dt$high1),]$high1 <- 0
  dt[is.na(dt$high2),]$high2 <- 0
  dt[is.na(dt$high3),]$high3 <- 0
  
  dt$REGION_NAME <- as.factor(dt$REGION_NAME)
  dt$variable <- as.factor(dt$variable)
  
  #names of original emission scenarios: s1-s3 are the scenarios 1-3 from paper, each with low, medium, high specification
  emis <- c("emi_s1_low","emi_s2_low","emi_s3_low",
            "emi_s1_med","emi_s2_med","emi_s3_med","emi_s1_high","emi_s2_high","emi_s3_high")
  #names of modified emission specifications: n1-n3 are the scenarios with new specifications
  emin <- c("emi_n1_low","emi_n2_low","emi_n3_low",
            "emi_n1_med","emi_n2_med","emi_n3_med","emi_n1_high","emi_n2_high","emi_n3_high")
  
  #calculate change in daily 2020 emissions: s123 for original specification, n123 for new specifications
  if(isTRUE(original)){
    dt <- dt %>% mutate(emi_s1_med = value * (1+ med1/100))
    dt <- dt %>% mutate(emi_s2_med = value * (1+ med2/100))
    dt <- dt %>% mutate(emi_s3_med = value * (1+ med3/100))
    dt <- dt %>% mutate(emi_s1_low = value * (1+ low1/100))
    dt <- dt %>% mutate(emi_s2_low = value * (1+ low2/100))
    dt <- dt %>% mutate(emi_s3_low = value * (1+ low3/100))
    dt <- dt %>% mutate(emi_s1_high = value * (1+ high1/100))
    dt <- dt %>% mutate(emi_s2_high = value * (1+ high2/100))
    dt <- dt %>% mutate(emi_s3_high = value * (1+ high3/100))
  } else {
    dt <- dt %>% mutate(emi_n1_med = value * (1+ med1/100))
    dt <- dt %>% mutate(emi_n2_med = value * (1+ med2/100))
    dt <- dt %>% mutate(emi_n3_med = value * (1+ med3/100))
    dt <- dt %>% mutate(emi_n1_low = value * (1+ low1/100))
    dt <- dt %>% mutate(emi_n2_low = value * (1+ low2/100))
    dt <- dt %>% mutate(emi_n3_low = value * (1+ low3/100))
    dt <- dt %>% mutate(emi_n1_high = value * (1+ high1/100))
    dt <- dt %>% mutate(emi_n2_high = value * (1+ high2/100))
    dt <- dt %>% mutate(emi_n3_high = value * (1+ high3/100))
  }
  
  # #re-tidy data
  dt <- dt %>% rename(constant=value)
  dt <- dt %>% rename(sector=variable)
  dt$med1 <- NULL
  dt$med2 <- NULL
  dt$med3 <- NULL
  dt$low1 <- NULL
  dt$low2 <- NULL
  dt$low3 <- NULL
  dt$high1 <- NULL
  dt$high2 <- NULL
  dt$high3 <- NULL
  
  
  dt <- dt %>% gather(variable, value, -REGION_CODE, -REGION_NAME, -sector, -date)
  
  #correct sectoral and regional aggregates (Total CO2, Global and EUandUK)
  totco2 <- dt %>% filter(sector != "TOTAL_CO2") %>% group_by(REGION_CODE,date,variable) %>% summarise(value=sum(value)) %>% ungroup()
  totco2$sector <- "TOTAL_CO2"
  dt$REGION_NAME <- NULL
  dt <- rbind(dt[dt$sector != "TOTAL_CO2",],totco2)
  
  glob <- 
    dt %>% filter(REGION_CODE != "GLOBAL",REGION_CODE!="EUandUK") %>% group_by(date,variable,sector) %>% summarise(value=sum(value)) %>% ungroup()
  glob$REGION_CODE <- "GLOBAL"
  dt <- rbind(dt[dt$REGION_CODE!="GLOBAL",],glob)
  
  euuk <- 
    dt %>% filter(REGION_CODE %in% c("AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
                                       "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
                                       "ROU","SVK","SVN","ESP","SWE","GBR")) %>% group_by(date,variable,sector) %>% summarise(value=sum(value)) %>% ungroup()
  euuk$REGION_CODE <- "EUandUK"
  dt <- rbind(dt[dt$REGION_CODE!="EUandUK",],euuk)
  
  dt <- dt %>% rename(scenario=variable)
  dt <- dt %>% rename(variable=sector)
  dt$scenario <- as.factor(dt$scenario)
  
  if(isTRUE(original)){#save data with old specification
    save(dt,file="dt_20200708.RData")
    #calculate yearly reductions for old specification - to cross-check consistency with original study results (minor deviations due to not disaggregating China and US, and updated CI table)
    reduc_orig <- dt %>% filter(variable=="TOTAL_CO2" & REGION_CODE == "GLOBAL" & date<"2021-01-01" & scenario %in% c(emis,"constant")) %>% 
      group_by(scenario) %>% summarise(value=sum(value)/36604-1) 
  } else {#save data with new specifications
    dt_new <- dt
    save(dt_new,file="dt_new_20200708.RData")
    ##calculate yearly reductions for new specification, but yet without corrections for power
    reduc_new <- dt_new %>% filter(variable=="TOTAL_CO2" & REGION_CODE == "GLOBAL" & date<"2021-01-01" & scenario %in% c(emin,"constant")) %>% 
      group_by(scenario) %>% summarise(value=sum(value)/36604-1) 
    
    #####  3rd "B"est guess - using new specification, and correcting monthly values for India, US and Europe
    dt_adj <- dt_new
    
    #rename scenario names
    emic <- c("emi_b1_low","emi_b2_low","emi_b3_low",
              "emi_b1_med","emi_b2_med","emi_b3_med","emi_b1_high","emi_b2_high","emi_b3_high")
    
    dt_adj$scenario <- factor(dt_adj$scenario, levels=c(levels(dt_adj$scenario),emic))
    
    dt_adj[dt_adj$scenario=="emi_n1_low",]$scenario <- emic[1]
    dt_adj[dt_adj$scenario=="emi_n2_low",]$scenario <- emic[2]
    dt_adj[dt_adj$scenario=="emi_n3_low",]$scenario <- emic[3]
    
    dt_adj[dt_adj$scenario=="emi_n1_med",]$scenario <- emic[4]
    dt_adj[dt_adj$scenario=="emi_n2_med",]$scenario <- emic[5]
    dt_adj[dt_adj$scenario=="emi_n3_med",]$scenario <- emic[6]
    
    dt_adj[dt_adj$scenario=="emi_n1_high",]$scenario <- emic[7]
    dt_adj[dt_adj$scenario=="emi_n2_high",]$scenario <- emic[8]
    dt_adj[dt_adj$scenario=="emi_n3_high",]$scenario <- emic[9]
    
    
    ###manual corrections based on actual data
    
    #Indian power: monthly emission reductions, based on https://carbontracker.in/: 15% until 2020-06-16
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & dt_adj$date <"2020-02-01",]$value <-
      0.98*dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-01-31" & dt_adj$date <"2020-03-01"),]$value <-
      1.03*dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-02-29" & dt_adj$date <"2020-04-01"),]$value <-
      0.84*dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-03-31" & dt_adj$date <"2020-05-01"),]$value <- 
      0.68*dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-04-30" & dt_adj$date <"2020-06-01"),]$value <- 
      0.76*dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-05-31" & dt_adj$date <"2020-07-01"),]$value <- 
      0.81*dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-06-30" & dt_adj$date <"2020-08-01"),]$value <- 
      0.94*dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-07-31" & dt_adj$date <"2020-09-01"),]$value <- 
      0.96*dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-08-31" & dt_adj$date <"2020-10-01"),]$value <- 
      1.07*dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="IND" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
    
    #read in EIA generation data:https://www.eia.gov/beta/electricity/gridmonitor/dashboard/custom/pending  https://www.eia.gov/realtime_grid/sixMonthFiles/EIA930_BALANCE_2020_Jan_Jun.csv
    # eia <- read.csv("EIA930_BALANCE_2020_Jan_Jun.csv")
    # eia <- eia %>% select(Balancing.Authority,Data.Date,Hour.Number,Demand..MW.,Net.Generation..MW..from.Coal,Net.Generation..MW..from.Natural.Gas)
    # eia$Demand..MW. <- as.numeric(as.character(eia$Demand..MW.))
    # https://www.eia.gov/opendata/qb.php?category=3390105
    # https://www.eia.gov/opendata/qb.php?category=3390105&sdid=EBA.US48-ALL.NG.COL.H
    # https://www.eia.gov/opendata/qb.php?category=3390105&sdid=EBA.US48-ALL.NG.NG.H
    # https://www.eia.gov/opendata/qb.php?category=3390105&sdid=EBA.US48-ALL.NG.OIL.H
    usc <- read.csv("other_data/Net_generation_from_coal_for_United_States_Lower_48_(region)_hourly_-_UTC_time.csv",skip = 5,col.names=c("date","hour","coal"))
    usg <- read.csv("other_data/Net_generation_from_natural_gas_for_United_States_Lower_48_(region)_hourly_-_UTC_time.csv",skip = 5,col.names=c("date","hour","gas"))
    uso <- read.csv("other_data/Net_generation_from_petroleum_for_United_States_Lower_48_(region)_hourly_-_UTC_time.csv",skip = 5,col.names=c("date","hour","oil"))
    us <- full_join(usc,usg)
    us <- full_join(us,uso)
    us <- us %>% mutate(emi = 0.85*coal+0.42*gas+0.89*oil)
    us$date <- as.Date(us$date,tryFormats = c("%m/%d/%Y"))
    
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & dt_adj$date <"2020-02-01",]$value <- 
      sum(us[(us$date >"2019-12-31" & us$date <"2020-02-01"),]$emi)/sum(us[(us$date >"2018-12-31" & us$date <"2019-02-01"),]$emi) * #0.82
      dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-01-31" & dt_adj$date <"2020-03-01"),]$value <-  
      sum(us[(us$date >"2020-01-31" & us$date <"2020-03-01"),]$emi)/sum(us[(us$date >"2019-01-31" & us$date <"2019-03-01"),]$emi) * #0.89
      dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-02-29" & dt_adj$date <"2020-04-01"),]$value <- 
      sum(us[(us$date >"2020-02-29" & us$date <"2020-04-01"),]$emi)/sum(us[(us$date >"2019-02-28" & us$date <"2019-04-01"),]$emi) * #0.83
      dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-03-31" & dt_adj$date <"2020-05-01"),]$value <- 
      sum(us[(us$date >"2020-03-31" & us$date <"2020-05-01"),]$emi)/sum(us[(us$date >"2019-03-31" & us$date <"2019-05-01"),]$emi) * #0.85
      dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-04-30" & dt_adj$date <"2020-06-01"),]$value <- 
      sum(us[(us$date >"2020-04-30" & us$date <"2020-06-01"),]$emi)/sum(us[(us$date >"2019-04-30" & us$date <"2019-06-01"),]$emi) * #0.8
      dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-05-31" & dt_adj$date <"2020-07-01"),]$value <- 
      sum(us[(us$date >"2020-05-30" & us$date <"2020-06-30"),]$emi)/sum(us[(us$date >"2019-05-31" & us$date <"2019-07-01"),]$emi) * #0.94
      dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-06-30" & dt_adj$date <"2020-08-01"),]$value <- 
      sum(us[(us$date >"2020-06-30" & us$date <"2020-07-31"),]$emi)/sum(us[(us$date >"2019-06-30" & us$date <"2019-08-01"),]$emi) * #0.94
      dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value 
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-07-31" & dt_adj$date <"2020-09-01"),]$value <- 
      sum(us[(us$date >"2020-07-31" & us$date <"2020-08-31"),]$emi)/sum(us[(us$date >"2019-07-31" & us$date <"2019-08-31"),]$emi) * #0.94
      dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value 
    dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & (dt_adj$date >"2020-08-31" & dt_adj$date <"2020-10-01"),]$value <- 
      sum(us[(us$date >"2020-08-31" & us$date <"2020-09-30"),]$emi)/sum(us[(us$date >"2019-08-31" & us$date <"2019-09-30"),]$emi) * #0.94
      dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE =="USA" & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value 
    
    
    # take entsoe monthly emission reductions data to correct european power system emissions. Germany has been done before, high level of accuracy (only longer time span in entsoe data)
    load("red_entso-e.RData")
    #croatia, with reported emissions of zero in ENTSO-E, have INF in aug and sep, so set these to NaN (as in previous month)
    red[is.infinite(red$value),]$value <- NaN
    countries <- setdiff(red$AreaName,c("Sweden","Switzerland","Montenegro",setdiff(red$AreaName,map$REGION_NAME),"Germany")) # exclude countries without match, without dat, and Germany
    for(c in countries){
      dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & dt_adj$date <"2020-02-01",]$value <- 
        red[red$AreaName==c&red$period=="jan",]$value*
        dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
      dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & (dt_adj$date >"2020-01-31" & dt_adj$date <"2020-03-01"),]$value <- 
        red[red$AreaName==c&red$period=="feb",]$value*
        dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
      dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & (dt_adj$date >"2020-02-29" & dt_adj$date <"2020-04-01"),]$value <- 
        red[red$AreaName==c&red$period=="mar",]$value*
        dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
      dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & (dt_adj$date >"2020-03-31" & dt_adj$date <"2020-05-01"),]$value <- 
        red[red$AreaName==c&red$period=="apr",]$value*
        dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
      dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & (dt_adj$date >"2020-04-30" & dt_adj$date <"2020-06-01"),]$value <- 
        red[red$AreaName==c&red$period=="may",]$value*
        dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
      dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & (dt_adj$date >"2020-05-31" & dt_adj$date <"2020-07-01"),]$value <- 
        red[red$AreaName==c&red$period=="jun",]$value*
        dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
      dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & (dt_adj$date >"2020-06-30" & dt_adj$date <"2020-08-01"),]$value <- 
        red[red$AreaName==c&red$period=="jul",]$value*
        dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
      dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & (dt_adj$date >"2020-07-31" & dt_adj$date <"2020-09-01"),]$value <- 
        red[red$AreaName==c&red$period=="aug",]$value*
        dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
      dt_adj[dt_adj$scenario %in% emic & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & (dt_adj$date >"2020-08-31" & dt_adj$date <"2020-10-01"),]$value <- 
        red[red$AreaName==c&red$period=="sep",]$value*
        dt_adj[dt_adj$scenario =="constant" & dt_adj$REGION_CODE ==map[map$REGION_NAME==c,]$REGION_CODE & dt_adj$variable=="POWER" & dt_adj$date == "2020-01-01",]$value  
      
    }
    
    
    #correct sectoral and regional aggregates (Total CO2, Global and EUandUK)
    totco2 <- dt_adj %>% filter(variable != "TOTAL_CO2") %>% group_by(REGION_CODE,date,scenario) %>% summarise(value=sum(value)) %>% ungroup()
    totco2$variable <- "TOTAL_CO2"
    dt_adj <- rbind(dt_adj[dt_adj$variable != "TOTAL_CO2",],totco2)
    
    glob <- 
      dt_adj %>% filter(REGION_CODE != "GLOBAL",REGION_CODE!="EUandUK") %>% group_by(date,scenario,variable) %>% summarise(value=sum(value,na.rm = T)) %>% ungroup()
    glob$REGION_CODE <- "GLOBAL"
    dt_adj <- rbind(dt_adj[dt_adj$REGION_CODE!="GLOBAL",],glob)
    
    euuk <- 
      dt_adj %>% filter(REGION_CODE %in% c("AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU",
                                           "GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT",
                                           "ROU","SVK","SVN","ESP","SWE","GBR")) %>% group_by(date,scenario,variable) %>% summarise(value=sum(value,na.rm = T)) %>% ungroup()
    euuk$REGION_CODE <- "EUandUK"
    dt_adj <- rbind(dt_adj[dt_adj$REGION_CODE!="EUandUK",],euuk)
    
    
    reduc_best <- dt_adj %>% filter(variable=="TOTAL_CO2" & REGION_CODE == "GLOBAL" & date<"2021-01-01" & scenario %in% c(emic,"constant")) %>% 
      group_by(scenario) %>% summarise(value=sum(value)/36604-1) 
    dt_best <- dt_adj
    save(dt_best,file="dt_best_20200708.RData")  
    
    
  }#end original=FALSE statement: saving of new specification and best guess after corrections
  
  
}#end of for loop for(original in c("TRUE","FALSE"))


### combine all files, save all scenarios in dt_all, and delete other dataframes
load("dt_20200708.RData")
 dt_all <- rbind(dt_best,dt[dt$scenario %in% emis,],dt_new[dt_new$scenario %in% emin,])
 save(dt_all,file="dt_all_20200708.RData")
 rm(dt_adj,dt_best,dt_new,dt)

 
#plots for supplementary material

 # fraction of global CO2 emissions
emi <- dt_all %>% 
  filter(scenario %in% c("constant","ci3"),date<"2021-01-01",variable!="TOTAL_CO2",!REGION_CODE %in% c("EUandUK","GLOBAL")) %>% 
  spread(scenario,value=value) %>% 
  mutate(fraction_v=constant/100.01) %>% 
  group_by(date,ci3,REGION_CODE) %>% 
  summarise(value=sum(fraction_v)) %>% 
  ungroup() %>% 
  filter(!is.na(ci3),ci3!=0) %>% 
  data.frame()

emi2 <- emi %>% group_by(ci3,date) %>% summarise(value=sum(value))%>%ungroup()

# Global
test <- dt_all %>% filter(REGION_CODE=="GLOBAL" & scenario %in% c("emi_b2_med","emi_b2_low","emi_b2_high","emi_s3_med"))
str(test)
test <- test %>% group_by(variable,scenario) %>% 
  mutate(smooth=rollmean(value,11,fill="extend")) %>% 
  ungroup() %>% 
  group_by(variable) %>% 
  mutate(perc=ifelse(variable!="RESIDENTIAL",smooth/max(smooth)*100,smooth/5.7*100)) %>% 
  ungroup()

ggplot()+
  geom_line(data=test[test$date<"2020-12-30" & test$scenario!="emi_s3_med",],aes(x=date,y=perc,color=scenario))+
  geom_line(data=test[test$date<"2020-12-30" & test$scenario=="emi_s3_med",],aes(x=date,y=perc),color="black")+
    facet_wrap(~variable,scales="free_y")+theme_bw()
ggsave("SuppFigS1.pdf",width=6,height=5)  


#figure on India, US and EU power generation, comparing monthly data with old and new specification for daily CI model
# test <- dt_all %>% filter(REGION_CODE %in% c("IND","USA","EUandUK"),variable=="POWER", 
#             scenario %in% c("emi_n2_med","emi_n2_low","emi_n2_high","emi_b2_med","emi_s3_low","emi_s3_med","emi_s3_high"), date < "2020-08-25") 

for(j in c(1,3,2)){
test <- dt_all %>% filter(REGION_CODE %in% c("IND","USA","EUandUK"),
                          variable=="POWER", 
                          scenario %in% grep(pattern = paste0("n",j,"|s3|b2_med"),x = unique(dt_all$scenario),value = T),
                          date < "2020-10-01") 


test %>% group_by(REGION_CODE,scenario) %>% summarise(value=sum(value))
#normalise values to 2019 mean

for (i in c("EUandUK","IND","USA")){
test[test$REGION_CODE==i,]$value <- test[test$REGION_CODE==i,]$value /
  dt_all[dt_all$REGION_CODE==i & dt_all$variable=="POWER" & dt_all$scenario=="constant" & dt_all$date=="2020-01-01",]$value 
}

ggplot()+
  # geom_vline(xintercept = as.Date("2020-06-16"),color="grey")+
  geom_line(data=test %>% filter(date<"2020-12-30" & scenario!="emi_b2_med"),aes(x=date,y=value,color=scenario,linetype=scenario))+
  geom_line(data=test %>% filter(date<"2020-12-30" & scenario=="emi_b2_med"),aes(x=date,y=value),color="black")+
scale_linetype_manual(values=c(rep("solid",3),rep("dashed",3)))+
    facet_wrap(~REGION_CODE) + theme_bw()
}
ggsave("SuppFigS2.pdf",width=6,height=5)  

###########


#display reduction numbers quoted in SI

dt_all %>% filter(variable=="TOTAL_CO2" & REGION_CODE == "GLOBAL" & date<"2021-01-01" & scenario %in% c("emi_b2_low",
                                                                                                        "emi_b2_med","emi_b2_high","constant")) %>% 
  group_by(scenario) %>% summarise(value=sum(value)/36604-1) 

dt_all %>% filter(variable=="POWER" & REGION_CODE == "GLOBAL" & date<"2021-01-01" & scenario %in% c("emi_b2_low",
                                                                                                    "emi_b2_med","emi_b2_high","constant")) %>% 
  group_by(scenario) %>% summarise(value=sum(value)/16238-1) 
