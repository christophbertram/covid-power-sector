### file to estimate 2020-2024 power sector emissions

#power sector CO2 emissions from WEO
weo <- data.frame(region=rep("World"),
                  period=rep(seq(2011,2018),each=4),
                    variable=rep(c("Total","Coal","Oil","Gas"),8),
#https://www.iea.org/data-and-statistics?country=WORLD&fuel=CO2%20emissions&indicator=CO2%20emissions%20from%20electricity%20and%20heat%20by%20energy%20source
                   # 1990,7592,5003,1221,1368,MtCO2
                   # 1995,8108,5547,1086,1475,MtCO2
                   # 2000,9301,6452,1093,1756,MtCO2
                   # 2005,10912,7790,954,2168,MtCO2
                   # 2010,12412,8941,844,2627,MtCO2
                   # 2015,13273,9577,843,2853,MtCO2
          #2010 (WEO2019:)12413,8942,844,2627,
                  value=c(12954,9436,888,2630,#2011,WEO2013
                          13238,9547,948,2742,#2012,WEO2014
                          13441,9781,901,2760,#2013,WEO2015
                          13496,9899,868,2729,#2014,WEO2016
                          13422,9714,873,2834,#2015,WEO2017
                          13353,9527,862,2963,#2016,WEO2017
                          13445,9759,714,2972,#2017,WEO2019
                          13818,10066,692,3060))#2018,WEO2019
                          
weo_con <- data.frame(region=rep(c("US","EU","India","China"),each=8),
                     period=rep(c(2017,2018),each=4,times=4),
                     variable=rep(c("Total","Coal","Oil","Gas"),8),value=
                       c(1807, 1240, 23, 544, # 2017 US
                         1828, 1178, 30, 621, # 2018 US
                         1071, 731, 50, 290, # 2017 EU
                         1023, 694, 49, 280, # 2018 EU
                         1099, 1040, 26, 33, # 2017, India
                         1146, 1095, 18, 33, # 2018, India
                         4585, 4465, 22, 98, # 2017, China
                         4893, 4749, 24, 120)) # 2018, China

weo <- rbind(weo,weo_con)


min(weo[weo$variable=="Total",]$value)/max(weo[weo$variable=="Total",]$value)

# tidying datasheets containing generation for oil, gas, and oil
co <- co %>% rename(region=1) %>%
  gather(2:39,key = "period",value="value") %>%
  mutate(variable="CoalGen") %>%
  mutate(period=gsub("2019...36",replacement = "2019",x = period))

oi <- oi %>% rename(region=1) %>%
  gather(2:39,key = "period",value="value") %>%
  mutate(variable="OilGen") %>%
  mutate(period=gsub("2019...36",replacement = "2019",x = period))

ga <- ga %>% rename(region=1) %>%
  gather(2:39,key = "period",value="value") %>%
  mutate(variable="GasGen") %>%
  mutate(period=gsub("2019...36",replacement = "2019",x = period))

bp_gen <- rbind(co,oi,ga) %>% 
  filter(region %in% c("Total World","India","US","European Union","China"), 
         !period %in% c("2019...37","2008-18","2019...39")) %>% 
  mutate(region=gsub( "Total World", "World",region)) %>% 
  mutate(region=gsub("European Union", "EU",region))

# #combine data and calculate intensities                
comb <- rbind(bp_gen,weo) %>% spread(variable,value) %>% mutate(int_c = Coal/CoalGen,int_g = Gas/GasGen,int_o = Oil/OilGen)

#calculate 2019 total emissions based on 2019 generation and 2018 intensities
comb[comb$period==2019,]$Coal <- comb[comb$period==2019,]$CoalGen * comb[comb$period==2018,]$int_c
comb[comb$period==2019,]$Gas <- comb[comb$period==2019,]$GasGen * comb[comb$period==2018,]$int_g
comb[comb$period==2019,]$Oil <- comb[comb$period==2019,]$OilGen * comb[comb$period==2018,]$int_o
comb[comb$period==2019,]$Total <- comb[comb$period==2019,]$Coal + comb[comb$period==2019,]$Gas + comb[comb$period==2019,]$Oil

comb <- comb %>% gather(variable,value,-region,-period)

# Projection of emissions based on different assumptions 
traj <- comb %>% filter(variable=="Total")
tmp <- traj[traj$period %in% seq(2011,2015),]
tmp[,2] <- rep(seq(2020,2024),5) 
traj <- rbind(traj,tmp)
traj$scen <- "med" #medium assumptions
tmp <- traj
tmp$scen <- "low" # only coal reduced
traj <- rbind(traj,tmp)
tmp$scen <- "high" # only gas reduced
traj <- rbind(traj,tmp)
tmp$scen <- "verylow" #higher lowcarbon growth or lower demand growths: -200/-400/-600/-800 residual in 2021/2022/2023/2024
traj <- rbind(traj,tmp)
tmp$scen <- "veryhigh" #lower lowcarbon growth or higher demand/GDP growth: +200/+400/+600/+800 residual in 2021/2022/2023/2024
traj <- rbind(traj,tmp)

traj$period <- as.numeric(traj$period)

#high assumption: (constant) average (of all fossil fuels) intensity from 2019 times fossil generation projection
traj[traj$region=="World" & traj$scen=="high" & traj$period %in% seq(2020,2024),]$value <- 
  comb[comb$region=="World" & comb$variable=="Total"&comb$period==2019,]$value/(bp[bp$period == 2019 & bp$region=="Total World" & bp$variable=="SE|Electricity",]$value-
                                                           bp[bp$period ==2019 & bp$region=="Total World" & bp$variable=="Generation|LowC",]$value)*
  ((bp[bp$period %in% seq(2020,2024) & bp$region=="Total World" & bp$variable=="SE|Electricity",]$value-
  bp[bp$period %in% seq(2020,2024) & bp$region=="Total World" & bp$variable=="Generation|LowC",]$value))

#low assumption: Gas emissions remain constant, any reduction in projected fossil generation
# comes from not using coal
traj[traj$region=="World" & traj$scen=="low" & traj$period %in% seq(2020,2024),]$value <- 
  #so constant emissions from gas
  comb[comb$region=="World" & comb$variable=="Total"&comb$period==2019,]$value-
  #-coal emissions intensity in 2018 multiplied by loss of fossil generation
  comb[comb$region=="World" & comb$variable =="int_c" & comb$period==2018,]$value*
  (-(bp[bp$period %in% seq(2020,2024) & bp$region=="Total World" & bp$variable=="SE|Electricity",]$value-
     bp[bp$period %in% seq(2020,2024) & bp$region=="Total World" & bp$variable=="Generation|LowC",]$value)+
     rep((bp[bp$period == 2019 & bp$region=="Total World" & bp$variable=="SE|Electricity",]$value-
            bp[bp$period == 2019 & bp$region=="Total World" & bp$variable=="Generation|LowC",]$value),5))



#medium assumption: most reduction by coal, but not all: displaced generation has emission intensity
# calculated as average between coal and current fossil generation mix
traj[traj$region=="World" & traj$scen=="med" & traj$period %in% seq(2020,2024),]$value <- 
  #so total emissions in 2019
  comb[comb$region=="World" & comb$variable=="Total"&comb$period==2019,]$value-
  #-average emissions intensity (coal and fossil) in 2018 multiplied by loss of fossil generation
  (comb[comb$region=="World" & comb$variable =="int_c" & comb$period==2018,]$value +
     (comb[comb$region=="World" & comb$variable =="Total" & comb$period==2019,]$value)/
     (  comb[comb$region=="World" & comb$variable =="CoalGen" & comb$period==2019,]$value +  
      comb[comb$region=="World" & comb$variable =="GasGen" & comb$period==2019,]$value+ 
      comb[comb$region=="World" & comb$variable =="OilGen" & comb$period==2019,]$value))/2*
  (-(bp[bp$period %in% seq(2020,2024) & bp$region=="Total World" & bp$variable=="SE|Electricity",]$value-
        bp[bp$period %in% seq(2020,2024) & bp$region=="Total World" & bp$variable=="Generation|LowC",]$value)+
     rep((bp[bp$period == 2019 & bp$region=="Total World" & bp$variable=="SE|Electricity",]$value-
            bp[bp$period == 2019 & bp$region=="Total World" & bp$variable=="Generation|LowC",]$value),5))

#china
traj[traj$region=="China" & traj$scen=="med" & traj$period %in% seq(2020,2024),]$value <- 
  #so total emissions in 2019
  comb[comb$region=="China" & comb$variable=="Total"&comb$period==2019,]$value-
  #-average emissions intensity (coal and fossil) in 2018 multiplied by loss of fossil generation
  (comb[comb$region=="China" & comb$variable =="int_c" & comb$period==2018,]$value +
     (comb[comb$region=="China" & comb$variable =="Total" & comb$period==2019,]$value)/
     (  comb[comb$region=="China" & comb$variable =="CoalGen" & comb$period==2019,]$value +  
          comb[comb$region=="China" & comb$variable =="GasGen" & comb$period==2019,]$value+ 
          comb[comb$region=="China" & comb$variable =="OilGen" & comb$period==2019,]$value))/2*
  (-(bp[bp$period %in% seq(2020,2024) & bp$region=="China" & bp$variable=="SE|Electricity",]$value-
       bp[bp$period %in% seq(2020,2024) & bp$region=="China" & bp$variable=="Generation|LowC",]$value)+
     rep((bp[bp$period == 2019 & bp$region=="China" & bp$variable=="SE|Electricity",]$value-
            bp[bp$period == 2019 & bp$region=="China" & bp$variable=="Generation|LowC",]$value),5))


#verylow assumption: coal is reduced only, and higher lowcarb/ lower demand
traj[traj$region=="World" & traj$scen=="verylow" & traj$period %in% seq(2020,2024),]$value <- 
  #so constant emissions from gas
  comb[comb$region=="World" & comb$variable=="Total"&comb$period==2019,]$value-
  #-coal emissions intensity in 2018 multiplied by loss of fossil generation
  comb[comb$region=="World" & comb$variable =="int_c" & comb$period==2018,]$value*
  ((-(bp[bp$period %in% seq(2020,2024) & bp$region=="Total World" & bp$variable=="SE|Electricity",]$value-
       bp[bp$period %in% seq(2020,2024) & bp$region=="Total World" & bp$variable=="Generation|LowC",]$value)+
     rep((bp[bp$period == 2019 & bp$region=="Total World" & bp$variable=="SE|Electricity",]$value-
            bp[bp$period == 2019 & bp$region=="Total World" & bp$variable=="Generation|LowC",]$value),5))+
     # c(0,200,400,600,800))
    c(200,400,800,1200,1600))

#high assumption: gas is reduced only, and lower lowcarb/ higher demand/GDP
traj[traj$region=="World" & traj$scen=="veryhigh" & traj$period %in% seq(2020,2024),]$value <- 
  #so total emissions in 2019
  comb[comb$region=="World" & comb$variable=="Total"&comb$period==2019,]$value-
  #-gas emissions intensity in 2018 multiplied by loss of fossil generation
  (comb[comb$region=="World" & comb$variable =="Total" & comb$period==2019,]$value)/
  (  comb[comb$region=="World" & comb$variable =="CoalGen" & comb$period==2019,]$value +  
       comb[comb$region=="World" & comb$variable =="GasGen" & comb$period==2019,]$value+ 
       comb[comb$region=="World" & comb$variable =="OilGen" & comb$period==2019,]$value)*
  ((-(bp[bp$period %in% seq(2020,2024) & bp$region=="Total World" & bp$variable=="SE|Electricity",]$value-
       bp[bp$period %in% seq(2020,2024) & bp$region=="Total World" & bp$variable=="Generation|LowC",]$value)+
     rep((bp[bp$period == 2019 & bp$region=="Total World" & bp$variable=="SE|Electricity",]$value-
            bp[bp$period == 2019 & bp$region=="Total World" & bp$variable=="Generation|LowC",]$value),5))+
     # c(0,-200,-400,-600,-800))
     c(-200,-400,-800,-1200,-1600))

#for ribbons:
outerrib <- traj %>% filter(region =="World",period > 2018, scen %in% c("verylow","veryhigh")) %>% spread(scen,value)
innerrib <- traj %>% filter(region =="World",period > 2018, scen %in% c("low","high")) %>% spread(scen,value)
emiplot <- ggplot()+
  geom_ribbon(data=outerrib,aes(x=period,ymin=verylow/1000,ymax=veryhigh/1000),fill="grey",alpha=0.2) + 
  geom_ribbon(data=innerrib,aes(x=period,ymin=low/1000,ymax=high/1000),fill="grey",alpha=0.2) + 
  geom_line(data=traj[traj$region =="World" & traj$period<2022,],aes(x=period,y=value/1000,color=scen,linetype=scen)) +
  geom_line(data=traj[traj$region =="World" & traj$period>2020 & traj$scen %in% c("verylow","veryhigh","med"),] ,aes(x=period,y=value/1000,color=scen),linetype="dotted") +
  geom_line(data=traj[traj$region =="World" & traj$period>2020 & traj$scen %in% c("low","high"),] ,aes(x=period,y=value/1000,color=scen),linetype="twodash") +
  geom_line(data=traj[traj$region =="World" & traj$period<2020,],aes(x=period,y=value/1000),color="black")+
  scale_color_manual(values=c("grey","grey","red","grey","grey"))+
  scale_linetype_manual(values=c("solid","solid","solid","dashed","dashed"))+
   scale_y_continuous(breaks=seq(9,14),limits=c(8.7,14.3))+scale_x_continuous(breaks=seq(2012,2024,2),limits = c(2011,2024))+
  theme_bw()+ylab("Power sector emissions (Gt CO2/yr)")
ggsave("Fig1c.pdf",width=6,height=5)
write.table(x = "*Data shown in Figure 1c (see paper for data sources and methods); 
*'period' denotes the year (2018-2020)
*'scen' denotes the 5 different projections discussed for Figure 1c
*'value' denotes the global power sector emissions in each year in Mt CO2/yr",file = "Fig1c.csv",row.names = F,quote = F)
write.table(traj %>% filter(region=="World",period>2010) %>% select(-variable,-region),
            file = "Fig1c.csv",row.names = F,quote = F,append = T,sep=",")

#emission intensity with bp generation data from 04_read_in_imf_bp
tmp <- bp[bp$period %in% yrs & bp$region=="Total World" & bp$variable %in% c("SE|Electricity"),] %>% mutate(elec=value,period=as.numeric(period)) %>% select(-variable,-value,-region)
tmp <- left_join(traj %>% filter(region=="World") %>% select(-region),tmp) %>% mutate(value=value/elec)
touterrib <- tmp %>% filter(period > 2018, scen %in% c("verylow","veryhigh")) %>% spread(scen,value)
tinnerrib <- tmp %>% filter(period > 2018, scen %in% c("low","high")) %>% spread(scen,value)
intplot <- ggplot()+
  geom_ribbon(data=touterrib,aes(x=period,ymin=verylow,ymax=veryhigh),fill="grey",alpha=0.2) + 
  geom_ribbon(data=tinnerrib,aes(x=period,ymin=low,ymax=high),fill="grey",alpha=0.2) + 
  geom_line(data=tmp,aes(x=period,y=value,color=scen,linetype=scen)) +
  geom_line(data=tmp[tmp$period<2020,],aes(x=period,y=value),color="black")+
  scale_color_manual(values=c("grey","grey","red","grey","grey"))+
  scale_linetype_manual(values=c("solid","solid","solid","dashed","dashed"))+scale_x_continuous(breaks=seq(2012,2024,2),limits = c(2011,2024))+
  ylim(0.35,0.6)+
  theme_bw()+ggtitle("Emission intensity of power generation (t CO2/MWh)") +ylab("")+xlab("")
  ggsave("ExtDat3.pdf",width=6,height=5)
write.table(x = "*Data shown in Extended Data Figure 3 (see paper for data sources and methods); 
*'period' denotes the year (2018-2020)
*'scen' denotes the 5 different projections discussed for Figure 1c
*'value' denotes the carbon intensity of global power generation in each year in t CO2/MWh",file = "ExtDat3.csv",row.names = F,quote = F)
write.table(tmp %>% filter(period>2010) %>% select(-variable,-elec),
            file = "ExtDat3.csv",row.names = F,quote = F,append = T,sep=",")

