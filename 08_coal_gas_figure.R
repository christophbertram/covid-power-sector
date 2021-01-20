## Motivation##
# Compare change in coal and gas total generation in 2019 and 2020 for USA and Europe

# data from generation until (start of year) to 2020-07-31 compared to (start of year) until 2019-07-30 (to account for leap year)
# revision with added data
sum(us[(us$date >"2019-12-31" & us$date <"2020-10-01"),]$coal)/1e6 #567.4814
sum(us[(us$date >"2018-12-31" & us$date <"2019-09-30"),]$coal)/1e6 #730.2461
sum(us[(us$date >"2017-12-31" & us$date <"2018-09-30"),]$coal,na.rm=T)/1e6 #242.3073


sum(us[(us$date >"2019-12-31" & us$date <"2020-10-01"),]$gas)/1e6 #1138.054
sum(us[(us$date >"2018-12-31" & us$date <"2019-09-30"),]$gas)/1e6 #1079.628
sum(us[(us$date >"2017-12-31" & us$date <"2018-09-30"),]$gas,na.rm = T)/1e6 #341.0209

mat %>% filter(ProductionType=="Fossil Gas",AreaName=="ENTSOE", period %in% c("av18","av19","av20"))  %>% 
  mutate(aggr =value*24*((31+29+31+30+31+30+31+31+30)/1e6)) %>% select(aggr) #332.4335 422.6239 392.2535

mat %>% filter(ProductionType=="Fossil Hard coal",AreaName=="ENTSOE", period %in% c("av18","av19","av20")) %>% 
  mutate(aggr =value*24*((31+29+31+30+31+30+31+31+30)/1e6)) %>% select(aggr) #219.2041 151.1097 111.6017

mat %>% filter(ProductionType=="Fossil Brown coal/Lignite",AreaName=="ENTSOE", period %in% c("av18","av19","av20")) %>% 
  mutate(aggr =value*24*((31+29+31+30+31+30+31+31+30)/1e6)) %>% select(aggr) #222.4144 190.2300 149.6801

#India data directly from https://power.carboncopy.info/ range Jan 1, 2020- Oct 1,2020
#coal 636.513 TWh -10.8% 
#Gas, naphta, diesel 36.575 TWh 9.9%
#Lignite 19.543 TWh -11.6%

#for 2018 numbers: scale full-year data
ga %>% filter(period==2018,region=="US") #1579.276
ga %>% filter(period==2019,region=="US") #1700.876

co %>% filter(period==2018,region=="US") #1246.693
co %>% filter(period==2019,region=="US") #1053.53

ga %>% filter(period==2018,region=="India") #73.92521
ga %>% filter(period==2018,region=="India") #71.02061

co %>% filter(period==2018,region=="India") #1167.318
co %>% filter(period==2019,region=="India") #1137.368


#manually create data frame with values displayed above
coalgas <- data.frame(region=rep(c("USA","Europe","India"),6),
                      period=rep(c(2018,2018,2018,2019,2019,2019,2020,2020,2020),2),
                      fuel=rep(c("Coal","Gas"),each=9),
                      value=c(730.2461*1246.693/1053.53,219.2041+222.4144,(636.513/(1-0.108)+19.543/(1-0.116))*1167.318/1137.368,#coal 18 US,EU, In
                              730.2461,151.1097+190.2300,(636.513/(1-0.108)+19.543/(1-0.116)),#coal 19 US,EU, In
                              567.4814,111.6017+149.6801, 636.513+19.543,#coal 20 US, EU, In
                              1079.628*1246.693/1053.53,332.4335,36.575/(1+0.099)*73.92521/71.02061, #gas 18 US, EU, In 
                              1079.628,422.6239,36.575/(1+0.099),#gas 19 US, EU, In 
                              1138.054,392.2535,36.575 )) #gas 20 US, EU, In

#to plot red arrows with increase/decrease
arrow <- coalgas %>% spread(period,value) %>% mutate(red=round((`2020`/`2019`-1)*100),x1=c(2018.75,2019.25,2018.75,2019.25,2018.75,2019.25),
                                                     x2=c(2019.75,2020.25,2019.75,2020.25,2019.75,2020.25),x3=rep(2019.5,6))
arrow2 <- coalgas %>% spread(period,value) %>% mutate(red=round((`2019`/`2018`-1)*100),x1=c(2017.75,2018.25,2017.75,2018.25,2017.75,2018.25),
                                                     x2=c(2018.75,2019.25,2018.75,2019.25,2018.75,2019.25),x3=rep(2018.5,6))


#plot figure with total generation by gas and coal for US (first 168 days) and EU for first 172 days of 2019 and 2020 respectively
ggplot()+
  geom_bar(data=coalgas,aes(x=period,y=value,fill=fuel),stat="identity",position = "dodge")+
  scale_fill_manual(values=c("black","grey"))+
  # annotate("text", x=arrow$x3, y=arrow$`2019`,label=arrow$red ,
  #        color = "red", size = 3)+
  annotate("segment", x=arrow$x1, xend = arrow$x2, y=arrow$`2019`, yend = arrow$`2020`,
           color = "red", size = 1.1, arrow = arrow(length=unit(.2,"cm")))+
  annotate("segment", x=arrow2$x1, xend = arrow2$x2, y=arrow2$`2018`, yend = arrow2$`2019`,
           color = "red", size = 1.1, arrow = arrow(length=unit(.2,"cm")))+
  facet_wrap(~region) +theme_bw()
ggsave("ExtDat2.pdf",width=6,height=5)
write.table(x = "*Data shown in Extended Data 2 (see paper for data sources); 
*'region' denotes the countries (showing Europe (combined ENTSO-E countries), India and USA)
*'period' denotes the year (2018-2020)
*'fuel' denotes the 2 different fossil categories 'gas', and 'coal'
*'value' denotes the cumulative generation (in TWh) in the respective year from January trough September",file = "ExtDat2.csv",row.names = F,quote = F)
write.table(coalgas,file = "ExtDat2.csv",row.names = F,quote = F,append = T,sep=",")

