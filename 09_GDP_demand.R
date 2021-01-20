#plot historical power demand and gdp growth rates 


bp[bp$period %in% as.character(paste0("r",c(11:19))) & bp$region=="Total World" & bp$variable %in% c("SE|Electricity"),]

#world bank data (reformatted to semicolon, added a unit entry in first header row)
wb <- read.csv2("other_data/World_bank_GDP_ppp.csv",skip=4,header = T,colClasses = c(rep("character",5),rep("numeric",60)))
wb <- wb %>% filter(`Country.Name` %in% c("World","China","India","OECD members")) %>%
  mutate(r19 =(`X2019`/`X2018`)-1,r18 =(`X2018`/`X2017`)-1,r17 =(`X2017`/`X2016`)-1,r16 =(`X2016`/`X2015`)-1,r15 =(`X2015`/`X2014`)-1,
         r14 =(`X2014`/`X2013`)-1,r13 =(`X2013`/`X2012`)-1,r12 =(`X2012`/`X2011`)-1,r11 =(`X2011`/`X2010`)-1)
wb <- wb %>% select(-Country.Code,-Indicator.Name,-Indicator.Code,-Unit) %>% gather(period,value,-Country.Name) 
wb$variable <- "GDP growth"
rates <- as.character(paste0("r",c(11:19)))
# Historical (world) power demand and GDP growths rates
ggplot()+
  geom_point(data=wb[wb$period %in% rates & wb$Country.Name=="World",],aes(x=period,y=value*100,color=variable))+
  theme_bw()+
  ylab("yearly growth rates (%)")+
  geom_point(data=bp[bp$period %in% rates & bp$region=="Total World" & bp$variable=="SE|Electricity",],aes(x=period,y=value*100),color="black")+
  scale_x_discrete(labels=as.character(c(2011:2019)))+
  ylim(0,4)

ggsave("SuppFigS3.pdf",width=6,height=5)

