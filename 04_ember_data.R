

# embc <- read.csv2("other_data/Ember-Global-Electricity-Review-2020-half-year-data_changes.csv")


embd <- read.csv2("other_data/Ember-Global-Electricity-Review-2020-half-year-data_data.csv")
embd$Generation_twh <- as.numeric(as.character(embd$Generation_twh))

#read out low-carbon production changes in 2020H1 for the 5 regions considered in paper
emb_lowcarb <- c("Biomass.and.waste","Hydro","Nuclear","Solar","Wind","Other.renewables")

embeu <- unique(embd$Area)[c(3,5,7,12,13,14,16,17,18,19,20,22,23,25,26,27,28,31,32,33,35,36,38,39)]
embeu28 <- c(embeu,"United Kingdom")
oecd <- unique(embd$Area)[c(2,8,9,11,15,24,29,37,41,42,43)]
### rest: not OECD, not China, India
rest <- unique(embd$Area)[setdiff(seq(1,43),c(10,21,c(3,5,7,12,13,14,16,17,18,19,20,22,23,25,26,27,28,31,32,33,35,36,38,39),
                                  c(2,8,9,11,15,24,29,37,41,42,43)))]

embd %>% filter(Month=="5",Variable=="Coal",Area=="EU-27")
embd %>% filter(Month=="5",Variable=="Coal",Area %in% embeu) %>% group_by(Year) %>% summarise(Generation_twh=sum(Generation_twh))

#for use in plot
emb20 <- embd %>% filter(Area != "EU-27",Variable %in% emb_lowcarb) %>% group_by(Year,Variable) %>% summarise(Generation_twh=sum(Generation_twh,na.rm=T)) %>%
  spread(Year,value=Generation_twh) %>% mutate(diff=`2020`-`2019`) %>% select(-`2019`,-`2020`) %>% spread(Variable,value=diff) %>% 
  mutate(PV=Solar,AllOtherLowC=Biomass.and.waste+Other.renewables) %>% select(-Solar,-Biomass.and.waste,-Other.renewables) %>% 
  gather(variable,value) %>% mutate(region="Total World",period="add20")

emb20 <- rbind(emb20,embd %>% filter(Area %in% oecd,Variable %in% emb_lowcarb) %>% group_by(Year,Variable) %>% summarise(Generation_twh=sum(Generation_twh,na.rm=T)) %>%
  spread(Year,value=Generation_twh) %>% mutate(diff=`2020`-`2019`) %>% select(-`2019`,-`2020`) %>% spread(Variable,value=diff) %>% 
  mutate(PV=Solar,AllOtherLowC=Biomass.and.waste+Other.renewables) %>% select(-Solar,-Biomass.and.waste,-Other.renewables) %>% 
  gather(variable,value) %>% mutate(region="of which: OECD",period="add20"))

emb20 <- rbind(emb20,embd %>% filter(Area =="China",Variable %in% emb_lowcarb) %>% group_by(Year,Variable) %>% summarise(Generation_twh=sum(Generation_twh,na.rm=T)) %>%
                 spread(Year,value=Generation_twh) %>% mutate(diff=`2020`-`2019`) %>% select(-`2019`,-`2020`) %>% spread(Variable,value=diff) %>% 
                 mutate(PV=Solar,AllOtherLowC=Biomass.and.waste) %>% select(-Solar,-Biomass.and.waste) %>% 
                 gather(variable,value) %>% mutate(region="China",period="add20"))

emb20 <- rbind(emb20,embd %>% filter(Area =="India",Variable %in% emb_lowcarb) %>% group_by(Year,Variable) %>% summarise(Generation_twh=sum(Generation_twh,na.rm=T)) %>%
                 spread(Year,value=Generation_twh) %>% mutate(diff=`2020`-`2019`) %>% select(-`2019`,-`2020`) %>% spread(Variable,value=diff) %>% 
                 mutate(PV=Solar,AllOtherLowC=Biomass.and.waste+Other.renewables) %>% select(-Solar,-Biomass.and.waste,-Other.renewables) %>% 
                 gather(variable,value) %>% mutate(region="India",period="add20"))

emb20 <- rbind(emb20,embd %>% filter(Area %in% rest,Variable %in% emb_lowcarb) %>% group_by(Year,Variable) %>% summarise(Generation_twh=sum(Generation_twh,na.rm=T)) %>%
                 spread(Year,value=Generation_twh) %>% mutate(diff=`2020`-`2019`) %>% select(-`2019`,-`2020`) %>% spread(Variable,value=diff) %>% 
                 mutate(PV=Solar,AllOtherLowC=Biomass.and.waste) %>% select(-Solar,-Biomass.and.waste) %>% 
                 gather(variable,value) %>% mutate(region="r4o",period="add20"))


emb20$variable <- paste0("Generation|",emb20$variable)

#absolute reductions
emb20 <- rbind(emb20,embd %>% filter(Area != "EU-27",Variable=="Production") %>% group_by(Year) %>% summarise(Generation_twh=sum(Generation_twh,na.rm=T)) %>%
  spread(Year,value=Generation_twh) %>%  mutate(value=`2020`-`2019`,variable="SE|Electricity",region="Total World",period="add20") %>% 
  select(-`2019`,-`2020`)) 

emb20 <- rbind(emb20,embd %>% filter(Area %in% oecd,Variable=="Production") %>% group_by(Year) %>% summarise(Generation_twh=sum(Generation_twh)) %>%
   spread(Year,value=Generation_twh) %>%  mutate(value=`2020`-`2019`,variable="SE|Electricity",region="of which: OECD",period="add20") %>% 
   select(-`2019`,-`2020`)) 

emb20 <- rbind(emb20,embd %>% filter(Area =="China",Variable=="Production") %>% group_by(Year) %>% summarise(Generation_twh=sum(Generation_twh,na.rm=T)) %>%
   spread(Year,value=Generation_twh) %>%  mutate(value=`2020`-`2019`,variable="SE|Electricity",region="China",period="add20") %>% 
   select(-`2019`,-`2020`)) 

emb20 <- rbind(emb20,embd %>% filter(Area =="India",Variable=="Production") %>% group_by(Year) %>% summarise(Generation_twh=sum(Generation_twh)) %>%
   spread(Year,value=Generation_twh) %>%  mutate(value=`2020`-`2019`,variable="SE|Electricity",region="India",period="add20") %>% 
   select(-`2019`,-`2020`)) 

emb20 <- rbind(emb20,embd %>% filter(Area %in% rest,Variable=="Production") %>% group_by(Year) %>% summarise(Generation_twh=sum(Generation_twh)) %>%
   spread(Year,value=Generation_twh) %>%  mutate(value=`2020`-`2019`,variable="SE|Electricity",region="r4o",period="add20") %>% 
   select(-`2019`,-`2020`)) 
#relative reductions
emb20 <- rbind(emb20,embd %>% filter(Area != "EU-27",Variable=="Production") %>% group_by(Year) %>% summarise(Generation_twh=sum(Generation_twh,na.rm=T)) %>%
                 spread(Year,value=Generation_twh) %>%  mutate(value=181/182*`2020`/`2019`,variable="rel|Electricity",region="Total World",period="add20") %>% 
                 select(-`2019`,-`2020`)) 

emb20 <- rbind(emb20,embd %>% filter(Area %in% oecd,Variable=="Production") %>% group_by(Year) %>% summarise(Generation_twh=sum(Generation_twh)) %>%
                 spread(Year,value=Generation_twh) %>%  mutate(value=181/182*`2020`/`2019`,variable="rel|Electricity",region="of which: OECD",period="add20") %>% 
                 select(-`2019`,-`2020`)) 

emb20 <- rbind(emb20,embd %>% filter(Area %in% embeu28,Variable=="Production") %>% group_by(Year) %>% summarise(Generation_twh=sum(Generation_twh)) %>%
                 spread(Year,value=Generation_twh) %>%  mutate(value=181/182*`2020`/`2019`,variable="rel|Electricity",region="European Union",period="add20") %>% 
                 select(-`2019`,-`2020`)) 

emb20 <- rbind(emb20,embd %>% filter(Area =="United States",Variable=="Production") %>% group_by(Year) %>% summarise(Generation_twh=sum(Generation_twh,na.rm=T)) %>%
                 spread(Year,value=Generation_twh) %>%  mutate(value=181/182*`2020`/`2019`,variable="rel|Electricity",region="US",period="add20") %>% 
                 select(-`2019`,-`2020`)) 


emb20 <- rbind(emb20,embd %>% filter(Area =="China",Variable=="Production") %>% group_by(Year) %>% summarise(Generation_twh=sum(Generation_twh,na.rm=T)) %>%
                 spread(Year,value=Generation_twh) %>%  mutate(value=181/182*`2020`/`2019`,variable="rel|Electricity",region="China",period="add20") %>% 
                 select(-`2019`,-`2020`)) 

emb20 <- rbind(emb20,embd %>% filter(Area =="India",Variable=="Production") %>% group_by(Year) %>% summarise(Generation_twh=sum(Generation_twh)) %>%
                 spread(Year,value=Generation_twh) %>%  mutate(value=181/182*`2020`/`2019`,variable="rel|Electricity",region="India",period="add20") %>% 
                 select(-`2019`,-`2020`)) 

emb20 <- rbind(emb20,embd %>% filter(Area %in% rest,Variable=="Production") %>% group_by(Year) %>% summarise(Generation_twh=sum(Generation_twh)) %>%
                 spread(Year,value=Generation_twh) %>%  mutate(value=181/182*`2020`/`2019`,variable="rel|Electricity",region="r4o",period="add20") %>% 
                 select(-`2019`,-`2020`)) 




#calculate emissions

embemi <- embd %>% select(-Source) %>% spread(Variable,value=Generation_twh,fill = 0) %>% mutate(emissions =0.42*(Gas+Fossil.gas)+(0.85+1.1)*Coal+0.89*(Other.fossil)) %>%
gather(variable,value,-Date,-Year,-Month,-Area) %>% filter(variable=="emissions")

embemi <- embemi %>% select(-Date) %>% group_by(Area,Month) %>%
                 spread(Year,value=value) %>%  mutate(value=`2020`/`2019`,variable="relEmissions")


