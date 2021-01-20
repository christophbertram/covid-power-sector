# plot two panels of figure 1
loc <- c("Generation|Hydro","Generation|Nuclear","Generation|Wind","Generation|PV","Generation|AllOtherLowC")
#write header of csv files
write.table(x = "*Data shown in Figure 1b/Extended Data 1b/d/f/h (see paper for data sources); 
*'region' denotes the geographical scope (either 'World', 'India', 'China', 'OECD', or the Rest of the World, 'r4o')
*'period' denotes the year
*'value' denotes the change in demand/electricity generation in TWh
*'variable' denotes the different components/aggregates with the following options (see sheet Variable names)",file = "Fig1b.csv",row.names = F,quote = F)
write.table(x = "*Data shown in Figure 1a/Extended Data 1a/c/e/g (see paper for data sources); 
*'region' denotes the geographical scope (either 'World', 'India', 'China', 'OECD', or the Rest of the World, 'r4o')
*'period' denotes the year
*'value' denotes the absolute demand/electricity generation in TWh
*'variable' denotes the different components/aggregateswith the following options (see sheet Variable names)",file = "Fig1a.csv",row.names = F,quote = F)


#figure with year-on-year changes
# for(r in c("US","European Union")){
for(r in c("Total World","China","of which: OECD","India","r4o")){
  tmp <- bp[bp$period %in% adds & bp$region==r & bp$variable %in% c("SE|Electricity","Generation|LowC"),] %>% spread(variable,value) 
  inc_per <- tmp[tmp$`Generation|LowC`< tmp$`SE|Electricity`,]$period
  tmp1 <- tmp %>% filter(period %in% inc_per)
  tmp2 <- tmp %>% filter(!period %in% inc_per)
  dist <- max(tmp$`SE|Electricity`)-min(tmp$`SE|Electricity`)
  ggplot()+
    geom_bar(data=emb20[emb20$period %in% adds & emb20$region==r & emb20$variable %in% loc,],aes(x=period,y=value,fill=variable),width=0.45 ,stat="identity")+
    geom_bar(data=bp[bp$period %in% adds & bp$region==r & bp$variable %in% loc,],aes(x=period,y=value,fill=variable),stat="identity")+
     scale_fill_manual(values=c("darkgreen","darkblue","purple","gold","blue","grey"))+theme_bw()+ylab("Year-on-year changes (TWh)")+ggtitle(r)+#+ggtitle("Rest of world")+#+ggtitle("OECD")+#
    scale_x_discrete(labels=c("2011","2012","2013","2014","2015","2016","2017","2018","2019","2020p","2021p","2022p","2023p","2024p"))+
    geom_ribbon(data=tmp,aes(x=period,ymin=`Generation|LowC`,ymax=`SE|Electricity`,group=1,
                             # fill=`Generation|LowC`>`SE|Electricity`),alpha=0.5)
                             fill="yellow"),alpha=0.5)+
    geom_line(data=tmp,aes(x=period,y=`Generation|LowC`,group=1),color="green")+
    geom_line(data=tmp,aes(x=period,y=`SE|Electricity`,group=1),color="red")+
    geom_point(data=bp[bp$period %in% adds & bp$region==r & bp$variable == "SE|Electricity",],aes(x=period,y=value),color="red")+
    geom_point(data=bp[bp$period %in% adds & bp$region==r & bp$variable == "Generation|LowC",],aes(x=period,y=value),shape=21,fill="white",color="red")+
    geom_point(data=bp[bp$period %in% addsh & bp$region==r & bp$variable == "SE|Electricity",],aes(x=period,y=value))+
    geom_point(data=bp[bp$period %in% addsh & bp$region==r & bp$variable == "Generation|LowC",],aes(x=period,y=value),shape=21,fill="white")+
    geom_point(data=emb20[emb20$period %in% adds & emb20$region==r & emb20$variable %in% loc,],aes(x=period,y=sum(value)),shape=24,fill="white",color="red")+
    # geom_point(data=emb20[emb20$period %in% adds & emb20$region==r & emb20$variable %in% loc,],aes(x=period,y=2/0.75*sum(value)),shape=24,fill="white",color="red")+
    geom_point(aes(x="add20",y=bp[bp$period ==2019 & bp$region==r & bp$variable == "SE|Electricity",]$value*
                                          (emb20[emb20$variable=="rel|Electricity" & emb20$region==r, ]$value-1)),shape=17,fill="yellow")
    ggsave(paste0("Fig1b-ExtDat1_b_d_f_h_",substr(r,start=1,stop=2),".pdf"),width=6,height=5)
  write.table(bp[bp$period %in% adds & bp$region==r & bp$variable %in% c(loc,"SE|Electricity","Generation|LowC"),],
              file = "Fig1b.csv",row.names = F,quote = F,append = T,sep=",")
}


locf <- c(loc,"Generation_Fossil")
#figure with yearly totals
# for(r in c("US","European Union")){
for(r in c("Total World","China","of which: OECD","India","r4o")){
  tmp <- bp[bp$period %in% yrs & bp$region==r & bp$variable %in% c("SE|Electricity","Generation|LowC"),] %>% spread(variable,value) 
  inc_per <- tmp[tmp$`Generation|LowC`< tmp$`SE|Electricity`,]$period
  tmp1 <- tmp %>% filter(period %in% inc_per)
  tmp2 <- tmp %>% filter(!period %in% inc_per)
  dist <- max(tmp$`SE|Electricity`)-min(tmp$`SE|Electricity`)
  ggplot()+
    # geom_bar(data=eg[eg$period %in% yrs & eg$region=="Total World",],aes(x=period,y=value,fill=variable),stat="identity")+
    geom_bar(data=bp[bp$period %in% yrs & bp$region==r & bp$variable %in% locf,],aes(x=period,y=value,fill=variable,alpha=variable),stat="identity")+
    geom_point(data=bp[bp$period %in% yrs & bp$region==r & bp$variable == "SE|Electricity",],aes(x=period,y=value),color="red")+
    geom_point(data=bp[bp$period %in% yrs & bp$region==r & bp$variable == "Generation|LowC",],aes(x=period,y=value),shape=21,fill="white",color="red")+
    geom_point(data=bp[bp$period %in% yrsh & bp$region==r & bp$variable == "SE|Electricity",],aes(x=period,y=value))+
    geom_point(data=bp[bp$period %in% yrsh & bp$region==r & bp$variable == "Generation|LowC",],aes(x=period,y=value),shape=21,fill="white")+
    scale_alpha_manual(values=c(0.5,1,1,1,1,1))+scale_fill_manual(values=c("grey","darkgreen","darkblue","purple","gold","blue"))+theme_bw()+ylab("Total Generation (TWh)")+ggtitle(r)+#+ggtitle("Rest of world")+#+ggtitle("OECD")+#
    scale_x_discrete(labels=c("2011","2012","2013","2014","2015","2016","2017","2018","2019","2020p","2021p","2022p","2023p","2024p"))

  ggsave(paste0("Fig1a-ExtDat1_a_c_e_g_",substr(r,start=1,stop=2),".pdf"),width=6,height=5)
  write.table(bp[bp$period %in% yrs & bp$region==r & bp$variable %in% c(locf,"SE|Electricity","Generation|LowC"),],
              file = "Fig1a.csv",row.names = F,quote = F,append = T,sep=",")
}
