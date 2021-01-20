#file with 2020-2024 assumptions


#add additional time steps for additions plot
add20 <- bp %>% filter(period=="add19") 
add20$value <- 0
add20$period <- "add20"
bp <- rbind(bp,add20)
add20$period <- "add21"
bp <- rbind(bp,add20)
add20$period <- "add22"
bp <- rbind(bp,add20)
add20$period <- "add23"
bp <- rbind(bp,add20)
add20$period <- "add24"
bp <- rbind(bp,add20)
y20 <- bp %>% filter(period=="2019") 
y20$value <- 0
y20$period <- "2020"
bp <- rbind(bp,y20)
y20$period <- "2021"
bp <- rbind(bp,y20)
y20$period <- "2022"
bp <- rbind(bp,y20)
y20$period <- "2023"
bp <- rbind(bp,y20)
y20$period <- "2024"
bp <- rbind(bp,y20)


yrsh <- c("2011","2012","2013","2014","2015","2016","2017","2018","2019")
yrs <- c("2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024")
addsh <- c("add11","add12","add13","add14","add15","add16","add17","add18","add19")
adds <- c("add11","add12","add13","add14","add15","add16","add17","add18","add19","add20","add21","add22","add23","add24")

# in order to achieve somewhat more plausible regional disaggregation of total values: 
# less future additions in India and China, and OECD, leading to more additions in RoW (via difference from World total)
# maximum offset for India, China, OECD: 5*7 = 35 GW/yr less lowC additions in 2024, and 105 more for RoW
in_ch_offset <- -7
# 
# row4 <- bp %>% spread(region,value) %>% mutate(r4o=`Total World`-`China`-`of which: OECD`-`India`) %>% 
#   gather(region,value,-period,-variable) %>% filter(region=="r4o")
# 
# bp_ex <- rbind(bp[bp$region %in% c("China","of which: OECD","European Union","US","India")],row4)

#income elasticity:
elasticity <- 0.6
#reduction for low-c addition in 2020
red2020 <- 0.6

#changes to additions - should be done in parallel with changes to totals further done lines 89 ff.
# for 2020 and 2021, additions are prop to IMF GDP projections
# for 2022 to 2024, additions are dependent on long term mean 
for(r in c("Total World","China","of which: OECD","European Union","US","India")){
  if(r %in% c("China","India","of which: OECD","European Union")){offset <- in_ch_offset} else {offset <- 0}
  # 2020 estimate on total generation change: proportional to GDP
  bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="add20",]$value <- 
    bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="2019",]$value * 
    (imf[imf$Country==r & imf$period=="X2020",]$value/100)*elasticity
  # 2021: proportional to GDP
  bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="add21",]$value <- 
    bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="2019",]$value * 
    (1+imf[imf$Country==r & imf$period=="X2020",]$value/100*elasticity)*imf[imf$Country==r & imf$period=="X2021",]$value/100*elasticity
  # 2022: long-term medium increase
  bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="add22",]$value <- 
    mean(bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period %in% addsh,]$value) 
  # 2023: long-term medium increase  
  bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="add23",]$value <- 
    mean(bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period %in% addsh,]$value) 
  # 2024: long-term medium increase
  bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="add24",]$value <- 
    mean(bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period %in% addsh,]$value) 
  # 2020: low-carbon increase: yearly addition to increase with long-term change, 
  # plus offset term to reduce India and China (and increase RoW)
  bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="add20",]$value <- 
    red2020*(bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value + 
    (offset+bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value-bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add17",]$value)/2)
  # 2021: low-carbon increase:  yearly addition to increase with long-term change,
  # plus offset term to reduce India and China (and increase RoW)
  bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="add21",]$value <- 
    bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value +
    2*(offset+bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value-bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add17",]$value)/2
  # 2022: low-carbon increase:  yearly addition to increase even faster, plus offset term to reduce India and China (and incrase RoW)
  bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="add22",]$value <- 
    bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value +
    3*(offset+bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value-bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add17",]$value)/2
  # 2023: low-carbon increase:  yearly addition to increase even faster, plus offset term to reduce India and China (and incrase RoW)
  bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="add23",]$value <- 
    bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value +
    4*(offset+bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value-bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add17",]$value)/2
  # 2024: low-carbon increase:  yearly addition to increase even faster, plus offset term to reduce India and China (and incrase RoW)
  bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="add24",]$value <- 
    bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value +
    5*(offset+bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value-bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add17",]$value)/2
  
}

#changes to absolute values
for(r in c("Total World","China","of which: OECD","European Union","US","India")){
  if(r %in% c("China","India","of which: OECD","European Union")){offset <- in_ch_offset} else {offset <- 0}
  # 2020 estimate on total generation change: proportional to GDP
  bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="2020",]$value <- 
    bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="2019",]$value * 
    (1+(imf[imf$Country==r & imf$period=="X2020",]$value/100)*elasticity)
  
  bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="2021",]$value <- 
    bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="2020",]$value * 
    (1+imf[imf$Country==r & imf$period=="X2021",]$value/100*elasticity)
  
  bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="2022",]$value <- 
    bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="2021",]$value + 
    mean(bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period %in% addsh,]$value) 
  
  bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="2023",]$value <- 
    bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="2022",]$value + 
    mean(bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period %in% addsh,]$value) 
  
  bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="2024",]$value <- 
    bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period=="2023",]$value + 
    mean(bp[bp$region==r & bp$variable =="SE|Electricity" & bp$period %in% addsh,]$value) 
  
  bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="2020",]$value <- 
    bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="2019",]$value +
    red2020*(bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value +
    (offset+bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value-bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add17",]$value)/2)
  
  bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="2021",]$value <- 
    bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="2020",]$value +
    bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value +
    2*(offset+bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value-bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add17",]$value)/2
  
  bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="2022",]$value <- 
    bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="2021",]$value +
    bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value +
    3*(offset+bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value-bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add17",]$value)/2
  
  bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="2023",]$value <- 
    bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="2022",]$value +
    bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value +
    4*(offset+bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value-bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add17",]$value)/2
  
  bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="2024",]$value <- 
    bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period=="2023",]$value +
    bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value +
    5*(offset+bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add19",]$value-bp[bp$region==r & bp$variable =="Generation|LowC" & bp$period =="add17",]$value)/2
  
}

#calculate ROW only after imf calculations for 2020 and 2021 are done
row <- bp %>% spread(region,value) %>% mutate(RoW=`Total World`-`China`-`of which: OECD`) %>% 
  gather(region,value,-period,-variable) %>% filter(region=="RoW")

bp <- rbind(bp,row)

#calculate ROW only after imf calculations for 2020 and 2021 are done
row4 <- bp %>% spread(region,value) %>% mutate(r4o=`Total World`-`China`-`of which: OECD`-`India`) %>% 
  gather(region,value,-period,-variable) %>% filter(region=="r4o")

bp <- rbind(bp,row4)

