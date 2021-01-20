### file to read in IMF data (lines 5-20)

### and read-in and prepare BP power generation and capacity data (lines 23-120) 


## read in June update of IMF
imf <- read.csv2("other_data/WEO-Oct2020update.csv",stringsAsFactors = F)
imf$X <- NULL # remove unwanted columns
imf$X.1 <- NULL
imf$X.2 <- NULL
imf[imf$Country=="Russia",]$Country <- "Russian Federation"
imf[imf$Country=="United States",]$Country <- "US"
imf[imf$Country=="Korea",]$Country <- "South Korea"
imf[imf$Country=="World",]$Country <- "Total World"
imf[imf$Country=="Euro Area",]$Country <- "European Union"
imf[imf$Country=="Advanced Economies",]$Country <- "of which: OECD"
imf$Country <- as.factor(imf$Country)
imf <- imf %>% gather(period,value,-Country)
imf$value <- as.numeric(imf$value)

## read in BP data
filename <- "other_data/bp-stats-review-2020-all-data.xlsx"
sheets <- excel_sheets(filename)

# ct <- read_excel(filename, sheet ="Contents")
# def <- read_excel(filename, sheet ="Definitions")
# meth <- read_excel(filename, sheet ="Methodology")

eg <- read_excel(filename, sheet ="Electricity Generation " ,skip = 2)
pv <- read_excel(filename, sheet = "Solar Capacity",skip=3)
wi <- read_excel(filename, sheet = "Wind Capacity",skip=3)
co <- read_excel(filename, sheet = "Elec Gen from Coal",skip=2)
ga <- read_excel(filename, sheet = "Elec Gen from Gas",skip=2)
oi <- read_excel(filename, sheet = "Elec Gen from Oil",skip=2)
co %>% filter(`Terawatt-hours`=="Total World") %>% select(`Terawatt-hours`,as.character(c(2011:2018)),`2019...36`)
ga %>% filter(`Terawatt-hours`=="Total World") %>% select(`Terawatt-hours`,as.character(c(2011:2018)),`2019...36`)
oi %>% filter(`Terawatt-hours`=="Total World") %>% select(`Terawatt-hours`,as.character(c(2011:2018)),`2019...36`)

enu <- read_excel(filename, sheet = "Nuclear Generation - TWh",skip=2,col_types = c("text",rep("numeric",58)))
ehy <- read_excel(filename, sheet = "Hydro Generation - TWh",skip=2,col_types = c("text",rep("numeric",58)))
epv <- read_excel(filename, sheet = "Solar Generation - TWh",skip=2,col_types = c("text",rep("numeric",58)))
ewi <- read_excel(filename, sheet = "Wind Generation -TWh",skip=2,col_types = c("text",rep("numeric",58)))
gbo <- read_excel(filename, sheet = "Geo Biomass Other - TWh",skip=2,col_types = c("text",rep("numeric",58)))


eg <- eg %>% select(-'2019...37',-'2008-18',-'2019...39') %>% rename('2019'=`2019...36`) %>% 
  mutate(add19 =`2019`-`2018`,add18 =`2018`-`2017`,add17 =`2017`-`2016`,add16 =`2016`-`2015`,add15 =`2015`-`2014`,add14 =`2014`-`2013`,add13 =`2013`-`2012`,add12 =`2012`-`2011`,add11 =`2011`-`2010`,
         r19 =(`2019`/`2018`)-1,r18 =(`2018`/`2017`)-1,r17 =(`2017`/`2016`)-1,r16 =(`2016`/`2015`)-1,r15 =(`2015`/`2014`)-1,r14 =(`2014`/`2013`)-1,r13 =(`2013`/`2012`)-1,r12 =(`2012`/`2011`)-1,r11 =(`2011`/`2010`)-1) %>% 
  rename(region='Terawatt-hours') %>% gather(period,value,-region) %>% mutate(variable="SE|Electricity",value=as.numeric(value))
countries <- unique(eg$region)[seq(2,103)]
eg <- eg %>% filter(region %in% countries,!is.na(region))
eg[eg$region=="European Union #",]$region <- "European Union"

countries <- gsub(countries[!is.na(countries)],pattern = " #",replacement = "")

eg %>% filter(region == "Total World",period==2019)
eg %>% filter(region %in% c("Total World","European Union","India","US"),period==2019) %>% select(-period) %>% 
  spread(region,value) %>%  mutate(share=((US+India+`European Union`)/`Total World`))

pv <- pv %>% select(-'2019...26',-'2008-18',-'2019...28') %>% rename('2019'=`2019...25`) %>% 
  mutate(add19 =`2019`-`2018`,add18 =`2018`-`2017`,add17 =`2017`-`2016`,add16 =`2016`-`2015`,add15 =`2015`-`2014`,add14 =`2014`-`2013`,add13 =`2013`-`2012`,add12 =`2012`-`2011`,add11 =`2011`-`2010`,
         r19 =(`2019`/`2018`)-1,r18 =(`2018`/`2017`)-1,r17 =(`2017`/`2016`)-1,r16 =(`2016`/`2015`)-1,r15 =(`2015`/`2014`)-1,r14 =(`2014`/`2013`)-1,r13 =(`2013`/`2012`)-1,r12 =(`2012`/`2011`)-1,r11 =(`2011`/`2010`)-1) %>% 
  rename(region=Megawatts) %>% gather(period,value,-region) %>% mutate(variable="Capacity|PV")
pv <- pv %>% filter(region %in% countries,!is.na(region))
pv %>% filter(period=="add19") %>% arrange(desc(value))
pv %>% filter(period=="add18") %>% arrange(desc(value))

wi <- wi %>% select(-'2019...27',-'2008-2018',-'2019...29') %>% rename('2019'=`2019...26`) %>% 
  mutate(add19 =`2019`-`2018`,add18 =`2018`-`2017`,add17 =`2017`-`2016`,add16 =`2016`-`2015`,add15 =`2015`-`2014`,add14 =`2014`-`2013`,add13 =`2013`-`2012`,add12 =`2012`-`2011`,add11 =`2011`-`2010`,
         r19 =(`2019`/`2018`)-1,r18 =(`2018`/`2017`)-1,r17 =(`2017`/`2016`)-1,r16 =(`2016`/`2015`)-1,r15 =(`2015`/`2014`)-1,r14 =(`2014`/`2013`)-1,r13 =(`2013`/`2012`)-1,r12 =(`2012`/`2011`)-1,r11 =(`2011`/`2010`)-1) %>% 
  rename(region=Megawatts) %>% gather(period,value,-region) %>% mutate(variable="Capacity|Wind")
wi <- wi %>% filter(region %in% countries,!is.na(region))
wi %>% filter(period=="add19") %>% arrange(desc(value))
wi %>% filter(period=="add18") %>% arrange(desc(value))

epv <- epv %>% select(-'2019...57',-'2008-18',-'2019...59') %>% rename('2019'=`2019...56`) %>% 
  mutate(add19 =`2019`-`2018`,add18 =`2018`-`2017`,add17 =`2017`-`2016`,add16 =`2016`-`2015`,add15 =`2015`-`2014`,add14 =`2014`-`2013`,add13 =`2013`-`2012`,add12 =`2012`-`2011`,add11 =`2011`-`2010`,
         r19 =(`2019`/`2018`)-1,r18 =(`2018`/`2017`)-1,r17 =(`2017`/`2016`)-1,r16 =(`2016`/`2015`)-1,r15 =(`2015`/`2014`)-1,r14 =(`2014`/`2013`)-1,r13 =(`2013`/`2012`)-1,r12 =(`2012`/`2011`)-1,r11 =(`2011`/`2010`)-1) %>% 
  rename(region='Terawatt-hours') %>% gather(period,value,-region) %>% mutate(variable="Generation|PV")
epv[!is.na(epv$region) & epv$region=="European Union #",]$region <- "European Union"
epv <- epv %>% filter(region %in% countries,!is.na(region))
epv %>% filter(period=="add19") %>% arrange(desc(value))

ewi <- ewi %>% select(-'2019...57',-'2008-18',-'2019...59') %>% rename('2019'=`2019...56`) %>% 
  mutate(add19 =`2019`-`2018`,add18 =`2018`-`2017`,add17 =`2017`-`2016`,add16 =`2016`-`2015`,add15 =`2015`-`2014`,add14 =`2014`-`2013`,add13 =`2013`-`2012`,add12 =`2012`-`2011`,add11 =`2011`-`2010`,
         r19 =(`2019`/`2018`)-1,r18 =(`2018`/`2017`)-1,r17 =(`2017`/`2016`)-1,r16 =(`2016`/`2015`)-1,r15 =(`2015`/`2014`)-1,r14 =(`2014`/`2013`)-1,r13 =(`2013`/`2012`)-1,r12 =(`2012`/`2011`)-1,r11 =(`2011`/`2010`)-1) %>% 
  rename(region='Terawatt-hours') %>% gather(period,value,-region) %>% mutate(variable="Generation|Wind")
ewi[!is.na(ewi$region) & ewi$region=="European Union #",]$region <- "European Union"
ewi <- ewi %>% filter(region %in% countries,!is.na(region))
ewi %>% filter(period=="add19") %>% arrange(desc(value))

enu <- enu %>% select(-'2019...57',-'2008-18',-'2019...59') %>% rename('2019'=`2019...56`) %>% 
  mutate(add19 =`2019`-`2018`,add18 =`2018`-`2017`,add17 =`2017`-`2016`,add16 =`2016`-`2015`,add15 =`2015`-`2014`,add14 =`2014`-`2013`,add13 =`2013`-`2012`,add12 =`2012`-`2011`,add11 =`2011`-`2010`,
         r19 =(`2019`/`2018`)-1,r18 =(`2018`/`2017`)-1,r17 =(`2017`/`2016`)-1,r16 =(`2016`/`2015`)-1,r15 =(`2015`/`2014`)-1,r14 =(`2014`/`2013`)-1,r13 =(`2013`/`2012`)-1,r12 =(`2012`/`2011`)-1,r11 =(`2011`/`2010`)-1) %>% 
  rename(region='Terawatt-hours') %>% gather(period,value,-region) %>% mutate(variable="Generation|Nuclear")
enu[!is.na(enu$region) & enu$region=="European Union #",]$region <- "European Union"
enu <- enu %>% filter(region %in% countries,!is.na(region))
enu %>% filter(period=="add19") %>% arrange(desc(value))

ehy <- ehy %>% select(-'2019...57',-'2008-18',-'2019...59') %>% rename('2019'=`2019...56`) %>% 
  mutate(add19 =`2019`-`2018`,add18 =`2018`-`2017`,add17 =`2017`-`2016`,add16 =`2016`-`2015`,add15 =`2015`-`2014`,add14 =`2014`-`2013`,add13 =`2013`-`2012`,add12 =`2012`-`2011`,add11 =`2011`-`2010`,
         r19 =(`2019`/`2018`)-1,r18 =(`2018`/`2017`)-1,r17 =(`2017`/`2016`)-1,r16 =(`2016`/`2015`)-1,r15 =(`2015`/`2014`)-1,r14 =(`2014`/`2013`)-1,r13 =(`2013`/`2012`)-1,r12 =(`2012`/`2011`)-1,r11 =(`2011`/`2010`)-1) %>% 
  rename(region='Terawatt-hours') %>% gather(period,value,-region) %>% mutate(variable="Generation|Hydro")
ehy[!is.na(ehy$region) & ehy$region=="European Union #",]$region <- "European Union"
ehy <- ehy %>% filter(region %in% countries,!is.na(region))
ehy %>% filter(period=="add19") %>% arrange(desc(value))

gbo <- gbo %>% select(-'2019...57',-'2008-18',-'2019...59') %>% rename('2019'=`2019...56`) %>% 
  mutate(add19 =`2019`-`2018`,add18 =`2018`-`2017`,add17 =`2017`-`2016`,add16 =`2016`-`2015`,add15 =`2015`-`2014`,add14 =`2014`-`2013`,add13 =`2013`-`2012`,add12 =`2012`-`2011`,add11 =`2011`-`2010`,
         r19 =(`2019`/`2018`)-1,r18 =(`2018`/`2017`)-1,r17 =(`2017`/`2016`)-1,r16 =(`2016`/`2015`)-1,r15 =(`2015`/`2014`)-1,r14 =(`2014`/`2013`)-1,r13 =(`2013`/`2012`)-1,r12 =(`2012`/`2011`)-1,r11 =(`2011`/`2010`)-1) %>% 
  rename(region='Terawatt-hours') %>% gather(period,value,-region) %>% mutate(variable="Generation|AllOtherLowC")
gbo[!is.na(gbo$region) & gbo$region=="European Union #",]$region <- "European Union"
gbo <- gbo %>% filter(region %in% countries,!is.na(region))
gbo %>% filter(period=="add19") %>% arrange(desc(value))


bp <- rbind(eg,pv,wi,ewi,epv,enu,ehy,gbo)

#add total low carbon generation
lowc <- bp %>% spread(variable,value) %>% 
  mutate(`Generation|LowC`=`Generation|Hydro`+`Generation|Nuclear`+`Generation|Wind`+`Generation|PV`+`Generation|AllOtherLowC`)%>% 
  gather(variable,value,-period,-region) %>% filter(variable=="Generation|LowC")

bp <- rbind(bp,lowc)

fosl <- bp %>% spread(variable,value) %>% 
  mutate(`Generation_Fossil`=`SE|Electricity`-`Generation|LowC`)%>% 
  gather(variable,value,-period,-region) %>% filter(variable=="Generation_Fossil")

bp <- rbind(bp,fosl)
