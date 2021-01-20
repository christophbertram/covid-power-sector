##### main file for analysis
##### 
#####

#required packages
# general data structures
library(tidyverse)
library(data.table)
#reading in excel data
library(readxl)
#smoothing time series data
library(zoo)

#switch to select either full analysis (T) or simple analysis (F, default)
#Full analysis reproduces all paper figures, but requires data from ENTSO-E which is very large, can take a few days to obtain (see ENTSO-E_data/README.txt for details), and leads to longer code run times
#The default simple analysis setting only requires easily available data sources (see other_data/README.txt for how to obtain and prepare them), and runs relatively quickly, and reproduces Figure 1 and Extended Data Figures 1, and 3
full_analysis <- F

# read in IMF and BP data
source("02_read_in_imf_bp.R")

# extrapolate generation 2020-2024 
source("03_extrapolate_bp.R")

#read in H1 2020 data from ember for comparison
source("04_ember_data.R")

# plot panels for Figure 1a and b, and Extended Data 1
source("05_plot_power_sector.R")

# plot Figure 1c and Extended Data Figure 3
source("06_emissions_2020-24.R")

# display of numbers cited in the paper:
# power sector 2020 emission reduction based on projected low-carbon and total generation
traj %>% filter(region=="World",period=="2020",scen %in%c("high","med","low","veryhigh","verylow")) %>% select(value)/traj[traj$region=="World"&traj$period=="2019" & traj$scen=="med",]$value-1

#China power sector emissions share over time
traj %>% filter(period %in% seq(2017,2020),region %in% c("China","World"),scen=="med")%>% group_by(period) %>% spread(region,value) %>%
  mutate(share=`China`/`World`) 


if(isTRUE(full_analysis)){
#read in recent/historical power sector data (generation and emissions) from EU (ENTSO-E), US(EIA), and India (carbontracker.in, manually copied)
 source("07_power_data.R") #plot Figure2
## Figure on coal and gas in EU and US (involves manual run with updated data, some numbers hard-coded)
 source("08_coal_gas_figure.R") #plot Extended Data Figure 2
#print relative reductions:
 arrow %>% select(region,fuel,red)
 arrow2%>% select(region,fuel,red)
# plot SI Figure S3 with GDP and demand growth, using world bank data
 source("09_GDP_demand.R")
#data work for daily model from Le Quere et al 2020, ploting SI Figures S1 and S2
 source("10_daily_model.R")
# display of numbers cited in the paper, from changed and original replicated Le Quere model:

# total 2020 emission reduction based on daily model - Best Guess with our assumptions
dt_all %>% filter(variable=="TOTAL_CO2" & REGION_CODE == "GLOBAL" & date<"2021-01-01" & scenario %in% c("emi_b2_low",
                                                                                                        "emi_b2_med","emi_b2_high","constant")) %>% 
  group_by(scenario) %>% summarise(value=sum(value)/36604-1) 

# power sector 2020 emission reduction based on daily model - Best Guess with our assumptions
dt_all %>% filter(variable=="POWER" & REGION_CODE == "GLOBAL" & date<"2021-01-01" & scenario %in% c("emi_b2_low",
                                                                                                    "emi_b2_med","emi_b2_high","constant")) %>% 
  group_by(scenario) %>% summarise(value=sum(value)/16238-1) 

# power sector 2020 emission reduction based on daily model - replicated Le Quere et al specification
dt_all %>% filter(variable=="POWER" & REGION_CODE == "GLOBAL" & date<"2021-01-01" & scenario %in% c("emi_s3_low",
                                                                                                    "emi_s3_med","emi_s3_high","constant")) %>% 
  group_by(scenario) %>% summarise(value=sum(value)/16238-1) 

}

