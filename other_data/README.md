# Getting and preparing data for running the reduced analysis

In order to run the scripts (either reduced or full), you require the following list of files:

1. `Net_generation_from_coal_for_United_States_Lower_48_(region)_hourly_-_UTC_time.csv`     
	https://www.eia.gov/opendata/qb.php?category=3390105&sdid=EBA.US48-ALL.NG.COL.H (-> Download -> Download Data)
2. `Net_generation_from_natural_gas_for_United_States_Lower_48_(region)_hourly_-_UTC_time.csv`
	https://www.eia.gov/opendata/qb.php?category=3390105&sdid=EBA.US48-ALL.NG.NG.H (-> Download -> Download Data)
3. `Net_generation_from_petroleum_for_United_States_Lower_48_(region)_hourly_-_UTC_time.csv`    
	https://www.eia.gov/opendata/qb.php?category=3390105&sdid=EBA.US48-ALL.NG.OIL.H (-> Download -> Download Data)
4. `Demand_for_United_States_Lower_48_(region)_hourly_-_UTC_time.csv`     
	https://www.eia.gov/opendata/qb.php?category=3389935&sdid=EBA.US48-ALL.D.H (-> Download -> Download Data)
5. `World_bank_GDP_ppp.csv`           
        https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.KD then csv reformatted: comma to semicolon, added a unit entry in first header row              
6. `bp-stats-review-2020-all-data.xlsx`
	https://www.bp.com/content/dam/bp/business-sites/en/global/corporate/xlsx/energy-economics/statistical-review/bp-stats-review-2020-all-data.xlsx
7. `LeQuere.csv`                     
	This is the sheet "mean" from File 1 from https://www.icos-cp.eu/gcp-covid19 , saved as csv
8. `Confinement_index_11June2020.csv`                                  
	This is the sheet "CI level - global"  from File 5 from https://www.icos-cp.eu/gcp-covid19 , saved as csv
9. `Ember-Global-Electricity-Review-2020-half-year-data_data.csv`
    This is the sheet "data" from https://ember-climate.org/wp-content/uploads/2020/08/Ember-Global-Electricity-Review-2020-half-year-data.xlsx    , saved as csv


The following two additional files are already included in this git repository, listed here only for comprehensiveness

10. `India-power-carbon.xlsx`          
	created from manual queries from carbontracker.in 
11. `WEOJun2020update.csv` and `WEO-Oct2020update.csv`:
	This is sheet "Real GDP Growth" from https://www.imf.org/~/media/Files/Publications/WEO/2020/Update/June/English/data/WEOJun2020update.ashx?la=en , 
	saved as csv, and with additional entries for aggregate regions (World, Advanced Economies, Emerging and Developing Europe, Latin America and the Caribben, Middle East and Central Asia, Sub-Saharan Africa, Euro Area) from sheet "Table 1",
	Update October 2020: columns 2020 and 2021 have been updated with data from Tables A2 and A4 from https://www.imf.org/~/media/Files/Publications/WEO/2020/October/English/TablesA.ashx?la=en
