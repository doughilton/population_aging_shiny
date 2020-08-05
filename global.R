library(shiny)
library(dplyr)
library(plotly)
library(shinydashboard)
library(DT)
library(tidyr)

hnp_raw = read.csv("./data/hnp_stats_csv/HNP_StatsData.csv")

hnp_country_region_raw = read.csv("./data/hnp_stats_csv/HNP_StatsCountry.csv")
countrycode_type = read.csv("./data/hnp_stats_csv/CountryCode_Type.csv")

year_column_names = paste0("X", 1960:2019)

country_list = unique((hnp_raw %>% filter(!(Country.Code %in% countrycode_type$Country.Code)))$Country.Name)
region_list = unique((hnp_raw %>% filter(Country.Code %in% countrycode_type[countrycode_type$Type == "Region",]$Country.Code))$Country.Name)

population_count_indicator_codes = paste0('SP.POP.', rep(c("0004", "0509", "1014", "1519", "2024", "2529", "3034", "3539", "4044", "4549", "5054", "5559", "6064", "6569", "7074", "7579", "80UP"), 2), ".", c("FE", "MA"))
pop_age_labels = c("0 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29", "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80+")

age_dependency_ratio_old = "SP.POP.DPND.OL"
pop_65_above_percent = "SP.POP.65UP.TO.ZS"
health_expenditure_percent_gdp = "SH.XPD.CHEX.GD.ZS"
unemployment_percent = "SL.UEM.TOTL.ZS"
literacy_rate_adult = "SE.ADT.LITR.ZS"
school_enrollment_primary = "SE.PRM.ENRR"
school_enrollment_secondary = "SE.SEC.ENRR"
school_enrollment_tertiary = "SE.TER.ENRR"

# Increase in poverty gap at $1.90 ($ 2011 PPP) poverty line due to out-of-pocket health care expenditure (USD)
# Number of people pushed below the $1.90 ($ 2011 PPP) poverty line by out-of-pocket health care expenditure
# Increase in poverty gap at $1.90 ($ 2011 PPP) poverty line due to out-of-pocket health care expenditure (% of poverty line)
# Proportion of population pushed below the $1.90 ($ 2011 PPP) poverty line by out-of-pocket health care expenditure (%)
# Increase in poverty gap at $3.20 ($ 2011 PPP) poverty line due to out-of-pocket health care expenditure (USD)
# Number of people pushed below the $3.20 ($ 2011 PPP) poverty line by out-of-pocket health care expenditure
# Increase in poverty gap at $3.20 ($ 2011 PPP) poverty line due to out-of-pocket health care expenditure (% of poverty line)
# Proportion of population pushed below the $3.20 ($ 2011 PPP) poverty line by out-of-pocket health care expenditure (%)
#Increase in poverty gap due to out of pocket health care expenditure
# SH.UHC.NOP1.CG
# SH.UHC.NOP1.TO
# SH.UHC.NOP1.ZG
# SH.UHC.NOP1.ZS
# SH.UHC.NOP2.CG
# SH.UHC.NOP2.TO
# SH.UHC.NOP2.ZG
# SH.UHC.NOP2.ZS