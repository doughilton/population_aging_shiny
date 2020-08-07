library(shiny)
library(dplyr)
library(plotly)
library(shinydashboard)
library(DT)
library(tidyr)
library(ggrepel)

hnp_raw = read.csv("./data/hnp_stats_csv/HNP_StatsData_Filtered.csv")

hnp_country_region_raw = read.csv("./data/hnp_stats_csv/HNP_StatsCountry.csv")
countrycode_type = read.csv("./data/hnp_stats_csv/CountryCode_Type.csv")

year_column_names = paste0("X", 1960:2019)

country_list = unique((hnp_raw %>% filter(!(Country.Code %in% countrycode_type$Country.Code)))$Country.Name)
region_list = unique((hnp_raw %>% filter(Country.Code %in% countrycode_type[countrycode_type$Type == "Region",]$Country.Code) %>% filter(!(Country.Name %in% region_exclusion_list)))$Country.Name)

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
labor_force_total = "SL.TLF.TOTL.IN"
total_pop = "SP.POP.TOTL"
physician_per_1000 = "SH.MED.PHYS.ZS"
pop_birth_rate = "SP.DYN.CBRT.IN"
pop_death_rate = "SP.DYN.CDRT.IN"
pop_growth_rate = "SP.POP.GROW"
