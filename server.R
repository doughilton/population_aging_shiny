shinyServer(function(input, output, session) {
    
    #Landing Page - Population Age
    adro_top_values = reactive(
        hnp_raw %>% 
            filter(Indicator.Code == age_dependency_ratio_old) %>%
            arrange(desc(!!sym(paste0("X", input$world_year_slider)))) %>%
            select(Country.Name, paste0("X", input$world_year_slider)) %>%
            head(5)
    )
    
    output$world_adro = renderPlotly({
        hnp_raw_adro = hnp_raw %>% filter(Indicator.Code == age_dependency_ratio_old)
        world_adro_year_column = paste0("X", input$world_year_slider)
        
        plot_geo(hnp_raw_adro) %>% 
            add_trace(z = hnp_raw_adro[, world_adro_year_column], 
                color = hnp_raw_adro[, world_adro_year_column], 
                colors = 'Reds',
                text = hnp_raw_adro$Country.Name, 
                locations = hnp_raw_adro$Country.Code, 
                marker = list(line = list(width = 0.5))) %>% 
            colorbar(limits = c(0, 50)) %>%
            layout(geo = list(showframe = FALSE))
    })
    
    output$aging_title_text = renderText({
        paste0("Top 5 Countries by Older Dependent Population %: ", input$world_year_slider)
    })
    
    output$adro_1 = renderText({
        paste0(as.character(adro_top_values()[1,1]), ": ", as.character(adro_top_values()[1,2]))
    })
    
    output$adro_2 = renderText({
        paste0(as.character(adro_top_values()[2,1]), ": ", as.character(adro_top_values()[2,2]))
    })
    
    output$adro_3 = renderText({
        paste0(as.character(adro_top_values()[3,1]), ": ", as.character(adro_top_values()[3,2]))
    })
    
    output$adro_4 = renderText({
        paste0(as.character(adro_top_values()[4,1]), ": ", as.character(adro_top_values()[4,2]))
    })
    
    output$adro_5 = renderText({
        paste0(as.character(adro_top_values()[5,1]), ": ", as.character(adro_top_values()[5,2]))
    })
    
    #World Data - Health
    output$world_health_expenditure = renderPlotly({
        hnp_raw_health_expenditure = hnp_raw %>% filter(Indicator.Code %in% c(pop_65_above_percent, health_expenditure_percent_gdp), Country.Name %in% country_list) %>% 
            select(Country.Name, Indicator.Code, X2016) %>% 
            spread(Indicator.Code, X2016)
    
        #R doesn't like column names with spaces, but they look better for graphs
        #Do the rename here for display but keep the data frame clean
        ggplotly(hnp_raw_health_expenditure %>% select (`Pop. 65+` = pop_65_above_percent, `% GDP` = health_expenditure_percent_gdp, `Country Name` = Country.Name) %>%
            ggplot(aes(`Pop. 65+`, `% GDP`, label = `Country Name`)) + 
            geom_point(colour = "navyblue") +
            labs(title = "Health Care Expenditure", 
                x = "Population age 65+ %", 
                y = "Expenditure % of GDP")
            #geom_text_repel is not supported by ggplotly unfortunately
            #geom_text_repel(max.overlaps = 10)
        )
    })
    
    output$world_health_phys = renderPlotly({
        hnp_raw_phys = hnp_raw %>% filter(Indicator.Code %in% c(pop_65_above_percent, physician_per_1000), Country.Name %in% country_list) %>% 
            select(Country.Name, Indicator.Code, X2016) %>% 
            spread(Indicator.Code, X2016)
        
        ggplotly(hnp_raw_phys %>% select (`Pop. 65+` = pop_65_above_percent, `# Phys` = physician_per_1000, `Country Name` = Country.Name) %>%
            ggplot(aes(`Pop. 65+`, `# Phys`, label = `Country Name`)) + 
            geom_point(colour = "navyblue") +
            labs(title = "Number of Physicians", 
                x = "Population age 65+ %", 
                y = "Physicians (per 1,000 people)")
            #geom_text_repel(max.overlaps = 10)
        )
    })
    
    #World Data - Labor
    output$world_pop_labor = renderPlotly({
        hnp_raw_pop_unem = hnp_raw %>% filter(Indicator.Code %in% c(pop_65_above_percent, unemployment_percent), Country.Name %in% country_list) %>% 
            select(Country.Name, Indicator.Code, X2019) %>% 
            spread(Indicator.Code, X2019)
        
        ggplotly(hnp_raw_pop_unem %>% select (`Pop. 65+` = pop_65_above_percent, `Unemployment %` = unemployment_percent, `Country Name` = Country.Name) %>%
            ggplot(aes(`Pop. 65+`, `Unemployment %`, label = `Country Name`)) + 
            geom_point(colour = "navyblue") +
            labs(title = "Unemployment Percent", 
                x = "Population age 65+ %", 
                y = "Unemployment %")
        )
    })
    
    output$region_labor_school = renderPlotly({
        hnp_region_labor_school = hnp_raw %>% filter(Country.Name == input$region_labor_dropdown, Indicator.Code %in% c(school_enrollment_primary, school_enrollment_secondary, school_enrollment_tertiary)) %>% 
            gather(key = "Year", value = "Value", year_column_names) %>% 
            mutate(Year = as.numeric(sub("X", "", Year)))
        
        hnp_region_labor_school$Indicator.Code = ifelse(hnp_region_labor_school$Indicator.Code == school_enrollment_primary, "Primary School", ifelse(hnp_region_labor_school$Indicator.Code == school_enrollment_secondary, "Secondary School", "Tertiary School"))
        
        ggplotly(hnp_region_labor_school %>% 
            ggplot(aes(Year, Value)) + 
            geom_line(aes(color = Indicator.Code)) + 
            theme(axis.text.x = element_text(angle = -45)) +
            labs(title = "Student School Enrollment",
                x = "Year", 
                y = "School enrollment (% gross)", 
                color = "School Level")
        )
    })
    
    output$region_labor_literacy = renderPlotly({
        hnp_region_labor_literacy = hnp_raw %>% filter(Country.Name == input$region_labor_dropdown, Indicator.Code == literacy_rate_adult) %>% 
            gather(key = "Year", value = "Value", year_column_names) %>% 
            mutate(Year = as.numeric(sub("X", "", Year)))
        
        ggplotly(hnp_region_labor_literacy %>% 
            ggplot(aes(Year, Value)) + 
            geom_line(color = "navyblue") + 
            theme(axis.text.x = element_text(angle = -45)) +
            labs(title = "Literacy Rate", 
                x = "Year", 
                y = "Literacy rate % (Age 15+)")
        )
    })
    
    #Country Data
    output$birth_death_graph = renderPlotly({
        hnp_bdg = hnp_raw %>% filter(Country.Name == input$country_health_dropdown, Indicator.Code %in% c(pop_birth_rate, pop_death_rate)) %>% 
            gather(key = "Year", value = "Value", year_column_names, na.rm = TRUE) %>% 
            mutate(Year = as.numeric(sub("X", "", Year)))
        
        hnp_bdg$Indicator.Code = ifelse(hnp_bdg$Indicator.Code == pop_birth_rate, "Birth Rate", "Death Rate")
        
        ggplotly(hnp_bdg %>%
            ggplot(aes(Year, Value)) + 
            geom_line(aes(color = Indicator.Code)) +
            theme(axis.text.x = element_text(angle = -45)) +
            labs(title = "Birth and Death Rates",
                x = "Year", 
                y = "Rate per 1,000 people", 
                color = "Pop Change Rate")
        )
    })
    
    output$growth_graph = renderPlotly({
        hnp_bdg = hnp_raw %>% filter(Country.Name == input$country_health_dropdown, Indicator.Code == pop_growth_rate) %>% 
            gather(key = "Year", value = "Value", year_column_names, na.rm = TRUE) %>% 
            mutate(Year = as.numeric(sub("X", "", Year)))
        
        ggplotly(hnp_bdg %>%
            ggplot(aes(Year, Value)) + 
            geom_line(color = "navyblue") + 
            theme(axis.text.x = element_text(angle = -45)) +
            labs(title = "Population Growth",
                x = "Year", 
                y = "Growth %")
        )
    })
    
    output$pop_age_graph = renderPlotly({
        hnp_raw_age_count = hnp_raw %>% filter(Country.Name == input$country_health_dropdown, Indicator.Code %in% population_count_indicator_codes) %>% 
            gather(key = "Year", value = "Value", year_column_names, na.rm = TRUE) %>% 
            mutate(Year = as.numeric(sub("X", "", Year))) %>% 
            filter(Year == input$country_health_year_slider) %>%
            mutate(Indicator.Code = substr(Indicator.Code, 8, 11)) %>%
            group_by(Indicator.Code) %>%
            summarise(pop_count = sum(Value))
        
        ggplotly(hnp_raw_age_count %>%
            ggplot(aes(Indicator.Code, pop_count / 10000)) + 
            geom_bar(stat = "identity", fill = "navyblue") + 
            scale_x_discrete(labels = pop_age_labels) +
            theme(axis.text.x = element_text(angle = -45)) +
            labs(title = "Population Age Groups",
                x = "Age", 
                y = "Count in 10,000s")
        )
    })
    
    output$country_lab_65_ratios = renderPlotly({
        hnp_raw_lab_for = hnp_raw %>% filter(Indicator.Code == labor_force_total, Country.Name == input$country_health_dropdown) %>% 
            gather(key = "Year", value = "Value", year_column_names) 
        hnp_raw_tot_pop = hnp_raw %>% filter(Indicator.Code == total_pop, Country.Name == input$country_health_dropdown) %>% 
            gather(key = "Year", value = "Value", year_column_names) 
        
        hnp_raw_lab_pop_ratio = hnp_raw_lab_for %>% inner_join(hnp_raw_tot_pop, by = "Year") %>% 
            mutate(work_force_percent = Value.x / Value.y * 100) %>% 
            select(Country.Name = Country.Name.x, Country.Code = Country.Code.x, Indicator.Name = Indicator.Name.x, Indicator.Code = Indicator.Code.x, X = X.x, Year, Value = work_force_percent)
        
        hnp_raw_over_65 = hnp_raw %>% filter(Indicator.Code == pop_65_above_percent, Country.Name == input$country_health_dropdown) %>% 
            gather(key = "Year", value = "Value", year_column_names) 
        
        hnp_raw_lab_65_ratios = rbind(hnp_raw_over_65, hnp_raw_lab_pop_ratio) %>% mutate(Year = as.numeric(sub("X", "", Year)))
        
        hnp_raw_lab_65_ratios$Indicator.Code = ifelse(hnp_raw_lab_65_ratios$Indicator.Code == pop_65_above_percent, "Age 65+", "Labor Force")
        
        ggplotly(hnp_raw_lab_65_ratios %>% 
            ggplot(aes(Year, Value)) + 
            geom_line(aes(color = Indicator.Code)) + 
            theme(axis.text.x = element_text(angle = -45)) +
            labs(title = "Older Population and Labor Force",
                x = "Year", 
                y = "Percent", 
                color = "Pop Percents")
        )
    })
                 
    #Raw Data
    output$data_table=renderDataTable(
        datatable(hnp_raw, rownames = FALSE, options = list(scrollX = TRUE))
    )
    
    hnp_url = a("HNP", href="https://datacatalog.worldbank.org/dataset/health-nutrition-and-population-statistics")
    
    output$data_table_hnp_url <- renderUI({
        tagList("Health Nutrition And Population Statistics:", hnp_url)
    })
    
})