shinyServer(function(input, output, session) {
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
        paste("Top 5 Countries by Older Dependent Population %: ", input$world_year_slider)
    })
    
    output$adro_1 = renderText({
        paste(as.character(adro_top_values()[1,1]), " - ", as.character(adro_top_values()[1,2]))
    })
    
    output$adro_2 = renderText({
        paste(as.character(adro_top_values()[2,1]), " - ", as.character(adro_top_values()[2,2]))
    })
    
    output$adro_3 = renderText({
        paste(as.character(adro_top_values()[3,1]), " - ", as.character(adro_top_values()[3,2]))
    })
    
    output$adro_4 = renderText({
        paste(as.character(adro_top_values()[4,1]), " - ", as.character(adro_top_values()[4,2]))
    })
    
    output$adro_5 = renderText({
        paste(as.character(adro_top_values()[5,1]), " - ", as.character(adro_top_values()[5,2]))
    })
    
    output$world_health_expenditure = renderPlotly({
        hnp_raw_health_expenditure = hnp_raw %>% filter(Indicator.Code %in% c(pop_65_above_percent, health_expenditure_percent_gdp), Country.Name %in% country_list) %>% 
            select(Country.Name, Indicator.Code, X2017) %>% 
            spread(Indicator.Code, X2017)
        
        ggplotly(hnp_raw_health_expenditure %>%
            ggplot(aes(SP.POP.65UP.TO.ZS, SH.XPD.CHEX.GD.ZS, label = Country.Name)) + 
                geom_point(colour = "navyblue") +
                xlab("Population age 65+ %") +
                ylab("Expenditure % of GDP") +
                ggtitle("Health Care Expenditure") #+
                #geom_text_repel(max.overlaps = 10)
        )
    })
    
    output$world_health_phys = renderPlotly({
        hnp_raw_phys = hnp_raw %>% filter(Indicator.Code %in% c(pop_65_above_percent, physician_per_1000), Country.Name %in% country_list) %>% 
            select(Country.Name, Indicator.Code, X2017) %>% 
            spread(Indicator.Code, X2017)
        
        ggplotly(hnp_raw_phys %>%
            ggplot(aes(SP.POP.65UP.TO.ZS, SH.MED.PHYS.ZS, label = Country.Name)) + 
                 geom_point(colour = "navyblue") +
                 xlab("Population age 65+ %") +
                 ylab("Physicians (per 1,000 people)") +
                 ggtitle("Number of Physicians") #+
                 #geom_text_repel(max.overlaps = 10)
        )
    })
    
    output$world_pop_labor = renderPlotly({
        hnp_raw_pop_unem = hnp_raw %>% filter(Indicator.Code %in% c(pop_65_above_percent, unemployment_percent), Country.Name %in% country_list) %>% 
            select(Country.Name, Indicator.Code, X2019) %>% 
            spread(Indicator.Code, X2019)
        
        ggplotly(hnp_raw_pop_unem %>%
                     ggplot(aes(SP.POP.65UP.TO.ZS, SL.UEM.TOTL.ZS, label = Country.Name)) + 
                     geom_point(colour = "navyblue") +
                     xlab("Population age 65+ %") +
                     ylab("Unemployment %") +
                     ggtitle("Unemployment Percent")
        )
    })
    
    output$region_labor_school = renderPlotly({
        hnp_region_labor_school = hnp_raw %>% filter(Country.Name == input$region_labor_dropdown, Indicator.Code %in% c(school_enrollment_primary, school_enrollment_secondary, school_enrollment_tertiary)) %>% 
            gather(key = "Year", value = "Value", year_column_names) %>% 
            mutate(Year = as.numeric(sub("X", "", Year)))
        
        ggplotly(hnp_region_labor_school %>% 
                     ggplot(aes(Year, Value)) + 
                     geom_line(aes(color = Indicator.Code)) + 
                     theme(axis.text.x = element_text(angle = -45)) +
                     xlab("Year") +
                     ylab("School enrollment (% gross)") +
                     ggtitle("Student School Enrollment") +
                     scale_color_discrete(name = "School Level", labels = c("Primary", "Secondary", "Tertiary"))
        )
    })
    
    output$region_labor_literacy = renderPlotly({
        hnp_region_labor_literacy = hnp_raw %>% filter(Country.Name == input$region_labor_dropdown, Indicator.Code == literacy_rate_adult) %>% 
            gather(key = "Year", value = "Value", year_column_names) %>% 
            mutate(Year = as.numeric(sub("X", "", Year)))
        
        ggplotly(hnp_region_labor_literacy %>% 
                     ggplot(aes(Year, Value)) + 
                     geom_line(aes(color = Indicator.Code)) + 
                     theme(axis.text.x = element_text(angle = -45)) +
                     xlab("Year") +
                     ylab("Literacy rate % (Age 15+)") +
                     ggtitle("Literacy Rate") +
                     scale_color_discrete(guide = "none")
        )
    })
    
    output$birth_death_graph = renderPlotly({
        hnp_bdg = hnp_raw %>% filter(Country.Name == input$country_health_dropdown, Indicator.Code %in% c("SP.DYN.CBRT.IN", "SP.DYN.CDRT.IN")) %>% 
            gather(key = "Year", value = "Value", year_column_names, na.rm = TRUE) %>% 
            mutate(Year = as.numeric(sub("X", "", Year)))
        
        ggplotly(hnp_bdg %>%
                 ggplot(aes(Year, Value)) + 
                     geom_line(aes(color = Indicator.Code)) +
                     theme(axis.text.x = element_text(angle = -45)) +
                     xlab("Year") +
                     ylab("Rate per 1,000 people") +
                     ggtitle("Birth and Death Rates") +
                     scale_y_continuous(sec.axis = sec_axis(~., name = "Growth %"))
        )
    })
    
    output$growth_graph = renderPlotly({
        hnp_bdg = hnp_raw %>% filter(Country.Name == input$country_health_dropdown, Indicator.Code %in% c("SP.POP.GROW")) %>% 
            gather(key = "Year", value = "Value", year_column_names, na.rm = TRUE) %>% 
            mutate(Year = as.numeric(sub("X", "", Year)))
        
        ggplotly(hnp_bdg %>%
            ggplot(aes(Year, Value)) + 
                geom_line() + 
                theme(axis.text.x = element_text(angle = -45)) +
                xlab("Year") +
                ylab("Growth %") +
                ggtitle("Population Growth") +
                scale_y_continuous(sec.axis = sec_axis(~., name = "Growth %"))
        )
    })
    
    output$pop_age_graph = renderPlotly({
        ggplotly(hnp_raw %>% filter(Country.Name == input$country_health_dropdown, Indicator.Code %in% population_count_indicator_codes) %>% 
                 gather(key = "Year", value = "Value", year_column_names, na.rm = TRUE) %>% 
                 mutate(Year = as.numeric(sub("X", "", Year))) %>% 
                 filter(Year == input$country_health_year_slider) %>%
                 mutate(Indicator.Code = substr(Indicator.Code, 8, 11)) %>%
                 group_by(Indicator.Code) %>%
                 summarise(pop_count = sum(Value)) %>%
                 ggplot(aes(Indicator.Code, pop_count / 10000)) + 
                     geom_bar(stat = "identity", fill = "navyblue") + 
                     scale_x_discrete(labels = pop_age_labels) +
                     theme(axis.text.x = element_text(angle = -45)) +
                     xlab("Age") +
                     ylab("Count in 10,000s") +
                     ggtitle("Population Age Groups")
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
        
        ggplotly(hnp_raw_lab_65_ratios %>% 
            ggplot(aes(Year, Value)) + 
                geom_line(aes(color = Indicator.Code)) + 
                theme(axis.text.x = element_text(angle = -45)) +
                xlab("Year") +
                ylab("Percent") +
                ggtitle("Older Population and Labor Force") +
                scale_color_discrete(guide = "none", labels = c("Labor", "Age"))
        )
    })
                 
    
    output$data_table=renderDataTable(
        datatable(hnp_raw,rownames = F) %>% 
            formatStyle(input$selected,  
                        background="skyblue", fontWeight='bold')
    )
    
    hnp_url = a("HNP", href="https://datacatalog.worldbank.org/dataset/health-nutrition-and-population-statistics")
    
    output$data_table_hnp_url <- renderUI({
        tagList("Health Nutrition And Population Statistics:", hnp_url)
    })
    
})