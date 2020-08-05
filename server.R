shinyServer(function(input, output, session) {
    output$world_adro = renderPlotly({
        hnp_raw_adro = hnp_raw %>% filter(Indicator.Code == age_dependency_ratio_old)
        world_adro_year_colum = paste0("X", input$world_year_slider)
        
        plot_geo(hnp_raw_adro) %>% 
            add_trace(z = hnp_raw_adro[, world_adro_year_colum], 
                      color = hnp_raw_adro[, world_adro_year_colum], 
                      colors = 'Reds',
                      text = hnp_raw_adro$Country.Name, 
                      locations = hnp_raw_adro$Country.Code, 
                      marker = list(line = list(width = 0.5))) %>% 
            colorbar(limits = c(0, 50)) %>%
            layout(geo = list(showframe = FALSE))
    })
    
    output$world_pop_health = renderPlotly({
        hnp_raw_pop_health = hnp_raw %>% filter(Indicator.Code %in% c(pop_65_above_percent, health_expenditure_percent_gdp), Country.Name %in% country_list) %>% 
            select(Country.Name, Indicator.Code, X2017) %>% 
            spread(Indicator.Code, X2017)
        
        ggplotly(hnp_raw_pop_health %>%
            ggplot(aes(SP.POP.65UP.TO.ZS, SH.XPD.CHEX.GD.ZS)) + 
                geom_point(aes(label = Country.Name)) +
                xlab("X Axis") + 
                ylab("Y Axis"))
    })
    
    output$world_pop_labor = renderPlotly({
        hnp_raw_pop_unem = hnp_raw %>% filter(Indicator.Code %in% c(pop_65_above_percent, unemployment_percent), Country.Name %in% country_list) %>% 
            select(Country.Name, Indicator.Code, X2019) %>% 
            spread(Indicator.Code, X2019)
        
        ggplotly(hnp_raw_pop_unem %>%
                     ggplot(aes(SP.POP.65UP.TO.ZS, SL.UEM.TOTL.ZS)) + 
                     geom_point(aes(label = Country.Name)) +
                     xlab("X Axis") + 
                     ylab("Y Axis"))
    })
    
    output$region_literacy_labor = renderPlotly({
        hnp_country_labor_literacy = hnp_raw %>% filter(Country.Name == input$region_labor_dropdown, Indicator.Code %in% c(school_enrollment_primary, school_enrollment_secondary, school_enrollment_tertiary)) %>% 
            gather(key = "Year", value = "Value", year_column_names) %>% 
            mutate(Year = as.numeric(sub("X", "", Year)))
        
        ggplotly(hnp_country_labor_literacy %>% 
                     ggplot(aes(Year, Value)) + 
                     geom_line(aes(color = Indicator.Code)) + 
                     theme(axis.text.x = element_text(angle = -45)) +
                     xlab("X Axis") + 
                     ylab("Y Axis"))
    })
    
    output$birth_death_graph = renderPlotly({
        ggplotly(hnp_raw %>% filter(Country.Name == input$country_timeseries_dropdown, Indicator.Code %in% c("SP.DYN.CBRT.IN", "SP.DYN.CDRT.IN", "SP.POP.GROW")) %>% 
                gather(key = "Year", value = "Value", year_column_names, na.rm = TRUE) %>% 
                mutate(Year = as.numeric(sub("X", "", Year))) %>%
                ggplot(aes(Year, Value)) + 
                    geom_point(aes(color = Indicator.Code)) + 
                    geom_smooth(aes(color = Indicator.Code)) + 
                    theme(axis.text.x = element_text(angle = -45))
        )
    })
    
    output$pop_age_graph = renderPlotly({
        ggplotly(hnp_raw %>% filter(Country.Name == input$country_annual_dropdown, Indicator.Code %in% population_count_indicator_codes) %>% 
                 gather(key = "Year", value = "Value", year_column_names, na.rm = TRUE) %>% 
                 mutate(Year = as.numeric(sub("X", "", Year))) %>% 
                 filter(Year == input$country_annual_year_slider) %>%
                 mutate(Indicator.Code = substr(Indicator.Code, 8, 11)) %>%
                 group_by(Indicator.Code) %>%
                 summarise(pop_count = sum(Value)) %>%
                 ggplot(aes(Indicator.Code, pop_count)) + 
                     geom_bar(stat = "identity") + 
                     scale_x_discrete(labels = pop_age_labels) +
                     theme(axis.text.x = element_text(angle = -45))
        )
    })
    
    output$data_table=renderDataTable(
        datatable(hnp_raw,rownames = F) %>% 
            formatStyle(input$selected,  
                        background="skyblue", fontWeight='bold')
    )
})

# output$region_share = renderPlotly({
#     ggplotly(clean %>% filter(., Subregion==input$region_region) %>% 
#                  ggplot(., aes_string(x="Year", y=input$region_data, color="Country")) +
#                  geom_line() +
#                  ylim(0,100) +
#                  ylab("Renewable Share (%)") +
#                  xlab("") +
#                  ggtitle("Renewable Share of Energy") +
#                  theme_bw()
#     )
# })