shinyUI(
    dashboardPage(
        dashboardHeader(title = "Population Aging"),
        dashboardSidebar(
            sidebarUserPanel("NYC DSA",
                             image = "https://cbkdsanightschool.files.wordpress.com/2017/12/cropped-nyc-dsa-logo-simple.gif?w=200"),
            sidebarMenu(
                id = "sidebar_options",
                menuItem("Population Age", tabName = "world_data", icon = icon("users")),
                menuItem("World Data", icon = icon("globe"),
                         menuSubItem("Health", tabName = "world_health", icon = icon("heart")),
                         menuSubItem("Labor", tabName = "world_labor", icon = icon("wrench"))
                ),
                menuItem("Region Data", tabName = "region_data", icon = icon("globe-americas"),
                         menuSubItem("Health", tabName = "region_health", icon = icon("heart")),
                         menuSubItem("Labor", tabName = "region_labor", icon = icon("wrench"))
                ),
                menuItem("Country Data", icon = icon("flag"),
                         menuSubItem("Time Series Data", tabName = "country_timeseries", icon = icon("chart-line")),
                         menuSubItem("Annual Data", tabName = "country_annual", icon = icon("chart-bar"))
                ),
                menuItem("Raw Data", tabName = "raw_data"),
                menuItem("About Me", tabName = "about_me")
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "world_data",
                        fluidRow(
                                box(id = "world_data_box",
                                       sliderInput(
                                           inputId="world_year_slider",
                                           label="Select Year:",
                                           min=1960,
                                           max=2018,
                                           value=2010,
                                           sep=""
                                       ),
                                        plotlyOutput("world_adro")
                                )
                        )
                ),
                tabItem(tabName = "world_health",
                        fluidRow(
                            box(id = "world_health_box",
                                plotlyOutput("world_pop_health")
                            )
                        )
                ),
                tabItem(tabName = "world_labor",
                                          fluidRow(
                                              box(id = "world_labor_box",
                                                  plotlyOutput("world_pop_labor")
                                              )
                                          )
                ),
                tabItem(tabName = "region_health",
                        fluidRow(
                            box(id = "region_health_box",
                                tabItem("Region Health place holder")
                                #plotlyOutput("region_pop_health")
                            )
                        )
                ),
                tabItem(tabName = "region_labor",
                        fluidRow(
                            box(id = "region_labor_box",
                                selectizeInput(inputId='region_labor_dropdown', 
                                               label='Region',
                                               choices=region_list),
                                plotlyOutput("region_literacy_labor")
                            )
                        )
                ),
                tabItem(tabName = "country_timeseries",
                        fluidRow(
                            tabBox(id = "time_series_box",
                                    tabPanel("Analysis",
                                             selectizeInput(inputId='country_timeseries_dropdown', 
                                                            label='Country',
                                                            choices=country_list,
                                                            selected = "Japan"),
                                             plotlyOutput("birth_death_graph")
                                    ),
                                    tabPanel("Data", "Datatable place holder")
                            )
                        )
                ),
                tabItem(tabName = "country_annual",
                        fluidRow(
                            tabBox(id = "annual_box",
                                   tabPanel("Analysis",
                                            selectizeInput(inputId='country_annual_dropdown', 
                                                           label='Country',
                                                           choices=country_list,
                                                           selected = "Japan"),
                                            sliderInput(
                                                inputId="country_annual_year_slider",
                                                label="Select Year:",
                                                min=1960,
                                                max=2018,
                                                value=2010,
                                                sep=""
                                            ),
                                            plotlyOutput("pop_age_graph")
                                   ),
                                   tabPanel("Data", "Datatable place holder")
                            )
                        )
                ),
                tabItem(tabName = "raw_data",
                        fluidRow(
                                dataTableOutput('data_table')
                        )
                ),
                tabItem(tabName = "about_me",
                        fluidRow(
                                 tabPanel("About Me", "About Me place holder")
                        )
                )
            )
        )
    )
)
