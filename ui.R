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
                menuItem("Country Data", tabName = "country_data", icon = icon("flag"),
                         menuSubItem("Health", tabName = "country_health", icon = icon("heart")),
                         menuSubItem("Labor", tabName = "country_labor", icon = icon("wrench"))
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
                                       value=1990,
                                       sep=""),
                                    plotlyOutput("world_adro")
                                )
                        )
                ),
                tabItem(tabName = "world_health",
                        fluidRow(
                            box(id = "world_health_box",
                                width = 12,
                                column(6,
                                        plotlyOutput("world_health_expenditure")
                                ),
                                column(6,
                                       plotlyOutput("world_health_phys")
                                )
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
                                width = 12,
                                column(6,
                                        selectizeInput(inputId='region_labor_dropdown', 
                                                       label='Region',
                                                       choices=region_list),
                                        plotlyOutput("region_labor_school"),
                                        plotlyOutput("region_labor_literacy")
                                )
                            )
                        )
                ),
                tabItem(tabName = "country_health",
                        fluidRow(
                            box(id = "country_health_box",
                                width = 12,
                                 column(6,
                                        selectizeInput(inputId='country_health_dropdown', 
                                                        label='Country',
                                                        choices=country_list,
                                                        selected = "Japan",
                                                        width = 200),
                                        plotlyOutput("birth_death_graph"),
                                        plotlyOutput("country_lab_65_ratios")
                                 ),
                                 column(6,
                                        sliderInput(
                                             inputId="country_health_year_slider",
                                             label="Select Year:",
                                             min=1960,
                                             max=2018,
                                             value=1990,
                                             sep=""),
                                        plotlyOutput("pop_age_graph")
                                 )
                            )
                        )
                ),
                tabItem(tabName = "country_labor",
                        fluidRow(
                            tabBox(id = "country_labor_box",
                                   tabPanel("Analysis", "Analysis place holder"
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
