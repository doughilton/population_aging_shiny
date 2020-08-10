shinyUI(
    dashboardPage(
        dashboardHeader(title = "Population Aging"),
        
        dashboardSidebar(
            sidebarUserPanel("NYC DSA",
                image = "https://cbkdsanightschool.files.wordpress.com/2017/12/cropped-nyc-dsa-logo-simple.gif?w=200"
            ),
            sidebarMenu(
                id = "sidebar_options",
                menuItem("Population Age", tabName = "world_data", icon = icon("users")),
                menuItem("World Data", icon = icon("globe"),
                    menuSubItem("Health", tabName = "world_health", icon = icon("heart")),
                    menuSubItem("Labor + Education", tabName = "world_labor", icon = icon("wrench"))
                ),
                menuItem("Country Data", tabName = "country_data", icon = icon("flag")),
                menuItem("Raw Data", tabName = "raw_data", icon = icon("database")),
                menuItem("About Me", tabName = "about_me", icon = icon("user"))
            )
        ),
        
        dashboardBody(
            tabItems(
                tabItem(tabName = "world_data",
                    fluidRow(
                        box(
                            column(12,
                                h1("Explore Aging Populations Around the World"),
                                br(),
                                h4("Left Links will bring you to Regional and Country"),
                                h4("data related to Aging, Health, Labor and Education"),
                                br(),
                                br(),
                                strong(htmlOutput("aging_title_text")),
                                br(),
                                htmlOutput("adro_1"),
                                htmlOutput("adro_2"),
                                htmlOutput("adro_3"),
                                htmlOutput("adro_4"),
                                htmlOutput("adro_5")
                            )
                        ),
                        box(id = "world_data_box",
                            sliderInput(
                                inputId = "world_year_slider",
                                label = "Select Year:",
                                min = 1960,
                                max = 2018,
                                value = 1990),
                            plotlyOutput("world_adro")
                        )
                    )
                ),
                tabItem(tabName = "world_health",
                    fluidRow(
                        box(id = "world_health_box",
                            width = 12,
                            h1("World Aging Populations and Health Care"),
                            h4("All data from 2016"),
                            br(),
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
                            width = 12,
                            h1("World Aging Populations, Labor and Education"),
                            h4("Unemployment data from 2019"),
                            br(),
                            fluidRow(
                                column(6,
                                    plotlyOutput("world_pop_labor"),
                                    selectizeInput(inputId = 'region_labor_dropdown', 
                                        label = 'Region',
                                        choices = region_list,
                                        width = 300)
                                )
                            ),
                            fluidRow(
                                column(6,
                                    plotlyOutput("region_labor_school")
                                ),
                                column(6,
                                    plotlyOutput("region_labor_literacy")
                                )
                            )
                        )
                    )
                ),
                tabItem(tabName = "country_data",
                    fluidRow(
                        box(id = "country_health_box",
                            width = 12,
                            h1("Country Aging Population Data"),
                            br(),
                            column(6,
                                selectizeInput(inputId='country_health_dropdown', 
                                    label = 'Country',
                                    choices = country_list,
                                    selected = "United States",
                                    width = 200),
                                plotlyOutput("birth_death_graph"),
                                sliderInput(
                                    inputId = "country_health_year_slider",
                                    label = "Select Year:",
                                    min = 1960,
                                    max = 2018,
                                    value = 1990,
                                    width = 500),
                                plotlyOutput("pop_age_graph")
                            ),
                            column(6,
                                br(),
                                br(),
                                br(),
                                br(),
                                plotlyOutput("growth_graph"),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                plotlyOutput("country_lab_65_ratios")
                            )
                        )
                    )
                ),
                tabItem(tabName = "raw_data",
                    fluidRow(
                        box(width = 7,
                            column(12,
                                h1("Data for the project was acquired from the World Bank"),
                                h4("The data below is filtered to only what was needed for this site"),
                                h4("You can retrieve the full data set by clicking the link below"),
                                br(),
                                h4(uiOutput("data_table_hnp_url")),
                                br()
                            )
                        )
                    ),
                    br(),
                    fluidRow(
                        box(width = 12,
                            dataTableOutput('data_table')
                        )
                    )
                ),
                tabItem(tabName = "about_me",
                    fluidRow(
                        box(width = 6,
                            column(3,
                                br(),
                                br(),
                                img(
                                    src = "doug_headshot.jpeg",
                                    height = "150px",
                                    width = "150px"
                                )
                            ),
                            column(8,
                                h1("Douglas Hilton"),
                                h4("douglas.hilton@gmail.com"),
                                br(),
                                tags$a(
                                    href = "https://github.com/doughilton",
                                    img(
                                        src = "github.png",
                                        title = "github",
                                        height = "40px"
                                    )
                                ),
                                br(),
                                h4("Hello, I'm currently a student at the NYC Data Science Academy."),
                                h4("This project helped me explore data cleaning, manipulation,"),
                                h4("plotting, and specifically use of Shiny.")
                            )
                        )
                    )
                )
            )
        )
    )
)
