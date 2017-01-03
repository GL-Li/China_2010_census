library(shiny)
library(shinydashboard)

dashboardPage(
    dashboardHeader(title = "China census, 2010"),
    dashboardSidebar(
        sidebarMenu(
            # use only "list" instead of "glyphicon glyphicon-list"
            menuItem("Overview", tabName = "overview", icon = icon("list")),
            menuItem("Population", tabName = "population", icon = icon("bar-chart")),
            menuItem("Education", tabName = "education", icon = icon("bar-chart")),
            menuItem("House", tabName = "house", icon = icon("bar-chart"))
        )
    ),
    dashboardBody(
        # modify background color =============================================
        tags$head(tags$style(HTML('
                                  .skin-blue .wrapper {
                                  background-color: #ecf0f5;
                                  }
                                  .skin-blue .main-sidebar {
                                  background-color: rgb(50, 50, 50);
                                  }
                                  '))),
        tabItems(
            # overview ========================================================
            tabItem("overview",
                    column(
                        width = 6,
                        box(
                            width = 12, solidHeader = TRUE, status = "info",
                            title = "Introduction",
                            includeMarkdown("./included_Rmd/ui_overview/introduction.Rmd")
                        ),
                        box(
                            width = 12, solidHeader = TRUE, status = "info",
                            title = "What is Hukou",
                            includeMarkdown("./included_Rmd/ui_overview/hukou.Rmd")
                        ),
                        box(
                            width = 12, solidHeader = TRUE, status = "info",
                            title = "Population",
                            column(
                                width = 6,
                                includeMarkdown("./included_Rmd/ui_overview/population.Rmd")
                            ),
                            column(
                                width = 6,
                                tags$img(src = "population.png", width = "100%")
                            )
                        )
                    ),
                    column(
                        width = 6,
                        box(
                            width = 12, solidHeader = TRUE, status = "info",
                            title = "Education",
                            column(
                                width = 6,
                                includeMarkdown("./included_Rmd/ui_overview/education.Rmd")
                            ),
                            column(
                                width = 6,
                                tags$img(src = "education.png", width = "100%")
                            )
                        ),
                        box(
                            width = 12, solidHeader = TRUE, status = "info",
                            title = "House",
                            column(
                                width = 6,
                                tags$img(src = "house_education.png", width = "100%")
                            ),
                            column(
                                width = 6,
                                includeMarkdown("./included_Rmd/ui_overview/house.Rmd")
                            )
                        )
                    )
            ),
            
            # population ======================================================
            tabItem("population",
                    fluidRow(
                        box(
                            width = 4, height = 360,
                            status = "info", solidHeader = TRUE,
                            title = "Population @ year of birth",
                            plotOutput("totalPopul", width = "100%", height = 300,
                                       click = "clickPopulTotal")
                        ),
                        column(
                            width = 4,
                            strong("Click letters to view details:"),
                            textOutput("clickPopulTotalText")
                        ),
                        box(
                            width = 4, height = 360,
                            status = "info", solidHeader = TRUE,
                            title = "Male and female percentage",
                            plotOutput("totalPercent", width = "100%",height = 300,
                                       click = "clickPercentTotal")
                        )
                    ),
 
                    fluidRow(
                        box(
                            width = 12, height = 360,
                            status = "info", solidHeader = TRUE,
                            title = "Compare population @ age in city, town, and villiage",
                            plotOutput("comparePopul", width = "100%", height = 300,
                                       click = "clickComparePopul")
                        )
                    )
                    ),
            
            
            # education =======================================================
            tabItem("education",
                    box(
                        width = 4, height = 735,
                        status = "info", solidHeader = TRUE,
                        title = "Population at Each Education Level",
                        plotOutput("eduBar", click = "clickBar", 
                                   height = 250, width = "100%"),
                        plotOutput("ageDistr", height = 300, width = "100%")
                    ),
                        
                    column(
                        width = 8, 
                        box(
                            width = 12,
                            status = "info", solidHeader = TRUE,
                            title = "Education in City, Town, and Villiage",
                            plotOutput("eduCompare", height = 250, width = "100%")
                        ),

                        column(
                            width = 6,
                            h4("some text")
                        ),
                        
                        box(
                            width = 6,
                            status = "info", solidHeader = TRUE,
                            title = "House and Education"
                        )
                    )
            ),
            
            
            # house ===========================================================
            tabItem("house",
                    fluidRow(
                        box(
                            width = 4,
                            status = "info", solidHeader = TRUE,
                            title = "Number of rooms per household",
#                             h4("Most popular number of rooms"),
                            plotOutput(
                                "map", click = "clickMap",
                                height = "auto"
                            ),
                            hr(),
#                             h4("Distribution of number of rooms"),
                            plotOutput("distr", height = "auto")
                            ),
                        
                        column(width = 4, align = "center",
                               # when using img, the picture must be in the www directory
                               img(src = "city_villiage_house_long.jpg", width = "100%"),
                               br(), br(),
                               selectInput("chooseWhere", "Select all, city, town, or villiage:",
                                           choices = c("All", "City", "Town", "Villiage"))
                         ),
                        
                        box(width = 4, 
                            status = "info", solidHeader = TRUE,
                            title = "Average building area per person",
                            plotOutput("avgLivingArea", hover = "hoverAvgLivingArea", height = "auto"),
                            hr(),
#                             h4("Dependence on education level"),
                            plotOutput("areaPerPerson", height = "auto",
                                       hover = "clickAreaPerPerson")
                        )
                    )
            )
        )
    )
)