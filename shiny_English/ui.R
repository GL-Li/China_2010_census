library(shiny)
library(shinydashboard)

dashboardPage(
    dashboardHeader(title = "China census, 2010"),
    dashboardSidebar(
        sidebarMenu(
            # use only "list" instead of "glyphicon glyphicon-list"
            menuItem("Overview", tabName = "overview", icon = icon("list")),
            menuItem("Population", tabName = "population", icon = icon("line-chart")),
            menuItem("Education", tabName = "education", icon = icon("bar-chart")),
            menuItem("House", tabName = "house", icon = icon("map-o"))
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
                            title = "What Is Hukou",
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
                            width = 4, height = 370,
                            status = "info", solidHeader = TRUE,
                            title = "Population (Nov. 01, 2010) and Year of Birth",
                            plotOutput("totalPopul", width = "100%", height = 300,
                                       click = "clickPopulTotal")
                        ),
                        column(
                            width = 4,
                            includeMarkdown("./included_Rmd/population/population_timeline.Rmd"),
                            textOutput("clickPopulTotalText")
                        ),
                        box(
                            width = 4, height = 370,
                            status = "info", solidHeader = TRUE,
                            title = "Male and Female Percentage of Population",
                            plotOutput("totalPercent", width = "100%",height = 300,
                                       click = "clickPercentTotal")
                        )
                    ),
 
                    fluidRow(
                        box(
                            width = 12, height = 370,
                            status = "info", solidHeader = TRUE,
                            title = "Age Distribution of Population in cities, towns, and villages",
                            plotOutput("comparePopul", width = "100%", height = 300,
                                       click = "clickComparePopul")
                        )
                    )
                    ),
            
            
            # education =======================================================
            tabItem("education",
                    box(
                        width = 4,
                        status = "info", solidHeader = TRUE,
                        title = "Population at Each Education Level",
                        plotOutput("eduBar", click = "clickBar", 
                                   height = 250, width = "100%"),
                        hr(),
                        
                        plotOutput("eduWhere", height = 250),
                        hr(),
                        
                        plotOutput("eduGender", height = 250)
                    ),
                        
                    column(
                        width = 8, 
                        box(
                            width = 12,
                            status = "info", solidHeader = TRUE,
                            title = "Age Distribution of Population at Each Education Level in Cities, Towns, and Villages",
                            plotOutput("eduCompare", height = 250, width = "100%")
                        ),

                        column(
                            width = 12,
                            includeMarkdown("./included_Rmd/education/education_introduction.Rmd")
                        )
                    )
            ),
            
            
            # house ===========================================================
            tabItem("house",
                    fluidRow(
                        box(
                            width = 4,
                            status = "info", solidHeader = TRUE,
                            title = "Number of Rooms Per Household",
                            plotOutput(
                                "map", click = "clickMap",
                                height = "auto"
                            ),
                            p("The most popular number of rooms per household in each province. Click on a province to view the distribution below."),
                            hr(),

                            plotOutput("distr", height = "auto"),
                            p("Distribution of the number of rooms per household. The curve of the province clicked in the map is shown in red.")

                            ),
                        
                        column(width = 4, align = "center",
                               # when using img, the picture must be in the www directory
                               img(src = "city_villiage_house_long.jpg", width = "100%"),
                               br(), br(),
                               selectInput("chooseWhere", "Select all, city, town, or village:",
                                           choices = c("All", "City", "Town", "Village")),
                               p('"All" combines all the data from city, town, and village.'),
                               hr(style = "border-color: white"),
                               column(
                                   style = "padding-left: 0px; padding-right: 0px",
                                   width = 12, align = "left",
                                   includeMarkdown("./included_Rmd/house/house_introduction.Rmd")
                               )
                         ),
                        
                        box(width = 4, 
                            status = "info", solidHeader = TRUE,
                            title = "Average Building Area Per Person",
                            plotOutput("avgLivingArea", hover = "hoverAvgLivingArea", height = "auto"),
                            p("Average building area per person in each province. Move mouse over to read the numbers."),
                            hr(),
                            
                            plotOutput("areaPerPerson", height = "auto",
                                       hover = "hoverAreaPerPerson"),
                            p("Relationship between building area per person and education of the head of household. Move mouse over to view the numbers of household (million).")
                        )
                    )
            )
        )
    )
)