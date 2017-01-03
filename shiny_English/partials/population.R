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
)
