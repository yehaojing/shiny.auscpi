ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "Australian CPI Viewer"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("stats", lib="glyphicon")),
            menuItem("About", tabName = "about", icon = icon("question-sign", lib="glyphicon"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "overview",
                    fluidRow(
                        column(3, 
                               selectizeInput("hierarchy_quarter", "Quarter", NULL)
                        ),
                        column(3,
                               selectInput("hierarchy_region", "Region", NULL, selected = "50")
                        )
                    ),
                    fluidRow(
                        box(
                            shinyWidgets::radioGroupButtons("hierarchy_type", "", list("Treemap" = "treemap",
                                                                                       "Sunburst" = "sunburst")),
                            plotlyOutput("hierarchy_plot") %>% addSpinner(spin = "cube"),
                            width = 12
                        )
                    ),
                    fluidRow(
                        box(shinyWidgets::radioGroupButtons("bar_data", "", list("% Quarter" = "measurePercQuarter",
                                                                                 "% Annual" = "measurePercAnnual",
                                                                                 "Point Contribution" = "measureContribution")),
                            plotlyOutput("bar_plot") %>% addSpinner(spin = "cube")
                        ),
                        box(
                            plotlyOutput("line_plot") %>% addSpinner(spin = "cube")
                        )
                    )
            ),
            tabItem(tabName = "about",
                    "Authored by William Ye at github.com/yehaojing/shiny.auscpi")
        )
    )
)
