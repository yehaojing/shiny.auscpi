library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(Rcpp)
library(sf)
library(ozmaps)
library(leaflet)

ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "Australian CPI Viewer"),
    dashboardSidebar(sidebarMenu(
        menuItem(
            "Overview",
            tabName = "overview",
            icon = icon("stats", lib = "glyphicon")
        ),
        menuItem(
            "About",
            tabName = "about",
            icon = icon("question-sign", lib = "glyphicon")
        )
    )),
    dashboardBody(tabItems(
        tabItem(
            tabName = "overview",
            fluidRow(column(
                3,
                selectizeInput("hierarchy_quarter", "Quarter", NULL)
            ),
            column(
                3,
                selectInput("hierarchy_region", "City", NULL, selected = "50")
            )),
            fluidRow(
                box(
                    title = "Hierarchy",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    shinyWidgets::radioGroupButtons(
                        "hierarchy_type",
                        "",
                        list("Treemap" = "treemap",
                             "Sunburst" = "sunburst")
                    ),
                    plotlyOutput("hierarchy_plot") %>% addSpinner(spin = "cube"),
                    width = 12
                )
            ),
            fluidRow(
                box(
                    pickerInput(
                        "bar_data",
                        "",
                        list(
                            "% Quarter" = "measurePercQuarter",
                            "% Annual" = "measurePercAnnual",
                            "Point Contribution" = "measureContribution",
                            "Change to Point Contribution" = "measureContributionChange",
                            "Index" = "measureIndex"
                        )
                    ),
                    plotlyOutput("bar_plot") %>% addSpinner(spin = "cube")
                ),
                box(
                    fluidRow(column(
                        3,
                        radioGroupButtons(
                            inputId = "line_data",
                            label = " ",
                            choices = list("Original" = "measureIndex", "Adjusted" = "measureIndexAdjusted")
                        )
                    ),
                    column(
                        6,
                        selectizeInput("adjusted_quarter", "Adjustment Quarter", NULL)
                    )),
                    plotlyOutput("line_plot") %>% addSpinner(spin = "cube")
                )
            ),
            fluidRow(
                box(leafletOutput("map_plot") %>% addSpinner(spin = "cube")),
                box(plotlyOutput("heatmap_plot") %>% addSpinner(spin = "cube")),
                
            ),
            
        ),
        tabItem(tabName = "about",
                includeMarkdown("README.md"))
    ))
)
