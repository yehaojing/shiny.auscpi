#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(plotly)
shinyUI(fluidPage(

    titlePanel("Australian CPI Viewer"),
    fluidRow(
        #verbatimTextOutput("clickData"),
        column(3, 
        selectizeInput("hierarchy_quarter", "Quarter", NULL)
        ),
        column(3,
               selectInput("hierarchy_region", "Region", NULL, 
                              selected = "50")
               ),
        column(3, 
               shinyWidgets::radioGroupButtons("hierarchy_type", "", list("Treemap" = "treemap",
                                                     "Sunburst" = "sunburst"))
        )
    ),
    plotlyOutput("hierarchy_plot"),
    shinyWidgets::radioGroupButtons("bar_data", "", list("% Quarter" = "measurePercQuarter",
                                           "% Annual" = "measurePercAnnual",
                                           "Point Contribution" = "measureContribution")),
    fluidRow(
        column(6,
               plotlyOutput("bar_plot")),
        column(6,
               plotlyOutput("line_plot")
               )
    )

))
