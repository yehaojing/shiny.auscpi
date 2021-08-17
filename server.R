library(shiny)
library(tidyverse)
library(shinyWidgets)
library(plotly)
shinyServer(function(input, output, session) {
    source("get_data.R")
    dsd <- load_dsd()
    
    updateSelectInput(session,
                      "hierarchy_quarter",
                      choices = rev(generate_quarter_list()))
    
    updateSelectInput(session, 
                      "hierarchy_region", 
                      choices = split(get_region(dsd)$regionID, get_region(dsd)$regionName),
                      selected = "50")
    
    data <- load_data() %>%
        pivot_wider(names_from = MEASURE, values_from = obsValue) %>%
        left_join(get_region(dsd), by = c("REGION" = "regionID")) %>%
        left_join(get_tsest(dsd), by = c("TSEST" = "tsestID")) %>%
        left_join(get_frequency(dsd), by = c("FREQUENCY" = "frequencyID")) %>%
        left_join(get_index(dsd), by = c("INDEX" = "indexID")) %>%
        mutate(quarterEnd = recode_quarter(obsTime)) %>%
        mutate(parentName = replace_na(parentName, ""))

    colnames(data) <- recode(colnames(data),
                             "1" = "measureIndex",
                             "2" = "measurePercQuarter",
                             "3" = "measurePercAnnual",
                             "4" = "measureContribution",
                             "5" = "measureContributionChange")
    
    data <- data %>% 
        filter(!(parentID %in% c('115901', '104099', '131198', '104100', '102674')),
                              TSEST == '10') %>% 
        fix_rounding_issue()
    
    data_date_lookup <- data %>% 
        select(obsTime, quarterEnd) %>% 
        distinct() %>%
        as.data.frame()
        
    data_hierarchy_order <- data %>%
        filter(REGION == "50") %>%
        mutate(DataID = row_number() - 1)
    
    #If treemap/sunburst is clicked, filter subplots
    #If hierarchy quarter or region or type is changed, reset subplots
    clickData <- reactiveValues(DataID = 0)
    observeEvent({
        input$hierarchy_region
        input$hierarchy_quarter
        input$hierarchy_type
        }, {
        clickData$DataID <- 0
    })
    observeEvent({
        event_data("plotly_click", source = "hierarchy")
        },{
        c <- event_data("plotly_click", source = "hierarchy")
        if(is.null(c)) clickData$DataID <- list(DataID = 0)
        
        clickData$DataID <- c$pointNumber
    })
    
    selected_hierarchy <- reactive({
        data_hierarchy_order %>%
            filter(DataID == as.character(clickData$DataID)) %>%
            select(INDEX, indexName)
    })
    
    d_subplots <- reactive({
        d <- data %>%
            filter(INDEX == selected_hierarchy()$INDEX)
    })

    output$hierarchy_plot <- renderPlotly({
        plot_ly(data, source = "hierarchy") %>%
            filter(obsTime == input$hierarchy_quarter,
                   REGION == input$hierarchy_region) %>%
            add_trace(type = input$hierarchy_type,
                      ids = ~INDEX,
                      parents = ~parentID,
                      labels = ~indexName,
                      values = ~obsValueFix,
                      branchvalues = "total")
    })

    output$bar_plot <- renderPlotly({
        d <- d_subplots() %>%
            filter(obsTime == input$hierarchy_quarter)

        p1 <- plot_ly(d,
                x = unlist(d[,"regionName"]),
                y = unlist(d[,input$bar_data]),
                text = unlist(d[,input$bar_data]),
                textposition = "auto",
                type = "bar") %>%
            layout(title = paste(selected_hierarchy()$indexName),
                   margin = list(b = 180),
                   xaxis = list(tickangle = 90))
    })
    
    output$line_plot <- renderPlotly({
        
        d <- as.data.frame(d_subplots())
        
        p2 <- plot_ly(d,
                      x = ~quarterEnd,
                      y = ~measureIndex,
                      color = ~regionName,
                      colors = "Set3",
                      type = "scatter",
                      mode = "lines"
        ) %>%
            layout(title = selected_hierarchy()$indexName,
                   xaxis = list(title = 'Quarter'),
                   yaxis = list(title = 'Index'),
                   legend = list(orientation = 'h',
                                 xanchor = "center",
                                 x = 0.5, y = -0.2),
                   shapes=list(type='line', 
                                      x0 = {data_date_lookup %>% filter(obsTime == input$hierarchy_quarter) %>% .$quarterEnd}, 
                                      x1 = {data_date_lookup %>% filter(obsTime == input$hierarchy_quarter) %>% .$quarterEnd}, 
                                      y0 = min(d$measureIndex), 
                                      y1 = max(d$measureIndex), 
                                      line = list(dash='dot', width=1)))

    })
    
    

})
