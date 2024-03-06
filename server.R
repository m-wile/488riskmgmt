library(shiny)
library(DT)
library(tidyverse)
library(RTL)
library(plotly)

server <- function(input, output, session) {
  
  bonds_df <- reactiveValues(data = data.frame(
    C = numeric(),
    YTM = numeric(),
    Par = numeric(),
    TTM = numeric(),
    "Bonds held" = numeric(),
    Price = numeric()
  ))
  
  #######################################################
  # Add Bond Button
  #######################################################
  output$addBondButton <- renderUI({
    actionButton("addBond", "Add Bond")
  })
  
  #######################################################
  # Show Modal when Add Bond Button is clicked
  #######################################################
  observeEvent(input$addBond, {
    showModal(modalDialog(
      textInput("coupon", "Annual Coupon Rate (%):", "4"),
      textInput("ytm", "Yield to Maturity (%):", "5"),
      textInput("par_value", "Par Value:", "100"),
      textInput("ttm", "Years to Maturity:", "10"),
      textInput("principal", "Principal ($):", "1000"),
      actionButton("saveBond", "Save Bond"),
      easyClose = TRUE)
    )
  })
  
  #######################################################
  # Save Bond when Save Bond Button is clicked
  #######################################################
  observeEvent(input$saveBond, {
    new_bond <- data.frame(
      C = as.numeric(input$coupon),
      YTM = as.numeric(input$ytm),
      Par = as.numeric(input$par_value),
      TTM = as.numeric(input$ttm),
      "Bonds held" = as.numeric(input$principal),
      Price = round(as.numeric(bond_cpp_call(ytm = as.numeric(input$ytm)/100, 
                                             C = as.numeric(input$coupon)/100, 
                                             T2M = as.numeric(input$ttm), 
                                             m = 2, 
                                             face = as.numeric(input$par_value)
                                             )), 1)
    )
    removeModal()
    bonds_df$data <- rbind(bonds_df$data, new_bond)
  })
  
  #######################################################
  # Output df with checkboxes
  #######################################################
  output$bond_table <- renderDT({
    datatable(bonds_df$data, 
              options = list(
                pageLength = 10,
                lengthMenu = c(5, 10, 15),
                dom = 'lfrtip',
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#428bca', 'color': '#fff'});",
                  "}"),
                preDrawCallback = JS(
                  "function() {",
                  "$(this.api().table().header()).css({'background-color': '', 'color': ''});",
                  "}"),
                drawCallback = JS(
                  "function() {",
                  "$(this.api().table().header()).css({'background-color': '#428bca', 'color': '#fff'});",
                  "}"),
                language = list(
                  search = '<span>Search:</span> _INPUT_',
                  searchPlaceholder = 'Search...'
                )
              ),
              selection = list(mode = 'multiple', target = 'row'),
              editable = TRUE
    )
  })
  
  #######################################################
  # Delete selected rows
  #######################################################
  
  observeEvent(input$deleteButton, {
    selected_rows <- input$bond_table_rows_selected
    bonds_df$data <- bonds_df$data[-selected_rows, , drop = FALSE]
  })
  
  #######################################################
  # Add Delete Button
  #######################################################
  output$deleteButton <- renderUI({
    actionButton("deleteButton", "Delete Selected Bonds")
  })
  
  #######################################################
  # Calculate Total Portfolio Value
  #######################################################
  output$portfolio_value <- renderText({
    total_value <- sum(bonds_df$data$Price * bonds_df$data$"Bonds held")
    paste("Total Portfolio Value: $", total_value)
  })
  
  #######################################################
  # Plot Portfolio
  #######################################################
  output$ytm_price_plot <- renderPlotly({
    # Create an empty plot
    p <- plot_ly(type = 'scatter', mode = 'lines+markers')
    
    # Iterate through each bond in the portfolio
    for (i in seq_len(nrow(bonds_df$data))) {
      bond_data <- bonds_df$data[i, ]
      
      # Generate YTM values from 0% to 20% with a step of 1%
      ytm_values <- seq(0, 0.20, 0.01)
      price_values <- c()
      
      # Calculate bond prices for each YTM
      for (j in 1:length(ytm_values)) {
        price_value <- bond_cpp_call(ytm = ytm_values[j], 
                                      C = bond_data$C/100, 
                                      T2M = bond_data$TTM, 
                                      m = 2, 
                                      face = bond_data$Par)
        price_values <- c(price_values, price_value)
      }
      
      # Add a trace for each bond to the plot
      p <- add_trace(p, x = ytm_values * 100, y = price_values, 
                     type = 'scatter', mode = 'lines+markers', 
                     name = paste('Bond ', i))
      # Add current ytm
      p <- add_trace(p, x = bond_data$YTM, y = bond_data$Price, 
                     type = 'scatter', mode = 'markers', 
                    
                     showlegend = FALSE,
                     marker = list(size = 8, color = 'black'))
      
    }
    
    # Customize plot layout
    p1 <- layout(p, title = "Bond Prices Across Yields",
                xaxis = list(title = "Yield to Maturity (%)"),
                yaxis = list(title = "Bond Price"),
                showlegend = TRUE)
    
    p1
  })
  
}
