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
  
  bonds_stats <- reactiveValues(data = data.frame(Duration = numeric(), Convexity = numeric()))

  
  
  #######################################################
  #######################################################
  # BUTTONS
  #######################################################
  #######################################################
  
  #######################################################
  # Add Bond Button
  #######################################################
  output$addBondButton <- renderUI({
    actionButton("addBond", "Add Bond")
  })
  
  #######################################################
  # Add Delete Button
  #######################################################
  output$deleteButton <- renderUI({
    actionButton("deleteButton", "Delete Selected Bonds")
  })
  
  #######################################################
  # Delete selected rows
  #######################################################
  
  observeEvent(input$deleteButton, {
    selected_rows <- input$bond_table_rows_selected
    bonds_df$data <- bonds_df$data[-selected_rows, , drop = FALSE]
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
                                             )), 1))
    removeModal()
    bonds_df$data <- rbind(bonds_df$data, new_bond)
  })
  
  
  
#######################################################
  # Load Random Portfolio
#######################################################
  
  observeEvent(input$loadRandomPortfolio, {
    # Generate a random portfolio with 10 bonds
    random_portfolio <- data.frame(
      C = round(runif(10, min = 0, max = 12),1),         
      YTM = round(runif(10, min = 1, max = 18),1),  
      Par = round(runif(10, min = 100, max = 100), 1),     
      TTM = round(runif(10, min = 1, max = 30), 1),
      "Bonds held" = sample(100:1000, 10),      
      Price = round(numeric(10), 1)                   
    )
    
    # Calculate bond prices for the random portfolio
    for (i in 1:10) {
      random_portfolio$Price[i] <- round(
        as.numeric(bond_cpp_call(ytm = random_portfolio$YTM[i]/100,
                                 C = random_portfolio$C[i]/100,
                                 T2M = random_portfolio$TTM[i],
                                 m = 2,
                                 face = random_portfolio$Par[i])), 1)
    }
    
    # Update the reactiveValues with the new random portfolio
    bonds_df$data <- random_portfolio
  })
  
  
#######################################################
  # Duration and Convexity
#######################################################

  # Update duration and convexity when bond data changes
  observeEvent(bonds_df$data, {
    durations <- vector("numeric", length = nrow(bonds_df$data))
    convexities <- vector("numeric", length = nrow(bonds_df$data))
    
    for (i in seq_len(nrow(bonds_df$data))) {
      bond_data <- bonds_df$data[i, ]
      
      durations[i] <- duration_cpp_call(ytm = bond_data$YTM/100,
                                        C = bond_data$C/100,
                                        T2M = bond_data$TTM,
                                        m = 2,
                                        face = bond_data$Par)
      convexities[i] <- convexity_cpp_call(ytm = bond_data$YTM/100,
                                          C = bond_data$C/100,
                                          T2M = bond_data$TTM,
                                          m = 2,
                                          face = bond_data$Par)
    }
    
    # Update reactiveValues with new duration and convexity
    bonds_stats$data <- data.frame(Duration = durations, Convexity = convexities)
  })
  
  
  #######################################################
  # Update combined portfolio value when bond is added
  # sum or find avg of all cols in a df
  # THIS DOESNT WORK RN
  #######################################################
  observeEvent(bonds_df$data, {
    total_value <- sum(bonds_df$data$Price * bonds_df$data$"Bonds held")
    output$portfolio_value <- renderText({
      paste("Total Portfolio Value: $", total_value)
    })
  })

  #######################################################
  # Output df with selection
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
  # Plot Yield Curve
  #######################################################
  output$yield_curve_plot <- renderPlotly({
    # Create an empty plot
    p_yield_curve <- plot_ly(type = 'scatter', mode = 'lines+markers')
    
    # Iterate through each bond in the portfolio
    for (i in seq_len(nrow(bonds_df$data))) {
      bond_data <- bonds_df$data[i, ]
      
      # Add a trace for each bond to the plot
      p_yield_curve <- add_trace(p_yield_curve, x = bond_data$TTM, y = bond_data$YTM,
                                 type = 'scatter', mode = 'lines+markers',
                                 name = paste('Bond ', i),
                                 marker = list(size = 8, color = 'black'),
                                 line = list(color = 'black'))
    }
    # add conected line
    if (nrow(bonds_df$data) > 0){
    p_yield_curve <- add_lines(p_yield_curve, x = bonds_df$data$TTM, y = bonds_df$data$YTM,
                               line = list(color = 'black', width = 1))}
    # Customize plot layout
    p_curve <- layout(p_yield_curve, title = "Portfolio Yield Curve",
                      xaxis = list(title = "Time to Maturity (Years)", range = c(0, max(20, bonds_df$data$TTM + 2))),
                      yaxis = list(title = "Yield to Maturity (%)", range = c(0, max(10, bonds_df$data$YTM + 2))),
                      showlegend = F)
    
    p_curve
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
    # plot combined holdings
    if (nrow(bonds_df$data) > 0){
      ytm_values <- seq(0, 0.20, 0.01)
      price_values <- c()
    }
    # Customize plot layout
    
    p1 <- layout(p, title = "Bond Prices Across Yields",
                xaxis = list(title = "Yield to Maturity (%)"),
                yaxis = list(title = "Bond Price"),
                showlegend = TRUE)
    
    p1
  })
  
  
  #######################################################
  # Plot Duration and Convexity
  #######################################################
  output$duration_convexity_plot <- renderPlotly({
    plot_ly() %>%
      add_trace(x = bonds_df$data$TTM, y = bonds_stats$data$Duration, name = "Duration", type = 'scatter', mode = 'lines+markers') %>%
      add_trace(x = bonds_df$data$TTM, y = bonds_stats$data$Convexity, name = "Convexity", type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Duration and Convexity Across Bonds",
             xaxis = list(title = "Time to Maturity (Years)"),
             yaxis = list(title = "Value"),
             showlegend = TRUE)
  })
  
}


