library(shiny)
library(DT)
library(tidyverse)
library(RTL)
library(plotly)

server <- function(input, output, session) {
  
  bonds_df <- reactiveValues(
    data = data.frame(
    C = numeric(),
    YTM = numeric(),
    Par = numeric(),
    TTM = numeric(),
    bonds_held = numeric(),
    Price = numeric(0),
    Total = numeric(),
    Duration = numeric(),
    Delta = numeric(),
    Gamma = numeric()))

  
  
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
    # Only run if bonds are in portfolio
    if (length(input$bond_table_rows_selected) == 0) {
      return(NULL)
    } else {
      selected_rows <- input$bond_table_rows_selected
      bonds_df$data <- bonds_df$data[-selected_rows, , drop = FALSE]
    }
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
      textInput("num_bonds", "Number of Bonds:", "1000"),
      actionButton("saveBond", "Save Bond"),
      easyClose = TRUE)
    )
  })
  
  #######################################################
  # Save Bond when Save Bond Button is clicked
  #######################################################
  
  observeEvent(input$saveBond, {
    
    StepSize <- as.numeric(0.0001)
    bond_price <- as.numeric(bond_cpp_call(ytm = as.numeric(input$ytm)/100, 
                                                 C = as.numeric(input$coupon)/100, 
                                                 T2M = as.numeric(input$ttm), 
                                                 m = 2, 
                                                 face = as.numeric(input$par_value)
                                                 ))
    bond_price_minus <- as.numeric(bond_cpp_call(ytm = as.numeric(input$ytm)/100- StepSize, 
                                                 C = as.numeric(input$coupon)/100, 
                                                 T2M = as.numeric(input$ttm), 
                                                 m = 2, 
                                                 face = as.numeric(input$par_value)
                                                 ))
    bond_price_plus <- as.numeric(bond_cpp_call(ytm = as.numeric(input$ytm)/100+ StepSize, 
                                                 C = as.numeric(input$coupon)/100, 
                                                 T2M = as.numeric(input$ttm), 
                                                 m = 2, 
                                                 face = as.numeric(input$par_value)
                                                 ))
    
    new_bond <- data.frame(
      C = as.numeric(input$coupon),
      
      YTM = as.numeric(input$ytm),
      
      Par = as.numeric(input$par_value),
      
      TTM = as.numeric(input$ttm),
      
      bonds_held = as.numeric(input$num_bonds),
      
      Price = round(bond_price, 1),
      
      Total = round(as.numeric(input$num_bonds) * bond_price, 1),
      
      Duration = round(as.numeric(PVBdur(ytm = as.numeric(input$ytm)/100, 
                                         C = as.numeric(input$coupon)/100, 
                                         T2M = as.numeric(input$ttm), 
                                         m = 2, 
                                         face = as.numeric(input$par_value)
                                         )), 1),
      Delta = as.numeric((bond_price_plus - bond_price_minus) / (2 * StepSize) / 10000), 
      
      Gamma = as.numeric(0.5 * ((bond_price_plus - 2 * bond_price + bond_price_minus) / StepSize^2) / 10000^2)
                            )
    
    
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
      bonds_held = round(runif(10, min = 10, max = 300), 1),     
      Price = round(numeric(10), 1),
      Duration = round(numeric(10), 1),
      Total = round(numeric(10), 1),
      Delta = round(numeric(10), 1),
      Gamma = round(numeric(10), 1)
      
      )
    for (i in 1:10) {
      StepSize <- as.numeric(0.0001)
      bond_price <- as.numeric(bond_cpp_call(ytm = random_portfolio$YTM[i]/100, 
                                             C = random_portfolio$C[i]/100, 
                                             T2M = random_portfolio$TTM[i], 
                                             m = 2, 
                                             face = random_portfolio$Par[i]
      ))
      bond_price_minus <- as.numeric(bond_cpp_call(ytm = random_portfolio$YTM[i]/100- StepSize, 
                                                   C = random_portfolio$C[i]/100, 
                                                   T2M = random_portfolio$TTM[i], 
                                                   m = 2, 
                                                   face = random_portfolio$Par[i]
      ))
      bond_price_plus <- as.numeric(bond_cpp_call(ytm = random_portfolio$YTM[i]/100+ StepSize, 
                                                  C = random_portfolio$C[i]/100, 
                                                  T2M = random_portfolio$TTM[i], 
                                                  m = 2, 
                                                  face = random_portfolio$Par[i]
      ))
      random_portfolio$Price[i] <- round(bond_price, 1)
      random_portfolio$Duration[i] <- round(as.numeric(PVBdur(ytm = random_portfolio$YTM[i]/100, 
                                                              C = random_portfolio$C[i]/100, 
                                                              T2M = random_portfolio$TTM[i], 
                                                              m = 2, 
                                                              face = random_portfolio$Par[i]
      )), 1)
      random_portfolio$Total[i] <- round(as.numeric(random_portfolio$bonds_held[i]) * bond_price, 1)
      random_portfolio$Delta[i] <- as.numeric((bond_price_plus - bond_price - bond_price_minus) / (2 * StepSize) / 10000)
      random_portfolio$Gamma[i] <- as.numeric(0.5 * ((bond_price_plus - 2 * bond_price + bond_price_minus) / StepSize^2) / 10000^2)
  
    }

    
    # Bind to existing data
    bonds_df$data <- rbind(bonds_df$data, random_portfolio)
  })
  
#######################################################
  # Output Fred data
#######################################################
  # symbol date       price   rate    cf basischange Bond_price Frac              num  test Adj_price Yield    sd
  # <dbl> <date>     <dbl>  <dbl> <dbl>       <dbl>      <dbl> <chr>           <dbl> <dbl>     <dbl> <dbl> <dbl>
  #   1 0.0833 2001-08-01  3.65 0.0365   100       -2.00       96.4 833333333333333     0     0      96.4  3.65 NA   
  # 2 0.0833 2001-08-02  3.65 0.0365   100        0          96.4 833333333333333     0     0      96.4  3.65 NA   
  # 3 0.0833 2001-08-03  3.63 0.0363   100       -2.00       96.4 833333333333333     0     0      96.4  3.63 NA   
  # 4 0.0833 2001-08-06  3.62 0.0362   100       -1.00       96.4 833333333333333     0     0      96.4  3.62 NA   
  # 5 0.0833 2001-08-07  3.63 0.0363   100        1.00       96.4 833333333333333     0     0      96.4  3.63  1.17
  # 6 0.0833 2001-08-08  3.61 0.0361   100       -2.00       96.4 833333333333333     0     0      96.4  3.61  1.17
  # 7 0.0833 2001-08-09  3.61 0.0361   100        0          96.4 833333333333333     0     0      96.4  3.61  1.17
  # 8 0.0833 2001-08-10  3.58 0.0358   100       -3.00       96.4 833333333333333     0     0      96.4  3.58  1.41
  # 9 0.0833 2001-08-13  3.57 0.0357   100       -1.00       96.4 833333333333333     0     0      96.4  3.57  1.41
  # 10 0.0833 2001-08-14  3.54 0.0354   100       -3.00       96.5 833333333333333     0     0      96.5  3.54  1.17
  output$fred_plot <- renderPlotly({
    FRED %>% 
      # plot by symbol
      plot_ly(x = ~date, y = ~price, type = 'scatter', mode = 'lines', color = ~symbol) %>%
      layout(title = 'Fred Data',
             xaxis = list(title = 'Date'),
             yaxis = list(title = 'Rate'),
             showlegend = TRUE)
    
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
      # Create an empty plot
      p2 <- plot_ly(type = 'scatter', mode = 'lines+markers')
      
      # Iterate through each bond in the portfolio
      for (i in seq_len(nrow(bonds_df$data))) {
        bond_data <- bonds_df$data[i, ]
        
        # Generate YTM values from 0% to 20% with a step of 1%
        ytm_values <- seq(0, 0.20, 0.01)
        dur_values <- c()
        
        # Calculate bond durs for each YTM
        for (j in 1:length(ytm_values)) {
          dur_value <- PVBdur(ytm = ytm_values[j], 
                                C = bond_data$C/100, 
                                T2M = bond_data$TTM, 
                                m = 2, 
                                face = bond_data$Par)
          dur_values <- c(dur_values, dur_value)
        }
        
        # Add a trace for each bond to the plot
        p2 <- add_trace(p2, x = ytm_values * 100, y = dur_values, 
                       type = 'scatter', mode = 'lines+markers', 
                       name = paste('Bond ', i))
        # Add current ytm
        p2 <- add_trace(p2, x = bond_data$YTM, y = bond_data$Duration, 
                       type = 'scatter', mode = 'markers', 
                       showlegend = FALSE,
                       marker = list(size = 8, color = 'black'))
        
      }
      # plot combined holdings
      if (nrow(bonds_df$data) > 0){
        ytm_values <- seq(0, 0.20, 0.01)
        dur_values <- c()
      }
      # Customize plot layout
      
      p1_2 <- layout(p2, title = "Bond Durations Across Yields",
                   xaxis = list(title = "Yield to Maturity (%)"),
                   yaxis = list(title = "Bond Durations"),
                   showlegend = TRUE)
      
      p1_2
    })
  
}


