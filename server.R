library(shiny)
library(DT)
library(htmlwidgets)
library(shinyWidgets)
library(tidyverse)
library(RTL)
library(plotly)
library(stats)

server <- function(input, output, session) {
  
  bonds_df <- shiny::reactiveValues(
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
      Gamma = numeric()
    )
  )
  
  # Add Bond Button
  output$addBondButton <- shiny::renderUI({
    shiny::actionButton("addBond", "Add Bond")
  })
  
  
  total_value <- shiny::reactive({
    sum(bonds_df$data$Total)
  })
  output$total_value_output <- renderText({
    paste("Total Portfolio Value: $", format(total_value(), big.mark = ",", scientific = FALSE))
  })
  # Add Delete Button
  output$deleteButton <- shiny::renderUI({
    shiny::actionButton("deleteButton", "Delete Selected Bonds")
  })
  
  # Delete selected rows
  shiny::observeEvent(input$deleteButton, {
    # Only run if bonds are in portfolio
    if (length(input$bond_table_rows_selected) == 0) {
      return(NULL)
    } else {
      selected_rows <- input$bond_table_rows_selected
      bonds_df$data <- bonds_df$data[-selected_rows, , drop = FALSE]
    }
  })
  
  # Show Modal when Add Bond Button is clicked
  shiny::observeEvent(input$addBond, {
    shiny::showModal(shiny::modalDialog(
      shinyWidgets::numericInputIcon(inputId = "coupon", label = "Annual Coupon Rate (%):",
                                     value = 4, icon = shiny::icon("percent")),
      shinyWidgets::numericInputIcon(inputId = "ytm", label = "Yield to Maturity (%):",
                                     value = 5, icon = shiny::icon("percent")),
      # textInput("coupon", "Annual Coupon Rate (%):", "4"),
      # textInput("ytm", "Yield to Maturity (%):", "5"),
      shiny::numericInput(inputId = "par_value", label = "Par Value:", value = 100),
      shiny::numericInput(inputId = "ttm", label = "Years to Maturity:", value = 10),
      shiny::numericInput(inputId = "num_bonds", label = "Number of Bonds:", value = 1000),
      shiny::actionButton("saveBond", "Save Bond"),
      # textInput("coupon", "Annual Coupon Rate (%):", "4"),
      # textInput("ytm", "Yield to Maturity (%):", "5"),
      #textInput("par_value", "Par Value:", "100"),
      #textInput("ttm", "Years to Maturity:", "10"),
      #textInput("num_bonds", "Number of Bonds:", "1000"),
      #actionButton("saveBond", "Save Bond"),
      easyClose = TRUE)
    )
  })
  
  shiny::observeEvent(input$help, {
    shiny::showModal(
      shiny::modalDialog(
        shiny::h5("Instructions:"),
        shiny::HTML("<h6> Click <em> Load Random Portfolio </em> to generate a random
                    portfolio."),
        shiny::br(), 
        shiny::h6("Otherwise, you should insert the maturity, coupon, yield and redeption price"),
        shiny::h6("a) Insert coupon as a percentage"), 
        shiny::h6("b) Input the Yield to Maturity in percentage"), 
        shiny::h6("c) Enter Par Value in Dollars"),
        shiny::h6("d) Select the maturity in years"),
        shiny::h6("e) Insert number of bonds held"),
        shiny::h6("You will get the quotation of the bond as percentage and a graphic
                  of the sensibility of the price by yield."), 
        easyClose = TRUE
      )
    )
  })
  
  # Save Bond when Save Bond Button is clicked
  shiny::observeEvent(input$saveBond, {
    StepSize <- 0.0001
    browser()
    bond_price <- as.numeric(bond_cpp_call(ytm = as.numeric(input$ytm)/100, 
                                           C = as.numeric(input$coupon)/100, 
                                           T2M = as.numeric(input$ttm), 
                                           m = 2, 
                                           face = as.numeric(input$par_value)
    ))
    
    bond_price_minus <- as.numeric(bond_cpp_call(ytm = (as.numeric(input$ytm)/100 - StepSize), 
                                                 C = as.numeric(input$coupon)/100, 
                                                 T2M = as.numeric(input$ttm), 
                                                 m = 2, 
                                                 face = as.numeric(input$par_value)
    ))
    
    bond_price_plus <- as.numeric(bond_cpp_call(ytm = (as.numeric(input$ytm)/100 + StepSize), 
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
      Gamma = as.numeric(0.5 * ((bond_price_plus - 2 * bond_price + bond_price_minus) / StepSize^2) / 10000^2))
    
    shiny::removeModal()
    bonds_df$data <- rbind(bonds_df$data, new_bond)
  })
  
  # Load Random Portfolio
  shiny::observeEvent(input$loadRandomPortfolio, {
    # Generate a random portfolio with 10 bonds
    random_portfolio <- data.frame(
      C = round(stats::runif(10, min = 0, max = 12), 1),
      YTM = round(stats::runif(10, min = 1, max = 18), 1),
      Par = round(stats::runif(10, min = 100, max = 100), 1),
      TTM = round(stats::runif(10, min = 1, max = 30), 1),
      bonds_held = round(stats::runif(10, min = 10, max = 300), 1),
      Price = round(numeric(10), 1),
      Duration = round(numeric(10), 1),
      Total = round(numeric(10), 1),
      Delta = round(numeric(10), 1),
      Gamma = round(numeric(10), 1)
    )
    
    for (i in 1:10) {
      StepSize <- 0.0001
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
      random_portfolio$Delta[i] <- as.numeric((bond_price_plus  - bond_price_minus) / 
                                                (2 * StepSize) / 10000)
      random_portfolio$Gamma[i] <- as.numeric(0.5 * ((bond_price_plus - 2 * bond_price + bond_price_minus) / 
                                                       StepSize^2) / 10000^2)
      
    }
    bonds_df$data <- rbind(bonds_df$data, random_portfolio)
  })
  
  # Output and plot FRED data
  output$fred_plot <- renderPlotly({
    FRED %>% 
      # plot by symbol
      plot_ly(x = ~date, y = ~price, type = 'scatter', mode = 'lines', color = ~symbol) %>%
      plotly::layout(title = 'Historical US Treasury Bond Yields',
                     xaxis = list(title = ''),
                     yaxis = list(title = 'Rate (%)'),
                     showlegend = TRUE)
  })
  
  # Output and plot FRED data
  output$fred_plot_delta <- renderPlotly({
    FRED_data %>% 
      group_by(symbol) %>%
      # plot by symbol
      plot_ly(x = ~date, y = ~Delta, type = 'scatter', mode = 'lines', color = ~as.character(symbol), name = ~paste0(as.character(round(symbol, 1)), " Years")) %>%
      plotly::layout(title = 'Historical US Treasury Bond Deltas',
                     xaxis = list(title = ''),
                     yaxis = list(title = 'Delta'), 
                     legend = list(title = "Years to Maturity"),
                     showlegend = T) %>% 
      # adjust date limit
      layout(xaxis = list(range = range(FRED_data$date)))
      
  })
  
  # Output and plot FRED data
  output$fred_plot_gamma <- renderPlotly({
    FRED_data %>% 
      group_by(symbol) %>%
      # plot by symbol
      plot_ly(x = ~date, y = ~Gamma, type = 'scatter', mode = 'lines', color = ~as.character(symbol), name = ~paste0(as.character(round(symbol, 1)), " Years")) %>%
      plotly::layout(title = 'Historical US Treasury Bond Gammas',
                     xaxis = list(title = ''),
                     yaxis = list(title = 'Gamma'),
                     legend = list(title = "Years to Maturity"),
                     showlegend = TRUE) %>% 
      # adjust date limit
      layout(xaxis = list(range = range(FRED_data$date)))
  })
  
  
  # Output df with selection
  output$bond_table <- DT::renderDT({
    DT::datatable(bonds_df$data, 
                  options = list(
                    pageLength = 10,
                    lengthMenu = c(5, 10, 15),
                    dom = 'lfrtip',
                    initComplete = htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#428bca', 'color': '#fff'});",
                      "}"),
                    preDrawCallback = htmlwidgets::JS(
                      "function() {",
                      "$(this.api().table().header()).css({'background-color': '', 'color': ''});",
                      "}"),
                    drawCallback = htmlwidgets::JS(
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
  
  # Plot Yield Curve
  output$yield_curve_plot <- plotly::renderPlotly({
    # Create an empty plot
    p_yield_curve <- plotly::plot_ly(type = 'scatter', mode = 'lines+markers')
    
    # Iterate through each bond in the portfolio
    for (i in seq_len(nrow(bonds_df$data))) {
      bond_data <- bonds_df$data[i, ]
      
      # Add a trace for each bond to the plot
      p_yield_curve <- plotly::add_trace(p_yield_curve, x = bond_data$TTM, y = bond_data$YTM,
                                         type = 'scatter', mode = 'lines+markers',
                                         name = paste('Bond ', i),
                                         marker = list(size = 8, color = 'black'),
                                         line = list(color = 'black'))
    }
    # add connected line
    if (nrow(bonds_df$data) > 0){
      p_yield_curve <- plotly::add_lines(p_yield_curve, x = bonds_df$data$TTM, y = bonds_df$data$YTM,
                                         line = list(color = 'black', width = 1))}
    # Customize plot layout
    p_curve <- plotly::layout(p_yield_curve, title = "Portfolio Yield Curve",
                              xaxis = list(title = "Time to Maturity (Years)", 
                                           range = c(0, max(20, bonds_df$data$TTM + 2))),
                              yaxis = list(title = "Yield to Maturity", 
                                           ticksuffix = "%",
                                           range = c(0, max(10, bonds_df$data$YTM + 2))),
                              showlegend = F)
    
    p_curve
  })
  
# Plot Portfolio
  output$ytm_price_plot <- plotly::renderPlotly({
    # Create an empty plot
    p <- plotly::plot_ly(type = 'scatter', mode = 'lines+markers')
    p_port <- plotly::plot_ly(type = 'scatter', mode = 'lines+markers')
    
    # Iterate through each bond in the portfolio
    for (i in seq_len(nrow(bonds_df$data))) {
      bond_data <- bonds_df$data[i, ]
      
      # Generate YTM values from 0% to 20% with a step of 1%
      ytm_values <- seq(0, 0.20, 0.01)
      
      price_values <- c()
      price_values2 <- c()
      
      # Calculate bond prices for each YTM
      for (j in 1:length(ytm_values)) {
        price_value <- bond_cpp_call(ytm = ytm_values[j], 
                                     C = bond_data$C/100, 
                                     T2M = bond_data$TTM, 
                                     m = 2, 
                                     face = bond_data$Par)
        price_values <- c(price_values, bond_data$bonds_held * price_value)
        price_values2 <- c(price_values2, price_value)
        
      }
      
      # Add a trace for each bond to the plot
      p <- plotly::add_trace(p, x = ytm_values * 100, y = price_values2, 
                             type = 'scatter', mode = 'lines+markers',
                             name = paste('Bond ', i))
      
      # Add current ytm
      p <- plotly::add_trace(p, x = bond_data$YTM, y = bond_data$Price, 
                             type = 'scatter', mode = 'markers',
                             showlegend = FALSE,
                             marker = list(size = 8, color = 'black'))
    }
    
    # Plot combined holdings
    if (nrow(bonds_df$data) > 0) {
      ytm_values <- seq(0, 0.20, 0.01)
      price_values <- matrix(0, nrow = length(ytm_values), ncol = nrow(bonds_df$data))
      
      # Calculate total portfolio price for each YTM
      for (i in 1:nrow(bonds_df$data)) {
        bond_data <- bonds_df$data[i, ]
        
        for (j in 1:length(ytm_values)) {
          price_value <- bond_cpp_call(ytm = ytm_values[j], 
                                       C = bond_data$C/100, 
                                       T2M = bond_data$TTM, 
                                       m = 2, 
                                       face = bond_data$Par)
          price_values[j, i] <- bond_data$bonds_held * price_value
        }
      }
      
      # Sum the total portfolio price for each YTM
      total_portfolio_price <- rowSums(price_values)
      
      # Add a trace for the total portfolio to the plot
      p_port <- plotly::add_trace(p_port, x = ytm_values * 100, y = total_portfolio_price, 
                             type = 'scatter', mode = 'lines+markers',
                             name = 'Total Portfolio',
                             line = list(color = 'red', width = 2))
    }
    p_port <- plotly::layout(p_port, title = "Bond Prices Across Yields",
                             xaxis = list(title = "Yield to Maturity", 
                                          ticksuffix = "%"),
                             yaxis = list(title = "Bond Price"),
                             showlegend = TRUE)
    # Customize plot layout
    p1 <- plotly::layout(p, title = "Bond Prices Across Yields",
                         xaxis = list(title = "Yield to Maturity", 
                                      ticksuffix = "%"),
                         yaxis = list(title = "Bond Price"),
                         showlegend = TRUE)
    
    plotly::subplot(p1, p_port, nrows = 2)
    
  })
  
  # Plot Duration and Convexity
  output$duration_convexity_plot <- plotly::renderPlotly({
    # Create an empty plot
    p2 <- plotly::plot_ly(type = 'scatter', mode = 'lines+markers')
    
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
      p2 <- plotly::add_trace(p2, x = ytm_values * 100, y = dur_values, 
                              type = 'scatter', mode = 'lines+markers',
                              name = paste('Bond ', i))
      # Add current ytm
      p2 <- plotly::add_trace(p2, x = bond_data$YTM, y = bond_data$Duration, 
                              type = 'scatter', mode = 'markers',
                              showlegend = FALSE,
                              marker = list(size = 8, color = 'black'))
      
    }
    
    # Plot combined holdings for total portfolio duration
    if (nrow(bonds_df$data) > 0) {
      ytm_values <- seq(0, 0.20, 0.01)
      total_portfolio_dur_values <- rep(0, length(ytm_values))
      
      # Calculate total portfolio duration for each YTM
      for (i in 1:nrow(bonds_df$data)) {
        bond_data <- bonds_df$data[i, ]
        
        for (j in 1:length(ytm_values)) {
          dur_value <- PVBdur(ytm = ytm_values[j], 
                              C = bond_data$C/100, 
                              T2M = bond_data$TTM, 
                              m = 2, 
                              face = bond_data$Par)
          total_portfolio_dur_values[j] <- total_portfolio_dur_values[j] + (bond_data$Total/total_value()) * dur_value
        }
      }
      
      # Add a trace for the total portfolio to the plot
      p2 <- plotly::add_trace(p2, x = ytm_values * 100, y = total_portfolio_dur_values, 
                              type = 'scatter', mode = 'lines+markers',
                              name = 'Total Portfolio',
                              line = list(color = 'red', width = 2))
    }
    
    # Customize plot layout
    p1_2 <- plotly::layout(p2, title = "Bond Durations Across Yields",
                           xaxis = list(title = "Yield to Maturity", 
                                        ticksuffix = "%"),
                           yaxis = list(title = "Bond Durations"),
                           showlegend = TRUE)
    
    p1_2
  })
}