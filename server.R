library(shiny)
library(plotly)
library(tidyverse)

server <- function(input, output, session) {
  
  # ReactiveValues to store bond details
  bonds_list <- reactiveValues(bonds = list())
  
  #######################################################
  # PRICE BOND FUNCTION
  #######################################################
  pv_bond <- function(coupon, yield, redt, t) {
    price <- 0
    for (year in 1:(t - 1)) {
      price <- price + (coupon) / ((1 + yield / 100)^year)
    }
    price <- price + (redt + coupon) / ((1 + yield / 100)^t)
    return(price)
  }
  
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
    showModal(
      modalDialog(
        textInput("coupon", "Coupon Rate:", ""),
        textInput("ytm", "Yield to Maturity:", ""),
        textInput("par_value", "Par Value:", ""),
        textInput("ttm", "Time to Maturity:", ""),
        actionButton("saveBond", "Save Bond"),
        easyClose = TRUE
      )
    )
  })
  
  #######################################################
  # Save Bond when Save Bond Button is clicked
  #######################################################
  observeEvent(input$saveBond, {
    new_bond <- list(
      coupon = as.numeric(input$coupon),
      ytm = as.numeric(input$ytm),
      par_value = as.numeric(input$par_value),
      ttm = as.numeric(input$ttm)
    )
    
    bonds_list$bonds <- c(bonds_list$bonds, list(new_bond))
    
    # Close the modal dialog
    removeModal()
  })
  
  #######################################################
  # Output plotly
  #######################################################
  output$main_plot <- renderPlotly({
    dp <- data.frame()
    
    for (bond in bonds_list$bonds) {
      vc <- c(pv_bond(bond$coupon, bond$ytm, bond$par_value, bond$ttm), bond$ytm)
      dp <- rbind(dp, vc)
    }
    
    plot_ly(dp, x = ~dp[, 2], y = ~dp[, 1], type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Price vs Yield", xaxis = list(title = "Yield"), yaxis = list(title = "Price"))
  })
  
  #######################################################
  # Output price
  #######################################################
  output$value <- renderText({
    sum(sapply(bonds_list$bonds, function(bond) pv_bond(bond$coupon, bond$ytm, bond$par_value, bond$ttm)))
  })
}