library(shiny)
library(plotly)
library(tidyverse)
library(RTL)

server <- function(input, output, session) {
  
  bonds_df <- reactiveValues(data = data.frame(
    coupon = numeric(),
    ytm = numeric(),
    par_value = numeric(),
    ttm = numeric(),
    principal = numeric(),
    price = numeric()
    
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
      textInput("coupon", "Coupon Rate:", "4"),
      textInput("ytm", "Yield to Maturity:", "5"),
      textInput("par_value", "Par Value:", "100"),
      textInput("ttm", "Time to Maturity:", "10"),
      textInput("principal", "Principal:", "1000"),
      actionButton("saveBond", "Save Bond"),
      easyClose = TRUE)
    )
  })
  
  #######################################################
  # Save Bond when Save Bond Button is clicked
  #######################################################

observeEvent(input$saveBond, {
  new_bond <- data.frame(
    coupon = as.numeric(input$coupon),
    ytm = as.numeric(input$ytm),
    par_value = as.numeric(input$par_value),
    ttm = as.numeric(input$ttm),
    principal = as.numeric(input$principal),
    price = as.numeric(RTL::bond(C = as.numeric(input$coupon), ytm = as.numeric(input$ytm), T2M = as.numeric(input$ttm)))
  )
  removeModal()
  bonds_df$data <- rbind(bonds_df$data, new_bond)
})

  #######################################################
  # Output df
  #######################################################
  output$bondsss <- renderTable(bonds_df$data)
  
}