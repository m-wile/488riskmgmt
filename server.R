library(shiny)
library(DT)
library(tidyverse)
library(RTL)

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
      Price = round(as.numeric(bond_cpp_call(ytm = as.numeric(input$ytm), 
                                             C = as.numeric(input$coupon), 
                                             T2M = as.numeric(input$ttm), 
                                             m = 2, output = "price")), 1)
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
}
