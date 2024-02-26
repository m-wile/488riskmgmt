library(shiny)
library(plotly)
library(tidyverse)

server <- function(input, output) {
  #######################################################
  # PRICE BOND FUNCTION
  #######################################################
  pv_bond <- function(coupon,yield,redt,t){
    price <- 0
    for (year in 1:(t-1)) {
      price <- price + (coupon)/((1+yield/100)^year)
    }
    price <- price +  (redt+coupon)/((1+yield/100)^t)
    return(price)
  }
  #######################################################
  # output plotly
  #######################################################
  output$main_plot <- renderPlotly({
    dp <- data.frame()
    for (y in seq(1,10,0.25)) {
      vc <- c(pv_bond(input$coupon,y,input$par_value,input$ttm),y)
      dp <- rbind(dp,vc)
    }
    
    plot_ly(dp, x = ~ dp[,2], y = ~ dp[,1], type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Price vs Yield", xaxis = list(title = "Yield"), yaxis = list(title = "Price"))
  })
  
  #######################################################
  # output price 
  #######################################################
  output$value<- renderText({pv_bond(input$coupon,input$ytm,input$par_value,input$ttm)})
}

