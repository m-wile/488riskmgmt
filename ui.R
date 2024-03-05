library(shiny)
library(plotly)
   
ui <- fluidPage(
  
  # Application title
  titlePanel("Bond Price"),
  # Sidebar with a slider input 
  sidebarLayout(
    
    sidebarPanel(
      uiOutput("addBondButton"),
      DTOutput("bond_table"),
      uiOutput("deleteButton"),
      h6("Instruction:"),
      h6("You should insert the maturity, coupon, yield and redeption price"),
      h6("a) Select the maturity in years"),
      h6("b) Insert coupon and yield as percentage"),
      h6("c) Insert redemption price as percentage "),
      h6("You will get the quotation of the bond as percentage and a graphic"),
      h6("of the sensibility of the price by yield")),
    
    mainPanel(
      
    )
    
    
  )
)