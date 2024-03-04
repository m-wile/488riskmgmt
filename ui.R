library(shiny)
library(plotly)
   
ui <- fluidPage(
  
  # Application title
  titlePanel("Bond Price"),
  
  # Sidebar with a slider input 
  sidebarLayout(
    
    sidebarPanel(
      uiOutput("addBondButton"),
      sliderInput("ttm","Maturity in years",0,30,10),
      numericInput("coupon", "Coupon Rate:", value = 5),
      numericInput("ytm", "Yield:", value = 5),
      numericInput("par_value", "Par Value:", value = 100)
      #actionButton("click","Calculate")
    ),
    
    mainPanel(
      h3("Price of the bond:"),
      textOutput("value"),
      h3("Sensibility of the price by yield:"),
      plotly::plotlyOutput("main_plot"),
      h6("Instruction:"),
      h6("You should insert the maturity, coupon, yield and redeption price"),
      h6("a) Select the maturity in years"),
      h6("b) Insert coupon and yield as percentage"),
      h6("c) Insert redemption price as percentage "),
      h6("You will get the quotation of the bond as percentage and a graphic"),
      h6("of the sensibility of the price by yield")
    )
    
    
  )
)