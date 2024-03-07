library(shiny)
library(plotly)
library(DT)
   
ui <- fluidPage(
  
  # Application title
  titlePanel("Bond Price"),
  # Sidebar with a slider input 
  sidebarLayout(
    
    sidebarPanel(
      #buttons
      uiOutput("addBondButton"),
      uiOutput("deleteButton"),
      actionButton("loadRandomPortfolio", "Load Random Portfolio"),
      # output
      DTOutput("bond_table", width = 300, fill = TRUE),
      
      #text
      h6("Instruction:"),
      h6("You should insert the maturity, coupon, yield and redeption price"),
      h6("a) Select the maturity in years"),
      h6("b) Insert coupon and yield as percentage"),
      h6("c) Insert redemption price as percentage "),
      h6("You will get the quotation of the bond as percentage and a graphic"),
      h6("of the sensibility of the price by yield")),
    
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Price Across Yield", plotlyOutput("ytm_price_plot")),
        tabPanel("Yield Curve", plotlyOutput("yield_curve_plot")),
        tabPanel("Delta & Gamma", plotlyOutput("duration_convexity_plot")),
        tabPanel("Federal Reserve Data", plotlyOutput("fred_plot")),
      )
    )
  )
)