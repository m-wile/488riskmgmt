library(shiny)
   
ui <- fluidPage(
  
  # Application title
  titlePanel("Bond Price"),
  
  # Sidebar with a slider input 
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("ttm","Maturity in years",0,30,10),
      numericInput("coupon", "Coupon Rate:", value = 5, min = 0),
        # if this input is < 0, set it to be 0. the function 'numericInput' doesn't respect max and min parameters
        # suggestion is a concurrent reset function should the inputted value be less than 0
        # rationale: coupon rates & yields cannot be negative (or they aren't in reality), as such, repeat for the coupon & yield functions
      numericInput("ytm", "Yield:", value = 5, min = 1),
      numericInput("par_value", "Par Value:", value = 100)
      #actionButton("click","Calculate")
    ),
    
    mainPanel(
      h3("Price of the bond:"),
      textOutput("value"),
      h3("Sensitivity of the price by yield:"),
      plotlyOutput("main_plot"),
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