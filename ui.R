library(DT)
library(plotly)
library(shiny)

ui <- shiny::fluidPage(
  shiny::titlePanel("Bond Price"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 2,
      shiny::uiOutput("addBondButton"),
      shiny::uiOutput("deleteButton"),
      shiny::actionButton("loadRandomPortfolio", "Load Random Portfolio"),
      # Display the total value
      textOutput("total_value_output"),
      shiny::h6("Instruction:"),
      shiny::h6("You should insert the maturity, coupon, yield and redeption price"),
      shiny::h6("a) Select the maturity in years"),
      shiny::h6("b) Insert coupon and yield as percentage"),
      shiny::h6("c) Insert redemption price as percentage "),
      shiny::h6("You will get the quotation of the bond as percentage and a graphic"),
      shiny::h6("of the sensibility of the price by yield")),
    shiny::mainPanel(
      width = 10, 
      # plotly::plotlyOutput("ytm_price_plot"), 
      # plotly::plotlyOutput("yield_curve_plot"),
      # plotly::plotlyOutput("duration_convexity_plot"),
      # plotly::plotlyOutput("fred_plot"),
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel("Portfolio Summary", 
                        shiny::br(),
                        DT::DTOutput("bond_table", width = 300, fill = TRUE), 
                        shiny::br(), 
                        shiny::br(), 
                        plotly::plotlyOutput("yield_curve_plot")),
        shiny::tabPanel("Portfolio Charts", 
                        shiny::br(),
                        shiny::splitLayout(cellWidths = c("50%", "50%"), 
                                           plotly::plotlyOutput("ytm_price_plot"), 
                                           plotly::plotlyOutput("duration_convexity_plot")), 
                        # shiny::br(),
                        # shiny::br(),
                        # plotly::plotlyOutput("yield_curve_plot")
                        ),
        shiny::tabPanel("Historical Charts", 
                        shiny::br(),
                        plotly::plotlyOutput("fred_plot"), 
                        plotly::plotlyOutput("fred_plot_delta"), 
                        plotly::plotlyOutput("fred_plot_gamma")
                        ), 
          
        # shiny::tabPanel("Price Across Yield", 
        #                 plotly::plotlyOutput("ytm_price_plot")),
        # shiny::tabPanel("Yield Curve", 
        #                 plotly::plotlyOutput("yield_curve_plot")),
        # shiny::tabPanel("Delta & Gamma", 
        #                 plotly::plotlyOutput("duration_convexity_plot")),
        # shiny::tabPanel("Federal Reserve Data", 
        #                 plotly::plotlyOutput("fred_plot")),
      )
    )
  )
)