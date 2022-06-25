library(shinythemes)

# read list of equity ticker names
stock_list <- read.csv("./EQUITY.csv")

# Define UI for application
ui <- fluidPage(
          theme = shinytheme("darkly"),
          navbarPage(
            "Indian Stock Market Analysis",
            
            # First tab, for Chart and Technical Analysis UI
            tabPanel("Chart & Technical Analysis",
                     sidebarPanel(
                       # selection of stock
                       tags$h3("Select your stock"),
                       #  text input auto-complete option
                       selectizeInput("ticker","Select Ticker",choices = stock_list['SYMBOL']
                                      , multiple = F, options = list(create = F), selected = c("BURGERKING")),
                       
                       checkboxInput("log.y","Price on log scale", value = F),
                       
                       dateRangeInput("dates", "Date range", max = Sys.Date(), start = Sys.Date()-90, end= Sys.Date()),
                       
                       # select different chart type option
                       selectInput(inputId = "chart_type",
                                   label = "Chart type",
                                   choices = c("Candlestick" = "candlesticks",
                                               "Matchstick" = "matchsticks",
                                               "Bar" = "bars",
                                               "Line" = "line"),
                                   selected = c("candlesticks")),
                       
                       submitButton("Show")
                     ),
                     # here all the charts and details appear
                     mainPanel(
                       htmlOutput("detail")
                       ,
                       verbatimTextOutput(outputId = 'signal'),
                       plotOutput(outputId = "candlestickplot")
                       
                     )),   # first tab finish
            
            # second tab, for comparing stock UI
            tabPanel("Compare Stocks", 
                     sidebarLayout(position = 'left',
                     sidebarPanel(
                       # text input, multiple auto-complete option
                       selectizeInput("multi_ticker_names","Select Multiple",choices = stock_list['SYMBOL']
                                        , multiple = T, options = list(create = F), selected = c("HDFCBANK","ICICIBANK","SBIN","YESBANK"))
                       ,
                       br(),
                       submitButton("Show"),
                    ),
                    # here radar chart and a table containing financial details of stock goes
                     mainPanel(
                       tableOutput('b'),
                       plotOutput("radarplot")
                       
                     )
                     )
                    ),
            # Third tab, displays 'how-to?' tab
            tabPanel("How to?",
                     htmlOutput('how')
            ),
            
            # Fourth tab, for stock price forecast and chart
            tabPanel("Stock Price Forecast",
                     sidebarPanel(
                       tags$h3("Select your stock"),
                       
                       selectizeInput("ticker_fcast","Select",choices = stock_list['SYMBOL']
                                      , multiple = F, options = list(create = F), selected = c("BURGERKING")),


                       submitButton("Show")
                     ),

                     mainPanel(
                        plotOutput(outputId = 'forecastPlot')
                     )

              ),
            
            # Fifth tab, for disclaimer
            tabPanel("Disclaimer",
                     tags$h4("Investment/Trading in securities Market is subject to market risk, past performance is not a guarantee of future performance. The risk of loss in trading and investment in Securities markets including Equites can be substantial. These are leveraged products that carry a substantial risk of loss up to your invested capital and may not be suitable for everyone. You should therefore carefully consider whether such trading is suitable for you in light of your financial condition. Please ensure that you understand fully the risks involved and do invest money according to your risk bearing capacity. This project is purely made for the purpose of education. This website/website creator does not bear any liabilities")
            )
                
                     
          )
)
