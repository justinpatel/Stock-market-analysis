# Load all the necessary libraries
library(shiny)
library(shinythemes)
library(googleVis)
library(dplyr)
library(quantmod)

# Load all the NYSE listed stock
stock_list <- read.csv("./tickersName.csv")

# Define UI for application that draws the visualizations
ui <- fluidPage(

    # Application title
    titlePanel("Data Visualization"),
    
    # Application theme
    theme = shinytheme("slate"),

    # main navigation bar
    navbarPage(
        "Insights on Stock Market",
        
        # First tab, for candlestick and some other Charts
        tabPanel("Historical Performace",
                 sidebarPanel(
                     
                     br(),
                     
                     # selection of stock
                     tags$h3("Select your stock"),
                     
                     #  text input auto-complete option
                     selectizeInput("ticker","Select Ticker",choices = stock_list['Symbol']
                                    , multiple = F, options = list(create = F), selected = c("AMC")),
                     
                     checkboxInput("log.y","Price on log scale", value = F),
                     
                     br(),
                     
                     # select date range
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
                     htmlOutput("detail"),
                     plotOutput(outputId = "candlestickplot", height = 500),
                     br(),
                     br(),
                     htmlOutput("candle")
                     
                 )),   # first tab finish
        
        
        # second tab, for comparing daily and yearly returns of stocks
        tabPanel("Compare Returns", 
                 sidebarLayout(position = 'left',
                               sidebarPanel(
                                   
                                   # text input, multiple auto-complete option
                                   selectizeInput("multi_ticker_names","Select Multiple",choices = stock_list['Symbol']
                                                  , multiple = T, options = list(create = F), selected = c("NKE","SBUX","MCD","TJX","DIS"))
                                   ,
                                   br(),
                                   submitButton("Show"),
                               ),
                               # here bubble chart displays
                               mainPanel(
                                   htmlOutput("bubblechart")
                                   
                               )
                 )
        ), # second tab ends here
        
        # third tab
        tabPanel("Something Interesting!",
                 mainPanel(
                     htmlOutput("racechart")
                 )
        ), # end of third tab here
        
        # Fifth tab, for disclaimer
        tabPanel("Disclaimer",
                 tags$h4("Investment/Trading in securities Market is subject to market risk, past performance is not a guarantee of future performance. The risk of loss in trading and investment in Securities markets including Equites can be substantial. These are leveraged products that carry a substantial risk of loss up to your invested capital and may not be suitable for everyone. You should therefore carefully consider whether such trading is suitable for you in light of your financial condition. Please ensure that you understand fully the risks involved and do invest money according to your risk bearing capacity. This project is purely made for the purpose of education. This website/website creator does not bear any liabilities")
        )
    )
)

f_stock <- function(symbol, start, end)
{ 
    status <- tryCatch(getSymbols(symbol, src = "yahoo", from = start, to = end, auto.assign = FALSE , verbose = TRUE),error = identity)
    mt_st <- status
    df_st <- as.data.frame(mt_st)
    df_st$Stock<- symbol
    df_st$Date<- rownames(df_st)
    rownames(df_st)<-NULL
    names(df_st)[1:6] <- c("Open","High","Low","Close","Volume","Adjusted")
    
    return(df_st)
}


f_stock_processing <- function(start, end,stocks)
{ 
    
    for (i in 1:length(stocks))
    { 
        df_st = f_stock(stocks[i], start ,end)
        if (i==1) 
        {
            df_stocks = df_st
        }
        df_stocks = union(df_st,df_stocks)
    }
    
    return(df_stocks)
    
}

# Define server logic required to draw charts
addResourcePath("Data Viz Project", getwd())
server <- function(input, output) {
    
    output$racechart <- renderUI({
        tags$iframe(seamless = "seamless",
                    src = "Data Viz Project/racebarchart.html",
                    height = 500,
                    width  = 1200
                    )
    })
    
    output$bubblechart <- renderGvis({
        stocks <- as.character(input$multi_ticker_names)
        start <- as.Date("2021-01-01")
        end <- as.Date(Sys.Date())+1
        df_stocks <- f_stock_processing(start, end,stocks)
        
        df_stocks_Sub_start = df_stocks%>% 
            group_by(Stock) %>%
            filter(Date == min(Date)) %>% arrange(Stock)
        
        df_stocks_Sub = df_stocks%>% 
            group_by(Stock) %>%
            filter(Date == max(Date)) %>% arrange(Stock)
        
        df_stocks_Sub_start<- subset(df_stocks_Sub_start, select =-c(Date,High,Low, Adjusted)  )
        df_stocks_Sub<- subset(df_stocks_Sub, select =-c(Date,High,Low, Adjusted)  )
        
        df_stocks_Sub$TodayReturn <- 100*(df_stocks_Sub$Close - df_stocks_Sub$Open)/df_stocks_Sub$Open
        df_stocks_Sub$YearReturn <- 100*(df_stocks_Sub$Close - df_stocks_Sub_start$Open)/df_stocks_Sub_start$Open
        
        Bubble <- gvisBubbleChart(df_stocks_Sub, idvar="Stock", 
                                  xvar="TodayReturn", yvar="YearReturn",
                                  colorvar="Stock", sizevar="YearReturn",
                                  options=list( 
                                      #title="Daily Performance (Year's Return = Size of the Bubble)",
                                      vAxis="{title: 'Year Return (in %)'}",
                                      hAxis="{title: 'Today Return (in %)'}",
                                      hAxis="{minValue:75,  maxValue:125}",
                                      height=600,
                                      legend = T))
        
        Bubble
    })
    
    output$candle <- renderText({
        c("<img src='","https://www.wikihow.com/images/thumb/e/eb/Read-a-Candlestick-Chart-Step-1.jpg/v4-460px-Read-a-Candlestick-Chart-Step-1.jpg.webp","'>")
    })
    
    # Rendering chart of candlestick on first tab
    output$candlestickplot <- renderPlot({
        
        
        # check if valid
        validate(
            need(input$ticker, "Please enter valid ticker name")
        )
        
        name <- input$ticker
        
        # get data
        getSymbols(name,warnings = FALSE,auto.assign = T, src='yahoo')
        
        data <- get(name)
        names(data) <- c("Open","High","Low","Close","Volume","Adjusted")
        
        # get quote
        met <- yahooQF(c("Name","Exchange Full Name","Financial Currency","Open", "Ask","Bid","52-week Low","52-week High"))
        
        quote1 <- getQuote(name, what=met)
        
        # render details of company 
        output$detail <- renderUI({
            x <- paste0("<strong>Company Name : </strong>", paste(quote1['Name'],'',collapse  = ''))
            y <- paste0("<strong>Exchange Name : </strong>", paste(quote1['Exchange Full Name'],'',collapse  = ''))
            n <- paste0("<strong>Open Price : </strong>", paste(quote1[5],'',collapse  = ''))
            z <- paste0("<strong>Financial Currency : </strong>", paste(quote1[4],'',collapse  = ''))
            v <- paste0("<strong>Ask : </strong>", paste(quote1[6],'',collapse  = ''))
            w <- paste0("<strong>Bid : </strong>", paste(quote1[7],'',collapse  = ''))
            h <- paste0("<strong>52-Week High : </strong>", paste(quote1[8],'',collapse  = ''))
            l <- paste0("<strong>52-Week Low : </strong>", paste(quote1[9],'',collapse  = ''))
            HTML(c(x,"<br/>",y,"<br/>",n,"<br/>",z,"<br/>",h,"<br/>",l,"<br/>",v,"<br/>",w,"<br/>"))
        })
        
        # draw chart
        chartSeries(data,name = name, type = input$chart_type, theme='white', up.col = "green", dn.col = "red", log.scale = input$log.y)
        
        zoomChart(paste(input$dates, collapse = "::"))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
