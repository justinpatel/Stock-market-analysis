# load necessary packages
library(quantmod)
library(binhf)
library(TTR)
library(devtools)   
library(CandleStickPattern)   # for trend analysis

# for radar chart
library(fmsb)

library(forecast)   # for forecasting the price

# Define server logic required to draw a feed UI
server <- function(input, output) {
  
  # Rendering chart of candlestick on first tab
  output$candlestickplot <- renderPlot(height = 700,{
    
    # check if valid
    validate(
      need(input$ticker, "Please enter valid ticker name")
    )
    
    name <- paste(input$ticker,"NS",sep='.')
    
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
    chartSeries(data,name = name, type = input$chart_type, theme='white', up.col = "green", dn.col = "red", log.scale = input$log.y,
                TA = c(addEMA(n=10, col='blue'), addEMA(n=21, col='red'), addMACD(histogram = T), addRSI(), addVo()))

    zoomChart(paste(input$dates, collapse = "::"))
    
    # gives signal (buy/sell) & trend (up or down or neutral)
    output$signal <- renderText({
      signal <- "Current trend is neutral. No right time to buy/sell"
      
      if(last(up.trend(na.omit(data), delta = 0.01, S = 10, L = 50),1)){
        signal <- "Current trend is up.\nSignal : Buy"
      }
      if(last(down.trend(na.omit(data), delta = 0.01, S = 10, L = 50),1)){
        signal <- "Current trend is down.\nSignal : Sell"
      }
      signal
    }) 
    
  })
  
  # renders plot of stock comparison, radarplot on second tab
  output$radarplot <- renderPlot(height = 700,{
     
        # check valid input
        validate(
          need(input$multi_ticker_names, "Please select stocks to compare")
        )    
        
        Symbols <- NULL

        # assemble name in proper vector
        for (name in input$multi_ticker_names){
          Symbols <- append(Symbols, paste(name,"NS",sep='.'))
        }
        
        # get quote
        metrics <- yahooQF(c("Market Capitalization",
                             "Volume","P/E Ratio","Price/Book"))
        
        quote <- getQuote(paste(Symbols, sep="", collapse=";"), what=metrics)
        
        # render table of quote
        output$b <- renderTable({quote[-1]}, hover = T, bordered = T, rownames = T, spacing = "xs")
        
        q <- quote[-1]
        
        # min-max scaling function
        normalize <- function(x, ...) {
          return((x - min(x, ...)) /(max(x, ...) - min(x, ...)))
        }
        
        # scale data of quote
        for (x in names(q)){
          q[x] <- normalize(q[x], na.rm = T)
        }
        
        # bind min and max values for each column
        q <- cbind(rbind(max(q[1], na.rm = T), 0, q[1]), 
                    rbind(max(q[2], na.rm = T), 0, q[2]), 
                    rbind(max(q[3], na.rm = T), 0, q[3]),
                    rbind(max(q[4], na.rm = T), 0, q[4])
                )
        
        colors_fill <- c(scales::alpha("red", 0.1),
                         scales::alpha("orange", 0.1),
                         scales::alpha("yellow", 0.1),
                         scales::alpha("lightgreen", 0.1),
                         scales::alpha("blue", 0.1),
                         scales::alpha("purple", 0.1),
                         scales::alpha("gray", 0.1),
                         scales::alpha("cyan", 0.1))
        
        colors_line <-  c(scales::alpha("darkred", 0.9),
                          scales::alpha("darkorange", 0.9),
                          scales::alpha("gold", 0.9),
                          scales::alpha("darkgreen", 0.9),
                          scales::alpha("darkblue", 0.9),
                          scales::alpha("purple", 0.9),
                          scales::alpha("darkgray", 0.9),
                          scales::alpha("cyan", 0.9))
        
        # draws radar chart
        radarchart(q,
                   seg = 4,
                   title ="Stock comparison",
                   plty = 2,
                   pcol = colors_line,
                   pfcol = colors_fill,
                   plwd = 4,
                   cglcol = "grey",
                   axislabcol = "grey"
        )
        
        # Add a legend
        legend(x=1.15, 
               y=1.35, 
               legend = rownames(q[-c(1,2),]), 
               bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 1.5)
        
  
  })
  
  # render UI that explains how to use technical analysis, in third tab
  output$how <- renderUI({
    ma <- paste0("<h3>How to Use a Moving Average to Buy/Sell Stocks</h3>" , "<strong>Price Crossovers</strong>","<p>When the price crosses above or below a moving average to signal a potential change in the trend.</p>","<strong>One longer and one shorter</strong>","<p>When the shorter-term MA crosses above the longer-term MA, it's a buy signal, as it indicates that the trend is shifting up. This is known as a 'golden cross'. Meanwhile, when the shorter-term MA crosses below the longer-term MA, it's a sell signal, as it indicates that the trend is shifting down. This is known as a 'dead/death cross.'</p>",collapse  = '')
    
    macd <- paste0("<h3>How to Use a MACD to Buy/Sell Stocks</h3>","<strong>Zero Line Crossover</strong>","<p>The strategy is to buy when the MACD crosses above the zero line, and sell when the MACD line crosses below the zero line.</p>", "<strong>Signal Line Crossover</strong>","<p>The strategy is to buy when the MACDline crosses above the signal line else sell.</p>", collapse = '')
    
    rsi <- paste0("<h3>How to Use a RSI to Buy/Sell Stocks</h3>","<p> The RSI ranges from 0 to 100. If RSI value is above 70 it indicates overbought and if it is below 30 it indicates oversold. If the price is making new highs/lows, and the RSI is not, it indicates a reversal.</p>", collapse = '')
    HTML(c(ma,"</br>",macd,"</br>",rsi))
  })
  
  # render plot of stock comparison, in fourth tab
  output$forecastPlot <- renderPlot(height = 700,{
    
    # check for valid input
    validate(
      need(input$ticker_fcast, "Please enter valid ticker name")
    )

    name <- paste(input$ticker_fcast,"NS",sep='.')

    # get data
    getSymbols(name,warnings = FALSE,auto.assign = T, src='yahoo', from='2011-07-01', to = '2021-07-31')

    data <- get(name)
    
    # apply auto arima on closing price
    fit <- auto.arima(Cl(data), seasonal = F)
    # apply forecast function with term = 365 (a year)
    fcast <- forecast(fit, h = 365)
    # plot forecast plot
    plot(fcast)
  })
  
}