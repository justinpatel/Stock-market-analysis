library(quantmod)
library(tidyr)
install.packages("googleVis")

remotes::install_github("feddelegrand7/ddplot", build_vignettes = TRUE)
library(ddplot)

start <- as.Date(Sys.Date())-(365*10)
end <- as.Date(Sys.Date())

facebook <- getSymbols("FB", from = start, to = end, warnings = F, auto.assign = F, src='yahoo')
apple <- getSymbols("AAPL", from = start, to = end, warnings = F, auto.assign = F, src='yahoo')
amazon <- getSymbols("AMZN", from = start, to = end, warnings = F, auto.assign = F, src='yahoo')
netflix <- getSymbols("NFLX", from = start, to = end, warnings = F, auto.assign = F, src='yahoo')
google <- getSymbols("GOOG", from = start, to = end, warnings = F, auto.assign = F, src='yahoo')

merge <- cbind(facebook$FB.Close, apple$AAPL.Close, amazon$AMZN.Close, netflix$NFLX.Close, google$GOOG.Close)
merge.df <- data.frame(merge)
merge.df <- cbind(date = rownames(merge.df), merge.df)
rownames(merge.df) <- NULL
colnames(merge.df) <- c("date","Facebook","Apple","Amazon","Netflix","Google")
merge.df.long <- gather(merge.df, key = "Company", value = "Price", Facebook, Apple, Amazon, Netflix, Google)
barChartRace(merge.df.long, x = "Price", y = "Company", 
             time = "date", 
             frameDur = 10,   
             transitionDur = 10,
             colorCategory = "Dark2",
             title = "Comparing FAANG Performace Of Past 10 Years", 
             bgcol='gray',    
             panelcol = "white",
             xgridlinecol = "#EBEBEBFF",
             xtitle = "Price", ytitle = "Company"
             )
