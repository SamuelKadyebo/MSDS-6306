# Samuel Kadyebo Unit # 12 Home Work

library(dplyr)
library(TTR)
library(tidyverse)
library(fpp2)
library(dygraphs)
library(xts)
library(lubridate)

# 1 Brief Financial Data using EuStockMarkets 
#a.
data(EuStockMarkets) # Load data on European stock markets
plot(EuStockMarkets[,"DAX"])  # Pick and check that series data part from Germany
##Normalizing DAX index data with log funtion
dax <- diff(log(EuStockMarkets))[,"DAX"] # Use Log function to normalize DAX index data for parametric analysis
hist(dax) # checking the normalized DAX index data 

#b.
plot(EuStockMarkets[,"DAX"], main="Annual DAX index Data from 1990 Onward",col="blue") # Rudimentary plot of EuStockMarkets
abline(v=1997, col="red")

#c.
##Decomposing the DAX index time series into its components
tsData <- EuStockMarkets[,"DAX"] # ts data
decomposedRes <- decompose(tsData, type="mult") # use type = "additive" for additive components
plot(decomposedRes, col="blue") # see plot below
abline(v=1997, col="red")

#2. Temperature Data
#a
data(maxtemp) # Checking the dataset
plot(maxtemp)
View(maxtemp)
#b
newmaxtemp <- window(maxtemp, start = 1990) # subsetting for information after 1990
autoplot(newmaxtemp)+
  ylab("Max Temp") + xlab("Year")
#c
ses5year <- ses(newmaxtemp, h=5, alpha=0.1) # Using SES to predict the next five years
autoplot(ses5year, ts.colour = 'blue' ) +
  autolayer(fitted(ses5year), series="Fitted") +
    autolayer(predict(ses5year), series="Predicted", type="response") +
  ylab("Max Temp") + xlab("Year")

#d
 holt5year <- holt(newmaxtemp, initial = "optimal", h=5)
 holt5year1 <- holt(newmaxtemp, initial = "optimal", damped=TRUE, phi = 0.9, h=5)
 autoplot(newmaxtemp) +
   autolayer(holt5year, series="Holt's method", PI=FALSE) +
   autolayer(holt5year1, series="Damped Holt's method", PI=FALSE) +
   ggtitle("Forecasts from Holt's method") + xlab("Year") +
   ylab("Max Temp") +
   guides(colour=guide_legend(title="Forecast"))
 
 #e # Comparing the AICc of the ses() and holt() models:
 ses5year$model # Finding the AIACc of ses()
 holt5year1$model # Finding the AIACc of holt()
 holt5year1$model # Finding the AIACc of holt()
 # Accordingly ses() AICc has the lowest value and thus is a better model
 
#3 The Wands Choose the Wizard
#a,b,c
data1<-read.csv("C:/Users/Kadyebo/Desktop/SMU/Unit12TimeSeries_Gregorovitch.csv", header=FALSE, sep = ",")
data1$V1<- as.Date(data1[["V1"]], "%m/%d/%Y") #convert to date
time_series1 <- xts(data1, order.by = data1$V1) #make xts
dygraph(time_series1) #now plot

data2<-read.csv("C:/Users/Kadyebo/Desktop/SMU/Unit12TimeSeries_Ollivander.csv", header=FALSE, sep = ",")
data2$V1<- as.Date(data2[["V1"]], "%m/%d/%Y") #convert to date
time_series2 <- xts(data2, order.by = data2$V1) #make xts
dygraph(time_series2) #now plot

#d binding time_series1 and time_series2 and plotting
time_series1 <- xts(data1, order.by = data1$V1)
time_series2 <- xts(data2, order.by = data2$V1)

ts_merge <- cbind(time_series1,time_series2)
dygraph(ts_merge, main = "Gregorovitch and Ollivander Wands Time Series", 
        ylab = "Series' Plot", xlab = "Year") %>%
  dySeries("V1", color = "white") %>%
  dySeries("V1.1", color = "white") %>%
  dySeries("V2", label = "Gregorovitch", color = "blue") %>%
  dyLegend(width = 400, show="auto", hideOnMouseOut = FALSE)%>%
  dySeries("V2.1", label = "Ollivander", color = "red") %>%
  dyLegend(width = 400, show="auto", hideOnMouseOut = FALSE)%>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>%
  dyAxis("x", drawGrid = TRUE) %>%
  dyAxis("y", drawGrid = TRUE) %>%
  dyShading(from = "1995-01-01", to = "1999-01-01", color = "#EFEFEF") %>%
  dyOptions(stackedGraph = FALSE) %>%
  dyRangeSelector(height = 40)





