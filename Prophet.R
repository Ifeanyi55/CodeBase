library(prophet)
library(forecast)
library(highcharter)
library(dplyr)
library(lubridate)
library(ggplot2)

vacIn <- read.csv("vacInf_timeline.csv")
View(vacIn)

vacIn <- vacIn %>% 
  mutate(Date = as_date(dmy_hm(created_at))) %>% 
  select(Date,retweet_count) %>% 
  head(50)
  
View(vacIn)
vacIn
names(vacIn) <- c("ds","y")

vacIn$y <- log10(vacIn$y)

plot(y ~ ds,vacIn,type = "l")

m <- prophet(vacIn)
future <- make_future_dataframe(m,periods = 365)
forecast <- predict(m,future)

View(forecast)

## Visualize forecast
plot(m,forecast)

## Make interactive forecast plot
dyplot.prophet(m,forecast)

## Plot forecast components
prophet_plot_components(m,forecast)

vacForc <- data.frame(
  time = forecast$ds,
  top = forecast$yhat_upper,
  lower = forecast$yhat_lower,
  trend = forecast$trend
)

vacForc %>% 
  ggplot()+aes(x = time,y = trend)+geom_line(color = "lightblue",size = 3)

head(vacForc,20) %>% 
  hchart(type = "line",hcaes(x = time,y = trend))

## Predicting with AAPL Finance data
aapl <- read.csv("AAPL.csv")
View(aapl)

## Check for missing values
is.na(aapl)

## Select the variables of interest
aapldf <- aapl %>% 
  select(Date,Close) %>% 
  mutate(ds = Date,
         y = Close)

head(aapldf,20)

## Check data structure
str(aapldf)

## Convert date to factor
aapldf$Date <- as.factor(aapldf$Date)

## Visually inspect data
aapldf %>% 
  hchart(type = "line",hcaes(x = Date,y = Close))

glimpse(aapldf)
dim(aapldf)

## Eliminate noise with power transform 
lambd <- BoxCox.lambda(aapldf$Close,method = "loglik")
aapldf$y <- BoxCox(aapldf$Close,lambd)

## Create forecast future data frame
set.seed(123)
m <- prophet(aapldf,yearly.seasonality = TRUE,daily.seasonality = TRUE,weekly.seasonality = TRUE)
future <- make_future_dataframe(m,periods = 365)

## Predict with the model
forecast <- predict(m,future)
View(forecast)

## Visualize forecast
plot(m,forecast,xlabel = "Years",ylabel = "Price")

## Make plot interactive
dyplot.prophet(m,forecast)

## Plot forecast components
prophet_plot_components(m,forecast)
