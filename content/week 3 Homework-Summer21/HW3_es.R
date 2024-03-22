data_load <- read.table("temps.txt", header=TRUE)
data <- c()
for (year in 2:ncol(data_load)) {
  for (day in 1:nrow(data_load)) {
  data <- c(data, data_load[day,year])
  }
}
plot(data, type="l", main='Daily high-temp in July-Oct in Atlanta from 1996-2015',
     xlab='Day (Period = 123 days)', ylab='Temperature (F)')

library(TSA)
print(periodogram(data))

ts <- ts(data, start = c(1995,1), frequency = 123)
print(ts)

hw <- HoltWinters(ts, seasonal = "multiplicative")
print(hw)
fit <-(fitted(hw))
print(fit)
plot(fit, main='Level, Trend, and Seasonality after triple smoothing', col = "blue")
library(forecast)
plot(forecast(hw, 492), main='4-year forecast using HoltWinters',
     xlab='Day (Period = 123 days)', ylab='Temperature (F)', ylim=c(50,110))
