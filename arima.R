library(tseries)
library(quantmod)
library(forecast)
library(tidyverse)

# Download gia co phieu va lay gia dong cua 
getSymbols('GOOGL', 
           from = '2016-01-01',
           to = '2020-01-01')

stock_prices <- GOOGL[, 4]

# Kiem tra tinh dung
plot(stock_prices)

# Tim bac d (integrated) ---> chon d = 1
stock <- diff(log(stock_prices), lag = 1)
stock <- stock[!is.na(stock)]
plot(stock)
adf.test(stock)

# Tao tap train va test (70% - 30%)
cut <- floor(nrow(stock) * 0.7)
train <- stock[1:cut,]
test <- stock[-row(train),]

# Tim bac p va q
acf.train <- acf(train,
                 main = 'ACF Plot',
                 lag.max = 100) # Chon bac MA(1)

pacf.train <- pacf(train, 
                   main = 'PACF Plot',
                   lag.max = 100) # Chon bac AR(5) hoac AR(8)

# Xay dung model ARIMA(5,1,1) va ARIMA(8,1,1)
model <- arima(train,
               order = c(5,1,1),
               include.mean = FALSE)

model2 <- arima(train,
               order = c(8,1,1),
               include.mean = FALSE)

# So sanh va kiem dinh 1 so chi tieu
checkresiduals(model)
checkresiduals(model2) # Chon model2 ARIMA(8,1,1)

AIC(model)
AIC(model2) # Chon model2 ARIMA(8,1,1)

accuracy(model)
accuracy(model2) # Chon model2 ARIMA(8,1,1)

# Du doan tap test theo ARIMA(8,1,1)
forecast_series <- data.frame(STT = integer(),
                              forecast.vl = numeric(),
                              upper = numeric(),
                              lower = numeric())

for(b in nrow(train):(nrow(stock) - 1)){
  stock_train <- stock[1:b,]
  stock_test <- stock[-nrow(stock_train),]
  
  model_fit <- arima(stock_train,
                     order = c(8,1,1),
                     include.mean = FALSE)
  
  forecast.st <- forecast(model_fit,
                          h = 1,
                          level = 95)
  
  forecast_series <- forecast_series %>% 
    rbind(data.frame(STT = b+1,
                     forecast.vl = forecast.st$mean[1],
                     upper = forecast.st$upper[1],
                     lower = forecast.st$lower[1]))
}

actual_series <- stock[(nrow(train) + 1):nrow(stock),]

# Tao danh sach du doan
forecast_series <- xts(forecast_series, index(actual_series))

# So sanh thuc te va du doan
plot(actual_series)
lines(forecast_series$forecast.vl,
      lwd = 1.5,
      col = "blue")
lines(forecast_series$upper,
      lwd = 1.5,
      col = "red")
lines(forecast_series$lower,
      lwd = 1.5,
      col = "red")

# 
comparision <- merge(actual_series, forecast_series$forecast.vl)
comparision$Accuracy <- sign(comparision$GOOGL.Close) == sign(comparision$forecast.vl)

sum(comparision$Accuracy)/nrow(comparision)




