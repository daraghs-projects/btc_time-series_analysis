library("TSA")
library("tseries")
#library(forecast)

##Q1
#create time series saved by monthly highs
ts <- ts(BTC_USD1$High, frequency = 12, start = c(2014, 10))

##Q2
#removing last 10% of observations (creating train and test split)
vol10 <- round(0.1 * length(ts))
train <- ts[1:(length(ts) - vol10)]
tstrain <- ts(train, frequency = 12, start = c(2014, 10))
test <- ts[(length(ts) - vol10 + 1):length(ts)]
tstest <- ts(test, frequency = 12, start = c(2023, 05))


##Q3
##checking for stationarity (variance)
plot(tstrain, ylab = "Price", main = "Bitcoin Price vs. Time")

#managing stationary (variance)
BC <-BoxCox.ar(tstrain, lambda=seq(-0.2,0.2,0.01), method = "ols")
BC$mle
BC$ci
train2 <- log(tstrain)

#checking for stationarity (trend)
acf(train2, main = "ACF of log(Bitcoin)")
adf.test(train2)

#managing stationary (trend)
difflogbtc <- diff(train2)
acf(difflogbtc, lag.max = 40, main = "ACF of difference(log(Bitcoin))")
adf.test(difflogbtc)

#deciding on models
pacf(difflogbtc, lag.max = 40, main = "PACF of difference(log(Bitcoin))")
eacf(difflogbtc)

btcsubs <- armasubsets(difflogbtc, nar=12, nma=12)
plot(btcsubs)

#models
AR1 <- arima(x = train2, order = c(1, 1, 0))
AR18 <- arima(x = train2, order = c(18, 1, 0))
MA1 <- arima(x = train2, order = c(0, 1, 1))
MA7 <- arima(x = train2, order = c(0, 1, 7))
MA18 <- arima(x = train2, order = c(0, 1, 18))

AR1.18 <- arima(train2, order=c(1,1,0),
                seasonal=list(order=c(1,0,0), period=18))

MA16.MA18 <- arima(train2, order=c(0,1,16),
                       seasonal=list(order=c(0,0,1), period=18))

#checking residuals are white noise
residAR1 <- rstandard(AR1)
acf(residAR1, main = "ACF of AR(1) Residuals")

residAR18 <- rstandard(AR18)
acf(residAR18, main = "ACF of AR(18) Residuals")

residAR1.18 <- rstandard(AR1.18)
acf(residAR1.18, lag.max = 40, main = "ACF of Model One Residuals")

residMA1 <- rstandard(MA1)
acf(residMA1, main = "ACF of MA(1) Residuals")

residMA7 <- rstandard(MA7)
acf(residMA7, main = "ACF of MA(7) Residuals")

residMA18 <- rstandard(MA18)
acf(residMA18, main = "ACF of Ma(18) Residuals")

residMA16.MA18 <- rstandard(MA16.MA18)
acf(residMA16.MA18, lag.max = 40, main = "ACF of Model Two Residuals")

#checking residuals are normally distributed
qqnorm(residAR1.18, main="QQ Plot of Residuals for Model One")
qqline(residAR1.18)
shapiro.test(residAR1.18)

qqnorm(residMA16.MA18, main="QQ Plot of Residuals for Model Two")
qqline(residMA16.MA18)
shapiro.test(residMA16.MA18)

#fitted against residuals
fit1 <- fitted(AR1.18)
plot(as.vector(fit1), as.vector(residAR1.18), main="Fitted Values vs Residuals for Model One")

fit2 <- fitted(MA16.MA18)
plot(as.vector(fit2), as.vector(residMA16.MA18), main="Fitted Values vs Residuals for Model Two")

#residual plot for trend
plot(residAR1.18, main="Time Plot of Residuals for Model One")

plot(residMA16.MA18, main="Time Plot of Residuals for Model Two")

#Ljung-Box test 
LBpvals <- rep(NA, 15)
for(i in 5:15){
  LBpvals[i] <- LB.test(AR1.18, lag=i)$p.value
}
plot(LBpvals, ylim=c(0,1), main=" Ljung-Box Test of Residuals for Model One"); abline(h=0.05, lty=2)

LBpvals <- rep(NA, 30)
for(i in 18:30){
  LBpvals[i] <- LB.test(MA16.MA18, lag=i)$p.value
}
plot(LBpvals, ylim=c(0,1), main=" Ljung-Box Test of Residuals for Model Two"); abline(h=0.05, lty=2)

#overfitting
AR1.18o <- arima(train2, order=c(1,1,1),
                seasonal=list(order=c(1,0,0), period=18))

AR1.18oo <- arima(train2, order=c(2,1,0),
                 seasonal=list(order=c(1,0,0), period=18))

MA16.MA18o <- arima(train2, order=c(1,1,16),
                   seasonal=list(order=c(0,0,1), period=18))

MA16.MA18oo <- arima(train2, order=c(0,1,17),
                    seasonal=list(order=c(0,0,1), period=18))

#predictions
plot(AR1.18,n.ahead=11, transform = exp, main= "Predictions for Bitcoin Test Set")

lines(tstest, col = "red")

legend("topleft", legend = c("Model 1 Predictions", "Test Set", "95% Confidence Limits"), 
       col = c("black", "red", "black"), lty = c(1, 1, 2),
       pch = c(1, NA, NA))
