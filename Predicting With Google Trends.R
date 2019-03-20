rm(list=ls())


library(xts)
library(forecast)
library("lubridate")
library(scals)
library(ggplot2)
library(quantmod)

## Functions ###########################################################

pcchange=function(x,lag=1) c(diff(x,lag),rep(NA,lag))/x


Find_Max_CCF<- function(a,b)
{
  d <- ccf(a, b, plot = FALSE)
  cor = d$acf[,,1]
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  res_max = res[which.min(res$cor),]
  return(res_max)
} 

########################################################################






dailyReturn(DAX$Adj.Close)
#
trends <- read.delim("report(4).csv", sep=",", stringsAsFactors=FALSE)
trends$Date <- strtrim(trends$Woche, 10)
trends$Date <- as.Date(trends$Date, format = "%Y-%m-%d")







DAX <- read.csv("http://real-chart.finance.yahoo.com/table.csv?s=%5EGDAXI&a=05&b=29&c=2014&d=05&e=26&f=2015&g=w&ignore=.csv", stringsAsFactors=FALSE)
DAX$Date <- as.Date(DAX$Date, format = "%Y-%m-%d")
DAX <- DAX[order(DAX$Date),]
DAX$Date <- DAX$Date - 1

round(DAX$Adj.Close/max(DAX$Adj.Close)*100)


data <- data.frame(Date=trends$Date,
                   dax=trends$dax,
                   dax.chg=pcchange(trends$dax),
                   DAX.Idx=round(DAX$Adj.Close/max(DAX$Adj.Close)*100),
                   DAX.ret=pcchange(DAX$Adj.Close))

data <- data[1:nrow(data)-1,]
data.frame(cor(data[,2:5]))



ccf(drop(data$DAX.Idx), drop(data$dax))
ccf(drop(diff(data$crisis,na.pad=FALSE)), drop(diff(data$DAX,na.pad=FALSE)))




Find_Max_CCF(data$dax.chg, data$DAX.ret)




data$DAX.Diff <- c(0, diff(data$DAX))
data$crisis.Diff <- c(0, diff(data$crisis))


p <- ggplot() + 
  geom_line(data = data, aes(x = Date, y = DAX.ret, color = "red")) +
  geom_line(data = data, aes(x = Date, y = dax.chg, color = "blue"))  +
  xlab('data_date') +
  ylab('percent.change')
p



##### http://static.googleusercontent.com/media/www.google.com/de//googleblogs/pdfs/google_predicting_the_present.pdf

##### Define Predictors - Lagged DAX 1 to 7 weeks
data$s1 = c(NA, data$DAX[1:(nrow(data)-1)])
data$s2 = c(rep(NA, 2), data$DAX.Diff[1:(nrow(data)-2)])
data$s3 = c(rep(NA, 3), data$DAX.Diff[1:(nrow(data)-3)])
data$s4 = c(rep(NA, 4), data$DAX.Diff[1:(nrow(data)-4)])
data$s5 = c(rep(NA, 5), data$DAX.Diff[1:(nrow(data)-5)])
data$s6 = c(rep(NA, 6), data$DAX.Diff[1:(nrow(data)-6)])
data$s7 = c(rep(NA, 7), data$DAX.Diff[1:(nrow(data)-7)])


##### Define Predictor - Google Trends
## t.lag defines the time lag between the research and purchase.
## t.lag = 0 if you want to include last week of the previous month and
## 1st-2nd week of the corresponding month
## t.lag = 1 if you want to include 1st-3rd week of the corresponding month
t.lag = 1
id = which(data$Date[-1] != data$Date[-nrow(data)])
data = data[id + 1,]

# Add Lags
data$trends1 = data$crisis[id + t.lag]
data$trends2 = data$crisis[id + t.lag + 1]
data$trends3 = data$crisis[id + t.lag + 2]
data$trends4 = data$crisis[id + t.lag + 3]
data$trends5 = data$crisis[id + t.lag + 4]





##### Exploratory Data Analysis
## Testing Autocorrelation & seasonality
acf((data$DAX.Diff))
Box.test((data$DAX), type="Ljung-Box")


## Testing Correlation
plot(y = (data$DAX.Diff), x = data$trends4, main='', pch=19,
     ylab='log(DAX)', xlab= 'Google Trends - 1st week')


abline(lm(data$DAX.Diff) ~ data$trends4, lwd=2, col=2)

cor.test(y = data$DAX.Diff, x = data$trends1)
cor.test(y = data$DAX.Diff, x = data$trends2)
cor.test(y = data$DAX.Diff, x = data$trends3)
cor.test(y = data$DAX.Diff, x = data$trends4)
cor.test(y = data$DAX.Diff, x = data$trends5)




##### Divide data by two parts - model fitting & prediction
dat1 <- data[1:(nrow(data)-1), ]
dat2 <- data[nrow(data), ]




# Lineare Regression
linear.fit <- lm(DAX ~ (s1) + (s2) + (s3) + (s4) + (s5) + (s6) + (s7) + trends3, data=dat1)
summary(linear.fit)

##### Diagnostic Plot
par(mfrow=c(2,2));
plot(linear.fit)




# Logistische Regression

#Scale Adj.Close to 0-1
dat1$DAX.Logit <- as.numeric(1 / (1 + exp(- scale(dat1$DAX))))

logit.fit <- glm(DAX.Logit ~s1 + s2 + s3 + s4 + s5 + s6 + s7 + trends1, data=dat1, family = "binomial")
summary(logit.fit)

##### Diagnostic Plot
par(mfrow=c(2,2));
plot(logit.fit)




#### Prediction for the next week;
predict.fit = predict(linear.fit, newdata=dat2, se.fit=TRUE)
summary(predict.fit)
dat2$DAX.Logit
dat2$DAX.Logit <- as.numeric(1 / (1 + exp(- scale(dat2$Adj.Close))))




ar <- auto.arima(data$DAX)
summary(ar)


arima(data$DAX, order=c(0,1,0), xreg=data[,2:6])

