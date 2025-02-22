install.packages("MLmetrics")
install.packages("fpp2")
install.packages("tseries")
install.packages("urca")
library("MLmetrics")
library("fpp2")
library("tseries")
library("urca")
library("readxl")
#data reading
setwd("D:/B.Sc (H) Statistics/SEM 5/TSA project")
data_1=read_xlsx("Pro.xlsx",sheet = "Punjab")
train_1=data_1[1:1200,]
test_1=data_1[1201:1404,]

#stationarity test
x_1=train_1[['rainfall']]
stat_test_1=adf.test(x_1);stat_test_1
result_1=ifelse(stat_test_1$p.value<=0.05,"The time series is stationary","The time series is not stationary")
result_1

#acf and pacf graphs
d=pacf(train_1$rainfall,lag.max=100,type=c("correlation"),na.action=na.pass,plot=FALSE)
plot(d)
d=acf(train_1$rainfall,lag.max=100,type=c("correlation"),na.action=na.pass,plot=FALSE)
plot(d)

#ARIMA Model fitting
d1 = ts(train_1[,3],start=c(1901,1),end=c(2000,12),frequency=12)
arima_model_punjab = auto.arima(d1)
summary(arima_model_punjab)
fore_arima_1 = forecast::forecast(arima_model_punjab,h=204)
df_arima_1=as.data.frame(fore_arima_1)
test_1$forecast=df_arima_1$`Point Forecast`
MAE(test_1$forecast,test_1$rainfall)
rss <- sum((test_1$forecast - test_1$rainfall) ^ 2)  ## residual sum of squares
tss <- sum((test_1$rainfall - mean(test_1$rainfall)) ^ 2)  ## total sum of squares
rsq <- 1 - (rss/tss);rsq
plot(test_1$rainfall,main="Predicted rainfall (2001-2023)",type="l",col=c("red"),ylim=c(0,300),xlim=c(0,280),ylab="Rainfall",xlab="Time")
par(new=TRUE)
plot(forec,type="l",main="Predicted rainfall (2001-2023)",col=c("blue"),ylim=c(0,300),xlim=c(0,280),ylab="Rainfall",xlab="Time")
legend("topright",legend=c("Rainfall (in mm)","Forecasted rainfall (in mm)"),fill=c("red","blue"))
#residuals plot
RMSE_1=RMSE(test_1$rainfall,test_1$forecast)
standard_res_1=arima_model_punjab$residuals/RMSE_1
plot(standard_res_1,main="Residuals plot",ylab="Residual")
d=acf(arima_model_punjab$residuals,lag.max=100,type=c("correlation"),na.action=na.pass,plot=FALSE)
plot(d,main="ACF of Residuals")

#future forecasting
fore_arima_11 = forecast::forecast(arima_model_punjab,h=276)
df_arima_11=as.data.frame(fore_arima_11)
forec=df_arima_11$`Point Forecast`;forec

#prediction interval
df_arima_11$PI_LL=df_arima_11$`Point Forecast`-(1.96*sd(df_arima_11$`Point Forecast`))
df_arima_11$PI_UL=df_arima_11$`Point Forecast`+(1.96*sd(df_arima_11$`Point Forecast`))
df_arima_11
plot(df_arima_11$`Point Forecast`,type="l",col="blue",ylim=c(-150,150))
par(new=T)
plot(df_arima_11$PI_LL,type="l",col="red",ylim=c(-150,150))
par(new=T)
plot(df_arima_11$PI_UL,type="l",col="orange",ylim=c(-150,150))
