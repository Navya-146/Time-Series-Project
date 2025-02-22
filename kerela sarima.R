install.packages("dplyr")
install.packages("fpp2")
install.packages("tseries")
install.packages("urca")
library("dplyr")
library("fpp2")
library("tseries")
library("urca")
library("readxl")
library("MLmetrics")
#data reading
setwd("D:/B.Sc (H) Statistics/SEM 5/TSA project")
data_2=read_xlsx("Pro.xlsx",sheet = "Kerela")
train_2=data_2[1:1200,]
test_2=data_2[1201:1404,]

#stationarity test
x_2=train_2[['rainfall']]
stat_test_2=adf.test(x_2)
stat_test_2
result_2=ifelse(stat_test_2$p.value<=0.05,"The time series is stationary","The time series is not stationary")
result_2

#acf and pacf graphs
d=pacf(train_2$rainfall,lag.max=100,type=c("correlation"),na.action=na.pass,plot=FALSE)
plot(d,type="l",ylim=c(-1,1))
d=acf(train_2$rainfall,lag.max=200,type=c("correlation"),na.action=na.pass,plot=FALSE)
plot(d,type="l")

#ARIMA Model fitting
d2 = ts(train_2[,3],start=c(1901,1),end=c(2000,12),frequency=12)
arima_model_kerela = auto.arima(d2)
arima_model_kerela
summary(arima_model_kerela)
fore_arima_2 = forecast::forecast(arima_model_kerela,h=204)
df_arima_2=as.data.frame(fore_arima_2)
test_2$forecast=df_arima_2$`Point Forecast`
rss_2 <- sum((test_2$forecast - test_2$rainfall) ^ 2)  ## residual sum of squares
tss_2 <- sum((test_2$rainfall - mean(test_2$rainfall)) ^ 2)  ## total sum of squares
rsq_2 <- 1 - (rss_2/tss_2);rsq_2

#residuals plot
RMSE_2=RMSE(test_2$rainfall,test_2$forecast)
standard_res_2=arima_model_kerela$residuals/RMSE_2
mean(standard_res_2)
plot(standard_res_2,main="Residuals plot",ylab="Residual")
d=acf(arima_model_kerela$residuals,lag.max=100,type=c("correlation"),na.action=na.pass,plot=FALSE)
plot(d,main="ACF of Residuals")

#future forecasting
fore_arima_21 = forecast::forecast(arima_model_kerela,h=276)
df_arima_21=as.data.frame(fore_arima_21)
forec_2=df_arima_21$`Point Forecast`
year=rep(2001:2023,each=12)
month=rep(c("-Jan","-Feb","-Mar","-Apr","-May","-Jun","-Jul","-Aug","-Sep","-Oct","-Nov","-Dec"),23)
xlabel=paste0(year,month)
xlabel
plot(test_2$rainfall,main="Predicted rainfall (2001-2023)",type="l",col=c("red"),ylim=c(0,1000),xlim=c(0,280),ylab="Rainfall",xlab="Time")
par(new=TRUE)
plot(forec_2,main="Predicted rainfall (2001-2023)",type="l",col=c("blue"),ylim=c(0,1000),xlim=c(0,280),ylab="Rainfall",xlab="Time")
legend("topright",legend=c("Rainfall (in mm)","Forecasted rainfall (in mm)"),fill=c("red","blue"))
#prediction interval
df_arima_21$PI_LL=df_arima_21$`Point Forecast`-(1.96*sd(df_arima_21$`Point Forecast`))
df_arima_21$PI_UL=df_arima_21$`Point Forecast`+(1.96*sd(df_arima_21$`Point Forecast`))
df_arima_21
plot(df_arima_21$`Point Forecast`,type="l",col="blue",ylim=c(-450,700))
par(new=T)
plot(df_arima_21$PI_LL,type="l",col="red",ylim=c(-450,700))
par(new=T)
plot(df_arima_21$PI_UL,type="l",col="orange",ylim=c(-450,700))
