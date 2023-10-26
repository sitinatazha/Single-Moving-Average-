install.packages("forecast")
install.packages("TTR")
library("forecast")
library("TTR")
library("graphics")

data1<-read.delim("clipboard")
data1

#membentuk data time series
data.ts<-ts(data1)
data.ts

#membuat plot time series
ts.plot(data.ts, xlab="Time Period ", ylab="Inflasi ", main= "Time Series Plot Data inflasi Jayapura")
points(data.ts)


#pemulusan SMA dengan n=4
data.sma<-SMA(data.ts, n=4)
data.sma

data.ramal<-c(NA,data.sma)
data.ramal

data<-cbind(aktual=c(data.ts,rep(NA,5)),pemulusan=c(data.sma,rep(NA,5)),ramalan=c(data.ramal,rep(data.ramal[length(data.ramal)],4)))
data

#plot time series
ts.plot(data.ts, xlab="Time Period ", ylab="inflasi jayapura", main= "SMA N=4 Inflasi Jayapura")
points(data.ts)
lines(data.sma,col="green",lwd=2)
lines(data.ramal,col="red",lwd=2)
legend("topleft",c("data aktual","data pemulusan","data peramalan"), lty=8, col=c("black","green","red"), cex=0.8)

error=data.ts-data.ramal[1:length(data.ts)]
SSE=sum(error[5:length(data.ts)]^2)
SSE

MSE=mean(error[5:length(data.ts)]^2)
MSE

MAPE=mean(abs((error[5:length(data.ts)]/data.ts[5:length(data.ts)])*100))
MAPE