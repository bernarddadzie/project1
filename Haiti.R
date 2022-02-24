library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(trend)
library(strucchange)
library(tidyr)
library(dplyr)
library(e1071)
library(tseries)
library(tstools)
library(forecast)
library(strucchange)
library(nonlinearTseries)
library(Kendall)
library(TSA)
library(astsa)
library(seastests)
library(xtable)
library(tscount)
library(MASS)
library(stats)
library(aTSA)
library(nortest)
library(nortsTest)
library(readxl)
library(reshape2)
#####################Data Reading##############################

haiti<-read.table("Haiti.txt", header=T)
View(haiti)

##### Summary of the incidence####### 
a <- summary(haiti$`Monthly_cases`)
a
var(haiti$Monthly_cases)
skewness(haiti$Monthly_cases)
kurtosis(haiti$Monthly_cases)


########### Testing for the nature of the data set ############
haiti_cases <- ts(haiti$`Monthly_cases`, start = c(2010,11), end = c(2016,3), frequency = 12)
haiti_death <- ts(haiti$`Monthly_deaths`, start = c(2010,11), end = c(2016,3), frequency = 12)

pdf('cases_hai.pdf')

plot.ts(haiti_cases,                         # Draw first time series,
        type = "l",
        col = "red", pch = 8,
        xlab = "Year",
        ylab = "Incidence", main= "Cholera Cases in Haiti")

grid()
dev.off()

pdf('death_hai.pdf')
plot.ts(haiti_death,                         # Draw first time series,
        type = "l",
        col = "red", pch = 8,
        xlab = "Year",
        ylab = "Incidence", main= "Cholera Death in Haiti")
grid()
dev.off()
#####Trend ######
mk.test(haiti_cases)

##### Randomness###

Box.test(haiti_cases, type="Ljung-Box")

####### Linerartiy #########

nonlinearityTest(haiti_cases, verbose = TRUE)
keenanTest(haiti_cases)
Keenan.test(haiti_cases)

###### Sturctural Break #########

BB6 <- haiti$`Monthly_cases`; IBB6<-lag(BB6); IIBB6<-lag(IBB6); sctest(BB6~1, type="Chow", 12)

###### Seasonality #######
kw(haiti_cases)


####### Checking for Conditional Heteroscedasticity. #######

Lm.test(haiti_cases,alpha = 0.05)



###### Test for stationarity #######
adf.test(haiti_cases)

#pdf('cases_hait.pdf')
plot(haiti_cases, xlab = "Year",
     ylab = "Incidence", main= "Haiti")
#dev.off()

#plot(decompose(haiti_cases, type = "multiplicative"))

#pdf('haiti_auto.pdf')
pdf("Auto_hai.pdf")
ggacf(haiti_cases, title = "Autocorrelation")
dev.off()

pdf("Partial_hait.pdf")
ggpacf(haiti_cases,title = "Partial Autocorrelation")
dev.off()

###### INGARCH Model $$$$$$

haiti_xreg <- cbind(haiti$Rainfall,haiti$Temperature,haiti$Rainfall_lag1,haiti$Temperature_lag1)
haiti_ingarch <- tsglm(haiti$Monthly_cases, model=list(past_obs = 1:7,past_mean = 7), xreg=haiti_xreg, dist="nbinom", link="log"); summary(haiti_ingarch)

res <- residuals(haiti_ingarch)
pdf("residual.pdf")
ggacf(res, title = "Residual Autocorrelation")
dev.off()

Box.test(res, type = "Ljung-Box")

haiti_ARIMA<-auto.arima(haiti_cases, xreg=haiti_xreg); summary(haiti_ARIMA)
haiti_models <- cbind(haiti_cases, haiti_ingarch$fitted, haiti_ARIMA$fitted)


ab <- as.data.frame(haiti_models)

pdf("models_hai.pdf")
gg_hait <- autoplot(haiti_models) 

gg_hait + ggtitle("Incidence of Cholera") + 
  labs(x = "Year", y = "Incidence") + 
  labs(colour = "Lengend") + 
  scale_color_manual(labels = c("Observed", "INGARCH","ARIMA"), values = c(1, 3,6))+
  theme(
    plot.title = element_text(color="black", size=12, face="bold"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold")) +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

png("pi.png")
pit(haiti_ingarch)
dev.off()

predict(haiti_ingarch, n.ahead=5)


############################Finding Error####################################

##################INGARCH#########################3

rmse(haiti_cases, haiti_ingarch$fitted.values)

mape(haiti_cases, haiti_ingarch$fitted.values)

mae(haiti_cases, haiti_ingarch$fitted.values)

mse(haiti_cases, haiti_ingarch$fitted.values)



rmse(haiti_cases, haiti_ARIMA$fitted)

mape(haiti_cases, haiti_ARIMA$fitted)

mae(haiti_cases, haiti_ARIMA$fitted)

mse(haiti_cases, haiti_ARIMA$fitted)










ab %>% 
  rename(
    haiti_cases = Observed,
    haiti_ingarch$fitted = Ingarch,
    haiti_ARIMA$fitted = Arimano
  )


meltdf <- melt(ab,id="Year")
ggplot(meltdf,aes(x=haiti$Month,y=value,colour=variable,group=variable)) + geom_line()




#View(ab)
#pdf("trials.pdf")
plot(ab$haiti_cases, type = "l", col = "red")
lines(ab$`haiti_ingarch$fitted`,type = "l", col = "blue")
lines(ab$`haiti_ARIMA$fitted`,type = "l")
legend("topright",                           # Add legend to plot
       c("Observed", "INGARCH", "ARIMA"),
       lty = 1,
       col = c('red','blue','black'))
#dev.off()



nn <- as.data.frame(haiti_ingarch$fitted.values)
View(nn)

max(haiti_cases)
max(nn)
dim(as.data.frame(haiti_cases))
dim(nn)

plot(haiti_ingarch$fitted.values,type = "l")

#pdf('haiti_models.pdf')
plot.ts(haiti_cases,                         # Draw first time series,
  type = "l",
  col = "red", pch = 8,
  xlab = "Year",
  ylab = "Incidence", main= "Plot of the Incidence ")
lines(
  nn$`haiti_ingarch$fitted.values`,# Draw second time series
  type = "l",
  col = 'blue',pch = 8)
lines(                             # Draw third time series
  haiti_ARIMA$fitted,
  type = "l",
  col = 'black',pch = 8)
legend("topleft",                           # Add legend to plot
       c("Observed", "INGARCH", "ARIMA"),
       lty = 1,
       col = c('red','blue','black'))

grid()
#dev.off()


meltdf <- melt(haiti_models,id="haiti$Month")
ggplot(meltdf,aes(x=haiti$Month,y=value)) + geom_line()

