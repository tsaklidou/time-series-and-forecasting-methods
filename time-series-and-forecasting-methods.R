getwd()
setwd("C:\\Users\\sofia\\Documents\\AUEB\\Time Series\\Assignment")

#=========================================================================#

library("openxlsx")
library("readxl")
library("urca")
library("fGarch")
library("dplyr")


#=========================================================================#

#read data
data = read_excel("JP_MORGAN_US_FUNDS.xlsx",  sheet = 1, col_names = TRUE)

#set dependent variable
y = data$OSGIX[22:385]
y = as.numeric(y)


#make OSGIX as time series
y = ts(y, frequency = 12, start = c(1989 , 04),end=c(2019,07))
y



#plot of OSGIX
plot(y,, col='red', lwd=2, main="Time Series plot of Monthly Returns of OSGIX JPMorgan Mid Cap Growth A", ylab="Monthly Returns", xaxt='n')
axis(side = 1, at = as.numeric(floor(as.yearmon(time(y)))),cex.axis=1)
abline(v = as.numeric(floor(as.yearmon(time(y)))),lty = 2,col="lightblue")


# histogram
hist(y, col='grey',main='Histogram of OSGIX JPMorgan Mid Cap Growth A', xlab='OSGIX')


# normal Q-Q plot
qqnorm(y,main="Normal QQplot of OSGIX JPMorgan Mid Cap Growth A")
qqline(y)


# normality Test
shapiro.test(y)



# perform ADF test
m = ar(y)
m1=ur.df(y,type="drift",lags=m$order-1)
m1
summary(m1)


###########################################################################
###                       IDENTIFICATION STEP                           ###   
###########################################################################


#autocorrelation function plot
acf(ts(y,frequency = 1), 50, main="ACF of OSGIX JPMorgan Mid Cap Growth A", xaxt = 'no') 
axis(side = 1, at = seq(0, 51, by=2), lty = 1, lwd = 1)

#partial autocorrelation function 
pacf(ts(y,frequency = 1), 50, main="PACF of OSGIX JPMorgan Mid Cap Growth A", xaxt = 'no')
axis(side = 1, at = seq(0, 51, by=2), lty = 1, lwd = 1)


# box test
Box.test(y,50,type="Box-Pierce")
Box.test(y,50,type="Ljung-Box")


###########################################################################
###                        ESTIMATION STEP                              ###   
###########################################################################

model_fit=arima(y,order=c(0,0,0)) 
model_fit

residuals_model=model_fit$residuals
residuals_model
residuals=ts(residuals_model, frequency=12, start = c(1989 , 04),end=c(2019,07))
residuals

###########################################################################
###                       DIAGNOSTIC PLOTS                              ###   
###########################################################################

acf(ts(residuals,freq=1), 50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
pacf(ts(residuals,freq=1), 50, main="PACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)

acf(ts(residuals^2,freq=1), 50, main="ACF of squared residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
pacf(ts(residuals^2,freq=1), 50, main="PACF of squared residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)

qqnorm(residuals,main="Normal QQplot of residuals")
qqline(residuals)

# box test to check squared residuals for
Box.test(residuals^2,50,type="Ljung-Box")


###########################################################################
###                      Developing appropriate model                   ###   
###########################################################################
arch1 = garchFit(~garch(1,0),data =y,trace=F)
summary(arch1) 

garch11=garchFit(~garch(1,1),data=y,trace=F) 
summary(garch11)


m2garchst=garchFit(~garch(1,1),data=y, cond.dist="std",trace=F)
summary(m2garchst)
plot(m2garchst)



###########################################################################
###                          Regression analysis                        ###   
###########################################################################
X= read_excel("JP_MORGAN_US_FUNDS.xlsx", sheet = 2, col_names = TRUE)

x1 = x$`Mkt-RF`
x1 = as.numeric(x1)/100
x1 = x1[21:384]

x2 = x$SMB
x2 = as.numeric(x2)100
x2 = x2[21:384]

x3 = x$HML
x3 = as.numeric(x3)/100
x3 = x3[21:384]

x4 = x$RMW
x4 = as.numeric(x4)/100
x4 = x4[21:384]

x5 = x$CMA
x5 = as.numeric(x5)/100
x5 = x5[21:384]

x6 = x$MOM
x6 = as.numeric(x6)/100
x6 = x6[21:384]


#scatterplot of all variables
pairs(cbind(y,x1,x2,x3,x4,x5,x6))

#correlation coefficients and p-values
rcorr(as.matrix(cbind(y,x1,x2,x3,x4,x5,x6)))


fit_ <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6)
summary(fit_)


#stepwise Selection method
fitnull<-lm(y ~ 1)
stepwise<-step(fitnull, 
             scope=list(lower = ~ 1, upper = ~ x1 + x2 + x3 + x4 + x5 + x6),
             direction="both")

stepwise$anova
stepwise



#regrassion model and garch
X_ind<-matrix(cbind(x1,x2,x3,x4,x5,x6),ncol=6)
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)), 
                   mean.model = list(armaOrder=c(0,0), include.mean = TRUE, external.regressors = X_ind), distribution.model = "norm")
spec
model_res <- ugarchfit(spec = spec, data = y)
model_res

model_res@fit$matcoef
infocriteria(model_res)


#normality testing on the Residuals
qqnorm(residuals(model_res),main="Normal QQplot of uGarch model Residuals")  # normality violations in both tails
qqline(residuals(model_res)) 


jarque.bera.test(residuals(model_res)) 


###########################################################################
###                AIC and BIC information criteria                     ###   
###########################################################################

###exclude x3

X3<-matrix(cbind(x1,x2,x4,x5,x6),ncol=5)
spec3 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)),  mean.model = list(armaOrder=c(0,0), include.mean = TRUE, external.regressors = X3),
                    distribution.model = "std")
spec3

model_res3 <- ugarchfit(spec = spec3, data = y)
model_res3

###exclude x4

X4<-matrix(cbind(x1,x2,x3,x5,x6),ncol=5)
spec4 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)),  mean.model = list(armaOrder=c(0,0), include.mean = TRUE, external.regressors = X4),
                    distribution.model = "std")
spec4

model_res4 <- ugarchfit(spec = spec4, data = y)
model_res4

### exclude x4, x3
X34 <-matrix(cbind(x1,x2,x5,x6),ncol=4)
spec34 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0), include.mean = TRUE, external.regressors = X34),
                    distribution.model = "std")
spec34


spec34 <- ugarchfit(spec = spec34, data = y)
spec34




