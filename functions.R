
data<-read.csv("SSEC_data.csv",header=T)

source('Data_Cleaning_Treatment_Functions.R')


x=data


summary(x$B2P)
summary(x$E2P)
summary(x$S2P)
summary(x$CFO2EV)
summary(x$ROE)
summary(x$annualROE)
summary(x$EBITDA2EV)

######## signals treatment
signals.name <- c('B2P', 'E2P', 'S2P', 'CFO2EV', 'annualROE', 'EBITDA2EV')
# 'ROE',

signals.num = length(signals.name)

for(i in 1:signals.num)
{
  x = dataMiss(x,signals.name[i],0.3)
  x = dataTruncate(x,signals.name[i])
  x = dataStandardize(x,signals.name[i])
  x = dataWinsorize(x,signals.name[i]) # This is done after the standarization
  x = dataNeutralize(x,signals.name[i], 'sectorName')
  x = dataStandardize(x,signals.name[i])
  x = dataReplaceMissing(x,signals.name[i],miss.fill = "mean")
  x = dataDistribution(x,signals.name[i],1)
}

summary(x$B2P)
summary(x$E2P)
summary(x$S2P)
summary(x$CFO2EV)
summary(x$annualROE)
summary(x$EBITDA2EV)

######## the correlation between B2P, E2P, S2P, CFO2EV
corvarname <- c('B2P', 'E2P', 'S2P', 'CFO2EV', 'annualROE', 'EBITDA2EV')
dataselfcorrelation(x, corvarname)


######## the correlation between retf1, retf3, retf6, retf9 and B2P, E2P, S2P, CFO2EV
acovvarname <- c('retf1', 'retf3', 'retf6', 'retf9','retf12')
bcovvarname <- c('B2P', 'E2P', 'S2P', 'CFO2EV', 'annualROE', 'EBITDA2EV')

datacorrelation(x, acovvarname, bcovvarname)

fit <- lm(retf9~CFO2EV,data=x)
summary(fit)

x<-x[order(x[,2],x[,3]),]
x2014<-x[1:24000,]
x2015<-x[1:30000,]

#reft1:
fit <- lm(retf1~B2P+E2P+S2P+CFO2EV,data=x2015)
summary(fit)

weight<-dataweight(fit)

x2015$VALUE = weight[1,1]*x2015$B2P + weight[1,2]*x2015$E2P + weight[1,3]*x2015$S2P + weight[1,4]*x2015$CFO2EV

fit <- lm(retf1~annualROE+EBITDA2EV,data=x2015)
summary(fit)

weight<-dataweight(fit)

x2015$PROF = weight[1,1]*x2015$annualROE + weight[1,2]*x2015$EBITDA2EV

themename <- c('VALUE', 'PROF')

themenum = length(themename)

for(i in 1: themenum)
{
  x2015 = dataStandardize(x2015,themename[i])
}

summary(x2015[,themename])

retvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
datacorrelation(x2015, retvarname, themename)

dataselfcorrelation(x2015, themename)

fit <- lm(PROF~VALUE,data=x2015)
summary(fit)

x2015$PROF.resid = fit$residuals

fit <- lm(retf1 ~ VALUE + PROF.resid, data = x2015)
summary(fit)

weight<-dataweight(fit)
x2015$ALPHA1 = weight[1,1]*x2015$VALUE + weight[1,2]*x2015$PROF

summary(x2015$ALPHA1)

acorvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
bcorvarname <- c('ALPHA1')

datacorrelation(x2015, acorvarname, bcorvarname)

fit <- lm(retf1 ~ ALPHA1, data = x2015)
summary(fit)

###reft3:
fit <- lm(retf3~B2P+E2P+S2P+CFO2EV,data=x2015)
summary(fit)

weight<-dataweight(fit)

x2015$VALUE = weight[1,1]*x2015$B2P + weight[1,2]*x2015$E2P + weight[1,3]*x2015$S2P + weight[1,4]*x2015$CFO2EV

fit <- lm(retf3~annualROE+EBITDA2EV,data=x2015)
summary(fit)

weight<-dataweight(fit)

x2015$PROF = weight[1,1]*x2015$annualROE + weight[1,2]*x2015$EBITDA2EV

themename <- c('VALUE', 'PROF')

themenum = length(themename)

for(i in 1: themenum)
{
  x2015 = dataStandardize(x2015,themename[i])
}

summary(x2015[,themename])

retvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
datacorrelation(x2015, retvarname, themename)

dataselfcorrelation(x2015, themename)

fit <- lm(PROF~VALUE,data=x2015)
summary(fit)

x2015$PROF.resid = fit$residuals

fit <- lm(retf3 ~ VALUE + PROF.resid, data = x2015)
summary(fit)

weight<-dataweight(fit)
x2015$ALPHA3 = weight[1,1]*x2015$VALUE + weight[1,2]*x2015$PROF

summary(x2015$ALPHA3)

acorvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
bcorvarname <- c('ALPHA3')

datacorrelation(x2015, acorvarname, bcorvarname)

fit <- lm(retf3 ~ ALPHA3, data = x2015)
summary(fit)

######reft6:
fit <- lm(retf6~B2P+E2P+S2P+CFO2EV,data=x2015)
summary(fit)

weight<-dataweight(fit)

x2015$VALUE = weight[1,1]*x2015$B2P + weight[1,2]*x2015$E2P + weight[1,3]*x2015$S2P + weight[1,4]*x2015$CFO2EV

fit <- lm(retf6~annualROE+EBITDA2EV,data=x2015)
summary(fit)

weight<-dataweight(fit)

x2015$PROF = weight[1,1]*x2015$annualROE + weight[1,2]*x2015$EBITDA2EV

themename <- c('VALUE', 'PROF')

themenum = length(themename)

for(i in 1: themenum)
{
  x2015 = dataStandardize(x2015,themename[i])
}

summary(x2015[,themename])

retvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
datacorrelation(x2015, retvarname, themename)

dataselfcorrelation(x2015, themename)

fit <- lm(PROF~VALUE,data=x2015)
summary(fit)

x2015$PROF.resid = fit$residuals

fit <- lm(retf6 ~ VALUE + PROF.resid, data = x2015)
summary(fit)

weight<-dataweight(fit)
x2015$ALPHA6 = weight[1,1]*x2015$VALUE + weight[1,2]*x2015$PROF

summary(x2015$ALPHA6)

acorvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
bcorvarname <- c('ALPHA6')

datacorrelation(x2015, acorvarname, bcorvarname)

fit <- lm(retf6 ~ ALPHA6, data = x2015)
summary(fit)

#########retf9:
######## derive VALUE weights
fit <- lm(retf9~B2P+E2P+S2P+CFO2EV,data=x2015)
summary(fit)

weight<-dataweight(fit)

x2015$VALUE = weight[1,1]*x2015$B2P + weight[1,2]*x2015$E2P + weight[1,3]*x2015$S2P + weight[1,4]*x2015$CFO2EV


######## derive PROF weights
fit <- lm(retf9~annualROE+EBITDA2EV,data=x2015)
summary(fit)

weight<-dataweight(fit)

x2015$PROF = weight[1,1]*x2015$annualROE + weight[1,2]*x2015$EBITDA2EV


######## re-standardization the theme
themename <- c('VALUE', 'PROF')

themenum = length(themename)

for(i in 1: themenum)
{
  x2015 = dataStandardize(x2015,themename[i])
}


######## summary the theme
summary(x2015[,themename])


######## the Pearson and rank correlation between the theme and return
retvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
datacorrelation(x2015, retvarname, themename)



######## the Pearson and rank correlation of the theme
dataselfcorrelation(x2015, themename)


######## run OLS regressions of PROF on VALUE
fit <- lm(PROF~VALUE,data=x2015)
summary(fit)

x2015$PROF.resid = fit$residuals


######## derive ALPHA weights
fit <- lm(retf9 ~ VALUE + PROF.resid, data = x2015)
#fit <- lm(retf9 ~ VALUE + PROF, data = x)
summary(fit)

weight<-dataweight(fit)
x2015$ALPHA9 = weight[1,1]*x2015$VALUE + weight[1,2]*x2015$PROF



######## summary the ALPHA
summary(x2015$ALPHA9)


######## correlations with forward returns
acorvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
bcorvarname <- c('ALPHA9')

datacorrelation(x2015, acorvarname, bcorvarname)



######## OLS regression model for ALPHA scores.
fit <- lm(retf9 ~ ALPHA9, data = x2015)
summary(fit)

############reft12:
fit <- lm(retf12~B2P+E2P+S2P+CFO2EV,data=x2015)
summary(fit)

weight<-dataweight(fit)

x2015$VALUE = weight[1,1]*x2015$B2P + weight[1,2]*x2015$E2P + weight[1,3]*x2015$S2P + weight[1,4]*x2015$CFO2EV

fit <- lm(retf12~annualROE+EBITDA2EV,data=x2015)
summary(fit)

weight<-dataweight(fit)

x2015$PROF = weight[1,1]*x2015$annualROE + weight[1,2]*x2015$EBITDA2EV

themename <- c('VALUE', 'PROF')

themenum = length(themename)

for(i in 1: themenum)
{
  x2015 = dataStandardize(x2015,themename[i])
}

summary(x2015[,themename])

retvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
datacorrelation(x2015, retvarname, themename)

dataselfcorrelation(x2015, themename)

fit <- lm(PROF~VALUE,data=x2015)
summary(fit)

x2015$PROF.resid = fit$residuals

fit <- lm(retf12 ~ VALUE + PROF.resid, data = x2015)
summary(fit)

weight<-dataweight(fit)
x2015$ALPHA12 = weight[1,1]*x2015$VALUE + weight[1,2]*x2015$PROF

summary(x2015$ALPHA12)

acorvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
bcorvarname <- c('ALPHA12')

datacorrelation(x2015, acorvarname, bcorvarname)

fit <- lm(retf12 ~ ALPHA12, data = x2015)
summary(fit)

####开始选股:
retf9s <-read.csv("retf91to4.csv",header=T)
alpha9s <-read.csv('ALPHA91to4.csv',header=T)
retf9s <- retf9s[2:495]
alpha9s <- alpha9s[2:495]
cr9s <- colnames(retf9s)

racor <- alphacorrelation(retf9s, alpha9s, cr9s)
alpha_mean = apply(alpha9s,2,mean)
a9mean <- array(dim = c(length(cr9s),1),dimnames = list(cr9s))
for(j in 1:length(cr9s))
{
  a9mean[j,1] = alpha_mean[j]
}
ret_mean = apply(retf9s,2,mean)
r9mean <- array(dim = c(length(cr9s),1),dimnames = list(cr9s))
for(j in 1:length(cr9s))
{
  r9mean[j,1] = ret_mean[j]
}

racor <- scale(racor)
a9mean <- scale(a9mean)
r9mean <- scale(r9mean)
score = 0.4*racor[,1]+0.3*a9mean+0.3*r9mean
selection <- cbind(racor[,1],a9mean,r9mean,score)
selection = selection[order(selection[,4],decreasing = TRUE),]
write.csv(selection,'selection.csv')





####开始选股:
fit <- lm(retf1~B2P+E2P+S2P+CFO2EV,data=x2014)
#summary(fit)
weight<-array(dim = c(1, 8))
weight[1,1:4]<-dataweight(fit)
#weight<-dataweight(fit)

x2014$VALUE = weight[1,1]*x2014$B2P + weight[1,2]*x2014$E2P + weight[1,3]*x2014$S2P + weight[1,4]*x2014$CFO2EV

fit <- lm(retf1~annualROE+EBITDA2EV,data=x2014)
summary(fit)

weight[1,5:6]<-dataweight(fit)

x2014$PROF = weight[1,5]*x2014$annualROE + weight[1,6]*x2014$EBITDA2EV

themename <- c('VALUE', 'PROF')

themenum = length(themename)

for(i in 1: themenum)
{
  x2014 = dataStandardize(x2014,themename[i])
}

summary(x2014[,themename])

retvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
#datacorrelation(x2014, retvarname, themename)

#dataselfcorrelation(x2014, themename)

fit <- lm(PROF~VALUE,data=x2014)
#summary(fit)

x2014$PROF.resid = fit$residuals

fit <- lm(retf1 ~ VALUE + PROF.resid, data = x2014)
#summary(fit)

weight[1,7:8]<-dataweight(fit)
x2014$ALPHA1 = weight[1,7]*x2014$VALUE + weight[1,8]*x2014$PROF

#summary(x2014$ALPHA1)

#acorvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
#bcorvarname <- c('ALPHA1')

#datacorrelation(x2014, acorvarname, bcorvarname)

#fit <- lm(retf1 ~ ALPHA1, data = x2014)
#summary(fit)

Forecast_return <- array(dim = c(12, 500))
for(i in 1:500)
{
  for(j in 1:12)
    Forecast_return[i,j]=weight[1,7]#*x2015$
}




#x$ALPHA9<-x2015$ALPHA
#write.csv(x, file = "temp.csv", row.names = FALSE)

mode(x)
x<-x[order(x[,2],x[,3]),]
x[1,]
for(i in 1:84)
{
  #print(30001+i*500)
  x_temp=x[1:29500+i*500,]
  #reft1:
  fit <- lm(retf1~B2P+E2P+S2P+CFO2EV,data=x_temp)
  #summary(fit)
  weight1<-array(dim = c(1, 8))
  weight1[1,1:4]<-dataweight(fit)
  
  x_temp$VALUE = weight1[1,1]*x_temp$B2P + weight1[1,2]*x_temp$E2P + weight1[1,3]*x_temp$S2P + weight1[1,4]*x_temp$CFO2EV
  
  fit <- lm(retf1~annualROE+EBITDA2EV,data=x_temp)
  #summary(fit)
  
  weight1[1,5:6]<-dataweight(fit)
  
  x_temp$PROF = weight1[1,5]*x_temp$annualROE + weight1[1,6]*x_temp$EBITDA2EV
  
  themename <- c('VALUE', 'PROF')
  
  themenum = length(themename)
  
  for(i in 1: themenum)
  {
    x_temp = dataStandardize(x_temp,themename[i])
  }
  
  #summary(x_temp[,themename])
  
  retvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
  #datacorrelation(x_temp, retvarname, themename)
  
  #dataselfcorrelation(x_temp, themename)
  
  fit <- lm(PROF~VALUE,data=x_temp)
  #summary(fit)
  
  x_temp$PROF.resid = fit$residuals
  
  fit <- lm(retf1 ~ VALUE + PROF.resid, data = x_temp)
  #summary(fit)
  
  weight1[1,7:8]<-dataweight(fit)
  x_temp$ALPHA1 = weight1[1,7]*x_temp$VALUE + weight1[1,8]*x_temp$PROF
  
  #summary(x_temp$ALPHA1)
  
  #acorvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
  #bcorvarname <- c('ALPHA1')
  
  #datacorrelation(x_temp, acorvarname, bcorvarname)
  
  fit <- lm(retf1 ~ ALPHA1, data = x_temp)
  summary(fit)
  
  ###reft3:
  fit <- lm(retf3~B2P+E2P+S2P+CFO2EV,data=x_temp)
  #summary(fit)
  weight3<-array(dim = c(1, 8))
  weight3[1,1:4]<-dataweight(fit)
  
  x_temp$VALUE = weight3[1,1]*x_temp$B2P + weight3[1,2]*x_temp$E2P + weight3[1,3]*x_temp$S2P + weight3[1,4]*x_temp$CFO2EV
  
  fit <- lm(retf3~annualROE+EBITDA2EV,data=x_temp)
  #summary(fit)
  
  weight3[1,5:6]<-dataweight(fit)
  
  x_temp$PROF = weight3[1,5]*x_temp$annualROE + weight3[1,6]*x_temp$EBITDA2EV
  
  themename <- c('VALUE', 'PROF')
  
  themenum = length(themename)
  
  for(i in 1: themenum)
  {
    x_temp = dataStandardize(x_temp,themename[i])
  }
  
  #summary(x_temp[,themename])
  
  retvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
  #datacorrelation(x_temp, retvarname, themename)
  
  #dataselfcorrelation(x_temp, themename)
  
  fit <- lm(PROF~VALUE,data=x_temp)
  #summary(fit)
  
  x_temp$PROF.resid = fit$residuals
  
  fit <- lm(retf3 ~ VALUE + PROF.resid, data = x_temp)
  #summary(fit)
  
  weight3[1,7:8]<-dataweight(fit)
  x_temp$ALPHA3 = weight3[1,7]*x_temp$VALUE + weight3[1,8]*x_temp$PROF
  
  #summary(x_temp$ALPHA3)
  
  acorvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
  bcorvarname <- c('ALPHA3')
  
  #datacorrelation(x_temp, acorvarname, bcorvarname)
  
  fit <- lm(retf3 ~ ALPHA3, data = x_temp)
  summary(fit)
  
  ######reft6:
  fit <- lm(retf6~B2P+E2P+S2P+CFO2EV,data=x_temp)
  #summary(fit)
  weight6<-array(dim = c(1, 8))
  weight6[1,1:4]<-dataweight(fit)
  
  x_temp$VALUE = weight6[1,1]*x_temp$B2P + weight6[1,2]*x_temp$E2P + weight6[1,3]*x_temp$S2P + weight6[1,4]*x_temp$CFO2EV
  
  fit <- lm(retf6~annualROE+EBITDA2EV,data=x_temp)
  #summary(fit)
  
  weight6[1,5:6]<-dataweight(fit)
  
  x_temp$PROF = weight6[1,5]*x_temp$annualROE + weight6[1,6]*x_temp$EBITDA2EV
  
  themename <- c('VALUE', 'PROF')
  
  themenum = length(themename)
  
  for(i in 1: themenum)
  {
    x_temp = dataStandardize(x_temp,themename[i])
  }
  
  #summary(x_temp[,themename])
  
  retvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
  #datacorrelation(x_temp, retvarname, themename)
  
  #dataselfcorrelation(x_temp, themename)
  
  fit <- lm(PROF~VALUE,data=x_temp)
  #summary(fit)
  
  x_temp$PROF.resid = fit$residuals
  
  fit <- lm(retf6 ~ VALUE + PROF.resid, data = x_temp)
  #summary(fit)
  
  weight6[1,7:8]<-dataweight(fit)
  x_temp$ALPHA6 = weight6[1,7]*x_temp$VALUE + weight6[1,8]*x_temp$PROF
  
  #summary(x_temp$ALPHA6)
  
  acorvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
  bcorvarname <- c('ALPHA6')
  
  #datacorrelation(x_temp, acorvarname, bcorvarname)
  
  fit <- lm(retf6 ~ ALPHA6, data = x_temp)
  summary(fit)
  
  #########retf9:
  fit <- lm(retf9~B2P+E2P+S2P+CFO2EV,data=x_temp)
  #summary(fit)
  weight9<-array(dim = c(1, 8))
  weight9[1,1:4]<-dataweight(fit)
  
  x_temp$VALUE = weight9[1,1]*x_temp$B2P + weight9[1,2]*x_temp$E2P + weight9[1,3]*x_temp$S2P + weight9[1,4]*x_temp$CFO2EV
  
  fit <- lm(retf9~annualROE+EBITDA2EV,data=x_temp)
  #summary(fit)
  
  weight9[1,5:6]<-dataweight(fit)
  
  x_temp$PROF = weight9[1,5]*x_temp$annualROE + weight9[1,6]*x_temp$EBITDA2EV
  themename <- c('VALUE', 'PROF')
  
  themenum = length(themename)
  
  for(i in 1: themenum)
  {
    x_temp = dataStandardize(x_temp,themename[i])
  }
  #summary(x_temp[,themename])
  retvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
  #datacorrelation(x_temp, retvarname, themename)
  #dataselfcorrelation(x_temp, themename)
  fit <- lm(PROF~VALUE,data=x_temp)
  #summary(fit)
  
  x_temp$PROF.resid = fit$residuals
  fit <- lm(retf9 ~ VALUE + PROF.resid, data = x_temp)
  #summary(fit)
  
  weight9[1,7:8]<-dataweight(fit)
  x_temp$ALPHA9 = weight9[1,7]*x_temp$VALUE + weight9[1,8]*x_temp$PROF
  #summary(x_temp$ALPHA9)
  acorvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
  bcorvarname <- c('ALPHA9')
  
  #datacorrelation(x_temp, acorvarname, bcorvarname)
  fit <- lm(retf9 ~ ALPHA9, data = x_temp)
  #summary(fit)
  
  ############retf12
  fit <- lm(retf12~B2P+E2P+S2P+CFO2EV,data=x_temp)
  #summary(fit)
  weight12<-array(dim = c(1, 8))
  weight12[1,1:4]<-dataweight(fit)
  
  x_temp$VALUE = weight12[1,1]*x_temp$B2P + weight12[1,2]*x_temp$E2P + weight12[1,3]*x_temp$S2P + weight12[1,4]*x_temp$CFO2EV
  
  fit <- lm(retf12~annualROE+EBITDA2EV,data=x_temp)
  #summary(fit)
  
  weight12[1,5:6]<-dataweight(fit)
  
  x_temp$PROF = weight12[1,5]*x_temp$annualROE + weight12[1,6]*x_temp$EBITDA2EV
  
  themename <- c('VALUE', 'PROF')
  
  themenum = length(themename)
  
  for(i in 1: themenum)
  {
    x_temp = dataStandardize(x_temp,themename[i])
  }
  
  #summary(x_temp[,themename])
  
  retvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
  #datacorrelation(x_temp, retvarname, themename)
  
  #dataselfcorrelation(x_temp, themename)
  
  fit <- lm(PROF~VALUE,data=x_temp)
  #summary(fit)
  
  x_temp$PROF.resid = fit$residuals
  
  fit <- lm(retf12 ~ VALUE + PROF.resid, data = x_temp)
  #summary(fit)
  
  weight12[1,7:8]<-dataweight(fit)
  x_temp$ALPHA12 = weight12[1,7]*x_temp$VALUE + weight12[1,8]*x_temp$PROF
  
  #summary(x_temp$ALPHA12)
  
  acorvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
  bcorvarname <- c('ALPHA12')
  
  #datacorrelation(x_temp, acorvarname, bcorvarname)
  
  fit <- lm(retf12 ~ ALPHA12, data = x_temp)
  summary(fit)
  
  ###假设选股为1~10号
  
  
  
}
