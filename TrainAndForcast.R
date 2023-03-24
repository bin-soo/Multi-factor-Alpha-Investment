
setwd("D:/上海科技大学/量化投资/Quantitative Investing Term Project/代码及数据文件更新版/")

data<-read.csv("SSEC_data.csv",header=T)

source('Data_Cleaning_Treatment_Functions.R')


x=data


summary(x$S2P)
summary(x$B2P)
summary(x$E2P)
summary(x$CFO2EV)
summary(x$ROE)
summary(x$annualROE)
summary(x$EBITDA2EV)

######## signals treatment
signals.name <- c('B2P', 'E2P', 'S2P', 'CFO2EV', 'annualROE', 'EBITDA2EV')

signals.num = length(signals.name)

##retf1的i到83
nicecount=0
equalAndOurWeightReturnArray=array(dim=c(83,2))
for(i in 1:83)
{
  split=29500+i*500
  print(i)
  x_train=x[1:split,]
  x_test=x[1:(split+500),]
  #清理train和test
  for(j in 1:signals.num)
  {
    x_train = dataMiss(x_train,signals.name[j],0.3)
    x_train = dataTruncate(x_train,signals.name[j])
    x_train = dataStandardize(x_train,signals.name[j])
    x_train = dataWinsorize(x_train,signals.name[j]) # This is done after the standarization
    x_train = dataNeutralize(x_train,signals.name[j], 'sectorName')
    x_train = dataStandardize(x_train,signals.name[j])
    x_train = dataReplaceMissing(x_train,signals.name[j],miss.fill = "mean")
    x_train = dataDistribution(x_train,signals.name[j],1)
  }
  x_train<-x_train[order(x_train[,2],x_train[,3]),]
  
  for(j in 1:signals.num)
  {
    x_test = dataMiss(x_test,signals.name[j],0.3)
    x_test = dataTruncate(x_test,signals.name[j])
    x_test = dataStandardize(x_test,signals.name[j])
    x_test = dataWinsorize(x_test,signals.name[j]) # This is done after the standarization
    x_test = dataNeutralize(x_test,signals.name[j], 'sectorName')
    x_test = dataStandardize(x_test,signals.name[j])
    x_test = dataReplaceMissing(x_test,signals.name[j],miss.fill = "mean")
    x_test = dataDistribution(x_test,signals.name[j],1)
  }
  x_test<-x_test[order(x_test[,2],x_test[,3]),]
  
  #利用train计算ALPHA模型
  #reft1:
  weight1<-array(dim = c(1, 10))
  x_train[!is.na(x_train$retf1),]
  fit <- lm(retf1~B2P+E2P+S2P+CFO2EV,data=x_train[!is.na(x_train$retf1),])
  summary(fit)
  #install.packages("lmtest")
  library(lmtest)
  print(bptest(fit))
  fit_wls<- lm(retf1~B2P+E2P+S2P+CFO2EV,data=x_train[!is.na(x_train$retf1),],weights=1/abs(fit$residuals))
  dataweight(fit_wls)
  weight1[1,1:4]<-dataweight(fit_wls)
  x_train$VALUE = weight1[1,1]*x_train$B2P + weight1[1,2]*x_train$E2P + weight1[1,3]*x_train$S2P + weight1[1,4]*x_train$CFO2EV
  fit <- lm(retf1~annualROE+EBITDA2EV,data=x_train[!is.na(x_train$retf1),])
  print(bptest(fit))
  fit_wls<-lm(retf1~annualROE+EBITDA2EV,data=x_train[!is.na(x_train$retf1),],weights=1/abs(fit$residuals))
  weight1[1,5:6]<-dataweight(fit_wls)
  x_train$PROF = weight1[1,5]*x_train$annualROE + weight1[1,6]*x_train$EBITDA2EV
  themename <- c('VALUE', 'PROF')
  themenum = length(themename)
  for(j in 1: themenum)
  {
    x_train = dataStandardize(x_train,themename[j])
  }
  retvarname <- c('retf1', 'retf3', 'retf6', 'retf9', 'retf12')
  fit <- lm(PROF~VALUE,data=x_train)
  summary(fit)
  x_train$PROF.resid = fit$residuals
  fit <- lm(retf1 ~ VALUE + PROF.resid, data = x_train[!is.na(x_train$retf1),])
  print(bptest(fit))
  fit_wls<-lm(retf1 ~ VALUE + PROF.resid, data = x_train[!is.na(x_train$retf1),],weights=1/abs(fit$residuals))
  weight1[1,7:8]<-dataweight(fit_wls)
  x_train$ALPHA1 = weight1[1,7]*x_train$VALUE + weight1[1,8]*x_train$PROF
  fit <- lm(retf1 ~ ALPHA1, data = x_train[!is.na(x_train$retf1),])
  print(bptest(fit))
  fit_wls<-lm(retf1 ~ ALPHA1, data = x_train[!is.na(x_train$retf1),],weights=1/abs(fit$residuals))
  weight1[1,9]<-fit_wls$coefficients[2]
  weight1[1,10]<-fit_wls$coefficients[1]
  summary(fit)
  #预测
  x_test$ALPHA1=weight1[1,9]*(weight1[1,7]*(weight1[1,1]*x_test$B2P+weight1[1,2]*x_test$E2P+weight1[1,3]*x_test$S2P+weight1[1,4]*x_test$CFO2EV)
  +weight1[1,8]*(weight1[1,5]*x_test$annualROE+weight1[1,6]*x_test$EBITDA2EV))+weight1[1,10]
  
  x_test$ALPHA1[split+500]
  
  #提取数据计算协方差
  retf1<-read.csv("retf1.csv",header=T)
  select_stock<-read.csv("selection.csv",header=T)
  stock_num=10
  select_stock[1:stock_num,1]
  stock=list("X600381.SH","X600446.SH", "X600155.SH", "X600228.SH", "X600330.SH", "X600192.SH", "X600566.SH", "X600200.SH", "X600539.SH", "X600398.SH")
  #retf1$"X600000.SH"
  retf1_select<-read.csv("retf1_select.csv",header=T)
  #dataCovariance(retf1$X600600.SH,retf1$X600601.SH,2,61)
  Covariance <- array(dim = c(stock_num,stock_num))
  for (j in 1:stock_num)
  {
    for (k in 1:stock_num)
    {
      Covariance[j,k] = cov(retf1_select[2:(60+i),(j+1)],retf1_select[2:(60+i),(k+1)])
      #retf1_select[1:61,3]
      #i=1
      #j=k=2
      #cov(retf1_select[2:(60+i),(j+1)],retf1_select[2:(60+i),(k+1)])
      #cov(retf1_select[2:61,3],retf1_select[2:61,3])
    }
  }
  Covariance
  print(Covariance)
  
  #优化
  yyyymm=x_test$yyyymm[split+500]
  Forecast_return<-array(dim=c(1,10))
  stock=list("600381.SH","600446.SH", "600155.SH", "600228.SH", "600330.SH", "600192.SH", "600566.SH", "600200.SH", "600539.SH", "600398.SH")
  for(j in 1:10)
  {
    #yyyymm
    #stock[j]
    #x_test[x_test$yyyymm==yyyymm & x_test$Codes==stock[j],27]
    Forecast_return[1,j] = x_test[x_test$yyyymm==yyyymm & x_test$Codes==stock[j],27]
  }
  Forecast_return
  print(Forecast_return)
  alpha_target=-0.5
  
  portfolio_weight=dataOptimize1(Covariance, Forecast_return, alpha_target)$solution
  print(dataOptimize1(Covariance, Forecast_return, alpha_target))
  
  ##与equal weight比较
  Real_return<-array(dim=c(1,10))
  yyyymm=x$yyyymm[split+501]
  Real_return= retf1_select[retf1_select$yyyymm==yyyymm,2:11]
  Real_return
  sum(Real_return)
  equal_weight_return=sum(Real_return)/10
  our_weight_return=0
  for(j in 1:10)
  {
    our_weight_return=our_weight_return+portfolio_weight[j]*Real_return[j]
  }
  our_weight_return=as.numeric(our_weight_return)
  print(equal_weight_return)
  print(our_weight_return)
  equalAndOurWeightReturnArray[i,1]=equal_weight_return
  equalAndOurWeightReturnArray[i,2]=our_weight_return
  if (equal_weight_return<our_weight_return)
  {
    print("nice forcast!!")
    nicecount=nicecount+1
  }
  
}
print(nicecount)
write.csv(equalAndOurWeightReturnArray,"performance.csv")
summary(equalAndOurWeightReturnArray)
