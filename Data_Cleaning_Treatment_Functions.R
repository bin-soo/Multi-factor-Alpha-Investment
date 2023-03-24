dataMiss <- function(data,varname,cut.point)
{
  x=data
  cc=which(is.na(x[,varname]))
  if (sum(cc)>0)
  {
    ccc=length(cc)/dim(x)[1]
    x$missPercent=ccc
    if(ccc>cut.point)
    {
      x$missFlag=1 # date flag to avoid the factor
      x[,varname]=0 # assign zero for all names
    }
    else x$missFlag=0
    #cat("Missing:", varname, ":", length(cc), "of",dim(x)[1],"\n")
  }
  else
  {
    x$missPercent=0
    x$missFlag=0
    cat("Missing:", varname, ", no missing obs of ",dim(x)[1],"\n")
  }
  return(x)
}

#############################################################################
#############################dataTruncate####################################
#############################################################################

dataTruncate <- function(data,varname,method="sigma",
                         LtruncPoint=5,RtruncPoint=5)
{
  x=data
  ## deal with the left tail and right tail separately
  if(tolower(method)=="sigma")
  {
    tmp1<-mean(x[,varname],na.rm=T);## na.rm=T£º remove na value
    tmp2<-sd(x[,varname],na.rm=T)
    minx=tmp1-LtruncPoint*tmp2
    maxx=tmp1+RtruncPoint*tmp2
    cc=which(x[,varname]>maxx | x[,varname]<minx)
    if (sum(cc)>0)
    {
      #cat("Truncation: ",varname, "\n")
      x[cc,varname]<-NA
    }
    else cat("Truncation: ",varname," truncated ids = NA","\n")
  }
  else if(tolower(method)=="percentile")
  {
    minx<-as.vector(quantile(x[,varname],LtruncPoint/100,na.rm=T))
    maxx<-as.vector(quantile(x[,varname],(1-RtruncPoint/100),na.rm=T))
    cc=which(x[,varname]>maxx | x[,varname]<minx)
    if(sum(cc)>0)
    {
      cat("Truncation: ",varname, "\n")
      x[cc,varname]<-NA
    }
    else cat("Truncation: ",varname," truncated ids = NA","\n")
  }
  else stop("Please select the correct truncation method! \n\n")
  return(x)
}

#############################################################################
#############################dataStandardize#################################
#############################################################################

dataStandardize <- function(data,varname,method="robust")
{
  # check the name in case of the repeated standar dization
  # robust version
  x = data
  ## If there are too few observations do not use robust
  if(method=="robust" & nrow(x) < 30) {
    method <- "simple"
  }
  if(method=="robust")
  {
    library(rrcov)
    require(rrcov)
    #new.varname <- paste(varname,¡¯.sdz¡¯,sep=¡¯¡¯);
    tmp <- CovSde(x[,varname]) ## Stahel-Donoho Estimates
    x[,varname] <- x[,varname] - as.vector(tmp@center) ##tmp@center: mean
    x[,varname] <- x[,varname] / sqrt(as.vector(tmp@cov)) ##tmp@cov: covariance
  }
  else
  {
    theMean=mean(x[,varname],na.rm=T)
    theStd=sd(x[,varname],na.rm=T)
    if(theStd != 0) x[,varname]=(x[,varname]-theMean)/theStd
  }
  #write.csv(x, file = "G:/OneDrive - shanghaitech.edu.cn/Quantitative Investing/QI/R/Chapter4/dataStandardize.csv", row.names = FALSE)
  return(x)
}

#############################################################################
#############################dataWinsorize###################################
#############################################################################

dataWinsorize <- function(data,varname, Lwin=-3, Rwin=3)
{
  x = data
  #tmp1<-mean(x[,varname],na.rm=T);## na.rm=T£º remove na value
  #tmp2<-sd(x[,varname],na.rm=T)
  #minx=tmp1+Lwin*tmp2
  #maxx=tmp1+Rwin*tmp2
  # deal with the left tail and right tail separately
  gt.index <- which(x[,varname] > Rwin);
  if(length(gt.index) > 0)
  {
    #cat("Winsorization:",varname,"right tail:","\n")
    x[gt.index,varname] <- Rwin
  }
  else cat("Winsorization: ",varname, "right tail = NA","\n")
  lt.index <- which(x[,varname] < Lwin);
  if(length(lt.index) > 0)
  {
    #cat("Winsorization: ",varname,"left tail:","\n")
    x[lt.index,varname] <- Lwin
  }
  else cat("Winsorization: ",varname, "left ids = NA","\n")
  return(x);
}

#############################################################################
#############################dataNeutralize##################################
#############################################################################

dataNeutralize <- function(data,varname, neutral.name)
{
  x=data
  # get the mean for each group
  cc=which(is.na(x[,neutral.name]))
  if(sum(cc)>0) x[cc,neutral.name] = "NA"
  aa=tapply(x[,varname],x[,neutral.name],mean,na.rm=T)
  theMean= data.frame(theMean = as.vector(aa), sss=names(aa))
  names(theMean)[2]=neutral.name
  x=merge(x,theMean,by=neutral.name,all.x=T)
  # get the demeaned score
  x[,varname]=x[,varname] - x$theMean
  mm=match("theMean",names(x))
  x=x[,-mm]
  return(x)
}
#############################################################################
##############################dataExclude####################################
#############################################################################

dataExclude <-function(data, varname, col.exclude, exclude.name, theID="cusip")
{
  x=data
  # find the signal
  ss=match(varname,names(x))
  if(is.na(ss)) stop("varname does not exist in the data!\n")
  # exclude the group
  mm=match(col.exclude,names(x))
  if(is.na(mm)) stop ("col.exclude does not exist in the data!\n\n")
  nex=length(exclude.name)
  for(i in 1:nex)
  {
    cc=which(x[,mm]==exclude.name[i])
    if (sum(cc)>0)
    {
      x[cc,ss]<-'NA'
      #cat("Exclusion: ",varname, "=", exclude.name[i], ": ",x[cc,theID],"\n")
    }
  }
  return(x)
}

#############################################################################
##############################dataDelete####################################
#############################################################################

dataDelete <-function(data, col.exclude, exclude.name, theID="cusip")
{
  x=data
  # find the signal
  #ss=match(varname,names(x))
  #if(is.na(ss)) stop("varname does not exist in the data!\n")
  # exclude the group
  mm=match(col.exclude,names(x))
  if(is.na(mm)) stop ("col.exclude does not exist in the data!\n\n")
  nex=length(exclude.name)
  for(i in 1:nex)
  {
    cc=which(x[,mm]==exclude.name[i])
    if (sum(cc)>0)
    {
      x <- x[-cc,]
      #x[cc,ss]<-'NA'
      #cat("Exclusion: ",varname, "=", exclude.name[i], ": ",x[cc,theID],"\n")
    }
  }
  return(x)
}

#############################################################################
########################dataReplaceMissing###################################
#############################################################################

dataReplaceMissing <- function(data,varname,miss.fill="zero")
{
  x = data
  miss.index <- which(is.na(x[,varname]));
  if(length(miss.index) > 0) {
    if(tolower(miss.fill)=="mean") x[miss.index,varname] <- mean(x[,varname],na.rm=T)
    if(tolower(miss.fill)=="median") x[miss.index,varname]<-median(x[,varname],na.rm=T)
    if(tolower(miss.fill)=="zero") x[miss.index,varname] <- 0
  }
  #write.csv(x, file = "F:/OneDrive - shanghaitech.edu.cn/Quantitative Investing/QI/R/Chapter4/dataReplaceMissing.csv", row.names = FALSE)
  return(x)
}

#############################################################################
##########################dataDistribution###################################
#############################################################################

dataDistribution <- function(data,varname, distrib)
{
  x=data
  if(distrib==1 |distrib==2)
  {
    #x$uniform=rank(x[,varname])/dim(x)[1]
    x$uniform=rank(x[,varname],na.last="keep",ties.method="average")/dim(x)[1]
    mm=match("uniform",names(x))
    names(x)[mm]=paste(varname,".unif",sep="")
  }
  if(distrib==2)
  {
    x$normal=pnorm(x[,mm],0,1)
    mm2=match("normal",names(x))
    names(x)[mm2]=paste(varname,".norm",sep="")
  }
  #write.csv(x, file = "F:/OneDrive - shanghaitech.edu.cn/Quantitative Investing/QI/R/Chapter4/dataDistribution.csv", row.names = FALSE)
  return(x)
}

#############################################################################
###############################dataweights###################################
#############################################################################

dataweight <- function(data)
{
  fit = data
  a = dim(summary(fit)$coefficients)
  
  varnum = a[1]
  
  t_value_sum = 0
  
  t_value <- array(dim = c(1, varnum - 1))
  
  t_value_weight <- array(dim = c(3, varnum - 1))
  
  t_value[varnum - 1] = 1
  
  for(i in 2:varnum)
  {
    t_value_sum = t_value_sum + abs(summary(fit)$coefficients[i,3])
  }
  
  for(i in 1:(varnum - 1))
  {
    t_value_weight[1,i] <- rownames(summary(fit)$coefficients)[i+1]
    t_value_weight[2,i] = summary(fit)$coefficients[(i + 1),3] / t_value_sum
  }
  t_value_weight
  for(i in 1:(varnum - 2))
  {
    flag = 1
    if(t_value_weight[2,i] < 0)
    {
      flag = 0
    }
    t_value[i] = round(abs(as.numeric(t_value_weight[2,i]))*100)/100
    if(t_value[i] == 0)
    {
      t_value[i] = 0.05
    }
    t_value[varnum - 1] = t_value[varnum - 1] - t_value[i]
    if(flag == 0)
    {
      t_value[i] = -t_value[i]
    }
  }
  if(t_value_weight[2,varnum - 1] < 0)
  {
    t_value[varnum - 1] = - t_value[varnum - 1]
  }
  
  return(t_value)
}


#############################################################################
#############################datacorrelation#################################
#############################################################################

dataselfcorrelation <- function(data, corvarname)
{
  x = data
  row.names = corvarname
  column.names = corvarname
  
  corvarnum = length(corvarname)
  
  corvalue <- array(dim = c(corvarnum,corvarnum),dimnames = list(row.names,column.names))
  
  for(i in 1:corvarnum)
  {
    for(j in 1:corvarnum)
    {
      if(j >= i)
      {
        corvalue[i,j] = cor(x[,corvarname[i]], x[,corvarname[j]], method = "spearman")
      }
      else
      {
        corvalue[i,j] = cor(x[,corvarname[i]], x[,corvarname[j]], method = "pearson")
      }
    }
  }
  
  corvalue
}

#############################################################################
#############################ret_ALPHA_cor#################################
#############################################################################

datacorrelation <- function(data, acovvarname, bcovvarname)
{
  x = data
  
  anum = length(acovvarname)
  bnum = length(bcovvarname)
  
  row.names = bcovvarname
  column.names = acovvarname
  matrix.names <- c('pearson', 'spearman')
  
  corvalue <- array(dim = c(bnum,anum, 2),dimnames = list(row.names,column.names,matrix.names))
  
  for(i in 1:2)
  {
    for(j in 1:bnum)
    {
      for(q in 1:anum)
      {
        corvalue[j, q, i] = cor(x[,bcovvarname[j]], x[,acovvarname[q]], use = "complete.obs", method = matrix.names[i])
      }
    }
  }
  corvalue
}

#############################################################################
#############################datafindindex#################################
#############################################################################

datafindindex <- function(data1, data2, yyyymm_index, cusip_name) # data1- alpha, data2- portfolio
{
  cusip_length = length(cusip_name)
  #index <- array(data = 0,dim = c(1, cusip_length))
  index <- array(data = 0,cusip_length)
  
  alpha_length = length(data1$yyyymm)
  for(i in 1:cusip_length)
  {
    index[i] <- 'NA'
    for(j in 1:alpha_length)
    {
      if((data1[j,'yyyymm'] == data2[yyyymm_index,'yyyymm']) && (data1[j,'cusip'] == cusip_name[i]))
      {
        index[i] = as.numeric(j)
        break
      }
    }
  }
  return(index)
}

#############################################################################
#############################dataForecast#################################
#############################################################################

dataForecast <- function(data, VALUE_t, PM_t, PROF_t, EQ_t, MQ_t, MS_t, ALPHA_t, ALPHA_fit, index)
{
  Forecast_return <- array(dim = c(1, length(index)))
  for (i in 1:length(index))
  {
    VALUE_temp = as.numeric(VALUE_t[1])*data[index[i],'B2P'] + as.numeric(VALUE_t[2])*data[index[i],'E2P'] + as.numeric(VALUE_t[3])*data[index[i],'S2P'] + as.numeric(VALUE_t[4])*data[index[i],'CFO2EV']
    PM_temp = as.numeric(PM_t[1])*data[index[i],'PM1m'] + as.numeric(PM_t[2])*data[index[i],'PM6m'] + as.numeric(PM_t[3])*data[index[i],'PM9m']
    PROF_temp = as.numeric(PROF_t[1])*data[index[i],'ROE'] + as.numeric(PROF_t[2])*data[index[i],'EBITDA2EV']
    EQ_temp = as.numeric(EQ_t[1])*data[index[i],'accrualsCF'] + as.numeric(EQ_t[2])*data[index[i],'CFOxI.debt']
    MQ_temp = as.numeric(MQ_t[1])*data[index[i],'exfinDebt'] + as.numeric(MQ_t[2])*data[index[i],'CAPXg']
    MS_temp = as.numeric(MS_t[1])*data[index[i],'EM9m'] + as.numeric(MS_t[2])*data[index[i],'EM12m'] + as.numeric(MS_t[3])*data[index[i],'ED9m'] + as.numeric(MS_t[4])*data[index[i],'ED12m']
    Alpha_temp = as.numeric(ALPHA_t[1])*VALUE_temp + as.numeric(ALPHA_t[2])*PM_temp + as.numeric(ALPHA_t[3])*PROF_temp + as.numeric(ALPHA_t[4])*EQ_temp + as.numeric(ALPHA_t[5])*MQ_temp + as.numeric(ALPHA_t[6])*MS_temp
    Forecast_return[i] = ALPHA_fit$coefficients[1] + ALPHA_fit$coefficients[2]*Alpha_temp
  }
  return(Forecast_return)
}

#############################################################################
#############################dataReal#################################
#############################################################################
dataReal <- function(data, yyyymm,cusip_portfolio)
{
  Real_return <- array(dim = c(1, length(cusip_portfolio)))
  for(j in 1:length(cusip_portfolio))
  {
    Real_return[j] = data[yyyymm,cusip_portfolio[j]]
  }
  return(Real_return)
}



#############################################################################
#############################dataCovariance#################################
#############################################################################

dataCovariance <- function(x_portfolio,cusip_portfolio,yyyymm_index_start, yyyymm_index_end)
{
  Covariance <- array(dim = c(length(cusip_portfolio),length(cusip_portfolio)))
  for (i in 1:length(cusip_portfolio))
  {
    for (j in 1:length(cusip_portfolio))
    {
      Covariance[i,j] = cov(x_portfolio[yyyymm_index_start:yyyymm_index_end,cusip_portfolio[i]],x_portfolio[yyyymm_index_start:yyyymm_index_end,cusip_portfolio[j]])
    }
  }
  return(Covariance)
}

#############################################################################
#############################dataOptimize1#################################
#############################################################################

dataOptimize1 <- function(Covariance, Forecast_return, alpha_target)
{
  library(quadprog)
  Omega = Covariance
  dvec <- array(data = 0,dim = c(1, length(Forecast_return)))
  Amat <- array(data = 0,dim = c(length(Forecast_return), length(Forecast_return)+2))
  for(i in 1:length(Forecast_return))
  {
    Amat[i,1] = 1
    Amat[i,length(Forecast_return)+2] = Forecast_return[i]
  }
  for(i in 1:(length(Forecast_return)))
  {
    Amat[i,i+1] = 1
  }
  b0 <- array(data = 0,dim = c(1, length(Forecast_return)+2))
  b0[1] = 1
  b0[length(Forecast_return)+2] = alpha_target
  
  ans=solve.QP(Omega,dvec,Amat,b0,meq=1)
  return(ans)
}


#############################################################################
#############################dataOptimize2#################################
#############################################################################

dataOptimize2 <- function(Covariance, Forecast_return, lambda)
{
  library(quadprog)
  Omega = Covariance
  
  dvec <- array(data = 0,dim = c(1, length(Forecast_return)))
  
  for(i in 1:length(dvec))
  {
    dvec[i] = Forecast_return[i]
  }
  
  Amat <- array(data = 0,dim = c(length(Forecast_return), length(Forecast_return)+1))
  for(i in 1:length(Forecast_return))
  {
    Amat[i,1] = 1
  }
  for(i in 1:length(Forecast_return))
  {
    Amat[i,i+1] = 1
  }
  b0 <- array(data = 0,dim = c(1, length(Forecast_return)+1))
  b0[1] = 1
  
  Dmat = lambda * Omega
  
  ans=solve.QP(Dmat,dvec,Amat,b0,meq=1)
  return(ans)
}

#############################################################################
#############################dataResult#################################
#############################################################################

dataResult <- function(yyyymm,ans,Real_return,cusip_alpha)
{
  for(j in 1:length(cusip_alpha))
  {
    result[yyyymm,j] = round(ans$solution,4)[j]
  }
  for(j in 1:length(cusip_alpha))#length(cusip_alpha)
  {
    result[yyyymm,9] = result[yyyymm,9] + ans$solution[j] * Real_return[j]
  }
  for(j in 1:length(cusip_alpha))
  {
    result[yyyymm,10] = result[yyyymm,10] + Real_return[j] / (length(cusip_alpha))
  }
  return(result)
}