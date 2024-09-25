#' Model fit evaluation
#'
#' @param Obs observed data
#' @param Sim simulated data
#' @param data if data.frame
#' @param as.DF default is TRUE and produces data as data.frame(...)
#'
#' @return this function provides various model fit metric including
#' @export

model_fit_params=function(Obs,Sim,data,as.DF=T){

  if(!missing(data)){
    Obs=data[,deparse(substitute(Obs))]
    Sim=data[,deparse(substitute(Sim))]
  }

  dat=na.omit(data.frame(Obs=Obs,Sim=Sim))
  Obs=dat$Obs
  Sim=dat$Sim
  MSE=1/N.obs(Obs)*sum((Obs-Sim)^2,na.rm=T); # mean square errror

  # res=Obs-Sim
  # fit.vals=Sim
  # rdf=N.obs(res)-2
  # mss=sum((fit.vals - mean(fit.vals,na.rm=T))^2,na.rm=T)
  # rss=sum(res^2,na.rm=T)
  # resvar=rss/rdf
  # sigma=sqrt(resvar);#RMSE
  # r2=mss/(mss+rss)
  # n <- sum(complete.cases(Sim))
  # r2 = 1 - (sum((Obs-Sim)^2, na.rm = T)/((n-1)*var(Obs, na.rm = T)));# "traditional" from caret package
  r2 = cor(Obs, Sim, use = "complete.obs")^2 ;# "corr" from caret package
  RMSE = sqrt(mean((Sim - Obs)^2, na.rm = T));
  MAE = mean(abs(Sim - Obs), na.rm = T); # mean absolute error

  r.val=cov(Obs,Sim,use="pairwise.complete.obs")/(sd(Obs,na.rm=T)*sd(Sim,na.rm=T))
  # r2=r.val^2
  bias_mean=mean(Obs,na.rm=T)/mean(Sim,na.rm=T)
  sd_ratio=sd(Obs,na.rm=T)/sd(Sim,na.rm=T);
  MBE = mean(Sim - Obs,na.rm=T); # mean bias error

  KGE=1-sqrt((bias_mean-1)^2 + (sd_ratio-1)^2 + (r.val-1)^2); # kling gupta efficiency
  NSE = 1-MSE/sd(Obs)^2; # nas sutcliffe efficiency
  if(as.DF==T){rslt=data.frame(r.val=r.val,r2=r2,bias_mean=bias_mean,sd_ratio=sd_ratio,KGE=KGE,NSE=NSE,MAE = MAE,MSE=MSE,RMSE=RMSE,MBE=MBE)
  }else{rslt=list(r.val=r.val,r2=r2,bias_mean=bias_mean,sd_ratio=sd_ratio,KGE=KGE,NSE=NSE,MAE = MAE,MSE=MSE,RMSE=RMSE,MBE=MBE)}
  return(rslt)
}
