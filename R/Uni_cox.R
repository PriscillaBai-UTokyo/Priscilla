Uni_cox <- function(input,ynames,OStime,Status){
  library(tidyverse)
  library(ggplot2)
  library(ggstatsplot)
  library(survival)
  library(stringr)
  library(viridis)
  library(scales)

  Coxoutput=data.frame()
  for(i in ynames){
    cox <- coxph(Surv(OStime, Status) ~ input[,i], data = input)
    coxSummary = summary(cox)
    Coxoutput=rbind(Coxoutput,cbind(gene=colnames(input)[i],HR=coxSummary$coefficients[,"exp(coef)"],z=coxSummary$coefficients[,"z"],pvalue=coxSummary$coefficients[,"Pr(>|z|)"],lower=coxSummary$conf.int[,3],upper=coxSummary$conf.int[,4]))}
  for(i in c(2:6)){
    Coxoutput[,i] <- as.numeric(as.vector(Coxoutput[,i]))
  }
  Coxoutput <- arrange(Coxoutput,pvalue) #按照p值排序
  #filter(pvalue < 0.05)

  #保存到文件
  write.csv(Coxoutput,'univariate_cox.csv', row.names = F)
  return(Coxoutput)
}
