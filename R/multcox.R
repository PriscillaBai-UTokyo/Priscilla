multi_cox = function(data,ynames,OStime,OS){
  library(survival)
  form<-as.formula(paste0('sur2~',paste0(ynames,collapse = '+')))
  sur2<-Surv(time=OStime,event = OS)
  multicox<-coxph(formula = form,data = data)#数据集需要更改
  multisum<-summary(multicox)##汇总
  muHR<-round(multisum$coefficients[,2],3)#风险比
  muPvalue<-multisum$coefficients[,5]#p值
  muCIdown<-round(multisum$conf.int[,3],3)#下
  muCIup<-round(multisum$conf.int[,4],3)#上
  muCI<-paste0(muCIdown,'-',muCIup)##95%置信区间
  multicox2<-data.frame('characteristics'=names(muPvalue),
                        'muHazard Ration'=muHR,
                        'muCI95'=muCI,
                        'mupvalue'=ifelse(muPvalue < 0.001, "< 0.001", round(muPvalue,3)))
  rownames(multicox2)<-NULL
  multicox2$`HR(95%CI)`<-paste0(multicox2$muHazard.Ration,'(',multicox2$muCI95,')')
  multicox2<-dplyr::select(multicox2,characteristics,`HR(95%CI)`,mupvalue,-muCI95,-muHazard.Ration)
  write.csv(multicox2,'multicox.csv')

  return(multicox2)
}
