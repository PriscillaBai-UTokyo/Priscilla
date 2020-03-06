pairwise_correlation<-function(data,norm = F){
  data<-factor2num(data)
  if(norm == T){
    data<-apply(data,2,function(i){i+rnorm(123,mean=1,sd=1e-6)})}else{
    data<-data}
  cor<-cor(data,method="spearman")
  all=data.frame()
  coln=colnames(cor)
  rown=colnames(cor)
  for(i in 1:ncol(cor)){
    for(k in i:ncol(cor)){
      all2=data.frame()
      b=c(rown[i],coln[k],cor[i,k])
      all2=rbind(all2,b)
      colnames(all2)=c('a','b','num')
      all=rbind(all,all2)
    }
  }
  melt_cor=all
  melt_cor=melt_cor[which(melt_cor[,1]!=melt_cor[,2]),]
  melt_cor[,3]<-as.numeric(as.character(melt_cor[,3]))
  return(melt_cor)
}
