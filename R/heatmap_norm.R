heatmap_norm<-function(data,method=log10,probs1=0.90,probs2=0.1){
  data=as.data.frame(data)
  data = method(data+1)
  data=t(data)
  m = dim(data)[2]
  for(i in 1:m){data[which(data[,i] > quantile(data[,i],probs = probs1,na.rm = T)),i] = quantile(data[,i],probs = probs1,na.rm = T)}
  for(i in 1:m){data[which(data[,i] < quantile(data[,i],probs = probs2,na.rm = T)),i] = quantile(data[,i],probs = probs2,na.rm = T)}
  data = as.matrix(t(data))
  return(data)
}
