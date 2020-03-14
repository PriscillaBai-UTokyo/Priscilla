factor2num<-function(data,j=1,k=dim(data)[2]){
  data<-as.data.frame(data)
  for(i in j:k){data[,i]<-as.numeric(as.character(data[,i]))}
  return(data)
}
