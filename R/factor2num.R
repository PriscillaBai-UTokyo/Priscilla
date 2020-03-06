factor2num<-function(data,j=1){
  data<-as.data.frame(data)
  for(i in j:dim(data)[2]){data[,i]<-as.numeric(as.character(data[,i]))}
  return(data)
}
