correlation = function(data,i,j,cutoff){
  library(stringr)
  r=sapply(i,function(I){
    sapply(j,function(J){
      cor = cor(data[,J],data[,I])})
  })
  a=which(abs(r)>cutoff ,T)
  a = cbind(a,r[a])
  a[,1] = colnames(data)[j][a[,1]]
  a[,2] = colnames(data)[i][as.numeric(a[,2])]
  a[,1] = str_replace(a[,1], "HALLMARK_", "")
  a = as.data.frame(a)
  a[,3] = as.numeric(a[,3])
  write.csv(a,"correlation.csv",row.names = T)
  print(head(a))
  return(a)
}
