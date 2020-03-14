multicoxColumnClean = function(input,colNumbers){
  newdf = matrix(data=NA,nrow=dim(input)[1],ncol=1)
  for(i in colNumbers){
    for(j in 1:length(table(input[,i]))){
      temp = names(table(input[,i]))[j]
      newdf = cbind(newdf,ifelse(input[,i] == temp,1,0))
      colnames(newdf)[dim(newdf)[2]] = temp
    }
  }
  newdf = newdf[,-1]
  return(newdf)
}
