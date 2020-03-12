probeMax = function(geneSymbol,input){
  uni = as.character(unique(geneSymbol))
  temp = matrix(data=NA,ncol = dim(input)[2],nrow=1)
  temp = as.data.frame(temp)
  colnames(temp) = colnames(input)
  input = factor2num(input)
  for(i in 1:length(uni))
  {if(length(which(geneSymbol==uni[i]))!=1)
  {m = rowMeans(input[which(geneSymbol==uni[i]),])
  n = which(m==max(m))
  if(length(n)>1){
    n = n[1]
  }
  temp = rbind(temp,input[which(geneSymbol==uni[i])[n],])
  rownames(temp)[i+1] = uni[i]}
    else{temp<-rbind(temp,input[which(geneSymbol==uni[i]),])
    rownames(temp)[i+1] = uni[i]}}
  temp = temp[-1,]
  return(temp)
}

