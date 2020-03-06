MCA_normalization<-function(data){
  library(preprocessCore)
  data.norm <- normalize.quantiles(as.matrix(data))
  colnames(data.norm) <- colnames(data)
  rownames(data.norm) <- rownames(data)
  return(data.norm)
}
