forestplot_clean<- function(input){
  forestplot = input
  forestplot = forestplot[,-3]
  colnames(forestplot) = c("Variable","Point Estimate","pvalue","Low","High")
  forestplot[,2:5] = round(forestplot[,2:5],3)               
  forestplot[,3] = sapply(1:dim(forestplot)[1],function(i)
  {ifelse(forestplot[i,3]<0.001,"<0.001",forestplot[i,3])})
  return(forestplot)
}
