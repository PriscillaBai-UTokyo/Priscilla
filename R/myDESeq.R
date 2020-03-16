myDESeq<-function(data,group){
  library(DESeq2)
  ##将基因名和样本名做成一个列表
  dimnames=list(rownames(data),colnames(data))
  data=matrix(as.numeric(as.matrix(data)),nrow = nrow(data),dimnames = dimnames)
  #这个包要求表达值必须为整数
  data=round(data,0)
  ##设计分组信息
  design=as.factor(group)
  ##构建主程序
  dds<-DESeqDataSetFromMatrix(data,DataFrame(design),design = ~design)
  dds<-DESeq(dds,fitType = "local") ## or mean
  res<-as.data.frame(results(dds))

  #library(dplyr)
  res<-cbind(rownames(res),res)
  return(res)
}
