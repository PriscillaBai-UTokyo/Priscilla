TCGA_ENSEMBL2Symbol <- function(data){
  library(clusterProfiler)
  gene<-bitr(rownames(data),"ENSEMBL","SYMBOL","org.Hs.eg.db")
  data<-cbind(rownames(data),data)
  colnames(data)[1]<-"ENSEMBL"
  data<-as.data.frame(data)
  df<-merge(gene,as.data.frame(data),by="ENSEMBL")
  df = df[,-1]
  df = df[-which(duplicated(df[,1])),]
  rownames(df) = df[,1]
  df = df[,-1]
  return(df)
}

