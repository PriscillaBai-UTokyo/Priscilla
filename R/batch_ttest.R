batch_ttest<-function(data,group){
  if(dim(data)[1] != length(group)){stop('Plz check the input')}
  else{
  data<-factor2num(data,j=1)
  group<-as.character(group)
  pvalue<-sapply(1:dim(data)[2],function(i){t.test(data[which(group==unique(group)[1]),i],data[which(group==unique(group)[2]),i])[[3]]})
  padj<-p.adjust(pvalue)}
  mylist<-list('pvalue'=pvalue,'padj'=padj)
  return(mylist)
}


