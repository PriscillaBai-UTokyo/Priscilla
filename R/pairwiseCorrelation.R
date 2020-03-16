correlationNumBar = function(input,column,resultname,ylabname){
  library(ggplot2)
  corbar = sapply(unique(input[,column]),function(i){c(count(subset(input,input[,column]==i)$coefficient>0),count(subset(input,input[,column]==i)$coefficient<0))})
  corbar=t(corbar)
  colnames(corbar) = c("positive","negative")
  corbar = as.data.frame(corbar)
  ggplot(data=corbar)+geom_bar(aes(rownames(corbar),positive),stat="identity",fill="palegreen3")+
  geom_bar(aes(rownames(corbar),negative*(-1)),stat="identity",fill="dodgerblue4")+coord_flip()+
  xlab("")+ylab(ylabname)+theme_bw() + theme(panel.grid =element_blank()) +
  theme(panel.border = element_rect(size = 0.6)) +
  theme(axis.line.y = element_blank())+scale_y_continuous(breaks =seq((max(corbar$negative)+1)*(-1),max(corbar$positive)+1,by=2))
  ggsave(resultname,dpi=1200)
}
