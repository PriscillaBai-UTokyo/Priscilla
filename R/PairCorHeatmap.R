PairCorHeatmap<-function(data,norm=F){
  library(rentrez)
  library(RColorBrewer)
  library(corrplot)
  library(ggplot2)
  library(ggthemes)

  data<-factor2num(data)
  if(norm == T){
    data<-apply(data,2,function(i){i+rnorm(123,mean=1,sd=1e-6)})}else{data<-data}

  ###calculate p value of correlation coefficients
  p=sapply(1:dim(data)[2],function(I){
    sapply(1:dim(data)[2],function(J){
      cor = cor.test(data[,J],data[,I],method="spearman")[[3]]
    })
  })
  colnames(p)<-colnames(data)
  rownames(p)<-colnames(data)


  est=sapply(1:dim(data)[2],function(I){
    sapply(1:dim(data)[2],function(J){
      cor = cor(data[,J],data[,I],method="spearman")
    })
  })
  est[which(est==1)]<-NA
  colnames(est)<-colnames(data)
  rownames(est)<-colnames(data)

  cols=rev(colorRampPalette(brewer.pal(9,"RdBu"))(199))
  #library(Cairo)
  #CairoJPEG("crosstalk.jpeg",width=7200,height=4800,res=1200)
  p1 <- corrplot(as.matrix(est),
                 type = "upper", #只显示上三角
                 method="color",
                 order="hclust",
                 hclust.method="ward.D2",
                 col=cols,
                 tl.col ="black", #文字颜色
                 tl.cex = 0.5, #文字大小
                 tl.srt =45, #Text label color and rotation
                 is.corr = FALSE, #非相关系数矩阵。对于一般的矩阵，必须使用is.corr = FALSE
                 diag = F,  #不展示相关系数
                 p.mat = as.matrix(p), #p值的矩阵
                 sig.level = c(1e-25,1e-35,1e-50), #显著水平
                 insig = c("label_sig"), #显著水平以***、**、*表示
                 pch.cex = 0.5, #标注的星号*的大小
                 font = 3) #斜体
  reorder <- function(x, o){
    v <- x;
    v[lower.tri(v)] <- t(x)[lower.tri(x)]
    v <- v[o,o];
    y <- x;
    y[upper.tri(v)] <- t(x)[upper.tri(x)]
    y <- y[o,o];
    Z <- v;
    Z[lower.tri(Z)] <- y[lower.tri(Z)]
    return(Z)
  }

  p[p < 1.0e-150]=NA #or p[p == 0 ] = NA
  p[lower.tri(p)] <- NA

  # cluster ordering info by corrplot function
  o <- rownames(p1)

  # p value reordered by ward.D2 hclust
  p <- reorder(p, o)

  # get -log10 fdr value
  q <- -log10(p.adjust(p, method="fdr"))
  # get -log10 p value
  p <- -log10(p)

  o <- rownames(p1)
  r <- reorder(est,o)
  r[lower.tri(r)] <- NA
  r[which(r=="1")]<-NA

  pdf("Correlation_heatmap.pdf", 20, 19)
  par(bty="n",
      mar=c(4,4,4,8)+.1, #四周留空
      las=2,# the style of axis labels
      tcl=-.33) #The length of tick marks as a fraction of the height of a line of text

  m <- nrow(est)
  n <- ncol(est)

  # we should check range(est[!est==Inf]) for col and breaks setting
  # we should check data distribution hist(est[!est==Inf]) for col and breaks setting

  max_value<- range(r[!is.na(r)])[2]
  brks<- c(0,seq(0.1,0.85,l=9))
  cols2<- rev(colorRampPalette(brewer.pal(9,"RdBu"))(length(brks)-1))
  #cols2<-c("#2166AC","#4393C3","#92C5DE","#FDDBC7","#F4A582","#D6604D","#B2182B")
  # baseplot for heatmap
  image(x=1:n, y=1:m, r, col=cols2, breaks=brks, xaxt="n", yaxt="n", xlab="",ylab="", xlim=c(0, n+4), ylim=c(0, n+1))

  # add gene name
  mtext(side=2, at=1:n, o, font=3, col="black",cex=0.8) #left
  mtext(side=3, at=1:n, o, font=3, col="black",cex=0.8) #top

  # add white border
  abline(h=0:n+.5, col="white", lwd=.5)
  abline(v=0:n+.5, col="white", lwd=.5)

  # add title
  #text(x=n/2, y=m+1, "Enrichment based on Fisher's Exact Test (greater)", pos=3)

  q_range<- range(q[!is.na(q)])[2]
  # significant labels q>50
  w = arrayInd(which(q > q_range/2), rep(m,2))
  points(w, pch="*", col="black", cex=1)
  # significant labels q>35
  w = arrayInd(setdiff(which(q > q_range/3),which(q > q_range/2)),rep(m,2))
  points(w, pch=3, col="black", cex=1) # "+""
  # significant labels q>25
  w = arrayInd(setdiff(which(q > q_range/4),which(q > q_range/3)), rep(m,2))
  points(w, pch="-", col="black", cex=1)

  # set up legend color bar
  image(y = 1:16 +6, x=rep(n,2)+c(2.5,3)+1, z=matrix(c(1:16), nrow=1), col=cols2, add=TRUE)

  # add legend color bar scale value
  brks2 <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

  axis(side = 4,tcl=-.15, las=1, lwd=.5)

  points(x=rep(n,3)+3.5, y=1:3, pch=c("-","+","*"))
  text(x=n+2, y=15,"enrichment estimate value",pos=1,srt=90)
  mtext(side=4, at=c(1,2,3,4), c("-log10(FDR) > 25","-log10(FDR) > 35","-log10(FDR) > 50","ns"), line=0.2)
  dev.off()
}
