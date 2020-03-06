volcanoplot<-function(data){
  fc<-data[,3]
  data$group<-sapply(1:dim(data)[1],function(i){if(data$log2FoldChange[i]>1 & data$padj[i] < 0.05){"#dd6a64"}else if(data$log2FoldChange[i]<(-1) & data$padj[i] < 0.05){"#6d95e6"}else{"grey40"}})
  data$size<-sapply(1:dim(data)[1],function(i){if(abs(data$log2FoldChange[i])>1 & data$padj[i] < 0.05){"2"}
    else if(abs(data$log2FoldChange[i])>1 & abs(data$log2FoldChange[i])<1.5 & data$padj[i] < 0.05){"2.5"}
    else if(abs(data$log2FoldChange[i])>1.5 & abs(data$log2FoldChange[i])<2 & data$padj[i] < 0.05){"3"}
    else if(abs(data$log2FoldChange[i])>2 & data$padj[i] < 0.05){"3.5"}
    else{"1.5"}})
  #cols.names <- unique(res$group)
  #cols.code <- mycol[1:length(cols.names)]
  #res$col<-
  # base volcano plot
  p <- -log10(data[,7])
  names(p) <- rownames(data)
  i <- data$group %in% c("red","navy")
  sizes <- as.numeric(data$size)
  names(sizes) <- rownames(data)
  pdf("base_volcano.pdf", 8, 6)
  par(xpd = F, #all plotting is clipped to the plot region
      mar = par()$mar + c(0,0,0,6))
  plot(fc, -log10(data[,7]), log='y',
       col=res$group,
       pch=16, #实心圆点
       ylab=bquote(~-Log[10]~"Padj value"), xlab=bquote(~Log[2]~'Foldchange'),
       cex=sizes, # 用小泡泡画不感兴趣的pathway
       xlim=range(fc * 1.2))

  # 添加横线
  abline(h=1.30103, lty=2, lwd=1)
  abline(h=2, lty=3, lwd=1) #标黑圈和文字的阈值

  # 添加竖线
  abline(v=-1, col="blue", lty=2, lwd=1)
  abline(v=1, col="red", lty=2, lwd=1)

  ## 此处用pval列计算adjusted pval来画黑圈和标文字，你还可以另外提供其他信息来画
  # 给bonferroni correction pval < 0.001的泡泡标上半透明的文字
  w <- which(res[,7] < 0.05 & abs(res$log2FoldChange)>1)
  points(fc[w], p[w], pch=1,cex=sizes[w])
  ## Add an alpha value to a colour
  add.alpha <- function(col, alpha=1){
    if(missing(col))
      stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2,
          function(x)
            rgb(x[1], x[2], x[3], alpha=alpha))
  }
  cols.alpha <- add.alpha(res[,7], alpha=0.6)
  text(fc[w], p[w], names(fc[w]),pos=3,col=cols.alpha,cex = 0.5)
  par(xpd = TRUE) #all plotting is clipped to the fi
  f <- c(1,1.5,2,2.5,3)
  s <- sqrt(f*50)

  dev.off()
}
