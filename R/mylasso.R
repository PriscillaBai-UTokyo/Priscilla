mylasso <- function(data,OStime,OS){
  library("survival")
  library("glmnet")
  data = factor2num(data)
  if(length(which(OStime == 0)) != 0)
  {m=which(OStime == 0)
  data = data[-m,]
  OStime = OStime[-m]
  OS = OS[-m]}
  data = as.matrix(data)
  cvfit = cv.glmnet(data, Surv(as.numeric(OStime),OS),
                    nfolds=10,
                    family = "cox")
  pdf("lasso_plot1.pdf")
  plot(cvfit)
  dev.off()
  print(cvfit$lambda.min)
  fit <- glmnet(data, Surv(OStime,OS),family = "cox")

  pdf("lasso_plot2.pdf")
  plot(fit, label = TRUE)
  dev.off()

  coef.min = coef(cvfit, s = "lambda.min")
  active.min = which(coef.min != 0)
  index.min = coef.min[active.min]
  geneids <- colnames(data)[active.min]
  combine <- cbind(geneids, index.min)
  write.csv(combine,"gene_index.csv")
  return(combine)
}
