mylasso <- function(input,OStime,Status){
  library("survival")
  library("glmnet")
  cvfit = cv.glmnet(data, Surv(OStime,OS),
                    nfold=10,
                    family = "cox")
  pdf("lasso_plot1.pdf")
  plot(cvfit)
  dev.off()
  return(cvfit$lambda.min)
  fit <- glmnet(t(myexpr), Surv(mysurv$months,mysurv$status),family = "cox")

  pdf("lasso_plot2.pdf")
  plot(fit, label = TRUE)
  dev.off()

  coef.min = coef(cvfit, s = "lambda.min")
  active.min = which(coef.min != 0)
  geneids <- rownames(myexpr)[active.min]
  combine <- cbind(geneids, index.min)
  write.csv(combine,"gene_index.csv")
  return(combine)
}
