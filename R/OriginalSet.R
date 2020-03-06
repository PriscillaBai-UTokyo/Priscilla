OriginalSet<-function(path){
  library(ggplot2)
  library(ggrepel)
  library(ggthemes)
  library(pheatmap)
  library(dplyr)
  library(tidyr)
  library(stringr)

  setwd(paste('/Users/baiyunfan/documents/',path,sep=''))
  Sys.setenv(LANGUAGE = "en") #显示英文报错信息
  options(stringsAsFactors = FALSE) #禁止chr转成factor
}
