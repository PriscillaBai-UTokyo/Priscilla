TCGAdownload <- function(project_name,category,data_type,workflow_type){
  library(TCGAbiolinks)
  library(dplyr)
  library(DT)
  library(SummarizedExperiment)
  data_count <- GDCquery(project = project_name,data.category = category,data.type = data_type, workflow.type = workflow_type)
  GDCdownload(data_count, method = "api", files.per.chunk = 100)
  expdat <- GDCprepare(query = data_count)
  data_count=assay(expdat)
  return(data_count)
}
