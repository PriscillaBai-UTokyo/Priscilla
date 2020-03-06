TCGAclinical <- function(project_name){
  library(TCGAbiolinks)
  library(dplyr)
  library(DT)
  library(SummarizedExperiment)
  clinical_data <- GDCquery_clinic(project = project_name, type = "clinical")
  target_col = c("submitter_id","ajcc_pathologic_m","tissue_or_organ_of_origin","ajcc_staging_system_edition",
                 "tumor_stage","icd_10_code","primary_diagnosis","site_of_resection_or_biopsy","ajcc_pathologic_n","days_to_last_follow_up",
                 "tumor_grade","age_at_diagnosis","ajcc_pathologic_t","ajcc_pathologic_stage","days_to_birth","race","gender","age_at_index","vital_status",
                 "bcr_patient_barcode")
  clinical_data = clinical_data[,c(which(colnames(clinical_data) %in% target_col))]
  if(length(which(clinical_data$vital_status=="Not Reported")) != 0){
    clinical_data = clinical_data[-which(clinical_data$vital_status=="Not Reported"),]}
  if(length(which(is.na(clinical_data$days_to_last_follow_up))) != 0){clinical_data = clinical_data[-which(is.na(clinical_data$days_to_last_follow_up)),]}
  clinical_data = rename(clinical_data,c(vital_status="OS",days_to_last_follow_up = "OStime"))
  return(clinical_data)
}
