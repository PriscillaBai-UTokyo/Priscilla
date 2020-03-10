multiDataclean = function(data,target_clinical){
  data = data[,which(colnames(data) %in% target_clinical)]
  data$tumor_stage = as.character(data$tumor_stage)
  data$tumor_stage = sapply(data$tumor_stage,function(i){
    if(grepl("iv",i)){"stage iv"}
    else if(str_count(i,"i")==1){"stage i"}else if (str_count(i,"i")==2) {
      "stage ii"}else if (str_count(i,"i")==3) {"stage iii"}
    else{"not reported"}})
  data$age_at_diagnosis = as.numeric(as.character(data$age_at_diagnosis))/365
  data$age_at_diagnosis = ifelse((round(data$age_at_diagnosis)>60 | round(data$age_at_diagnosis)==60),1,0) #>=18,1,0
  data$ajcc_pathologic_t = sapply(data$ajcc_pathologic_t,function(i){
    if(grepl("1",i)){"T1"}
    else if(grepl("2",i)){"T2"}else if (grepl("3",i)) {
      "T3"}else if (grepl("4",i)) {"T4"}
    else{"TX"}})
  data$ajcc_pathologic_n = sapply(data$ajcc_pathologic_n,function(i){
    if(grepl("0",i)){"N0"}
    else if(grepl("1",i)){"N1"}else if (grepl("2",i)) {
      "N2"}else if (grepl("3",i)) {"N3"} else if (grepl("X",i)) {"NX"}else{NA}
  })
  return(data)
}
