TCGAclinical_clean <- function(data_count,clinical){
  samplename = colnames(data_count)
  samplename = sapply(1:length(samplename),function(i){
    paste(strsplit(samplename,"[-]")[[i]][1],strsplit(samplename,"[-]")[[i]][2],strsplit(samplename,"[-]")[[i]][3],sep="-")})
  samplename = as.data.frame(samplename)
  colnames(samplename)[1] = "bcr_patient_barcode"
  samplename[,1] = as.character(samplename[,1])
  clinical1 = merge(samplename,clinical,by="bcr_patient_barcode",all.x=T)
  m = order(samplename)
  data_count = data_count[,m]

  empty_sample = which(is.na(clinical1$submitter_id))
  data_count = data_count[,-empty_sample]
  clinical1 = clinical1[-empty_sample,]
  clinical1$OS = ifelse(clinical1$OS=="Alive",0,1)
  samplename = samplename[-empty_sample,]



  dataset = list()
  dataset[[1]] = data_count
  dataset[[2]] = clinical1
  return(dataset)
}
