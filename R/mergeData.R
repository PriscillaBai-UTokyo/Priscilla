mergeData = function(data1,data2,combineRowname_data1,combineRowname_data2,i,j,all.x,all.y){
  if(combineRowname_data1==T & combineRowname_data2==F){
    data1 = cbind(rownames(data1),data1)}
  else if(combineRowname_data1==F & combineRowname_data2==T){
    data2 = cbind(rownames(data2),data2)
  }else if(combineRowname_data1==T & combineRowname_data2==T){
    data1 = cbind(rownames(data1),data1)
    data2 = cbind(rownames(data2),data2)
  }
  colnames(data1)[i] = "SampleID"
  colnames(data2)[j] = "SampleID"
  df = merge(data1,data2,by="SampleID",all.x=all.x,all.y=all.y)
  return(df)
}
