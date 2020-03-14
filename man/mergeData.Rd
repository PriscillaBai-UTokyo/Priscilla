\name{mergeData}
\alias{mergeData}
\title{mergeData}
\usage{
mergeData = function(data1,data2,combineRowname_data1,combineRowname_data2,i,j,all.x,all.y)
}
\description{
Merge two dataframe
First, identify should combine rownames or not
Second, identify which column is the shared column
Third, identify which kind of merge
}
\examples{
data(expr_ARL3,package="Priscilla")
data(pData,package="Priscilla")
df = mergeData(expr_ARL3,pData,combineRowname_data1=T,combineRowname_data2=F,i=1,j=1,all.x=T,all.y=F)
}


