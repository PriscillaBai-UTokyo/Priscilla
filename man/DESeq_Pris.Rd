\name{myDESeq}
\alias{myDESeq}
\title{DESeq analysis and return outputs}
\usage{
myDESeq(data,group)
}
\description{
data: matrix or dataframe with rownames denotes different genes and colnames denotes samples
group: characters denote cancer or normal
}
\examples{
 data(TCGA_expr,package='Priscilla')
 data(TCGA_pheno,package='Priscilla')
 res<-myDESeq(TCGA_expr,TCGA_pheno[,2])
}

