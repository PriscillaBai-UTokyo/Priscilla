\name{DESeq_Pris}
\alias{DESeq_Pris}
\title{DESeq analysis and return outputs}
\usage{
DESeq_Pris(data,group)
}
\description{
data: matrix or dataframe with rownames denotes different genes and colnames denotes samples
group: characters denote cancer or normal
}
\examples{
 data(TCGA_expr,package='Priscilla')
 data(TCGA_pheno,package='Priscilla')
 res<-DESeq_Pris(TCGA_expr,TCGA_pheno[,2])
}

