\name{TCGA_ENSEMBL2Symbol}
\alias{TCGA_ENSEMBL2Symbol}
\title{Tranform Ensembl to symbol in orignal TCGA expression matrix}
\usage{
TCGA_ENSEMBL2Symbol(data)
}
\description{
data: matrix or dataframe with rownames denotes ENSEMBL ID and colnames denotes samples

}
\examples{
 data(Expr_matrix,package='Priscilla')
 data<-TCGA_ENSEMBL2Symbol(Expr_matrix)
}

