\name{factor2num}
\alias{factor2num}
\title{Transform factor/character to numeric in a dataframe or matrix}
\usage{
factor2num(data,j=1,k=dim(data)[2])
}
\description{
data: matrix or dataframe
j: from which column you wish to transform
k: to which column you wish to transform
}
\examples{
 data(expr_limma,package='Priscilla')
 expr_limma = factor2num(expr_limma,j=1,k=5)
}

