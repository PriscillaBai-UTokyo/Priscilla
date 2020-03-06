\name{factor2num}
\alias{factor2num}
\title{Transform factor/character to numeric in a dataframe or matrix}
\usage{
factor2num(data,j=1)
}
\description{
data: matrix or dataframe
j: from which column you wish to transform
}
\examples{
 data(expr_limma,package='Priscilla')
 expr_limma = factor2num(expr_limma,j=1)
}

