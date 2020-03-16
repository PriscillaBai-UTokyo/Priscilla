\name{correlation}
\alias{correlation}
\title{correlation analysis between two range of columns from the same dataframe}
\usage{
cor = correlation(data,i,j,cutoff)
}
\description{
data: dataframe, column with different genes and rows with different samples
i: which columns want to do correlation
j: which columns want to do correlation
cutoff: like 0.5
}
\examples{
data(CorExpr,package = "Priscilla")
Cor = correlation(CorExpr,1:22,23:72,0.3)
}


