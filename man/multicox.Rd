\name{multi_cox}
\alias{multi_cox}
\title{multi_cox}
\usage{
multicox=multi_cox(data,ynames)
}
\description{
data: result return from multiDataclean
ynames: which rownames want to analysis
}
\examples{
data(multicox,package='Priscilla')
ynames=colnames(data)[c(1:5,7,9)]
multicox=multi_cox(data,ynames)
}


