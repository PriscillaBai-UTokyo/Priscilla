\name{multiCox}
\alias{multiCox}
\title{Conduct multivariate cox analysis}
\usage{
multicox=multiCox(data,ynames,OStime,OS,i=1,j=dim(data)[2])
}
\description{
data: result return from multiDataclean
ynames: which factors (colnames) want to analysis
OStime: OStime
OS: OS
i: from which column want to tranform from factor to numeric
j: to which column want to tranform from factor to numeric
}
\examples{
data(multicox,package='Priscilla')
ynames=colnames(data)[c(1:5,7,9)]
multicox=multiCox(data,ynames,OStime = data$OStime,OS=data$OS,i=1,j=5)
}


