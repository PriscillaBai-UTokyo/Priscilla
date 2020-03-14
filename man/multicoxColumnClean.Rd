\name{multicoxColumnClean}
\alias{multicoxColumnClean}
\title{Clean column with three or more variables for multivariable cox analysis}
\usage{
data = multicoxColumnClean(input,colNumbers)
}
\description{
input: dataframe with clinical information
colNumbers: which columns want to analysis
}
\examples{
data(multi_GEO2)
multi_GEO3 = multicoxColumnClean(multi_GEO2,colNumbers = c(1,4,5))
multi_GEO3 = cbind(multi_GEO3,multi_GEO2[,c(2,3,6,7,8)])
}


