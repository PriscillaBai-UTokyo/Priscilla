\name{correlationNumBar}
\alias{correlationNumBar}
\title{draw barplot reveals the number of positive correlation and negative correlation}
\usage{
correlationNumBar(input,column,resultname,ylabname)
}
\description{
input: dataframe, result return from correlation function
column: which column want to show
ylabname: xlab name is number, what about y lab name?
}
\examples{
data(Cor,package = "Priscilla")
correlationNumBar(Cor,2,"correaltiongene.jpeg","Number of pathways")
}


