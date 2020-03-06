\name{volcanoplots}
\alias{volcanoplot}
\title{Draw volcano plot after DESeq}
\usage{
volcanoplot(data)
}
\description{
data: matrix after DESeq, the first column is rownames(data).
}
\examples{
 data(res,package='Priscilla')
 volcanoplot(res)
}

