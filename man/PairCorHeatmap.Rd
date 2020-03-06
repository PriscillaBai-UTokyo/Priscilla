\name{PairCorHeatmap}
\alias{PairCorHeatmap}
\title{Draw the heatmap of pairwise correlation}
\usage{
PairCorHeatmap(data,norm=F)
}
\description{
data: matrix or dataframe with rownames denotes different samples and colnames denotes genes
norm: do normalization or not
}
\examples{
 data(TCGA_auto,package='Priscilla')
 PairCorHeatmap(TCGA_auto,norm=T)
}

