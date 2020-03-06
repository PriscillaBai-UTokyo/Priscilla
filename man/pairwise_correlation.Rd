\name{pairwise_correlation}
\alias{pairwise_correlation}
\title{Do pairwise correlation between any genes}
\usage{
pairwise_correlation(data,norm = F)
}
\description{
data: matrix or dataframe with rownames denotes different samples and colnames denotes genes
norm: do normalization or not
}
\examples{
 data(TCGA_auto,package='Priscilla')
 Cor_coefficient<-pairwise_correlation(TCGA_auto,norm = F)
}

