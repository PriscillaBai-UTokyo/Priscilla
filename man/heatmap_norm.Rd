\name{heatmap_norm}
\alias{heatmap_norm}
\title{normalization for heatmap input to obtain better heatmap plot}
\usage{
heatmap_norm(data,method=log10,probs1=0.90,probs2=0.1)
}
\description{
data: matrix or dataframe with row denotes genes and col denotes samples
method: log2 or log10
probs1: 0.9 or 0.95
probs2: 0.1 or 0.05
}
\examples{
 data(expr_limma,package='Priscilla')
 expr_limma = factor2num(expr_limma,j=1)
 expr_limma_1 = heatmap_norm(expr_limma,method=log10,probs1=0.90,probs2=0.1)
}

