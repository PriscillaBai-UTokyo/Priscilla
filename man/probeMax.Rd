\name{probeMax}
\alias{probeMax}
\title{probeMax}
\usage{
expr_ARL2 = probeMax(geneSymbol,expr_ARL1)
}
\description{
if multiple probe ids corrspond to the same gene symbol, then return the max expression row of probe ids.
geneSymbol: gene symbols, should have the same length with dim(expr_ARL1)[1]
expr_ARL1: rownames denote geneSymbol and colnames are samples
}
\examples{
data(expr_ARL1,"Priscilla")
data(geneSymbol,"Priscilla")
expr_ARL2 = probeMax(geneSymbol,input)
}


