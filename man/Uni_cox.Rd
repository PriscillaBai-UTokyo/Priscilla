\name{TCGAclinical_clean}
\alias{TCGAclinical_clean}
\title{clean TCGA clinical data}
\usage{
TCGAclinica_clean(data_count,clinical)
}
\description{
data_count: rownames denote gene symbols and colnames denote sample names
clinical: return from TCGAclinical function
}
\examples{
load("clinical.Rdata")
dataset_clinical = TCGAclinica_clean(data_count,clinical)}


