\name{multiDataclean}
\alias{multiDataclean}
\title{multiDataclean}
\usage{
data = multiDataclean(data,target_clinical)
}
\description{
data: dataframe with gene and clinical information
target_clinical: which clinical information I want to analysis
}
\examples{
data(multiDataclean,"Priscilla")
target_clinical =   c("ARL4A_expr","ajcc_pathologic_m","tumor_stage","age_at_diagnosis",
"ajcc_pathologic_t","ajcc_pathologic_n","gender","OS","OStime")
data = multiDataclean(data,target_clinical)
}


