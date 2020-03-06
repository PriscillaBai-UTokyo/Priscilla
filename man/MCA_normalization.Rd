\name{MCA_normalization}
\alias{MCA_normalization}
\title{Microarray data normalization}
\usage{
MCA_normalization(data)
}
\description{
Normalize microarray data,and provide input for MCAN_bar function
}
\examples{
 data(expr_log2,package='Priscilla')
 data.norm = MCA_normalization(expr_log2)
}

