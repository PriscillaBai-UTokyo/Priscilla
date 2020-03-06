\name{MCAN_bar}
\alias{MCAN_bar}
\title{Microarray data normalization and compariason: bar plot}
\usage{
MCAN_bar(data,data.norm,width=14,height=7,xaxt = 'n',xlab='Sample',ylab='Relative amount')
}
\description{
Normalization microarray data;
Draw the boxplot of Microarray data before and after normalization, and return the Microarray data after normalization.
}
\examples{
 data(expr_log2,package='Priscilla')
 data.norm = MCA_normalization(expr_log2)
 MCAN_bar(expr_log2,data.norm,width=14,height=7,xaxt = 'n',xlab='Sample',ylab='Relative amount')
}

