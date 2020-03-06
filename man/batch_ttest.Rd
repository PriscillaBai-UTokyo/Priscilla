\name{batch_ttest}
\alias{batch_ttest}
\title{Do student T-test for many genes at the same time}
\usage{
batch_ttest(data,group)
}
\description{
data: matrix or dataframe with rownames denotes different samples and colnames denotes genes
group: characters, the length must be the same with dim(df_50)[1]
}
\examples{
 data(df_50,package='Priscilla')
 data(class,package='Priscilla')
 batch_p<-batch_ttest(df_50,class)
}

