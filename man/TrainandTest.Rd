\name{TrainandTest}
\alias{TrainandTest}
\title{Divide train and test datasets in an expression matrix}
\usage{
TrainandTest(data,group,p=0.25)
}
\description{
data: matrix or dataframe with rownames denotes different samples and colnames denotes genes
p: the proportion of train or test dataset
}
\examples{
 data(df_50,package='Priscilla')
 data(class,package='Priscilla')
 dataset<-TrainandTest(df_50,class,p=0.25)
 train<-dataset$train
 test<-dataset$test
}

