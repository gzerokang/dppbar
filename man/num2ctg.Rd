% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/num2ctg.R
\name{num2ctg}
\alias{num2ctg}
\title{Transform numerical variable into categorical variable}
\usage{
num2ctg(dataframe,col.id,col.name=NA,partition,level.name,type='quantile')
}
\arguments{
\item{dataframe:}{a dataframe-like object}

\item{col.id:}{an integer, specify which column in the dataframe that you need to transfer}

\item{col.name:}{a character, another way to specify the to-be-transformed column,
will be disregarded if col.id is specified}

\item{partition:}{an interger or a vectorof numerics, based on the type, specified how
to transfer.}

\item{level.name:}{a vactor of strings, the category that will be assigned to each group of
numerical variable}

\item{type:}{a character that can take values of "quantile", "equal", and "criteria", see
"Details", default "quantile"}
}
\value{
Categorical vairable which is transformed from the assigned numerical variable
based on the specified conditions.
}
\description{
Transform a numerical column into a categorical column, based on specified conditions.
}
\details{
If partition is an interger, "type" should be set to equal,
which means the numerical variable will be separated into equal groups.
If partition is a vector and sums up to 1,then type should be set to "quantile",
which means the cut will be based on those specified quantiles.
Otherwise set type to "criteria", which means groupping will be based on whether
the element of numerical variable is less than or equal to criterias.
}
\examples{
data(macro_data_chn)
test_df=macro_data_chn
test_df$PPI[2:4]=NA
test_df$PPI
num2ctg(test_df,col.name = 'PPI',partition = c(0.2,0.5,0.3))
num2ctg(test_df,col.name = 'PPI',partition = c(98,100),
level.name = c('low','medium','high'),type='criteria')
num2ctg(test_df,col.name = 'PPI',partition = 3,
level.name = c('low','medium','high'),type='equal')
num2ctg(test_df,col.id = 4,partition = 3,
level.name = c('low','medium','high'),type='equal')
}
