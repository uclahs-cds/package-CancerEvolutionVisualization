\name{data.frame.to.array}
\alias{data.frame.to.array}
\title{Convert Data Frame to Array}
\description{
Converts a long-format data frame into a 2D matrix/array where:
- Rows correspond to unique values from the x.axis column
- Columns correspond to unique values from the y.axis column
- Cell values are taken from the value column
- NA values are replaced with 0
}
\usage{
data.frame.to.array(
    DF,
    value = 'CCF',
    x.axis = 'SNV.id',
    y.axis = 'ID'
    );
}
\arguments{
    \item{DF}{A data frame in long format containing the data to be reshaped.}
    \item{value}{The name of the column containing the values to be reshaped. Defaults to \code{'CCF'}.}
    \item{x.axis}{The name of the column to be used as row names in the output matrix. Defaults to \code{'SNV.id'}.}
    \item{y.axis}{The name of the column to be used as column names in the output matrix. Defaults to \code{'ID'}.}
}
\value{A 2D `matrix`.}
\author{Helena Winata}

\examples{
df <- data.frame(
    SNV.id = c(rep(paste0('snv', 1:3), 3)),
    ID = c(rep('S1', 3), rep('S2', 3), rep('S3', 3)),
    CCF = runif(9)
    );

arr <- data.frame.to.array(df);
}
