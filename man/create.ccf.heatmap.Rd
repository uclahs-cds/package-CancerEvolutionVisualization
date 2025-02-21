\name{create.ccf.heatmap}
\alias{create.ccf.heatmap}
\title{CCF Distribution Heatmap}
\description{
Creates a heatmap of cancer cell fraction (CCF) distribution across tumour samples. The function is a wrapper around \code{BoutrosLab.plotting.general::create.heatmap()} with some changes in the default parameters. All parameter description are the same as in \code{BoutrosLab.plotting.general::create.heatmap()} except for \code{ccf.thres}.
}
\usage{
create.ccf.heatmap(
    x,
    cluster.dimensions = 'both',
    clustering.method = 'complete',
    distance.method = 'euclidean',
    xaxis.lab = '',
    xlab.label = 'SNVs',
    yaxis.lab = colnames(x),
    print.colour.key = FALSE,
    colour.scheme = c('white', 'blue'),
    ...
    )
}
\arguments{
    \item{x}{Either a data-frame or a matrix from which the heatmap is to created}
    \item{cluster.dimensions}{Defaults to \dQuote{both}.}
    \item{clustering.method}{Defaults to \dQuote{complete}.}
    \item{distance.method}{Defaults to \dQuote{euclidean}.}
    \item{xaxis.lab}{Defaults to an empty string.}
    \item{xlab.label}{Defaults to \dQuote{SNVs}.}
    \item{xaxis.lab}{Defaults to the column names of array \code{x}.}
    \item{print.colour.key}{Defaults to \code{FALSE}.}
    \item{colour.scheme}{Defaults to \code{c('white', 'blue')}.}
    \item{...}{Pass through argument. See BoutrosLab.plotting.general::create.heatmap() for further details.}
}
\value{A `grob` object of the heatmap.}
\author{Helena Winata}
\seealso{\code{\link[BoutrosLab.plotting.general]{create.heatmap}}}

\examples{
    set.seed(123);
    arr <- matrix(rnorm(500), nrow = 100, ncol = 5);
    rownames(arr) <- paste0('SNV.', 1:100);
    colnames(arr) <- paste0('Sample.', 1:5);
    create.ccf.heatmap(arr);
}
