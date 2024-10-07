\name{create.ccf.heatmap}
\alias{create.ccf.heatmap}
\title{Subclone Tree Plot}
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
    xlab.label = 'Mutations',
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
    \item{xlab.label}{Defaults to \dQuote{Mutations}.}
    \item{print.colour.key}{Defaults to \code{FALSE}.}
    \item{colour.scheme}{Defaults to \code{c('white', 'blue')}.}
    \item{...}{Pass through argument. See BoutrosLab.plotting.general::create.heatmap() for further details.}
}
\value{A `grob` object of the heatmap.}
\author{Helena Winata}
\seealso{\code{\link[BoutrosLab.plotting.general]{create.heatmap}}}