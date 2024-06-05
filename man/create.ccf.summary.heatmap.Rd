\name{create.ccf.summary.heatmap}
\alias{create.ccf.summary.heatmap}
\title{Subclone Tree Plot}
\description{
Creates a heatmap of cancer cell fraction (CCF) distribution across tumour samples with clone IDs as a covariate beneath the heatmap. Subplot parameters controls the appearance of the heatmap and barplots. See \code{BoutrosLab.plotting.general::create.barplot()} or \code{BoutrosLab.plotting.general::create.heatmap()}  for parameter description. Legend parameters are passed to \code{BoutrosLab.plotting.general::legend.grob()}.
}

\usage{
create.ccf.summary.heatmap <- function(
    DF,
    ccf.thres = 0,
    median.col = 'median.ccf.per.sample',
    clone.order = NULL,
    sample.order = NULL,
    hm.col.scheme = c('white', 'blue'),
    subplot.xlab.cex = 1.2,
    subplot.xaxis.cex = 1,
    subplot.xaxis.fontface = 'bold',
    subplot.xaxis.rot = 90,
    subplot.ylab.cex = 1.2,
    subplot.yaxis.cex = 1,
    subplot.yaxis.fontface = 'bold',
    hm.xaxis.rot = 90,
    legend.size = 3,
    legend.title.cex = 1.2,
    legend.label.cex = 1
    )
}
\arguments{
    \item{DF}{A data-frame with the following column names: 'ID', 'SNV.id', 'clone.id', 'CCF'.}
    \item{ccf.thres}{CCF threshold to be applied to the heatmap. Values below the threshold will be set to 0. Defaults to \code{NULL}}
    \item{median.col}{Defaults to \dQuote{median.ccf.per.sample}}
    \item{clone.order}{Defaults to \code{NULL}}
    \item{sample.order}{Defaults to \code{NULL}}
    \item{hm.col.scheme}{Heatmap colour scheme. Defaults to \code{c('white', 'blue')}}
    \item{subplot.xlab.cex}{Subplot parameter. Defaults to 1.2}
    \item{subplot.xaxis.cex}{Subplot parameter. Defaults to 1}
    \item{subplot.xaxis.fontface}{Subplot parameter. Defaults to \dQuote{bold}}
    \item{subplot.xaxis.rot}{Subplot parameter. Defaults to 90}
    \item{subplot.ylab.cex}{Subplot parameter. Defaults to 1.2}
    \item{subplot.yaxis.cex}{Subplot parameter. Defaults to 1}
    \item{subplot.yaxis.fontface}{Subplot parameter. Defaults to \dQuote{bold}}
    \item{hm.xaxis.rot}{Subplot parameter. Defaults to 90}
    \item{legend.size}{Legend parameter. Defaults to 3}
    \item{legend.title.cex}{Legend parameter. Defaults to 1.2}
    \item{legend.label.cex}{Legend parameter. Defaults to 1}
    \item{...}{Pass through argument. See BoutrosLab.plotting.general::create.multipanelplot() for further details.}

}
\value{A `grob` object of the summary plot.}
\author{Helena Winata}
