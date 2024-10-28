\name{create.cluster.heatmap}
\alias{create.cluster.heatmap}
\title{Subclone Tree Plot}
\description{
Creates a heatmap of cancer cell fraction (CCF) distribution across tumour samples with clone IDs as a covariate beneath the heatmap.
}
\usage{
create.cluster.heatmap(
    DF,
    ccf.limits = NULL,
    clone.colours = NULL,
    height = 6,
    width = 11,
    xaxis.col = NULL,
    legend.size = 3,
    legend.title.cex = 1.2,
    legend.label.cex = 1,
    filename = NULL,
    xlab.cex = 1.2,
    xaxis.cex = 1,
    xaxis.fontface = 'bold',
    y.spacing = 1,
    colour.scheme = c('white', 'blue'),
    ...
    );
}
\arguments{
    \item{DF}{A data-frame with the following column names: 'ID', 'SNV.id', 'clone.id', 'CCF'.}
    \item{ccf.limits}{CCF limits to be applied to the heatmap. Must be a vector of length 2 for min and max thresholds. Defaults to \code{NULL}}
    \item{clone.colours}{Named list to provide a colour scheme for the clone ID covariate bar. If NULL, colours will be randomly generated. Defaults to \code{NULL}.}
    \item{height}{Defaults to 6}
    \item{width}{Defaults to 11}
    \item{xaxis.col}{Column in DF to extract x-axis labels from. Defaults to \code{NULL}.}
    \item{legend.size}{Width of the legend boxes in 'character' units. Defaults to 3}
    \item{legend.title.cex}{Size of titles in the legends. Defaults to 1.2}
    \item{legend.label.cex}{Size of text labels in the legends. Defaults to 1}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself. Defaults to \code{NULL}.}
    \item{xlab.cex}{Defaults to 1.2}
    \item{xaxis.cex}{Defaults to 1}
    \item{xaxis.fontface}{Defaults to \dQuote{bold}.}
    \item{y.spacing}{Spacing between heatmap and clone covariate bar. Defaults to 1}
    \item{colour.scheme}{Colour scheme for the heatmap. Defaults to \code{c('white', 'blue')}.}
    \item{...}{Pass through argument. See BoutrosLab.plotting.general::create.heatmap() for further details.}
}
\value{A `grob` object of the heatmap.}
\author{Helena Winata}
\seealso{\code{\link{create.ccf.heatmap}}, \code{\link[BoutrosLab.plotting.general]{create.heatmap}}}

\examples{
setseed(123);
load(file.path(system.file('data', package = 'CancerEvolutionVisualization'), 'SNV.Rda'));

create.cluster.heatmap(
    DF = snv,
    ccf.limits = c(0, 1),
    ylab.label = 'Samples'
    );
}
