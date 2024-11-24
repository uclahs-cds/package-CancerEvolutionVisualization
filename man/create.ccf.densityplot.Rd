\name{create.ccf.densityplot}
\alias{create.ccf.densityplot}
\title{CCF Density Plot}
\description{
Creates a density plot of cancer cell fraction (CCF) distribution across a sample.
}
\usage{
create.ccf.densityplot(
    x,
    filename = NULL,
    clone.colours = NULL,
    breaks = 100,
    xlab.label = 'CCF',
    ylab.label = 'SNV Density',
    xlimits = c(0, 1.5),
    xat = seq(0, 1.5, 0.25),
    legend.size = 3,
    legend.title.cex = 1.2,
    legend.label.cex = 1,
    legend.x = 0.8,
    legend.y = 0.9,
    height = 6,
    width = 10,
    size.units = 'in',
    resolution = 1000,
    ...
    );
}
\arguments{
    \item{x}{A data-frame with the following column names: 'SNV.id', 'clone.id', 'CCF'.}
    \item{filename}{Filename for tiff output, or if NULL returns the trellis object itself. Defaults to \code{NULL}.}
    \item{clone.colours}{Named list to provide a colour scheme for the clone ID covariate bar. If NULL, colours will be randomly generated. Defaults to \code{NULL}.}
    \item{breaks}{Number of breaks for the histogram. Defaults to 100.}
    \item{xlab.label}{Defaults to \dQuote{CCF}.}
    \item{ylab.label}{Defaults to \dQuote{SNV Density}.}
    \item{x.limits}{Limits for the x-axis. Defaults to \code{c(0, 1.5)}.}
    \item{xat}{Positions for the x-axis labels. Defaults to \code{seq(0, 1.5, 0.25)}.}
    \item{legend.size}{Width of the legend boxes in 'character' units. Defaults to 3}
    \item{legend.title.cex}{Size of titles in the legends. Defaults to 1.2}
    \item{legend.label.cex}{Size of text labels in the legends. Defaults to 1}
    \item{legend.x}{x position of the legend. Defaults to 0.8}
    \item{legend.y}{y position of the legend. Defaults to 0.9}
    \item{height}{Height of the plot. Defaults to 6}
    \item{width}{Width of the plot. Defaults to 10}
    \item{size.units}{Units for the height and width. Defaults to \dQuote{in}.}
    \item{resolution}{Resolution of the plot. Defaults to 1000}
    \item{...}{Pass through argument. See BoutrosLab.plotting.general::create.histogram() for further details.}
}
\value{A `grob` object of the heatmap.}
\author{Helena Winata}
\seealso{\code{\link[BoutrosLab.plotting.general]{create.histogram}}, \code{\link[BoutrosLab.plotting.general]{create.scatterplot}}}
