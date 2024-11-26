\name{create.clone.genome.distribution.plot}
\alias{create.clone.genome.distribution.plot}
\title{Create Clone Genome Distribution Plot}
\description{
This function creates a plot showing the distribution of clones across the genome. It generates a scatter plot of the SNVs colored by clone ID and a density plot showing the density of each clone across the genome. The function can handle both single and multi-sample inputs.
}
\usage{
create.clone.genome.distribution.plot(
    snv.df,
    genome.build = 'GRCh37',
    clone.order = NULL,
    clone.colours = NULL,
    filename = NULL,
    multi.sample = FALSE,
    alpha = 0.25,
    legend.x = 0.1,
    legend.y = 0.55,
    ...
    )
}
\arguments{
    \item{snv.df}{A data frame containing the SNV data. It must have columns 'chr', 'pos', and 'clone.id'. If \code{multi.sample = TRUE}, it must also have a column 'ID' specifying the sample ID for each SNV.}
    \item{genome.build}{The genome build to use. Defaults to \dQuote{GRCh37}.}
    \item{clone.order}{The order in which to plot the clones. If \code{NULL}, clones will be sorted alphabetically.}
    \item{clone.colours}{A named vector specifying the color to use for each clone. If \code{NULL}, colors will be automatically assigned.}
    \item{filename}{Directory or filepath to save the plot in. If \code{multi.sample = TRUE}, this must be a directory. if \code{multi.sample = FALSE}, this must be a filepath. If \code{NULL}, the plot will not be saved.}
    \item{multi.sample}{Logical indicating whether the input data contains multiple samples. Defaults to \code{FALSE}.}
    \item{alpha}{Transparency level of the points in the scatter plot. Defaults to 0.25.}
    \item{legend.x}{Legend parameter. Defaults to 0.1}
    \item{legend.y}{Legend parameter. Defaults to 0.55}
    \item{...}{Additional arguments to be passed to \code{BoutrosLab.plotting.general::create.multipanelplot()}.}
}
\details{
This function preprocesses the input data frame, extracts chromosome information, and iterates over each sample to create a clone genome distribution plot. For each sample, it calculates the density of each clone across the genome and creates a scatter plot of the SNVs colored by clone ID and a density plot showing the density of each clone.
}
\value{A `grob` object.}
\author{Helena Winata, Selina Wu}
\seealso{\code{\link[BoutrosLab.plotting.general]{legend.grob}}, \code{\link[BoutrosLab.plotting.general]{create.multipanelplot}}, \code{\link[BoutrosLab.plotting.general]{create.scatterplot}}, \code{\link[BoutrosLab.plotting.general]{create.densityplot}}}

\examples{
setseed(123);
data('SNV.rda');

create.clone.genome.distribution.plot(snv);

}
