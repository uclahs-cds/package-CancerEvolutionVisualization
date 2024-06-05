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
    save.plt.dir = NULL,
    multi.sample = FALSE,
    ...
    )
}
\arguments{
    \item{snv.df}{A data frame containing the SNV data. It must have columns 'chr', 'pos', and 'clone.id'. If \code{multi.sample = TRUE}, it must also have a column 'ID' specifying the sample ID for each SNV.}
    \item{genome.build}{The genome build to use. Defaults to \dQuote{GRCh37}.}
    \item{clone.order}{The order in which to plot the clones. If \code{NULL}, clones will be sorted alphabetically.}
    \item{clone.colours}{A named vector specifying the color to use for each clone. If \code{NULL}, colors will be automatically assigned.}
    \item{save.plt.dir}{Directory to save the plot in. If \code{NULL}, the plot will not be saved.}
    \item{multi.sample}{Logical indicating whether the input data contains multiple samples. Defaults to \code{FALSE}.}
    \item{...}{Additional arguments to be passed to \code{BoutrosLab.plotting.general::create.multipanelplot()}.}
}
\details{
This function preprocesses the input data frame, extracts chromosome information, and iterates over each sample to create a clone genome distribution plot. For each sample, it calculates the density of each clone across the genome and creates a scatter plot of the SNVs colored by clone ID and a density plot showing the density of each clone.
}
\value{A `grob` object.}
\author{Helena Winata, Selina Wu}

