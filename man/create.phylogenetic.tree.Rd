\name{create.phylogenetic.tree}
\alias{create.phylogenetic.tree}
\title{Phylogenetic Tree Plot}
\description{
    Create a phylogenetic tree plot using `SRCGrob`. The `width`, `heigth`, `units`, `res`, and `bg` arguments are passed to the `grDevices` function to save the plot to file.
    }
\usage{
create.phylogenetic.tree(
    tree,
    filename = NULL,
    width = 5,
    height = 5,
    units = 'in',
    res = 500,
    bg = 'white',
    ...
    )
}
\arguments{
    \item{tree}{Tree structure data.frame}
    \item{filename}{Filename for plot output, or if \code{NULL} displays the tree using \code{grid.draw}. Defaults to \code{NULL}.}
    \item{width}{Width of the plot. Defaults to 5.}
    \item{height}{Height of the plot. Defaults to 5.}
    \item{units}{Units of the plot. Defaults to \dQuote{in}.}
    \item{res}{Resolution of the plot. Defaults to 500.}
    \item{bg}{Background colour of the plot. Defaults to \dQuote{white}.}
    \item{...}{Pass through argument. See SRCGrob() for further details.}
}
\value{A `grob` of class "SRCGrob".}
\author{Helena Winata}
\seealso{
    \code{\link{SRCGrob}},
    \code{\link[grDevices]{pdf}},
    \code{\link[grDevices]{png}},
    \code{\link[grDevices]{tiff}},
    \code{\link[grDevices]{svg}}
    }

\examples{
# Simple Tree Plot
simple.tree <- data.frame(
    parent = c(NA, 1, 1)
    );
create.phylogenetic.tree(simple.tree);

# Non-numeric node.id
phy.dt <- data.frame(
    node.id = c('MRCA', 'C01', 'C02', 'C03', 5:7),
    parent = c(NA, 'MRCA', 'MRCA', 'MRCA', 'C02', 'C03', 'C03')
    );
create.phylogenetic.tree(phy.dt);

# Numeric disordered node.id with labels
phy.dt <- data.frame(
    node.id = c(1, 2, 5, 6, 4, 9, 10),
    parent = c(NA, 1, 1, 1, 5, 6, 6),
    label = c('MRCA', 'C01', 'C02', 'C03', 5:7)
    );
create.phylogenetic.tree(phy.dt);
}