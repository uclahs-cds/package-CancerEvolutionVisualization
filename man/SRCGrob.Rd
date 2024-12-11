\name{SRCGrob}
\alias{SRCGrob}
\title{Subclone Tree Plot}
\description{
Creates a phylogenetic tree using subclonal reconstruction data.
An optional node text data.frame can be used to annotate notable genes alonside branches.
}
\usage{
SRCGrob(
    tree,
    node.text = NULL,
    main = NULL,
    horizontal.padding = 0.1,
    scale1 = 1,
    scale2 = 1,
    yat = NULL,
    yaxis1.label = NULL,
    yaxis2.label = NULL,
    xlab.cex = 1.55,
    ylab.cex = 1.55,
    xaxis.cex = 1.45,
    yaxis.cex = 1.45,
    xaxis.label = 'CCF',
    label.cex = NA,
    node.text.cex = 0.85,
    main.y = NULL,
    main.cex = 1.7,
    node.text.line.dist = 0.1,
    polygon.colour.scheme = 'grey',
    add.normal = FALSE,
    use.radians = FALSE,
    normal.cex = 1,
    label.nodes = TRUE,
    disable.polygons = FALSE,
    polygon.shape = 3,
    polygon.width = 1.2,
    length.from.node.edge = TRUE,
    size.units = 'npc',
    scale.bar = FALSE,
    scale.bar.coords = c(0.5, 1),
    scale.size.1 = NA,
    scale.size.2 = NA,
    scale.padding = 1

    );
}
\arguments{
    \item{tree}{Tree structure data.frame}
    \item{node.text}{Dataframe for text labels to be displayed next to nodes}
    \item{main}{Main plot title}
    \item{horizontal.padding}{
    Increase/reduce the plot's horizontal padding proportionally.
    A positive value will expand the padding, and a negative value will reduce it.
    }
    \item{scale1}{
    Proportionally scale the values of the first branch length column in the tree input.
    }
    \item{scale2}{
    Proportionally scale the values of the second branch length column in the tree input.
    }
    \item{yat}{
    Specific values to be used for the y-axis ticks. A list is required, with each element
    corresponding to an axis.
    }
    \item{yaxis1.label}{Text label for the first, leftmost y-axis}
    \item{yaxis2.label}{Text label for the second, rightmost y-axis}
    \item{xlab.cex}{Font size for the x-axis label}
    \item{ylab.cex}{Font size for the y-axis labels}
    \item{xaxis.cex}{Font size for the x-axis tick labels}
    \item{yaxis.cex}{Font size for the y-axis tick labels}
    \item{xaxis.label}{Text label for the x-axis}
    \item{label.cex}{Font size for the node labels}
    \item{node.text.cex}{Font size for the node text}
    \item{main.y}{Move the main plot title position up or down}
    \item{main.cex}{Font size for the main plot title}
    \item{node.text.line.dist}{
    Distance between node text and tree branches (as a value between 0 and 1)
    }
    \item{colour.scheme}{Vector of colour values to be used for CCF polygons}
    \item{add.normal}{Adds a normal}
    \item{use.radians}{Unit to be used for 'angle' column (degrees or radians)}
    \item{normal.cex}{Font size within the normal 'box'}
    \item{label.nodes}{Enable/disable node labels}
    \item{disable.polygons}{Disables CCF polygon drawing (even when CCF values are provided)}
    \item{polygon.shape}{
    Changes the shape of the CCF shading.
    Lower values are smoother.
    }
    \item{polygon.width}{Width of the CCF shading}
    \item{length.from.node.edge}{
    Sets the branch length to be calculated from the edge of the node instead of the centre
    }
    \item{size.units}{Grid units to be used for all specific size/length parameters}
    \item{scale.bar}{Set to \code{TRUE} to add a scale bar instead of y-axis}
    \item{scale.bar.coords}{Coordinates for the scale bar placement \code{c(x.pos, y.pos)}}
    \item{scale.size.1}{Overide size of the first scale bar}
    \item{scale.size.2}{Overide size of the second scale bar}
    \item{scale.padding}{Padding between scale bars if more than 1 is present}
}
\value{A `grob` of class 'SRCGrob'}
\author{Dan Knight}
\examples{
# Simple Tree Plot
simple.tree <- data.frame(
    parent = c(NA, 1, 1)
    );

SRCGrob(simple.tree);


# Specify Branch Lengths
branch.lengths.tree <- data.frame(
    simple.tree,
    length1 = c(10, 20, 15)
    );

SRCGrob(branch.lengths.tree);


# Cellular Prevalence
CCF.tree <- data.frame(
    branch.lengths.tree,
    CCF = c(1, 0.3, 0.5)
    );

SRCGrob(CCF.tree);


# Scaled Branch Lengths
SRCGrob(
    branch.lengths.tree,
    scale1 = 0.8
    );


# Override Branch Angles in Degrees
degrees.tree <- data.frame(
    parent = c(NA, 1, 2),
    angle = c(NA, NA, 90)
    );

SRCGrob(degrees.tree);


# Override Branch Angles in Radians
radians.tree <- data.frame(
    parent = c(NA, 1, 2),
    angle = c(NA, NA, pi / 2)
    );

SRCGrob(
    radians.tree,
    use.radians = TRUE
    );


# Plot Title
SRCGrob(
    simple.tree,
    main = 'Example Plot'
    );


# Y-Axis Label
SRCGrob(
    branch.lengths.tree,
    yaxis1.label = 'SNVs'
    );


# Y-Axis Ticks
yaxis1.ticks <- seq(0, 10, 2);

SRCGrob(
    branch.lengths.tree,
    yat = list(yaxis1.ticks)
    );


# Scale Bar
SRCGrob(
    branch.lengths.tree,
    yaxis1.label = 'SNVs',
    scale.bar = TRUE
    );


# Normal
SRCGrob(
    simple.tree,
    add.normal = TRUE
    );


# Nodeless Mode
nodeless.tree <- data.frame(
    parent = c(NA, 1, 2, 2),
    draw.node = c(TRUE, FALSE, TRUE, TRUE)
    );

SRCGrob(nodeless.tree);
}
