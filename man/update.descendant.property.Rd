\name{update.descendant.property}
\alias{update.descendant.property}
\title{Update Tree Descendant Property}
\description{
Function to iteratively update a specified property for all descendants of a given parent node(s) in a tree structure. The function works recursively, meaning it will update values for all descendants, not just immediate children. If a parent_id has no children, the original data frame is returned unchanged.
}
\usage{
update.descendant.property(
    x,
    parent.id,
    property,
    value,
    overwrite = TRUE
    );
}
\arguments{
    \item{x}{A data frame representing a tree structure with at least the following columns: \code{parent} (parent node identifiers), and the column specified by \code{property}}
    \item{parent.id}{Character or numeric vector specifying the identifier(s) of the parent node(s)}
    \item{property}{Character string specifying the name of the column/property to be updated}
    \item{value}{The value to assign to the property column}
    \item{overwrite}{Logical; if \code{TRUE}, overwrites existing values; if \code{FALSE}, only fills in NA values. Defaults to \code{TRUE}}
}
\value{A `data.frame` with modified columns.}
\author{Helena Winata}

\examples{
tree <- data.frame(
    node.id = c('A', 'B', 'C', 'D', 'E', 'F'),
    parent = c(NA, 'A', 'B', 'A', 'C', 'E'),
    node.size = 1
    );

tree2 <- update.descendant.property(
    x = tree,
    parent.id = 'B',
    property = 'node.size',
    value = 2
    );
}
