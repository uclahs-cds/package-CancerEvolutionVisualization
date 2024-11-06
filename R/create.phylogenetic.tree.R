create.phylogenetic.tree <- function(
    tree,
    filename = NULL,
    ...
    ) {

    if (node.id %in% colnames(tree)) {
        rownames(tree) <- tree$node.id;
        }

    plt <- SRCGrob(
        tree,
        ...
        );

    supported.extensions <- c('png', 'pdf', 'tiff', 'svg');
    if (!is.null(filename)) {
        save.plot(plt, filename)
    } else {
        grid.newpage();
        grid.draw(plt);
        }
    return(tree);
    }