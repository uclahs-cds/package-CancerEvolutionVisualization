create.phylogenetic.tree <- function(
    tree,
    filename = NULL,
    width = 5,
    height = 5,
    units = 'in',
    res = 500,
    bg = 'white',
    ...
    ) {

    if ('node.id' %in% colnames(tree)) {
        rownames(tree) <- tree$node.id;
        }

    plt <- SRCGrob(
        tree,
        ...
        );

    supported.extensions <- c('png', 'pdf', 'tiff', 'svg');
    if (!is.null(filename)) {
        save.plot(
            plt,
            filename,
            width = width,
            height = height,
            units = units,
            res = res,
            bg = bg
            );
    } else {
        grid.newpage();
        grid.draw(plt);
        }
    return(plt);
    }