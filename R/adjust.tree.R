adjust.lengths <- function(x, length.cols, node.df, max.length.col) {
    adjusted <- list();
    for (column in length.cols) {
        if (x[column] > 0) {
            length.adj <- x[column];

            # Only the longest of the parallel branches reaches the child node,
            # so only that one is extended by the child's radius. A shorter
            # parallel branch is meant to stop short of the node.
            if (x[column] == x[max.length.col]) {
                length.adj <- length.adj + node.df$node.radius[node.df$id == x['tip']];
                }

            if (x['parent'] != -1) {
                length.adj <- length.adj + node.df$node.radius[node.df$id == x['parent']];
                }
        } else {
            length.adj <- 0;
            }

        var.name <- paste0(names(x)[column], '.adj');
        adjusted[var.name] <- length.adj;
        }

    return(as.data.frame(adjusted));
    }

adjust.branch.lengths <- function(node.df, tree, node.radius, scale1) {
    # Reserve the space the node actually occupies, not its nominal radius.
    # add.node.ellipse() enlarges the node so its label fits, so reserving
    # node.radius * node.size left every node overlapping the end of its edge
    # and made the drawn branch shorter than the value it encodes.
    # get.node.ellipse.size() is shared with the node grob to keep the two in
    # step. Branch lengths are in scale1 units, hence the division.
    node.df$node.radius <- get.node.ellipse.size(
        node.radius,
        node.df$node.size,
        get.node.plot.labels(node.df)
        ) / scale1;

    node.df$node.radius[node.df$id == -1] <- 0;
    node.df[!node.df$draw.node, 'node.radius'] <- 0;
    length.cols <- grep('length', colnames(tree));

    # make.clone.tree.grobs() sets 'length' to max(length1, length2.c) - the
    # branch that actually reaches the node. Name it rather than relying on it
    # happening to be the last matched column.
    max.length.col <- match('length', colnames(tree));
    if (is.na(max.length.col)) {
        max.length.col <- length.cols[length(length.cols)];
        }

    tree.adj <- apply(
        tree,
        MARGIN = 1,
        FUN = function(x) adjust.lengths(x, length.cols, node.df, max.length.col)
        );
    tree.adj <- do.call('rbind', tree.adj);
    rownames(tree.adj) <- rownames(tree);
    tree.adj <- cbind(tree, tree.adj);

    tree$length <- tree.adj$length.adj;
    tree$length1 <- tree.adj$length1.adj;

    # Guarded because assigning NULL would delete the column rather than leave
    # it alone. Single-branch trees have no 'length2.c'.
    if (!is.null(tree.adj$length2.c.adj)) {
        tree$length2.c <- tree.adj$length2.c.adj;
        }

    return(tree);
    }
