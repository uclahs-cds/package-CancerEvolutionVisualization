adjust.lengths <- function(x, length.cols, node.df) {
    adjusted <- list();
    for (column in length.cols) {
        if (x[column] > 0) {
            length.adj <- x[column];

            #  Max
            if (x[column] == x[length.cols[length(length.cols)]]) {
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
    if (is.null(node.df$node.radius)) {
        node.radius <- node.radius / scale1;
        node.df$node.radius <- rep(node.radius, nrow(node.df));
        }

    node.df$node.radius[node.df$id == -1] <- 0;
    node.df[!node.df$draw.node, 'node.radius'] <- 0;
    length.cols <- grep('length', colnames(tree));

    tree.adj <- apply(
        tree,
        MARGIN = 1,
        FUN = function(x) adjust.lengths(x, length.cols, node.df)
        );
    tree.adj <- do.call('rbind', tree.adj);
    rownames(tree.adj) <- rownames(tree);
    tree.adj <- cbind(tree, tree.adj);

    tree$length <- tree.adj$length.adj;
    tree$length1 <- tree.adj$length1.adj;
    tree$length2.c <- tree.adj$length2.c.adj;

    return(tree);
    }
