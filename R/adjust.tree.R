adjust.lengths <- function(x, cols, node.df) {
    out.df <- x;

    for (column in cols) {
        if (x[1, column] > 0) {
            length.adj <- x[1, column];

            #  Max
            if (x[1, column] == x[1, cols[length(cols)]]) {
                length.adj <- length.adj + node.df$node.radius[node.df$id == x$tip];
                }

            if (x$parent != -1) {
                length.adj <- length.adj + node.df$node.radius[node.df$id == x$parent];
                }

        } else {
            length.adj <- 0;
            }

        var.name <- paste0(names(x)[column], '.adj');
        out.df <- cbind(out.df, length.adj);
        colnames(out.df)[ncol(out.df)] <- var.name;
        }

    return(out.df);
    }

adjust.branch.lengths <- function(node.df, tree, node.radius, scale1) {
    if (is.null(node.df$node.radius)) {
        node.radius <- node.radius / scale1;
        node.df$node.radius <- rep(node.radius, nrow(node.df));
        }

    node.df$node.radius[node.df$id == -1] <- 0;
    node.df[!node.df$draw.node, 'node.radius'] <- 0;
    length.cols <- grep('length', colnames(tree));

    tree.adj <- adply(
        tree,
        .margins = 1,
        .fun = function(x) {
            adjust.lengths(x, length.cols, node.df);
            }
        );

    tree$length <- tree.adj$length.adj;
    tree$length1 <- tree.adj$length1.adj;
    tree$length2.c <- tree.adj$length2.c.adj;

    return(tree);
    }

adjust.tree <- function(in.tree.node.radius, tree.in, node.radius, scale.x.real) {
    if (is.null(in.tree.node.radius$node.radius)) {
        node.radius <- node.radius / scale.x.real;
        in.tree.node.radius$node.radius <- rep(node.radius, nrow(in.tree.node.radius));
        }

    in.tree.node.radius$node.radius[in.tree.node.radius$id == -1] <- 0;
    length.cols <- grep('length', colnames(tree.in));
    tree.adj <- adply(
        tree.in,
        .margins = 1,
        .fun = function(x) {
            adjust.lengths(x, length.cols, in.tree.node.radius);
            }
        );

    tree.in$length <- tree.adj$length.adj;
    tree.in$length1 <- tree.adj$length1.adj;
    tree.in$length2.c <- tree.adj$length2.c.adj;

    return(tree.in);
    }
