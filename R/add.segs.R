get.seg.coords <- function(
    tree,
    v,
    offset = 0,
    node.radius = 0,
    scale1 = NULL
    ) {

    # Calculate offset based on the line width
    offset <- offset / scale1 / 2;

    tree.segs <- adply(
        tree,
        .margins = 1,
        .fun = function(x) {
            if (x$parent == -1) {
                basey <- 0;
                basex <- 0;
            } else {
                basey <- v$y[v$id == x$parent];
                basex <- v$x[v$id == x$parent];
                }

            tipy <- basey + x$length1 * cos(x$angle);
            tipx <- basex + x$length1 * sin(x$angle);

            return(data.frame(basex, basey, tipx, tipy));
            }
        );

    tree.out <- list();
    second.tree.segs.adjusted <- NULL;

    tree.segs.adjusted <- tree.segs;
    offset.x <- offset * cos(tree.segs.adjusted$angle);
    offset.y <- offset * sin(tree.segs.adjusted$angle);

    tree.segs.adjusted$basey <- tree.segs.adjusted$basey + offset.y;
    tree.segs.adjusted$tipy <- tree.segs.adjusted$tipy + offset.y;
    tree.segs.adjusted$basex <- tree.segs.adjusted$basex - offset.x;
    tree.segs.adjusted$tipx <- tree.segs.adjusted$tipx - offset.x;

    if (length(grep('length',colnames(tree))) == 4) {
        second.tree.segs.adjusted <- tree.segs;
        second.tree.segs.adjusted$tipy <- (
            second.tree.segs.adjusted$basey
            + second.tree.segs.adjusted$length2.c
            * cos(second.tree.segs.adjusted$angle)
            );
        second.tree.segs.adjusted$tipx <- (
            second.tree.segs.adjusted$basex
            + second.tree.segs.adjusted$length2.c
            * sin(second.tree.segs.adjusted$angle)
            );

        second.tree.segs.adjusted$basey <- second.tree.segs.adjusted$basey - offset.y;
        second.tree.segs.adjusted$tipy <- second.tree.segs.adjusted$tipy - offset.y;
        second.tree.segs.adjusted$basex <- second.tree.segs.adjusted$basex + offset.x;
        second.tree.segs.adjusted$tipx <- second.tree.segs.adjusted$tipx + offset.x;

        second.tree.segs.adjusted <- second.tree.segs.adjusted[
            which(second.tree.segs.adjusted$basey != second.tree.segs.adjusted$tipy), ];
        }

    tree.out <- list(
        tree.segs = tree.segs.adjusted,
        tree.segs2 = second.tree.segs.adjusted
        );

    return(tree.out);
    }

add.tree.segs <- function(
    clone.out,
    node.radius,
    line.lwd,
    scale1,
    seg1.col,
    seg2.col
    ) {

    offset <- line.lwd / 96;

    if (!('length2' %in% colnames(clone.out$tree))) {
        offset <- 0
        }

    tree.out <- get.seg.coords(clone.out$tree, clone.out$v, offset, node.radius, scale1);

    tree.segs1 <- tree.out[[1]];
    tree.segs2 <- tree.out[[2]];

    out <- list();

    out$tree.segs1 <- segmentsGrob(
        name = 'tree.segs.1',
        x0 = tree.segs1$basex,
        y0 = tree.segs1$basey,
        x1 = tree.segs1$tipx,
        y1 = tree.segs1$tipy,
        default.units = 'native',
        gp = gpar(
            col = clone.out$v$edge.colour.1,
            lwd = clone.out$v$edge.width.1,
            lty = clone.out$v$edge.type.1
            )
        );

    if (!is.null(tree.segs2)) {
        tree.segs2 <- tree.segs2[which(tree.segs2$basey != tree.segs2$tipy), ];

        if (nrow(tree.segs2) > 0) {
            out$tree.segs2 <- segmentsGrob(
                name = 'tree.segs.2',
                x0 = tree.segs2$basex,
                y0 = tree.segs2$basey,
                x1 = tree.segs2$tipx,
                y1 = tree.segs2$tipy,
                default.units = 'native',
                gp = gpar(
                    col = clone.out$v$edge.colour.2,
                    lwd = clone.out$v$edge.width.2,
                    lty = clone.out$v$edge.type.2
                    )
                );
            }
        }

    clone.out$grobs <- c(clone.out$grobs, out)
    }
