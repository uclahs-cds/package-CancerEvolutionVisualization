######################################################################
# calculate.coords.radial
#
# Description:
# - Calculates the coordinates for a radial layout of a tree or dendrogram.
#
# Arguments:
#  - x              A data frame or matrix containing the tree or dendrogram data.
#  - v              A data frame or matrix containing additional vertex information.
#  - length.colname The name of the column in x that contains the branch lengths.
#  - parent.id      The ID of the parent node.
#  - offset         The offset value for positioning the nodes.
#  - side           The side of the tree or dendrogram ("left" or "right").
#
# Returns:
#  - A data frame with the calculated base and tip coordinates (basex, basey, tipx, tipy).

calculate.coords.radial <- function(
    x,
    v,
    length.colname,
    parent.id,
    offset,
    side
    ) {

    angle <- x['angle'];
    offset.x.modifier <- offset.y.modifier <- 1;

    if (side == 'left') {
        offset.x.modifier <- -1;
    } else if (side == 'right') {
        offset.y.modifier <- -1;
    } else if (side == 'center') {
        stop(paste(
            'Side "center" only needed with > 3 segments.',
            'This will be supported in a future version.'
            ));
    } else {
        stop(paste(
            'Side must be one of "left", "right", or "center".',
            paste0('(received ', side, ').')
            ));
        }

    if (x['parent'] == -1) {
        basey <- 0;
        basex <- 0;
    } else {
        basey <- v[parent.id, 'y'];
        basex <- v[parent.id, 'x'];
        }

    dy <- x[length.colname] * cos(angle);
    dx <- x[length.colname] * sin(angle);

    offset.x <- offset * cos(angle) * offset.x.modifier;
    offset.y <- offset * sin(angle) * offset.y.modifier;

    tipx <- basex + dx + offset.x;
    tipy <- basey + dy + offset.y;

    basex <- basex + offset.x;
    basey <- basey + offset.y;

    return(data.frame(
        basex,
        basey,
        tipx,
        tipy
        ));
    }

######################################################################
# calculate.coords.dendrogram
#
# Description:
# - Calculates the coordinates for a dendrogram layout of a tree.
#
# Arguments:
#  - x               A data frame or matrix containing the tree data.
#  - v               A data frame or matrix containing additional vertex information.
#  - length.colname  The name of the column in x that contains the branch lengths.
#  - parent.id       The ID of the parent node.
#  - offset          The offset value for positioning the nodes.
#  - side            The side of the dendrogram ("left" or "right").
#
# Returns:
#  - A data frame with the calculated base and tip coordinates (basex, basey, tipx, tipy).
#
# Note:
#  - The function assumes that the tree or dendrogram has a binary structure.
#  - The function uses the branch lengths and angles to calculate the coordinates.
#  - If the "x.length" value is available in v, it will be used instead of calculating dx from the branch length and angle.

calculate.coords.dendrogram <- function(
    x,
    v,
    length.colname,
    parent.id,
    offset,
    side
    ) {

    angle <- x['angle'];
    offset.x.modifier <- offset.y.modifier <- 1;

    if (side == 'left') {
        offset.x.modifier <- -1;
    } else if (side == 'right') {
        # No Y offset needed
    } else if (side == 'center') {
        stop(paste(
            'Side "center" only needed with > 3 segments.',
            'This will be supported in a future version.'
            ));
    } else {
        stop(paste(
            'Side must be one of "left", "right", or "center".',
            paste0('(received ', side, ').')
            ));
        }

    if (x['parent'] == -1) {
        basey <- 0;
        basex <- 0;
    } else {
        basey <- v[parent.id, 'y'];
        basex <- v[parent.id, 'x'];
        }

    dy <- x[, length.colname];
    x.length <- v[v$tip == x['tip'], 'x.length'];
    dx <- if (is.na(x.length)) x['length'] * tan(angle) else x.length;

    offset.x <- offset * offset.x.modifier;
    basex <- basex + dx + offset.x;

    tipx <- basex;
    tipy <- basey + dy;

    return(data.frame(
        basex,
        basey,
        tipx,
        tipy
        ));
    }

###################################################################################################
# calculate.seg.coords
#
# Description:
#  - Calculates the coordinates of segments in a tree structure for plotting purposes. The 'calculate.seg.coords' function calculates the coordinates of segments in a tree structure based on the provided tree data frame and additional node information in 'v'. The function supports two modes of coordinate calculation: 'radial' and 'dendrogram'.

# Arguments:
#  - tree          A data frame representing the tree structure. Each row corresponds to a segment in the tree.
#  - v             A data frame or matrix containing additional information about the nodes in the tree.
#  - length.colname   A character string specifying the column name in 'v' that contains the length information for each node.
#  - offset        A numeric value specifying the offset to be applied to the coordinates.
#  - side          A character string specifying the side of the tree on which the segments should be plotted.

calculate.seg.coords <- function(
    tree,
    v,
    length.colname,
    offset,
    side
    ) {

    segs <- apply(
        tree,
        MARGIN = 1,
        FUN = function(x) {
            node.id <- which(v$id == x['tip']);
            parent.id <- which(v$id == x['parent']);

            coords <- if (v[node.id, 'mode'] == 'radial') {
                calculate.coords.radial(
                    x,
                    v,
                    length.colname = length.colname,
                    parent.id = parent.id,
                    offset = offset,
                    side = side
                    );
            } else {
                calculate.coords.dendrogram(
                    x,
                    v,
                    length.colname = length.colname,
                    parent.id = parent.id,
                    offset = offset,
                    side = side
                    );
                }

            return(coords);
            }
        );

    segs <- do.call('rbind', segs);
    rownames(segs) <- rownames(tree);
    segs <- cbind(tree, segs);
    return(segs);
    }

add.tree.segs <- function(
    clone.out,
    node.radius,
    line.lwd,
    scale1,
    seg1.col,
    seg2.col
    ) {

    offset <- line.lwd / 96 / scale1 / 2;

    if (!('length2' %in% colnames(clone.out$tree))) {
        offset <- 0;
        }

    tree.segs1 <- calculate.seg.coords(
            clone.out$tree,
            clone.out$v,
            length.colname = 'length1',
            offset = offset,
            side = 'left'
        );

    second.seg.colname <- 'length2.c';
    if (second.seg.colname %in% colnames(clone.out$tree)) {
        tree.segs2 <- calculate.seg.coords(
            clone.out$tree,
            clone.out$v,
            length.colname = second.seg.colname,
            offset = offset,
            side = 'right'
            );

        valid.segs <- tree.segs2$basey != tree.segs2$tipy & tree.segs2$basex == tree.segs2$tipx;
        tree.segs2 <- tree.segs2[valid.segs, ];
    } else {
        tree.segs2 <- NULL;
        }

    seg.grobs <- list();

    seg.grobs[[1]] <- segmentsGrob(
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
            seg.grobs[[2]] <- segmentsGrob(
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

    dendrogram.ids <- clone.out$v[clone.out$v$mode == 'dendrogram', 'id'];
    dendrogram.coords <- rbind(tree.segs1, tree.segs2);
    dendrogram.coords <- dendrogram.coords[dendrogram.coords$parent %in% dendrogram.ids, ];

    if (nrow(dendrogram.coords) > 0) {
        connector.segs <- get.dendrogram.connector.segs(dendrogram.coords);
        connector.gpar <- data.frame(t(sapply(
            connector.segs$i,
            function(i) clone.out$v[
                clone.out$v$id == i,
                c('connector.col', 'connector.width', 'connector.type')
                ]
            )));
        seg.grobs$connectors <- segmentsGrob(
            name = 'connector.segs',
            x0 = connector.segs$basex,
            y0 = connector.segs$basey,
            x1 = connector.segs$tipx,
            y1 = connector.segs$tipy,
            default.units = 'native',
            gp = gpar(
                col = as.character(connector.gpar$connector.col),
                lwd = as.numeric(connector.gpar$connector.width),
                lty = as.character(connector.gpar$connector.type)
                )
            );
        }
    clone.out$grobs <- c(clone.out$grobs, seg.grobs);
    }

get.dendrogram.connector.segs <- function(branch.coords) {
    tree.levels <- by(
        branch.coords[, c('basex', 'basey')],
        branch.coords$parent,
        function(row) {
            x.range <- range(row$basex);
            y <- unique(row$basey);
            if (length(y) > 1) {
                stop();
                }

            return(list(
                basex = x.range[1],
                tipx = x.range[2],
                basey = y,
                tipy = y
                ));
            }
        );

    # Cannot directly coerce to data.frame
    horizontal.branch.coords <- as.data.frame(t(sapply(tree.levels, function(x) x)));
    horizontal.branch.coords$i <- rownames(horizontal.branch.coords);

    horizontal.levels <- as.numeric(horizontal.branch.coords$basex) != as.numeric(horizontal.branch.coords$tipx);
    horizontal.branch.coords <- horizontal.branch.coords[horizontal.levels, ];

    if (nrow(horizontal.branch.coords) > 0) {
        for (missing.column in setdiff(colnames(branch.coords), colnames(horizontal.branch.coords))) {
            }
        }

    return(horizontal.branch.coords);
    }
