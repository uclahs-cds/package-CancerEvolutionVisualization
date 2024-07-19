add.segs3 <- function(
    tree,
    v,
    offset = 0,
    node.radius = 0,
    scale.x.real = NULL
    ) {

    # Calculate offset based on line width
    offset <- offset / scale.x.real / 2;

    tree.segs.adjusted <- tree.segs <- adply(
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

    if (length(grep('length', colnames(tree))) == 4) {
        tree.segs.adjusted <- adply(
            tree.segs,
            .margins = 1,
            .fun = function(r) {
                offset.x <- offset * cos(r$angle);
                offset.y <- offset * sin(r$angle);

                if (r$angle > 0) {
                    basey <- r$basey + offset.y;
                    tipy <- r$tipy + offset.y;
                } else {
                    basey <- r$basey + offset.y;
                    tipy <- r$tipy + offset.y;
                    }

                basex <- r$basex - offset.x;
                tipx <- r$tipx - offset.x;

                return(data.frame(basex, basey, tipx, tipy));
                }
            );

        tree.segs.adjusted <- tree.segs.adjusted[which(!(tree.segs.adjusted$basey == tree.segs.adjusted$tipy & tree.segs.adjusted$basex == tree.segs.adjusted$tipx)), ]

        second.tree.segs <- tree.segs;
        second.tree.segs$tipy <- second.tree.segs$basey + second.tree.segs$length2.c * cos(second.tree.segs$angle);
        second.tree.segs$tipx <- second.tree.segs$basex + second.tree.segs$length2.c * sin(second.tree.segs$angle);

        second.tree.segs.adjusted <- adply(
            second.tree.segs,
            .margins = 1,
            .fun = function(r) {
                offset.x  <- offset * cos(r$angle);
                offset.y  <- offset * sin(r$angle);

                if (r$angle > 0) {
                    basey <- r$basey - offset.y;
                    tipy <- r$tipy - offset.y;
                } else {
                    basey <- r$basey - offset.y;
                    tipy <- r$tipy - offset.y;
                    }

                basex <- r$basex + offset.x;
                tipx <- r$tipx + offset.x;

                return(data.frame(basex, basey, tipx, tipy));
                }
            );

        second.tree.segs.adjusted <- second.tree.segs.adjusted[which(!(second.tree.segs.adjusted$basey == second.tree.segs.adjusted$tipy & second.tree.segs.adjusted$basex == second.tree.segs.adjusted$tipx)),]
        }

    tree.out <- list(
        tree.segs = tree.segs.adjusted,
        tree.segs2 = second.tree.segs.adjusted
        );

    return(tree.out);
    }

calculate.coords.radial <- function(
    x,
    v,
    length.colname,
    parent.id,
    offset,
    side
    ) {

    angle <- x$angle;
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

    if (x$parent == -1) {
        basey <- 0;
        basex <- 0;
    } else {
        basey <- v$y[parent.id];
        basex <- v$x[parent.id];
        }

    dy <- x[, length.colname] * cos(angle);
    dx <- x[, length.colname] * sin(angle);

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

calculate.coords.dendrogram <- function(
    x,
    v,
    length.colname,
    parent.id,
    offset,
    side
    ) {

    angle <- x$angle;
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

    if (x$parent == -1) {
        basey <- 0;
        basex <- 0;
    } else {
        basey <- v$y[parent.id];
        basex <- v$x[parent.id];
        }

    dy <- x[, length.colname];
    x.length <- v[x$tip, 'x.length']
    dx <- if (is.na(x.length)) x[, 'length'] * tan(angle) else x.length;

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

calculate.seg.coords <- function(
    tree,
    v,
    length.colname,
    offset,
    side
    ) {

    segs <- adply(
        tree,
        .margins = 1,
        .fun = function(x) {
            node.id <- which(v$id == x$tip);
            parent.id <- which(v$id == x$parent);

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
