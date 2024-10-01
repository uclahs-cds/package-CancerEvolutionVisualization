
make.clone.tree.grobs <- function(
    ccf.df,
    tree,
    scale1,
    scale2,
    wid,
    yat,
    default.branch.width,
    length.from.node.edge,
    seg1.col,
    seg2.col,
    cluster.list,
    add.polygons,
    sig.shape,
    spread,
    fixed.angle,
    add.node.text,
    text.df,
    node.text.line.dist,
    node.text.cex,
    text.on.nodes,
    yaxis.position,
    yaxis1.label,
    yaxis2.label,
    axis.label.cex,
    axis.cex,
    xaxis.label,
    min.width,
    node.radius,
    label.nodes,
    node.col,
    label.cex,
    add.normal,
    normal.cex,
    main,
    main.cex,
    main.y,
    size.units,
    scale.bar,
    scale.bar.coords,
    scale.size.1,
    scale.size.2,
    ...
    ) {

    #initializing dataframe for subclones
    if ('excluded' %in% colnames(ccf.df)) {
        v <- ccf.df[!ccf.df$excluded,];
    } else {
        v <- ccf.df;
        v$excluded <- FALSE;
        }

    v <- v[order(v$id), ];
    no.ccf <- FALSE;

    if (!('ccf' %in% colnames(ccf.df)) || all(is.na(ccf.df$ccf)) || add.polygons == FALSE) {
        v$vaf <- NULL;
        v$vaf[v$parent == -1] <- 1;
        no.ccf <- TRUE;
    } else {
        v <- v[order(v$id),]
        v$vaf[!v$excluded] <- v$ccf[!v$excluded] / max(v$ccf[!v$excluded]);
        }

    if (all(is.null(ccf.df$colour))) {
        v$colour <- node.col
        }

    extra.len <- if (no.ccf) node.radius else node.radius * 4;

    v$x <- v$y <- v$len <- v$x.mid <- numeric(length(nrow(v)));
    v <- v[order(v$tier, v$parent), ];

    #initializing line segment dataframe and adjusting lengths to accomodate the node circles
    tree$angle <- numeric(length = nrow(tree));
    tree$angle[tree$parent == -1] <- 0;
    if ('length2' %in% colnames(tree)) {
        tree$length2.c <- tree$length2 / scale1 * scale2;

        tree$length <- apply(
            tree,
            MARGIN = 1,
            FUN = function(x) {
                max(x[c(3, 6)]);
                }
            );
    } else {
        tree$length <- tree$length1;
        }

    if (length.from.node.edge == TRUE) {
        tree <- adjust.branch.lengths(v, tree, node.radius, scale1);
        }

    extra.len <- extra.len * (1 / scale1);

    clone.out <- make.clone.polygons(
        v,
        tree,
        wid,
        scale1,
        scale2,
        extra.len,
        node.col,
        spread = spread,
        sig.shape = sig.shape,
        fixed.angle = fixed.angle,
        add.polygons = add.polygons,
        no.ccf = no.ccf
        );

    clone.out$no.ccf <- no.ccf;
    plot.size <- calculate.main.plot.size(
        clone.out,
        scale1,
        wid,
        min.width,
        node.radius
        );

    if (!no.ccf) {
        get.CP.polygons(clone.out);
        }

    add.tree.segs(clone.out, node.radius, default.branch.width, scale1, seg1.col, seg2.col);

    if (!is.null(cluster.list)) {
        message(paste(
            'Clustered pie nodes will be supported in a future version.',
            'Plain nodes will be used.'
            ));
        # TODO Implement pie nodes
        # add.pie.nodes(clone.out, node.radius, cluster.list);
        }

    add.node.ellipse(clone.out,node.radius, label.nodes, label.cex, scale1);

    if (add.normal == TRUE) {
        add.normal(clone.out,node.radius,label.cex, normal.cex)
        }

    if (yaxis.position != 'none' & scale.bar == FALSE) {
        add.axes(
            clone.out,
            yaxis.position,
            scale1 = scale1,
            scale2 = scale2,
            yat = yat,
            axis.label.cex = axis.label.cex,
            axis.cex = axis.cex,
            no.ccf = no.ccf,
            xaxis.label = xaxis.label,
            yaxis1.label = yaxis1.label,
            yaxis2.label = yaxis2.label
            );
        }

    if (scale.bar) {
        scale.lengths <- prep.scale.length(
            tree,
            scale.size.1,
            scale.size.2
            );

        add.scale.bar(
            clone.out,
            scale1,
            scale2,
            yaxis1.label = yaxis1.label,
            yaxis2.label = yaxis2.label,
            scale.length = scale.lengths,
            main.cex = axis.label.cex$y,
            label.cex = axis.cex$y,
            pos = scale.bar.coords
            );
        }

    if (add.node.text == TRUE & !is.null(text.df)) {
        node.text.grobs <- add.text2(
            clone.out$tree,
            text.df,
            label.nodes = text.on.nodes,
            line.dist = node.text.line.dist,
            main.y = clone.out$height,
            panel.height = clone.out$height,
            panel.width = clone.out$width,
            xlims = clone.out$xlims,
            ymax = clone.out$ymax,
            cex = node.text.cex,
            v = clone.out$v,
            axis.type = yaxis.position,
            node.radius = node.radius,
            scale = scale1,
            clone.out = clone.out,
            alternating = FALSE
            );

        clone.out$grobs <- c(clone.out$grobs, list(node.text.grobs));
        }

    if (!is.null(main)) {
        add.main(clone.out, main, main.cex, main.y, size.units);
        }

    return(clone.out);
    }
