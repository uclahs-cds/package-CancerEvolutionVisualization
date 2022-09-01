SRCGrob <- function(
    tree,
    genes = NULL,
    main = NULL,
    horizontal.padding = 0.1,
    scale1 = 1,
    scale2 = 1,
    yat = NULL,
    yaxis1.label = NULL,
    yaxis2.label = NULL,
    xlab.cex = 1.55,
    ylab.cex = 1.55,
    xaxis.cex = 1.45,
    yaxis.cex = 1.45,
    xaxis.label = 'CP',
    label.cex = NA,
    gene.cex = 0.85,
    main.y = NULL,
    main.cex = 1.7,
    node.radius = 0.1,
    node.col = 'grey29',
    seg1.col = 'black',
    seg2.col = 'green',
    line.lwd = 3,
    text.line.dist = 0.1,
    colour.scheme = CancerEvolutionVisualization::colours,
    draw.nodes = TRUE,
    add.normal = FALSE,
    normal.cex = 1,
    sig.shape = 3,
    label.nodes = TRUE,
    disable.polygons = FALSE,
    length.from.node.edge = TRUE,
    size.units = 'npc'
    ) {

    add.genes <- !is.null(genes);
    add.polygons <- !is.null(tree$CP) && !disable.polygons;
    text.on.nodes <- FALSE;
    text.line.dist <- prep.text.line.dist(text.line.dist);

    yat <- prep.yat(yat);

    yaxis.position <- if (is.null(yaxis2.label)) 'left' else {
        if (!is.null(yaxis1.label)) 'both' else 'right';
        };

    inputs <- prep.tree(
        tree,
        genes,
        yaxis.position,
        colour.scheme = colour.scheme
        );

    fixed.angle <- pi / 6;
    min.width <- get.plot.width(horizontal.padding);
    wid <- 1.2;
    spread <- 1;
    cluster.list <- NULL;

    axis.cex <- list(
        x = xaxis.cex,
        y = yaxis.cex
        );

    axis.label.cex <- list(
        x = xlab.cex,
        y = ylab.cex
        );

    tree.depth <- max(inputs$in.tree.df$tier);

    scale1 <- get.branch.length.scale(inputs$tree$length1, tree.depth, scale1);

    if (!is.null(inputs$tree$length2)) {
        scale2 <- get.branch.length.scale(inputs$tree$length2, tree.depth, scale2);
        }

    clone.out <- make.clone.tree.grobs(
        ccf.df = inputs$in.tree.df,
        tree = inputs$tree,
        text.df = inputs$text.df,
        node.radius = node.radius,
        scale1 = scale1,
        scale2 = scale2,
        yat = yat,
        wid = wid,
        line.lwd = line.lwd,
        length.from.node.edge = length.from.node.edge,
        seg1.col = seg1.col,
        seg2.col = seg2.col,
        add.polygons = add.polygons,
        sig.shape = sig.shape,
        spread = spread,
        fixed.angle = fixed.angle,
        add.genes = add.genes,
        text.on.nodes = text.on.nodes,
        text.line.dist = text.line.dist,
        gene.cex = gene.cex,
        yaxis.position = yaxis.position,
        yaxis1.label = yaxis1.label,
        yaxis2.label = yaxis2.label,
        axis.label.cex = axis.label.cex,
        axis.cex = axis.cex,
        xaxis.label = xaxis.label,
        min.width = min.width,
        draw.nodes = draw.nodes,
        label.nodes = label.nodes,
        node.col = node.col,
        label.cex = label.cex,
        cluster.list = cluster.list,
        add.normal = add.normal,
        normal.cex = normal.cex,
        main = main,
        main.cex = main.cex,
        main.y = main.y,
        size.units = size.units
        );

    out.tree <- gTree(
        children = package.clone.grobs(clone.out),
        vp = clone.out$vp,
        cl = 'SRCGrob'
        );

    return(out.tree);
    }
