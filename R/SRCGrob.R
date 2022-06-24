SRCGrob <- function(
    tree,
    genes = NULL,
    filename = 'SRC_tree.pdf',
    scale1 = 0.05443424,
    scale2 = 0.5 / 362,
    wid = 1.2,
    line.lwd = 3,
    length.from.node.edge = TRUE,
    seg1.col = 'black',
    seg2.col = 'green',
    extra.len = 10,
    sig.shape = 3,
    spread = 1,
    gene.line.dist = 0.1,
    colour.scheme = colours,
    gene.cex = 0.85,
    genes.on.nodes = FALSE,
    yaxis.position = 'left',
    yaxis1.label = 'SNVs',
    yaxis2.label = NULL,
    xlab.cex = 1.55,
    ylab.cex = 1.55,
    xaxis.cex = 1.45,
    yaxis.cex = 1.45,
    yaxis1.interval = NA,
    yaxis2.interval = NA,
    ylimit = NULL,
    xaxis.label = NULL,
    horizontal.padding = 0.1,
    nodes = 'circle',
    node.radius = 0.1,
    label.nodes = TRUE,
    node.col = 'grey29',
    label.cex = NA,
    cluster.list = NULL,
    disable.polygons = FALSE,
    add.normal = FALSE,
    normal.cex = 1,
    main = NULL,
    main.y = NULL,
    main.cex = 1.7,
    size.units = 'npc'
    ) {

    add.genes <- !is.null(genes);
    add.polygons <- !is.null(tree$CP) && !disable.polygons;

    inputs <- prep.tree(
        tree,
        genes,
        yaxis.position,
        colour.scheme
        );

    fixed.angle <- pi / 6;
    min.width <- get.plot.width(horizontal.padding);

    axis.cex = list(
        x = xaxis.cex,
        y = yaxis.cex
        );

    axis.label.cex = list(
        x = xlab.cex,
        y = ylab.cex
        );

    clone.out <- make.clone.tree.grobs(
        ccf.df = inputs$in.tree.df,
        tree = inputs$tree,
        genes.df = inputs$genes.df,
        node.radius = node.radius,
        scale1 = scale1,
        scale2 = scale2,
        wid = wid,
        line.lwd = line.lwd,
        length.from.node.edge = length.from.node.edge,
        seg1.col = seg1.col,
        seg2.col = seg2.col,
        add.polygons = add.polygons,
        extra.len = extra.len,
        sig.shape = sig.shape,
        spread = spread,
        fixed.angle = fixed.angle,
        add.genes = add.genes,
        genes.on.nodes = genes.on.nodes,
        gene.line.dist = gene.line.dist,
        gene.cex = gene.cex,
        yaxis.position = yaxis.position,
        yaxis1.label = yaxis1.label,
        yaxis2.label = yaxis2.label,
        axis.label.cex = axis.label.cex,
        axis.cex = axis.cex,
        yaxis1.interval = yaxis1.interval,
        yaxis2.interval = yaxis2.interval,
        ylimit = ylimit,
        xaxis.label = xaxis.label,
        min.width = min.width,
        nodes = nodes,
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

    return(list(out.tree, clone.out));
    }
