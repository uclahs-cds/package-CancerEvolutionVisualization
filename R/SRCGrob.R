SRCGrob <- function(
    tree,
    node.text = NULL,
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
    node.text.cex = 0.85,
    main.y = NULL,
    main.cex = 1.7,
    node.text.line.dist = 0.1,
    colour.scheme = CancerEvolutionVisualization::colours,
    add.normal = FALSE,
    use.radians = FALSE,
    normal.cex = 1,
    label.nodes = TRUE,
    disable.polygons = FALSE,
    polygon.shape = 3,
    polygon.width = 1.2,
    length.from.node.edge = TRUE,
    size.units = 'npc'
    ) {

    add.node.text <- !is.null(node.text);
    add.polygons <- !is.null(tree$CP) && !disable.polygons;
    text.on.nodes <- FALSE;
    node.text.line.dist <- prep.text.line.dist(node.text.line.dist);

    yat <- prep.yat(yat);
    yaxis.position <- get.y.axis.position(colnames(tree));

    node.col <- 'grey40';

    inputs <- prep.tree(
        tree,
        node.text,
        colour.scheme = colour.scheme,
        use.radians = use.radians,
        default.node.colour = node.col
        );

    fixed.angle <- pi / 6;
    min.width <- get.plot.width(horizontal.padding);
    spread <- 1;
    cluster.list <- NULL;
    node.radius <- 0.1;

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
        wid = polygon.width,
        length.from.node.edge = length.from.node.edge,
        default.branch.width = 4,
        add.polygons = add.polygons,
        sig.shape = polygon.shape,
        spread = spread,
        fixed.angle = fixed.angle,
        add.node.text = add.node.text,
        text.on.nodes = text.on.nodes,
        node.text.line.dist = node.text.line.dist,
        node.text.cex = node.text.cex,
        yaxis.position = yaxis.position,
        yaxis1.label = yaxis1.label,
        yaxis2.label = yaxis2.label,
        axis.label.cex = axis.label.cex,
        axis.cex = axis.cex,
        xaxis.label = xaxis.label,
        min.width = min.width,
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

    out.tree$input.data <- list(
        tree = inputs$in.tree.df[-1, ], # Remove Normal node placeholder row
        text = inputs$text.df
        );
    return(out.tree);
    }
