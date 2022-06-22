create.test.tree <- function(tree, genes, sample, ...) {
    out <- SRCGrob(
        tree,
        genes,
        filename = NULL,
        scale2 =  0.5 / 1500,
        wid = 2,
        extra.len = 0.1,
        scale1 = 1 / 20,
        node.radius = 0.1,
        gene.cex = 0.85,
        seg1.col = 'navy',
        seg2.col = 'gold',
        node.col = 'grey40',
        sig.curve = 3,
        line.lwd = 4,
        xaxis.space.left = 0.1,
        xaxis.space.right = 0.1,
        yaxis1.interval = 10,
        yaxis1.label = 'PGA',
        yaxis2.label = 'SNV',
        yaxis2.interval = 1000,
        spread = 1.1,
        xaxis.label = 'CP',
        title = sample,
        title.cex = 1.55,
        title.y = 0.3,
        title.y.units = 'inches',
        ...
        );

    return(out[[1]]);
    }
