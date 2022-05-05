create.test.tree <- function(tree, genes, sample, ...) {
    out <- SRCGrob(
        tree,
        genes,
        filename = NULL,
        scale2 =  0.5 / 1500,
        wid = 2,
        extra_len = 0.1,
        scale1 = 1 / 20,
        h_padding = 1,
        rad = 0.1,
        gene.cex = 0.85,
        fixed_angle = pi / 6,
        seg1.col = 'navy',
        seg2.col = 'gold',
        node_col = "grey40",
        sig_curve = 3,
        line.lwd = 4,
        xaxis_space_left = 0.1,
        xaxis_space_right = 0.1,
        yaxis1_interval = 10,
        min_width = 1,
        yaxis1_label = "PGA",
        yaxis2_label = "SNV",
        yaxis2_interval = 1000,
        spread = 1.1,
        xaxis_label = "CP",
        title = sample,
        title.cex = 1.55,
        title.y = 0.3,
        title.y.units = "inches",
        ...
        );
    
    return(out[[1]]);
    }
