create.cluster.heatmap <- function(
    DF,
    plt.height = 6,
    plt.width = 11,
    hm.cols = NULL,
    xaxis.col = NULL,
    filename = NULL,
    cluster.colours = NULL,
    ...
    ) {

    if (is.null(levels(DF$ID))) {
        DF$ID <- factor(DF$ID, levels = sort(unique(DF$ID)));
        }

    DF              <- droplevels(DF)[order(DF$clone.id, -abs(DF$CCF)), ];
    arr             <- data.frame.to.array(DF);
    snv.order       <- unique(DF[, c('SNV.id', 'clone.id')]);
    arr             <- arr[snv.order$SNV.id, levels(DF$ID)];

    if (is.null(cluster.colours)) {
        cluster.colours <- get.colours(levels(DF$clone.id), return.names = TRUE);
    } else {
        cluster.colours <- cluster.colours[levels(DF$clone.id)];
        }

    if (!is.null(xaxis.col)) {
        xaxis.label <- unique(DF[DF$SNV.id %in% rownames(arr), xaxis.col]);
        }

    hm <- create.ccf.heatmap(
        hm.array = arr,
        filename = NULL,
        cls.dim = 'none',
        hm.cols = hm.cols,
        ...
        );

    cov <- BoutrosLab.plotting.general::create.heatmap(
        x = t(cluster.colours[snv.order$clone.id]),
        input.colours = TRUE,
        clustering.method = 'none',
        grid.col = FALSE,
        print.colour.key = FALSE,
        resolution = 5000
        );

    legend.clone <- BoutrosLab.plotting.general::legend.grob(
        list(
            legend = list(
                title = 'Clones',
                labels = names(cluster.colours),
                colours = cluster.colours,
                border = 'black'
                ),
            legend = list(
                title = 'CCF',
                labels =  c(0, round(max(arr), digits = 2)),
                colours = if (is.null(hm.cols)) c('white', 'blue') else hm.cols,
                border = 'black',
                continuous = TRUE,
                size = 0.6
                )
            ),
        size = 1,
        title.cex = 0.75,
        label.cex = 0.6
        );

    return(BoutrosLab.plotting.general::create.multiplot(
        filename = filename,
        plot.objects = list(cov, hm),
        plot.layout = c(1, 2),
        panel.heights = c(1, 0.05),
        xaxis.lab = if (!is.null(xaxis.col)) xaxis.label else NULL,
        xaxis.cex = 0.6,
        xaxis.rot = 90,
        xaxis.fontface = 1,
        xaxis.tck = 0,
        yaxis.lab = list(NULL, colnames(arr)),
        yaxis.cex = 0.6,
        yaxis.tck = 0,
        yaxis.fontface = 1,
        y.spacing = 0.5,
        left.padding = 17,
        print.new.legend = TRUE,
        legend = list(right = list(
            fun = legend.clone
        )),
        height = plt.height,
        width = plt.width
        ));
    }
