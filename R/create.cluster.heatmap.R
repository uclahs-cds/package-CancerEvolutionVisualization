create.cluster.heatmap <- function(
    DF,
    plt.height = 6,
    plt.width = 11,
    hm.cols = NULL,
    xaxis.col = NULL,
    ...
    ) {

    if (is.null(levels(DF$ID))) {
        DF$ID <- factor(DF$ID, levels = sort(unique(DF$ID)));
        }
    DF              <- droplevels(DF)[order(DF$clone.id, -abs(DF$CCF)), ];
    arr             <- data.frame.to.array(DF);
    snv.order       <- unique(DF[, c('snv.id', 'clone.id')]);
    cls.colours     <- get.colours(DF$clone.id, return.names = TRUE);
    arr             <- arr[snv.order$snv.id, levels(DF$ID)];

    if (!is.null(xaxis.col)) {
        xaxis.label <- unique(DF[DF$snv.id %in% rownames(arr), xaxis.col]);
        }

    hm <- create.ccf.heatmap(
        hm.array = arr,
        fname = NULL,
        cls.dim = 'none',
        hm.cols = hm.cols,
        ...
        );

    cov <- BoutrosLab.plotting.general::create.heatmap(
        x = t(cls.colours[snv.order$clone.id]),
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
                labels = names(cls.colours),
                colours = cls.colours,
                border = 'black'
                ),
            legend = list(
                title = 'CCF',
                labels = c(min(arr), max(arr)),
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
        filename = NULL,
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
