create.cluster.heatmap <- function(
    DF,
    clone.colours = NULL,
    height = 6,
    width = 11,
    xaxis.col = NULL,
    legend.size = 3,
    legend.title.cex = 1.2,
    legend.label.cex = 1,
    filename = NULL,
    xlab.cex = 1.2,
    xaxis.cex = 1,
    xaxis.fontface = 'bold',
    y.spacing = 1,
    colour.scheme = c('white', 'blue'),
    ...
    ) {

    if (is.null(levels(DF$ID))) {
        DF$ID <- factor(DF$ID, levels = sort(unique(DF$ID)));
        }

    if (is.null(clone.colours)) {
        clone.colours <- get.colours(DF$clone.id, return.names = TRUE);
        }
    DF              <- droplevels(DF)[order(DF$clone.id, -abs(DF$CCF)), ];
    snv.order       <- unique(DF[, c('SNV.id', 'clone.id')]);
    arr             <- data.frame.to.array(DF);
    arr             <- arr[snv.order$SNV.id, levels(DF$ID)];

    if (!is.null(xaxis.col)) {
        xaxis.label <- unique(DF[DF$SNV.id %in% rownames(arr), xaxis.col]);
    } else {
        xaxis.label <- NULL;
        }

    hm <- create.ccf.heatmap(
        x = arr,
        cluster.dimensions = 'none',
        xlab.label = '',
        xaxis.lab = xaxis.label,
        colour.scheme = colour.scheme,
        ...
        );

    cov <- BoutrosLab.plotting.general::create.heatmap(
        x = t(clone.colours[snv.order$clone.id]),
        xlab.label = 'Mutations',
        xlab.cex =  xlab.cex,
        xaxis.cex = xaxis.cex,
        xaxis.fontface = xaxis.fontface,
        input.colours = TRUE,
        clustering.method = 'none',
        grid.col = FALSE,
        print.colour.key = FALSE,
        yaxis.tck = 0
        );

    legend.clone <- BoutrosLab.plotting.general::legend.grob(
        list(
            legend = list(
                title = 'Clones',
                labels = names(clone.colours),
                colours = clone.colours,
                border = 'black'
                ),
            legend = list(
                title = 'CCF',
                labels = c(min(arr), max(arr)),
                colours = colour.scheme,
                border = 'black',
                continuous = TRUE
                )
            ),
        size = legend.size,
        title.cex = legend.title.cex,
        label.cex = legend.label.cex
        );

    return(BoutrosLab.plotting.general::create.multipanelplot(
        filename = filename,
        plot.objects = list(hm, cov),
        layout.width = 1,
        layout.height = 2,
        plot.objects.heights = c(1, 0.2),
        legend = list(right = list(
            fun = legend.clone
        )),
        y.spacing = y.spacing,
        right.legend.padding = 0.5,
        height = height,
        width = width
        ));
    }
