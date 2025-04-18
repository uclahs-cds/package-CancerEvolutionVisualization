create.cluster.heatmap <- function(
    DF,
    ccf.limits = NULL,
    sample.order = NULL,
    clone.order = NULL,
    clone.colours = NULL,
    height = 6,
    width = 11,
    xaxis.col = NULL,
    legend.size = 3,
    legend.title.cex = 1.2,
    legend.label.cex = 1,
    filename = NULL,
    xlab.label = 'SNVs',
    xlab.cex = 1.2,
    xaxis.cex = 1,
    xaxis.fontface = 'bold',
    y.spacing = 1,
    colour.scheme = c('white', 'blue'),
    plot.objects.heights = c(1, 0.2),
    ...
    ) {

    # Define the order of the samples
    if (is.null(sample.order)) {
        sample.order <- sort(unique(DF$ID));
        }
    DF$ID <- factor(DF$ID, levels = sample.order);

    # Define the order of the clones
    if (is.null(clone.order)) {
        clone.order <- sort(unique(DF$clone.id));
        }
    DF$clone.id <- factor(DF$clone.id, levels = clone.order);

    if (is.null(clone.colours)) {
        clone.colours <- get.colours(DF$clone.id, return.names = TRUE);
        }
    DF        <- droplevels(DF)[order(DF$clone.id, -abs(DF$CCF)), ];
    snv.order <- unique(DF[, c('SNV.id', 'clone.id')]);
    arr       <- data.frame.to.array(DF);
    arr       <- arr[snv.order$SNV.id, rev(levels(DF$ID))];

    if (!is.null(xaxis.col)) {
        xaxis.label <- unique(DF[DF$SNV.id %in% rownames(arr), xaxis.col]);
    } else {
        xaxis.label <- NULL;
        }

    if (!is.matrix(arr)) {
        arr <- t(arr);
        yaxis.label <- levels(DF$ID);
    } else {
        yaxis.label <- colnames(arr);
        }

    if (!is.null(ccf.limits)) {
        if (length(ccf.limits) != 2) {
            stop('ccf.limits must be a vector of length 2');
            }
        arr[arr < ccf.limits[1]] <- ccf.limits[1];
        arr[arr > ccf.limits[2]] <- ccf.limits[2];
        }

    hm <- create.ccf.heatmap(
        x = arr,
        cluster.dimensions = 'none',
        xlab.label = '',
        xaxis.lab = xaxis.label,
        yaxis.lab = yaxis.label,
        colour.scheme = colour.scheme,
        ...
        );

    cov <- BoutrosLab.plotting.general::create.heatmap(
        x = t(clone.colours[snv.order$clone.id]),
        xlab.label = xlab.label,
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
                title = 'CCF',
                labels = c(signif(min(arr), 2), rep('', legend.size), signif(max(arr), 2)),
                colours = colour.scheme,
                border = 'black',
                continuous = TRUE,
                cex = legend.label.cex
                ),
            legend = list(
                title = 'Clone',
                labels = names(clone.colours),
                colours = clone.colours,
                border = 'black',
                cex = legend.label.cex
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
        plot.objects.heights = plot.objects.heights,
        legend = list(right = list(
            fun = legend.clone
        )),
        y.spacing = y.spacing,
        right.legend.padding = 0.5,
        height = height,
        width = width
        ));
    }
