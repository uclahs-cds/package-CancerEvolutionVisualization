create.cluster.heatmap <- function(
    cluster.df,
    colour.scheme = NULL,
    cluster.colours = NULL,
    xaxis.col = NULL,
    xlab.label = 'Mutations per Clone',
    ylab.label = 'Sample ID',
    plt.height = 6,
    plt.width = 11,
    filename = NULL,
    ...
    ) {

    if (is.null(levels(cluster.df$ID))) {
        cluster.df$ID <- factor(
            cluster.df$ID,
            levels = sort(unique(cluster.df$ID))
            );
        }

    cluster.df <- droplevels(cluster.df)[order(cluster.df$clone.id, -abs(cluster.df$CCF)), ];
    arr <- data.frame.to.array(cluster.df);
    snv.order <- unique(cluster.df[, c('SNV.id', 'clone.id')]);
    arr <- arr[snv.order$SNV.id, levels(cluster.df$ID)];

    cluster.colours <- if (is.null(cluster.colours)) {
        get.colours(cluster.df$clone.id, return.names = TRUE);
    } else {
        # check if cluster.colours is a named vector that corresponds to clone.id
        if (!all(levels(cluster.df$clone.id) %in% names(cluster.colours))) {
            stop('cluster.colours must be a named vector that corresponds to clone.id');
            }
        cluster.colours[levels(cluster.df$clone.id)];
        }

    heatmap.colours <- if (!is.null(colour.scheme)) {
        colour.scheme;
    } else {
        default.heatmap.colours();
        }

    if (!is.null(xaxis.col)) {
        xaxis.label <- unique(cluster.df[cluster.df$SNV.id %in% rownames(arr), xaxis.col]);
        }

    hm <- create.ccf.heatmap(
        CCF.array = arr,
        cluster.dim = 'none',
        colour.scheme = colour.scheme,
        ...
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
                labels = c(min(arr), max(arr)),
                colours = heatmap.colours,
                border = 'black',
                continuous = TRUE,
                size = 0.6
                )
            ),
        size = 1,
        title.cex = 0.75,
        label.cex = 0.6
        );

    # Suppress "three-colour scheme" warning with 3 clones.
    cov <- suppressWarnings(BoutrosLab.plotting.general::create.heatmap(
        x = t(cluster.colours[snv.order$clone.id]),
        input.colours = TRUE,
        clustering.method = 'none',
        grid.col = FALSE,
        print.colour.key = FALSE,
        resolution = 5000
        ));

    return(BoutrosLab.plotting.general::create.multiplot(
        filename = filename,
        plot.objects = list(cov, hm),
        plot.layout = c(1, 2),
        panel.heights = c(1, 0.05),
        xlab.label = xlab.label,
        xlab.cex = 1,
        xaxis.lab = if (!is.null(xaxis.col)) xaxis.label else NULL,
        xaxis.cex = 0.6,
        xaxis.rot = 90,
        xaxis.fontface = 1,
        xaxis.tck = 0,
        ylab.label = ylab.label,
        ylab.cex = 1,
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
