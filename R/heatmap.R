plot.ccf.hm <- function(
    CCF.df,
    CCF.threshold = NULL,
    cluster.dim = 'both',
    cluster.method = 'complete',
    dist.method = 'euclidean',
    colour.scheme = NULL,
    xaxis.lab = NULL,
    xlab.label = 'Mutations',
    filename = NULL,
    ...
    ) {

    if (!is.null(CCF.threshold)) {
        CCF.df[CCF.df <= CCF.threshold] <- 0;
        }
    col.labels <- seq(0, 1, .2);
    sample.names <- colnames(CCF.df);

    heatmap.colours <- if (!is.null(colour.scheme)) {
        colour.scheme;
    } else {
        default.heatmap.colours();
        }

    hm <- BoutrosLab.plotting.general::create.heatmap(
        filename = filename,
        x = CCF.df,
        force.clustering = TRUE,
        cluster.dimensions = cluster.dim,
        clustering.method = cluster.method,
        rows.distance.method = dist.method,
        cols.distance.method = dist.method,
        xaxis.lab = xaxis.lab,
        xlab.label = xlab.label,
        xlab.cex = 1,
        xaxis.cex = 0.6,
        xaxis.fontface = 1,
        xaxis.rot = 90,
        yaxis.lab = sample.names,
        ylab.cex = 1,
        yaxis.cex = 0.6,
        yaxis.fontface = 1,
        colourkey.cex = 0.6,
        colour.scheme = heatmap.colours,
        left.padding = 1,
        right.padding = 1,
        resolution = 3000,
        width = 9,
        height = 5,
        colourkey.labels.at = col.labels,
        ...
        );

    return(hm);
    }

plot.cluster.hm <- function(
    cluster.df,
    plt.height = 6,
    plt.width = 11,
    colour.scheme = NULL,
    xaxis.col = NULL,
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
    cluster.colours <- get.colours(cluster.df$clone.id, return.names = TRUE);
    arr <- arr[snv.order$SNV.id, levels(cluster.df$ID)];

    heatmap.colours <- if (!is.null(colour.scheme)) {
        colour.scheme;
    } else {
        default.heatmap.colours();
        }

    if (!is.null(xaxis.col)) {
        xaxis.label <- unique(cluster.df[cluster.df$SNV.id %in% rownames(arr), xaxis.col]);
        }

    hm <- plot.ccf.hm(
        CCF.df = arr,
        cluster.dim = 'none',
        colour.scheme = heatmap.colours,
        ...
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

    plt <- BoutrosLab.plotting.general::create.multiplot(
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
        );
    return(plt);
    }

plot.summary.ccf.hm <- function(
    mutation.df,
    CCF.threshold = 0,
    filename = NULL
    ) {

    median.ccf <- aggregate(
        mutation.df$CCF,
        by = list(mutation.df$ID, mutation.df$clone.id),
        FUN = median
        );

    colnames(median.ccf) <- c('ID', 'clone.id', 'median.CCF');

    arr <- data.frame.to.array(
        median.ccf,
        value = 'median.CCF',
        x.axis = 'clone.id',
        y.axis = 'ID'
        );
    arr[arr <= CCF.threshold] <- 0;

    filtered.CCFs <- mutation.df$CCF > 0;
    SNV.per.clone <- aggregate(SNV.id ~ clone.id, mutation.df[filtered.CCFs, ], FUN = length);
    colnames(SNV.per.clone) <- c('clone.id', 'num.SNV');

    SNV.per.sample <- aggregate(SNV.id ~ ID, mutation.df[filtered.CCFs, ], FUN = length);
    colnames(SNV.per.sample) <- c('ID', 'num.SNV');

    heatmap.colours <- default.heatmap.colours();
    barplot.padding.percentage <- 0.05;

    max.clone.SNV <- max(SNV.per.clone$num.SNV);

    clone.bar <- BoutrosLab.plotting.general::create.barplot(
        formula = num.SNV ~ clone.id,
        data = SNV.per.clone,
        yaxis.cex = 0,
        xaxis.lab = rep('', nrow(arr)),
        xaxis.cex = 0,
        ylimits = c(
            -(max.clone.SNV * barplot.padding.percentage),
            max.clone.SNV * (1 + barplot.padding.percentage)
            ),
        resolution = 50
        );

    max.sample.SNV <- max(SNV.per.sample$num.SNV);

    sample.bar <- BoutrosLab.plotting.general::create.barplot(
        formula = ID ~ num.SNV,
        data = SNV.per.sample,
        xlab.label = 'SNV per sample',
        xlimits = c(
            -(max.sample.SNV * barplot.padding.percentage),
            max.sample.SNV * (1 + barplot.padding.percentage)
            ),
        ylab.label = NULL,
        yaxis.lab = rep('', length(arr)),
        yaxis.cex = 0,
        resolution = 50,
        plot.horizontal = TRUE
        );

    hm <- BoutrosLab.plotting.general::create.heatmap(
        x = arr,
        cluster.dimensions = 'none',
        xlab.cex = 1,
        xlab.label = 'Clone ID',
        xaxis.lab = rownames(arr),
        xaxis.cex = 0.6,
        xaxis.fontface = 1,
        xaxis.rot = 90,
        ylab.cex = 1,
        ylab.label = 'Sample ID',
        yaxis.lab = colnames(arr),
        yaxis.cex = 0.6,
        yaxis.fontface = 1,
        print.colour.key = FALSE,
        colour.scheme = heatmap.colours,
        left.padding = 1,
        right.padding = 1,
        width = 9,
        height = 5
        );

    legend.ccf <- BoutrosLab.plotting.general::legend.grob(
        list(
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

    plt <- BoutrosLab.plotting.general::create.multiplot(
        filename = filename,
        plot.objects = list(hm, sample.bar, clone.bar),
        plot.layout = c(2, 2),
        layout.skip = c(FALSE, FALSE, FALSE, TRUE),
        panel.heights = c(0.3, 1),
        panel.widths = c(1, 0.2),
        plot.labels.to.retrieve = 1:3,
        xlab.label = c('\t', 'Clone ID', '\t', '\t', 'SNV per sample'),
        xlab.cex = 0.7,
        xaxis.cex = 0.6,
        xaxis.tck = 0.4,
        xaxis.rot = 90,
        xaxis.fontface = 1,
        xlab.to.xaxis.padding = - 0.5,
        ylab.label = c( 'SNV per clone', '\t', '\t', 'Sample ID', '\t'),
        ylab.padding = 8,
        ylab.cex = 0.7,
        yaxis.cex = 0.6,
        yaxis.tck = 0.4,
        yaxis.fontface = 1,
        x.spacing = c(0),
        y.spacing = c(-0.5),
        left.padding = 10,
        bottom.padding = 3,
        merge.legends = FALSE,
        print.new.legend = TRUE,
        legend = list(right = list(
            fun = legend.ccf
        )),
        height = 6,
        width = 11
        )
    return(plt);
    }

default.heatmap.colours <- function() {
    return(c('white', 'blue'))
    }
