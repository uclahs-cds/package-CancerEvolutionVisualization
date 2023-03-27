plot.ccf.hm <- function(
    hm.array,
    ccf.thres = NULL,
    cls.dim = 'both',
    cls.method = 'complete',
    dist.method = 'euclidean',
    hm.cols = NULL,
    xaxis.lab = NULL,
    xlab.label = 'Mutations',
    ...
    ) {

    if (!is.null(ccf.thres)) {
        hm.array[hm.array <= ccf.thres] <- 0;
        }
    col.labels <- seq(0, 1, .2);
    
    heatmap.colours <- if (!is.null(hm.cols)) hm.cols else default.heatmap.colours();

    hm <- BoutrosLab.plotting.general::create.heatmap(
        filename = NULL,
        x = hm.array,
        force.clustering = TRUE,
        cluster.dimensions = cls.dim,
        clustering.method = cls.method,
        rows.distance.method = dist.method,
        cols.distance.method = dist.method,
        xaxis.lab = xaxis.lab,
        xlab.label = xlab.label,
        xlab.cex = 1,
        xaxis.cex = 0.6,
        xaxis.fontface = 1,
        xaxis.rot = 90,
        yaxis.lab = colnames(hm.array),
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
    
    heatmap.colours <- if (!is.null(hm.cols)) hm.cols else default.heatmap.colours();
    
    if (!is.null(xaxis.col)) {
        xaxis.label <- unique(DF[DF$snv.id %in% rownames(arr), xaxis.col]);
        }

    hm <- plot.ccf.hm(
        hm.array = arr,
        cls.dim = 'none',
        hm.cols = heatmap.colours,
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

plot.summary.ccf.hm <- function(
    DF,
    ccf.thres = 0,
    clone.order = NULL,
    sample.order = NULL
    ) {

    arr <- data.frame.to.array(
        DF = DF,
        value = 'median.ccf.per.sample',
        x.axis = 'clone.id'
        );
    arr[arr <= ccf.thres] <- 0;

    clone.df <- unique(DF[, c('clone.id', 'total.snv')]);
    sample.df <- aggregate(CCF ~ ID, data = DF[DF$CCF > 0, ], FUN = length);
    names(sample.df)[2] <- 'nsnv';
    
    heatmap.colours <- default.heatmap.colours();

    if (!is.null(clone.order) & !is.null(sample.order)) {
        arr                 <- arr[clone.order, sample.order];
        clone.df$clone.id   <- factor(clone.df$clone.id, levels = clone.order);
        sample.df$ID        <- factor(sample.df$ID, levels = sample.order);
        }

    clone.bar <- BoutrosLab.plotting.general::create.barplot(
        formula = total.snv ~ clone.id,
        data = clone.df,
        yaxis.cex = 0,
        xaxis.lab = rep('', nrow(arr)),
        xaxis.cex = 0,
        ylimits = c( - max(clone.df$total.snv) * 0.05, max(clone.df$total.snv) * 1.05),
        resolution = 50
        );

    sample.bar <- BoutrosLab.plotting.general::create.barplot(
        formula = ID ~ nsnv,
        data = sample.df,
        xlab.label = 'SNV per sample',
        xlimits = c( - max(sample.df$nsnv) * 0.05, max(sample.df$nsnv) * 1.05),
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

    return(BoutrosLab.plotting.general::create.multiplot(
        filename = NULL,
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
        ylab.padding = 13,
        ylab.cex = 0.7,
        yaxis.cex = 0.6,
        yaxis.tck = 0.4,
        yaxis.fontface = 1,
        x.spacing = c(- 3),
        y.spacing = c(- 1.5),
        left.padding = 10,
        bottom.padding = 3,
        # merge.legends = FALSE,
        print.new.legend = TRUE,
        legend = list(right = list(
            fun = legend.ccf
        )),
        height = 6,
        width = 11
        ));
    }

default.heatmap.colours <- function() {
    return(c('white', 'blue'))
    }