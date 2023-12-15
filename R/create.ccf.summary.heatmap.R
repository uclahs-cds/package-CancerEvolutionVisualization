create.ccf.summary.heatmap <- function(
    mutation.df,
    CCF.threshold = 0,
    clone.order = NULL,
    sample.order = NULL,
    colour.scheme = NULL,
    plt.height = 6,
    plt.width = 11,
    filename = NULL,
    ...
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

    # get data for barplots
    filtered.CCFs <- mutation.df$CCF > 0;
    SNV.per.clone <- aggregate(SNV.id ~ clone.id, mutation.df[filtered.CCFs, ], FUN = length);
    colnames(SNV.per.clone) <- c('clone.id', 'num.SNV');

    SNV.per.sample <- aggregate(SNV.id ~ ID, mutation.df[filtered.CCFs, ], FUN = length);
    colnames(SNV.per.sample) <- c('ID', 'num.SNV');

    heatmap.colours <- if (!is.null(colour.scheme)) {
        colour.scheme;
    } else {
        default.heatmap.colours();
        }

    if (!is.null(clone.order) & !is.null(sample.order)) {
        arr                 <- arr[clone.order, sample.order];
        SNV.per.clone$clone.id   <- factor(SNV.per.clone$clone.id, levels = clone.order);
        SNV.per.sample$ID        <- factor(SNV.per.sample$ID, levels = sample.order);
        }

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
        print.colour.key = FALSE,
        colour.scheme = heatmap.colours
        );

    legend.ccf <- BoutrosLab.plotting.general::legend.grob(
        list(
            legend = list(
                title = 'CCF',
                labels = c(min(arr), max(arr)),
                colours =  heatmap.colours,
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
        merge.legends = FALSE,
        print.new.legend = TRUE,
        legend = list(right = list(
            fun = legend.ccf
        )),
        height = plt.height,
        width = plt.width,
        ...
        ));
    }
