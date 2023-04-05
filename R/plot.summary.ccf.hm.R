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
        colour.scheme = c('white', 'blue'),
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
                colours = c('white', 'blue'),
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
