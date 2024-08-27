create.ccf.summary.heatmap <- function(
    DF,
    ccf.thres = NULL,
    median.col = 'median.ccf.per.sample',
    clone.order = NULL,
    sample.order = NULL,
    hm.col.scheme = c('white', 'blue'),
    clone.colours = NULL,
    subplot.xlab.cex = 1.2,
    subplot.xaxis.cex = 1,
    subplot.xaxis.fontface = 'bold',
    subplot.xaxis.rot = 90,
    subplot.ylab.cex = 1.2,
    subplot.yaxis.cex = 1,
    subplot.yaxis.fontface = 'bold',
    hm.xaxis.rot = 90,
    legend.size = 3,
    legend.title.cex = 1.2,
    legend.label.cex = 1,
    ...
    ) {

    arr <- data.frame.to.array(
        DF = DF,
        value = median.col,
        x.axis = 'clone.id'
        );

    if (!is.null(ccf.thres)) {
        arr[arr <= ccf.thres] <- 0;
        }

    clone.df  <- aggregate(CCF ~ clone.id, data = DF[DF$CCF > 0, ], FUN = length);
    sample.df <- aggregate(CCF ~ ID, data = DF[DF$CCF > 0, ], FUN = length);
    names(sample.df)[2] <- names(clone.df)[2] <- 'nsnv';

    if (!is.null(clone.order) & !is.null(sample.order)) {
        arr                 <- arr[clone.order, sample.order];
        clone.df$clone.id   <- factor(clone.df$clone.id, levels = clone.order);
        sample.df$ID        <- factor(sample.df$ID, levels = sample.order);
        }

    if (!is.null(clone.colours)) {
        clone.covariate <- list(
            rect = list(
                fill = clone.colours[clone.df$clone.id],
                lwd = 1.5
                )
            );
    } else {
        clone.covariate <- NULL;
        }

    clone.bar <- BoutrosLab.plotting.general::create.barplot(
        formula = nsnv ~ clone.id,
        data = clone.df,
        xaxis.cex = 0,
        xlab.label = NULL,
        xaxis.tck = 0,
        ylab.label = 'SNV per clone',
        ylab.cex = subplot.ylab.cex,
        yaxis.cex = subplot.yaxis.cex,
        yaxis.fontface = subplot.yaxis.fontface,
        ylimits = c( - max(clone.df$nsnv) * 0.05, max(clone.df$nsnv) * 1.05)
        );

    sample.bar <- BoutrosLab.plotting.general::create.barplot(
        formula = ID ~ nsnv,
        data = sample.df,
        xlab.label = 'SNV per sample',
        xlab.cex = subplot.xlab.cex,
        xaxis.cex = subplot.xaxis.cex,
        xaxis.fontface = subplot.xaxis.fontface,
        xlimits = c( - max(sample.df$nsnv) * 0.05, max(sample.df$nsnv) * 1.05),
        yaxis.cex = 0,
        yaxis.tck = 0,
        ylab.label = NULL,
        plot.horizontal = TRUE
        );

    hm <- BoutrosLab.plotting.general::create.heatmap(
        x = arr,
        cluster.dimensions = 'none',
        xlab.label = 'Clone ID',
        xlab.cex = subplot.xlab.cex,
        xaxis.lab = rownames(arr),
        xaxis.cex = subplot.xaxis.cex,
        xaxis.fontface = subplot.xaxis.fontface,
        xaxis.rot = hm.xaxis.rot,
        ylab.label = 'Sample ID',
        ylab.cex = subplot.ylab.cex,
        yaxis.lab = colnames(arr),
        yaxis.cex = subplot.yaxis.cex,
        yaxis.fontface = subplot.yaxis.fontface,
        print.colour.key = FALSE,
        colour.scheme = hm.col.scheme,
        xaxis.covariates = clone.covariate,
        xaxis.covariates.y = if (is.null(clone.covariate)) NULL else -0.02,
        # axis.xlab.padding = if (is.null(clone.covariate)) NULL else 3,
        xaxis.tck = if (is.null(clone.covariate)) NULL else 4
        );

    legend.ccf <- BoutrosLab.plotting.general::legend.grob(
        list(
            legend = list(
                title = 'CCF',
                labels = c(min(arr), rep('', legend.size), max(arr)),
                colours = c('white', 'blue'),
                border = 'black',
                continuous = TRUE,
                cex = legend.label.cex
                )
            ),
        size = legend.size,
        title.cex = legend.title.cex,
        label.cex = legend.label.cex
        );

    return(BoutrosLab.plotting.general::create.multipanelplot(
        plot.objects = list(clone.bar, hm, sample.bar),
        layout.width = 2,
        layout.height = 2,
        plot.objects.heights = c(0.3, 1),
        plot.objects.widths = c(1, 0.2),
        layout.skip = c(FALSE, TRUE, FALSE, FALSE),
        legend = list(inside = list(
            fun = legend.ccf,
            x = 0.9,
            y = 0.8
            )),
        ...
        ));
    }
