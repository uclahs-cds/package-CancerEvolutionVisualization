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
    legend.x = 0.9,
    legend.y = 0.8,
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

    clone.df  <- aggregate(
        SNV.id ~ clone.id,
        data = DF[DF$CCF > 0, ],
        FUN = function(x) length(unique(x))
        );
    sample.df <- aggregate(SNV.id ~ ID, data = DF[DF$CCF > 0, ], FUN = length);
    names(sample.df)[2] <- names(clone.df)[2] <- 'nsnv';

    if (!is.null(clone.order) & !is.null(sample.order)) {
        arr                 <- arr[clone.order, sample.order];
        clone.df$clone.id   <- factor(clone.df$clone.id, levels = clone.order);
        sample.df$ID        <- factor(sample.df$ID, levels = sample.order);
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
        ylimits = c( - 0.05, 1.05) * max(clone.df$nsnv)
        );

    # restrict number of ticks in the SNV per sample barplot
    sample.xaxis <- auto.axis(
        x = sample.df$nsnv,
        log.scaled = FALSE,
        num.labels = 3
        );
    sample.bar <- BoutrosLab.plotting.general::create.barplot(
        formula = ID ~ nsnv,
        data = sample.df,
        xlab.label = 'SNV\nper sample',
        xlab.cex = subplot.xlab.cex,
        xaxis.cex = subplot.xaxis.cex,
        xaxis.fontface = subplot.xaxis.fontface,
        xlimits = c( - 0.05, 1.05) * max(sample.xaxis$at),
        yaxis.cex = 0,
        yaxis.tck = 0,
        ylab.label = NULL,
        plot.horizontal = TRUE,
        xat = sample.xaxis$at
        );

    hm <- BoutrosLab.plotting.general::create.heatmap(
        x = arr,
        cluster.dimensions = 'none',
        xlab.label = 'Clone ID',
        xlab.cex = ifelse(is.null(clone.colours), subplot.xlab.cex, 0),
        xaxis.lab = rownames(arr),
        xaxis.cex = ifelse(is.null(clone.colours), subplot.xaxis.cex, 0),
        xaxis.fontface = subplot.xaxis.fontface,
        xaxis.rot = hm.xaxis.rot,
        ylab.label = 'Sample ID',
        ylab.cex = subplot.ylab.cex,
        yaxis.lab = colnames(arr),
        yaxis.cex = subplot.yaxis.cex,
        yaxis.fontface = subplot.yaxis.fontface,
        print.colour.key = FALSE,
        colour.scheme = hm.col.scheme
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

    if (!is.null(clone.colours)) {
        clone.cov <- BoutrosLab.plotting.general::create.heatmap(
            x = t(clone.colours[rownames(arr)]),
            xlab.label = 'Clone ID',
            xlab.cex = subplot.xlab.cex,
            xaxis.lab = rownames(arr),
            xaxis.cex = subplot.xaxis.cex,
            xaxis.fontface = subplot.xaxis.fontface,
            xaxis.rot = hm.xaxis.rot,
            input.colours = TRUE,
            clustering.method = 'none',
            grid.col = FALSE,
            print.colour.key = FALSE,
            yaxis.tck = 0
            );
        plot.list <- list(clone.bar, hm, sample.bar, clone.cov);
        layout.skip <- c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE);
        layout.height <- 3;
        plot.objects.heights <- c(0.3, 0.9, 0.2);
    } else {
        plot.list <- list(clone.bar, hm,sample.bar);
        layout.skip <- c(FALSE, TRUE, FALSE, FALSE);
        layout.height <- 2;
        plot.objects.heights <- c(0.3, 1);
        }

    return(BoutrosLab.plotting.general::create.multipanelplot(
        plot.objects = plot.list,
        layout.width = 2,
        layout.height = layout.height,
        plot.objects.heights = plot.objects.heights,
        plot.objects.widths = c(1, 0.2),
        layout.skip = layout.skip ,
        legend = list(inside = list(
            fun = legend.ccf,
            x = legend.x,
            y = legend.y
            )),
        ...
        ));
    }
