create.ccf.densityplot <- function(
    x,
    filename = NULL,
    clone.colours = NULL,
    breaks = 100,
    xlab.label = 'CCF',
    ylab.label = 'SNV Density',
    xlimits = c(0, 1.5),
    xat = seq(0, 1.5, 0.25),
    legend.size = 3,
    legend.title.cex = 1.2,
    legend.label.cex = 1,
    legend.x = 0.8,
    legend.y = 0.9,
    height = 6,
    width = 10,
    size.units = 'in',
    resolution = 1000,
    ...
    ) {

    if (is.null(clone.colours)) {
        clone.colours <- get.colours(x$clone.id, return.names = TRUE);
        }

    mean.ccf <- aggregate(CCF ~ clone.id, data = x, FUN = mean);
    nsnv <- aggregate(SNV.id ~ clone.id, data = x, FUN = length);

    density.list <- list();
    for (k in unique(x$clone.id)) {
        density.list[[k]] <- calculate.density(
            x = x[x$clone.id == k, ],
            value = 'CCF',
            adjust = 1,
            scale = FALSE
            );
        }
    density.df <- do.call(rbind, density.list);
    density.df$y <- density.df$y * (nsnv$SNV.id[match(density.df$clone.id, nsnv$clone.id)] / nrow(x));

    legend.label <- sapply(names(clone.colours), function(k) {
        nsnv <- nsnv[nsnv$clone.id == k, ]$SNV.id;
        return(paste0(k, ' (', nsnv, ')'));
        });
    clone.legend <- BoutrosLab.plotting.general::legend.grob(
        list(
            legend = list(
                title = 'Clone (SNVs)',
                labels = legend.label[names(clone.colours)],
                colours = c(clone.colours),
                border = 'black'
                )
            ),
        size = legend.size,
        title.just = 'left',
        title.cex = legend.title.cex,
        label.cex = legend.label.cex
        );

    ymax <- ceiling(max(density.df$y, na.rm = TRUE));

    hist <- BoutrosLab.plotting.general::create.histogram(
        x = x$CCF,
        type = 'density',
        col = 'gray90',
        border.col = 'gray30',
        lwd = 0.1,
        xlab.label = xlab.label,
        ylab.label = ylab.label,
        xlimits = xlimits,
        xat = xat,
        ylimits = c(-0.05, 1.05) * ymax,
        legend = list(inside = list(
            fun = clone.legend,
            x = legend.x,
            y = legend.y
            )),
        ...
        );

    scatter <- BoutrosLab.plotting.general::create.scatterplot(
        formula = y ~ x,
        data = density.df,
        groups = density.df$clone.id,
        type = 'l',
        lwd = 3,
        col = clone.colours,
        xlimits = xlimits,
        ylimits = c(-0.05, 1.05) * ymax,
        abline.v = mean.ccf$CCF,
        abline.lwd = 0.5,
        abline.lty = 'longdash',
        abline.col = 'gray50',
        add.text = TRUE,
        text.labels = lapply(mean.ccf$CCF, round, 2),
        text.x = mean.ccf$CCF,
        text.y = ymax,
        text.fontface = 'bold',
        text.cex = legend.title.cex
        );

    combn.plt <- hist + scatter;
    return(BoutrosLab.plotting.general::write.plot(
        trellis.object = combn.plt,
        filename = filename,
        height = height,
        width = width,
        size.units = size.units,
        resolution = resolution
        ));
    }
