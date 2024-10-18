## Function: plot.clone.densityplot ----------------------------------------------------------------
plot.clone.densityplot <- function(
    dpclust.df,
    sampleID,
    src.tool,
    output.dir
    ) {
    # Create table of densities for plotting each cluster
    density.list <- list();
    for (clone in unique(dpclust.df$clone.id)) {
        clone.df <- calculate.density.each.clone(
            cluster.df = dpclust.df[dpclust.df$clone.id == clone, ],
            cloneID = clone
            );
        density.list[[clone]] <- clone.df;
        }
    density.df <- do.call(rbind, density.list);

    # Calculate average CCF per cluster
    cluster.meanCCFs <- unique(dpclust.df$location);

    # Get plot legend
    clone.IDs <- unique(density.df$clone.id);
    colors.qual <- BoutrosLab.plotting.general::default.colours(12);
    cluster.colours <- setNames(colors.qual[1:length(clone.IDs)], clone.IDs);
    cluster.legend <- BoutrosLab.plotting.general::legend.grob(
        list(
            legend = list(
                title = 'Clone',
                labels = as.character(names(cluster.colours)),
                colours = c(cluster.colours),
                border = 'black'
                )
            ),
        size = 1,
        title.cex = 1,
        label.cex = 1
        );

    # Set y-axis max limit and y-increments
    ymax <- ceiling(max(density.df$count) / 5) * 5;
    yseq <- if (ymax > 1000) {
        100;
    } else if (ymax > 100) {
        20;
    } else if (ymax > 20) {
        10;
    } else {
        5;
    }

    save.plt <- file.path(
        output.dir,
        paste(sampleID, src.tool, 'clone_densityplot.png', sep = '_')
        );
    return(
        BoutrosLab.plotting.general::create.scatterplot(
            filename = save.plt,
            formula = count ~ x,
            data = density.df,
            groups = density.df$clone.id,
            xlab.label = 'CCF',
            ylab.label = 'SNV Count',
            xlab.top.label = paste('Total SNVs Clustered:', nrow(dpclust.df)),
            xlab.top.cex = 1.5,
            xlab.top.x = 0.5,
            xlab.top.y = 1,
            xlab.cex = 1.5,
            ylab.cex = 1.5,
            xaxis.cex = 1.2,
            yaxis.cex = 1.2,
            xlimits = c(0, 1.5),
            ylimits = c(0, ymax),
            xat = seq(0, 1.5, 0.25),
            yat = seq(0, ymax, yseq),
            xaxis.tck = c(1, 0),
            yaxis.tck = c(1, 0),
            col = cluster.colours,
            type = 'l',
            lwd = 3,
            abline.v = cluster.meanCCFs,
            abline.lwd = 0.5,
            abline.lty = 'longdash',
            # add.text = TRUE,
            text.labels = lapply(cluster.meanCCFs, round, 2),
            text.x = cluster.meanCCFs,
            text.y = ymax - 1,
            text.fontface = 'plain',
            text.cex = 1,
            legend = list(
                right = list(
                    fun = cluster.legend
                    )
                ),
            top.padding = 2,
            right.padding = 1,
            left.padding = 1,
            height = 6,
            width = 6.5,
            resolution = 800
            )
        );
    }
