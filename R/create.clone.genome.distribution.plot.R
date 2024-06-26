create.clone.genome.distribution.plot <- function(
    snv.df,
    genome.build = 'GRCh37',
    clone.order = NULL,
    cluster.colours = NULL,
    save.plt.dir = NULL,
    multi.sample = FALSE,
    ...
    ) {

    # Preprocess ----------------------------------------------------------------------------------
    if (!all(c('chr', 'pos', 'clone.id') %in% names(snv.df))) {
        stop('snv.df does not contain at least one of chr, pos or clone.id columns')
        }
    if (is.null(clone.order)) {
        clone.order <- sort(unique(snv.df$clone.id));
        }
    if (multi.sample) { # if multi-sample is true, check for sample ids in 'ID' column
        if (is.null(snv.df$ID)) {
            stop('ID column must contain sample ID if multi.sample is TRUE')
        }
    } else {
        snv.df$ID <- 'all';
        }

    if (is.null(cluster.colours)) {
        cluster.colours <- get.colours(clone.order, return.names = TRUE);
        }
    snv.df$clone.id <- factor(snv.df$clone.id, levels = clone.order);
    genome.pos.df   <- get.genome.pos(snv.df, genome.build);
    snv.df          <- genome.pos.df$snv;

    # use chr.info to set x-axis label positions
    chr.info <- genome.pos.df$chr.info;
    chr.info$xat <- (chr.info$length / 2) + chr.info$start;

    for (s in unique(snv.df$ID)) {
        # Iterate through each sample -------------------------------------------------------------
        sample.df <- droplevels(snv.df[snv.df$ID == s, ])
        print(paste('Plotting clone distribution across the genome for sample:', s));
        plt <- create.clone.genome.distribution.plot.per.sample(
            sample.df,
            cluster.colours[levels(sample.df$clone.id)],
            chr.info,
            save.plt = ifelse(
                is.null(save.plt.dir),
                NULL,
                file.path(save.plt.dir, paste0(s, '_clone-genome-dist.png'))
                ),
            ...
            );
        }
    }

create.clone.genome.distribution.plot.per.sample <- function(
    sample.df,
    cluster.colours,
    chr.info,
    save.plt = NULL,
    width = 18,
    xaxis.tck = 0.5,
    yaxis.tck = 0.5,
    xaxis.fontface = 'bold',
    yaxis.fontface = 'bold',
    xlab.cex = 1.65,
    ylab.cex = 1.65,
    xaxis.cex = 1.5,
    yaxis.cex = 1.5,
    xlab.top.cex = 1.2,
    legend.size = 3,
    legend.title.cex = 1.2,
    legend.label.cex = 1,
    ...
    ) {

    # calculate densities for each cluster --------------------------------------------------------
    density.list <- list();
    for (c in unique(sample.df$clone.id)) {
        if (sum(sample.df$clone.id == c) <= 1) {
            warning(paste('Skipping clone', c, 'in sample', unique(sample.df$ID), 'since there is only one SNV'));
            next;
        }
        density.list[[c]] <- calculate.density.and.scale(
            cluster.df = sample.df[sample.df$clone.id == c, ],
            total.nsnv = nrow(sample.df)
            );
        }
    density.df <- do.call(rbind, density.list);

    # get plot legend -----------------------------------------------------------------------------
    cluster.colours <- cluster.colours[levels(sample.df$clone.id)];
    cluster.legend <- BoutrosLab.plotting.general::legend.grob(
        list(
            legend = list(
                title = 'Clones',
                labels = names(cluster.colours),
                colours = c(cluster.colours),
                border = 'black'
                )
            ),
        size = legend.size,
        title.cex = legend.title.cex,
        label.cex = legend.label.cex
        );

    # create individual plot ----------------------------------------------------------------------
    sample.df$colour <- cluster.colours[sample.df$clone.id];
    scatter.plt <- create.clone.genome.distribution.scatterplot(
        scatter.df = sample.df,
        nsnv = nrow(sample.df),
        nclone = length(unique(sample.df$clone.id)),
        chr.info = chr.info,
        xlab.top.cex = xlab.top.cex,
        xaxis.tck = 0,
        yaxis.tck = yaxis.tck,
        yaxis.fontface = yaxis.fontface,
        xlab.cex = 0,
        ylab.cex = ylab.cex,
        xaxis.cex = 0,
        yaxis.cex = yaxis.cex
        );

    density.plt <- create.clone.genome.distribution.densityplot(
        density.df,
        cluster.colours,
        chr.info = chr.info,
        xaxis.tck = xaxis.tck,
        yaxis.tck = yaxis.tck,
        xaxis.fontface = xaxis.fontface,
        yaxis.fontface = yaxis.fontface,
        xlab.cex = xlab.cex,
        ylab.cex = ylab.cex,
        xaxis.cex = xaxis.cex,
        yaxis.cex = yaxis.cex
        );

    # create multipanel plot ----------------------------------------------------------------------
    # automate plot sizing based on cumber of clones in the scatter plot
    height.scatter <- 0.5 * length(unique(sample.df$clone.id));
    total.height <- height.scatter + 5;

    return(BoutrosLab.plotting.general::create.multipanelplot(
        filename = save.plt,
        plot.objects = list(
            scatter.plt,
            density.plt
            ),
        layout.width = 1,
        layout.height = 2,
        plot.objects.heights = c(height.scatter, 5) / total.height,
        legend = list(right = list(
                fun = cluster.legend
                )),
        height = total.height,
        width = width,
        ...
        ));
    }
