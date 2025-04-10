create.clone.genome.distribution.plot <- function(
    snv.df,
    genome.build = 'GRCh37',
    clone.order = NULL,
    clone.colours = NULL,
    filename = NULL,
    multi.sample = FALSE,
    alpha = 0.25,
    legend.x = 0.1,
    legend.y = 0.55,
    ...
    ) {

    # Preprocess ----------------------------------------------------------------------------------
    if (!all(c('chr', 'pos', 'clone.id') %in% names(snv.df))) {
        stop('snv.df does not contain at least one of chr, pos or clone.id columns')
        }
    if (is.null(clone.order)) {
        clone.order <- sort(unique(snv.df$clone.id));
        }

    if (multi.sample) {
        # if multi-sample is true, check for sample ids in 'ID' column
        if (is.null(snv.df$ID)) {
            stop('ID column must contain sample ID if multi.sample is TRUE');
            }
        # filename must be a directory
        if (!is.null(filename) && !dir.exists(filename)) {
            stop('filename must be a directory if multi.sample is TRUE');
            }
    } else {
        if (!is.null(filename) && dir.exists(filename)) {
            stop('filename must be a path (not a directory) if multi.sample is FALSE');
            }
        snv.df$ID <- 'all';
        }

    if (is.null(clone.colours)) {
        clone.colours <- get.colours(clone.order, return.names = TRUE);
        }
    snv.df$clone.id <- factor(snv.df$clone.id, levels = clone.order);
    genome.pos.df   <- get.genome.pos(snv.df, genome.build);
    snv.df          <- genome.pos.df$snv;

    # use chr.info to set x-axis label positions
    chr.info <- genome.pos.df$chr.info;
    chr.info$xat <- (chr.info$length / 2) + chr.info$start;

    plt.list <- list();
    for (s in unique(snv.df$ID)) {
        # Iterate through each sample -------------------------------------------------------------
        message(paste('Plotting clone distribution across the genome for sample:', s));

        sample.df <- droplevels(snv.df[snv.df$ID == s, ]);
        sample.df <- unique(sample.df[, c('clone.id', 'genome.pos', 'SNV.id', 'ID')]);
        if (!is.null(filename) && multi.sample) {
            save.plt <- file.path(filename, paste0(s, '.png'));
        } else {
            save.plt <- filename;
            }

        plt.list[[s]] <- create.clone.genome.distribution.plot.per.sample(
            sample.df,
            clone.colours[levels(sample.df$clone.id)],
            chr.info,
            save.plt = save.plt,
            alpha = alpha,
            legend.x = legend.x,
            legend.y = legend.y,
            ...
            );
        }
    return(plt.list);
    }

create.clone.genome.distribution.plot.per.sample <- function(
    sample.df,
    clone.colours,
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
    legend.x = 0.1,
    legend.y = 0.55,
    alpha = 0.25,
    ...
    ) {

    # calculate densities for each cluster --------------------------------------------------------
    density.list <- list();
    for (k in unique(sample.df$clone.id)) {
        if (sum(sample.df$clone.id == k, na.rm = TRUE) <= 1) {
            warning(paste('Skipping clone', k, 'in sample', unique(sample.df$ID), 'since there is only one SNV'));
            next;
        }
        density.list[[k]] <- calculate.density(
            x = sample.df[sample.df$clone.id == k, ],
            adjust = 0.05
            );
        }
    density.df <- do.call(rbind, density.list);

    # get plot legend -----------------------------------------------------------------------------
    legend.label <- sapply(names(clone.colours), function(k) {
        nsnv <- length(unique(sample.df[sample.df$clone.id == k, ]$SNV.id));
        return(paste0(k, ' (', nsnv, ')'));
        });
    clone.colours <- clone.colours[levels(sample.df$clone.id)];
    cluster.legend <- BoutrosLab.plotting.general::legend.grob(
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

    # create individual plot ----------------------------------------------------------------------
    sample.df$colour <- clone.colours[sample.df$clone.id];
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
        yaxis.cex = yaxis.cex,
        alpha = alpha
        );

    density.plt <- create.clone.genome.distribution.densityplot(
        density.df,
        clone.colours,
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

    if (legend.x > 1) {
        cluster.legend <- list(right = list(fun = cluster.legend));
    } else {
        cluster.legend <- list(inside = list(
            fun = cluster.legend,
            x = legend.x,
            y = legend.y
            ));
        }

    return(BoutrosLab.plotting.general::create.multipanelplot(
        filename = save.plt,
        plot.objects = list(
            scatter.plt,
            density.plt
            ),
        layout.width = 1,
        layout.height = 2,
        plot.objects.heights = c(height.scatter, 5) / total.height,
        legend = cluster.legend,
        height = total.height,
        width = width,
        ...
        ));
    }

get.genome.pos <- function(
    snv.df,
    genome.build = 'GRCh37',
    chr.order = c(1:22, 'X', 'Y')
    ) {

    if (!(genome.build %in% c('GRCh37', 'GRCh38'))) {
        stop('genome.build must be either GRCh37 or GRCh38')
        }
    assign('chr.info', get(genome.build));

    snv.df$chr      <- droplevels(factor(snv.df$chr, levels = chr.order));
    chr.info        <- chr.info[chr.info$chr %in% levels(snv.df$chr), c('chr', 'length')];
    chr.info$chr    <- droplevels(factor(chr.info$chr, levels = chr.order));
    chr.info$length <- as.numeric(chr.info$length);
    chr.info$start  <- c(0, cumsum(chr.info$length[-length(chr.info$length)]));

    snv.df$genome.pos <- chr.info[match(snv.df$chr, chr.info$chr), 'start'] + as.integer(snv.df$pos);

    return(list('snv' = snv.df, 'chr.info' = chr.info));
    }
