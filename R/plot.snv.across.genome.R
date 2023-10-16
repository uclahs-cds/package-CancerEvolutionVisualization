plot.cluster.genome.distribution <- function(
    snv.df,
    genome.build = 'GRCh37',
    clone.order = NULL,
    save.plt.dir = NULL
    ) {

    print('Plotting clone distribution across the genome');
    # Preprocess ----------------------------------------------------------------------------------
    if (!all(c('chr', 'pos', 'clone.id') %in% names(snv.df))) {
        stop('snv.df does not contain at least one of chr, pos or clone.id columns')
        }
    if (is.null(clone.order)) {
        clone.order <- sort(unique(snv.df$clone.id));
        }
    snv.df$clone.id <- factor(snv.df$clone.id, levels = clone.order);
    genome.pos.df   <- get.genome.pos(snv.df, genome.build);
    snv.df          <- genome.pos.df$snv;
    cluster.colours <- get.colours(unique(snv.df$clone.id), return.names = TRUE);

    # use chr.info to set x-axis label positions
    chr.info <- genome.pos.df$chr.info;
    chr.info$xat <- (chr.info$length / 2) + chr.info$start;

    for (s in unique(snv.df$ID)) { # s = 'MSK-AB-0014-T13-3'
        # Iterate through each sample -------------------------------------------------------------
        print(paste('Plotting', s));
        sample.df <- droplevels(snv.df[snv.df$ID == s, ]);
        # sample.df <- sample.df[order(sample.df$clone.id), ];
        # calculate densities for each cluster
        density.list <- list();
        for (c in unique(sample.df$clone.id)) {
            if (sum(sample.df$clone.id == c) <= 1) {
                warning(paste('Skipping clone', c, 'in sample', s, 'since there is only one SNV'));
                next;
            }
            density.list[[c]] <- calculate.density.and.scale(
                cluster.df = sample.df[sample.df$clone.id == c, ],
                total.nsnv = nrow(sample.df)
                );
            }
        density.df <- do.call(rbind, density.list);
        plt <- plot.cluster.genome.distribution.per.sample(
            sample.df,
            density.df,
            cluster.colours[unique(sample.df$clone.id)],
            chr.info,
            save.plt = file.path(save.plt.dir, paste0(unique(sample.df$ID), '_test.png'))
            );
        }
    }

plot.cluster.genome.distribution.per.sample <- function(
    sample.df,
    density.df,
    cluster.colours,
    chr.info,
    save.plt = NULL
    ) {
    
    cluster.colours <- cluster.colours[]
    cluster.legend <- BoutrosLab.plotting.general::legend.grob(
        list(
            legend = list(
                colours = cluster.colours,
                title = 'Clones',
                labels = names(cluster.colours),
                border = 'black'
                )
            ),
        title.just = 'left',
        title.cex = 1.3,
        label.cex = 1.2,
        size = 2,
        x = 0.5,
        y = 0.5
        );

    # create individual plot ----------------------------------------------------------------------
    sample.df$colour <- cluster.colours[sample.df$clone.id];
    scatter.plt <- plot.snv.across.genome.scatter(
        scatter.df = sample.df,
        nsnv = nrow(sample.df),
        nclone = length(unique(sample.df$clone.id)),
        chr.info = chr.info
        );  

    density.plt <- plot.snv.across.genome.density(
        density.df,
        cluster.colours,
        chr.info
        );
    # create multipanel plot ----------------------------------------------------------------------
    mp <- create.multipanelplot(
        filename = save.plt,
        plot.objects = list(
            scatter.plt,
            density.plt
            ),
        layout.width = 1,
        layout.height = 2,
        plot.objects.heights = c(0.4, 0.6),
        legend = list(right = list(
                fun = cluster.legend
                )),
        left.padding = - 0.75,
        y.spacing = - 0.5,
        height = 7,
        width = 18,
        resolution = 800
        );
    }
