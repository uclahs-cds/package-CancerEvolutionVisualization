create.clone.genome.distribution.densityplot <- function(
    density.df,
    cluster.colours,
    chr.info,
    save.plt = NULL,
    ...
    ) {

    return(BoutrosLab.plotting.general::create.scatterplot(
        filename = save.plt,
        formula = count ~ x,
        data = density.df,
        groups = density.df$clone.id,
        xlab.label = 'Chromosome',
        ylab.label = 'SNV Density',
        xlimits = c(0, sum(chr.info$length)),
        xaxis.lab = chr.info$chr,
        xat = chr.info$xat,
        col = cluster.colours,
        type = 'l',
        lwd = 2,
        abline.v = chr.info$start,
        abline.lwd = 1.2,
        ...
        ));
    }

calculate.density.and.scale <- function(cluster.df) {
    # density should be generated using unque SNV count
    density <- density(
        x = cluster.df$genome.pos,
        bw = 'nrd',
        adjust = 0.05, # set to 1E9/3E9 to get density per megabase
        na.rm = TRUE);
    density.df <- as.data.frame(density[c('x','y')]);
    density.df$clone.id <- unique(cluster.df$clone.id);
    density.df$count <- nrow(cluster.df) / sum(density.df$y) * density.df$y;

    return(density.df)
    }
