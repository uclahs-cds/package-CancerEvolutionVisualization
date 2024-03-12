create.clone.genome.distribution.density <- function(
    density.df,
    cluster.colours,
    chr.info,
    save.plt = NULL
    ) {

    return(BoutrosLab.plotting.general::create.scatterplot(
        filename = save.plt,
        formula = scaled.y ~ x,
        data = density.df,
        groups = density.df$clone.id,
        xlab.label = 'Chromosome',
        ylab.label = 'Number of SNVs',
        xlab.cex = 1.65,
        ylab.cex = 1.65,
        xaxis.cex = 1.5,
        yaxis.cex = 1.5,
        xlimits = c(0, sum(chr.info$length)),
        xaxis.lab = chr.info$chr,
        yaxis.lab = seq(0, 100, 10),
        xat = chr.info$xat,
        xaxis.tck = 0,
        yaxis.tck = 0.5,
        xaxis.fontface = 1,
        yaxis.fontface = 1,
        col = cluster.colours,
        type = 'l',
        lwd = 2,
        abline.v = chr.info$start,
        abline.col = 'black',
        abline.lwd = 1.2,
        abline.lty = 1,
        ));
    }

calculate.density.and.scale <- function(cluster.df, total.nsnv) {
    density <- density(x = cluster.df$genome.pos, bw = 'nrd', adjust = 0.05);
    density.df <- as.data.frame(density[c('x','y')]);
    density.df$clone.id <- unique(cluster.df$clone.id);
    density.df$scaled.y <- density.df$y * nrow(cluster.df) / total.nsnv;

    return(density.df)
    }