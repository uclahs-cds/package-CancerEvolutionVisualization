
## Function: plot.density --------------------------------------------------------------------------
# Description: Plot the densityplot (using create.scatterplot() that shows SNV density along the genome
    # per cluster for this sample
# Input: `density.df` (dataframe) - dataframe of density values and the color assignments

plot.snv.across.genome.density <- function(
    density.df,
    cluster.colours,
    chr.info,
    save.plt = NULL
    ) {

    create.scatterplot(
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
        );
    }

calculate.density.and.scale <- function(cluster.df, total.nsnv) {
    # Calculate the density of each cluster.df
    density <- density(x = cluster.df$genome.pos, bw = 'nrd', adjust = 0.05);
    # Convert to a dataframe and assign a group number
    density.df <- as.data.frame(density[c('x','y')]);
    density.df$clone.id <- unique(cluster.df$clone.id);
    # Scale by multiplying by the ratio of: (# of group)/(total count)
    density.df$scaled.y <- density.df$y * nrow(cluster.df) / total.nsnv;

    return(density.df)
    }