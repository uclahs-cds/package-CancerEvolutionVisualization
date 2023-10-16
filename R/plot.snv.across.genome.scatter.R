
## Function: plot.scatterplot ----------------------------------------------------------------------
# Description: Plot the scatterplot that shows SNV distribution by cluster
# Input:
    # `scatter.df` (dataframe) - clusters of SNVs dataframe to be plotted
    # `nsnv` (integer) - number of SNVs for this sample
    # nclone (integer) - number of clusters returned by SRC tool for this sample
plot.snv.across.genome.scatter <- function(
    scatter.df,
    nsnv,
    nclone,
    chr.info,
    save.plt = NULL
    ) {

    create.scatterplot(
        filename = save.plt,
        formula = clone.id ~ genome.pos,
        data = scatter.df,
        main = 'SNV Distribution by Cluster',
        main.cex = 1.75,
        main.x = 0.5,
        xlab.top.lab = paste0('Total SNVs: ', nsnv),
        xlab.top.cex = 1.2,
        xlab.top.y = 0.5,
        xlab.label = '',
        ylab.label = 'Clone ID',
        ylab.cex = 1.5,
        xaxis.lab = chr.info$chr,
        yaxis.lab = levels(scatter.df$clone.id),
        xaxis.tck = 0,
        yaxis.tck = 1.2,
        xaxis.cex = 0,
        yaxis.cex = 1.2,
        xaxis.fontface = 1,
        yaxis.fontface = 1,
        xat = chr.info$xat,
        yat = seq(1, nclone, 1),
        xlimits = c(0, sum(chr.info$length)),
        ylimits = NULL,
        pch = 21,
        col = scatter.df$colour,
        alpha = 0.25,
        cex = 1.3,
        abline.v = chr.info$start,
        abline.lwd = 1,
        );
    }
