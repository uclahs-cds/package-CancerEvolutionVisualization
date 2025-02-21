create.clone.genome.distribution.densityplot <- function(
    density.df,
    cluster.colours,
    chr.info,
    save.plt = NULL,
    ...
    ) {

    return(BoutrosLab.plotting.general::create.scatterplot(
        filename = save.plt,
        formula = y ~ x,
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
