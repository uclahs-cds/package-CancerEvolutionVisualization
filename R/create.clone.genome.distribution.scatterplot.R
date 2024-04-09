create.clone.genome.distribution.scatterplot <- function(
    scatter.df,
    nsnv,
    nclone,
    chr.info,
    save.plt = NULL,
    xlab.label = '',
    xaxis.tck = 0,
    xaxis.cex = 0,
    ...
    ) {

    return(BoutrosLab.plotting.general::create.scatterplot(
        filename = save.plt,
        formula = clone.id ~ genome.pos,
        data = scatter.df,
        xlab.top.lab = paste0('Total SNVs: ', nsnv),
        xlab.top.y = 0.5,
        xlab.label = xlab.label,
        ylab.label = 'Clone ID',
        xaxis.lab = chr.info$chr,
        yaxis.lab = rev(levels(scatter.df$clone.id)),
        xaxis.tck = xaxis.tck,
        xaxis.cex = xaxis.cex,
        xat = chr.info$xat,
        yat = seq(1, nclone, 1),
        xlimits = c(0, sum(chr.info$length)),
        col = scatter.df$colour,
        alpha = 0.5,
        cex = 1.3,
        abline.v = chr.info$start,
        abline.lwd = 1,
        ...
        ));
    }
