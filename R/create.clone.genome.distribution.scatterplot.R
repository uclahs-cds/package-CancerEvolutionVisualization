create.clone.genome.distribution.scatterplot <- function(
    scatter.df,
    nsnv,
    nclone,
    chr.info,
    save.plt = NULL,
    ...
    ) {

    scatter.df[, clone.id := factor(clone.id, levels = rev(levels(clone.id)))];
    return(BoutrosLab.plotting.general::create.scatterplot(
        filename = save.plt,
        formula = clone.id ~ genome.pos,
        data = scatter.df,
        xlab.top.lab = paste0('Total SNVs: ', nsnv),
        xlab.top.y = 0.5,
        ylab.label = 'Clone ID',
        xaxis.lab = chr.info$chr,
        yaxis.lab = levels(scatter.df$clone.id),
        xat = chr.info$xat,
        yat = seq(1, nclone, 1),
        xlimits = c(0, sum(chr.info$length)),
        col = scatter.df$colour,
        abline.v = chr.info$start,
        ...
        ));
    }
