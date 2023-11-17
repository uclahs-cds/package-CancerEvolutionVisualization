create.ccf.heatmap <- function(
    hm.array,
    ccf.thres = NULL,
    cls.dim = 'both',
    cls.method = 'complete',
    dist.method = 'euclidean',
    hm.cols = c('white', 'blue'),
    xaxis.lab = NULL,
    xlab.label = 'Mutations',
    ...
    ) {

    if (!is.null(ccf.thres)) {
        hm.array[hm.array <= ccf.thres] <- 0;
        }
    col.labels <- seq(0, 1, .2);

    hm <- BoutrosLab.plotting.general::create.heatmap(
        filename = NULL,
        x = hm.array,
        force.clustering = TRUE,
        cluster.dimensions = cls.dim,
        clustering.method = cls.method,
        rows.distance.method = dist.method,
        cols.distance.method = dist.method,
        xaxis.lab = xaxis.lab,
        xlab.label = xlab.label,
        xlab.cex = 1,
        xaxis.cex = 0.6,
        xaxis.fontface = 1,
        xaxis.rot = 90,
        yaxis.lab = colnames(hm.array),
        ylab.cex = 1,
        yaxis.cex = 0.6,
        yaxis.fontface = 1,
        colourkey.cex = 0.6,
        colour.scheme = hm.cols,
        left.padding = 1,
        right.padding = 1,
        resolution = 3000,
        width = 9,
        height = 5,
        colourkey.labels.at = col.labels,
        ...
        );
    return(hm);
    }
