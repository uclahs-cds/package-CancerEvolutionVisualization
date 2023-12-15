create.ccf.heatmap <- function(
    CCF.array,
    CCF.threshold = NULL,
    cluster.dim = 'both',
    cluster.method = 'complete',
    dist.method = 'euclidean',
    colour.scheme = NULL,
    xaxis.lab = NULL,
    xlab.label = 'Mutations',
    filename = NULL,
    ...
    ) {

    if (!is.null(CCF.threshold)) {
        CCF.array[CCF.array <= CCF.threshold] <- 0;
        }
    col.labels <- seq(0, 1, .2);

    heatmap.colours <- if (!is.null(colour.scheme)) {
        colour.scheme;
    } else {
        default.heatmap.colours();
        }

    return(BoutrosLab.plotting.general::create.heatmap(
        filename = filename,
        x = CCF.array,
        force.clustering = TRUE,
        cluster.dimensions = cluster.dim,
        clustering.method = cluster.method,
        rows.distance.method = dist.method,
        cols.distance.method = dist.method,
        xaxis.lab = xaxis.lab,
        xlab.label = xlab.label,
        xlab.cex = 1,
        xaxis.cex = 0.6,
        xaxis.fontface = 1,
        xaxis.rot = 90,
        yaxis.lab = colnames(CCF.array),
        ylab.cex = 1,
        yaxis.cex = 0.6,
        yaxis.fontface = 1,
        colour.scheme = heatmap.colours,
        colourkey.cex = 0.6,
        colourkey.labels.at = col.labels,
        left.padding = 1,
        right.padding = 1,
        resolution = 3000,
        ...
        ));
    }
