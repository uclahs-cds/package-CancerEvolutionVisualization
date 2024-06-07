create.ccf.heatmap <- function(
    x,
    ccf.thres = NULL,
    cls.dim = 'both',
    cls.method = 'complete',
    dist.method = 'euclidean',
    hm.cols = NULL,
    xaxis.lab = NULL,
    xlab.label = 'Mutations',
    filename = NULL,
    ...
    ) {

    if (!is.null(ccf.thres)) {
        x[x <= ccf.thres] <- 0;
        }
    if (is.null(hm.cols)) {
        hm.cols <- c('white', 'blue');
        }
    col.labels <- seq(0, max(hm.array), .2);

    hm <- BoutrosLab.plotting.general::create.heatmap(
        filename = filename,
        x = hm.array,
        force.clustering = TRUE,
        cluster.dimensions = cluster.dimensions,
        clustering.method = clustering.method,
        rows.distance.method = distance.method,
        cols.distance.method = distance.method,
        xaxis.lab = xaxis.lab,
        xlab.label = xlab.label,
        yaxis.lab = colnames(x),
        print.colour.key = print.colour.key,
        colourkey.labels.at = col.labels,
        colour.scheme = colour.scheme,
        ...
        );
    }
