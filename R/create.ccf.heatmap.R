create.ccf.heatmap <- function(
    x,
    ccf.thres = NULL,
    cluster.dimensions = 'both',
    clustering.method = 'complete',
    distance.method = 'euclidean',
    xaxis.lab = '',
    xlab.label = 'Mutations',
    print.colour.key = FALSE,
    colour.scheme = c('white', 'blue'),
    ...
    ) {

    if (!is.null(ccf.thres)) {
        x[x <= ccf.thres] <- 0;
        }

    col.labels <- seq(min(x), max(x), length.out = 5);

    return(BoutrosLab.plotting.general::create.heatmap(
        x = x,
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
        ));
    }
