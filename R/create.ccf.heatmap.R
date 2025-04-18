create.ccf.heatmap <- function(
    x,
    cluster.dimensions = 'both',
    clustering.method = 'complete',
    distance.method = 'euclidean',
    xaxis.lab = '',
    xlab.label = 'SNVs',
    yaxis.lab = NULL,
    print.colour.key = FALSE,
    colour.scheme = c('white', 'blue'),
    ...
    ) {

    if (is.null(yaxis.lab)) {
        yaxis.lab <- colnames(x);
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
        yaxis.lab = yaxis.lab,
        print.colour.key = print.colour.key,
        colourkey.labels.at = col.labels,
        colour.scheme = colour.scheme,
        ...
        ));
    }
