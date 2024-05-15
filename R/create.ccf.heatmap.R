create.ccf.heatmap <- function(
    hm.array,
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
        hm.array[hm.array <= ccf.thres] <- 0;
        }

    col.labels <- seq(min(hm.array), max(hm.array), length.out = 5);

    return(BoutrosLab.plotting.general::create.heatmap(
        x = hm.array,
        force.clustering = TRUE,
        cluster.dimensions = cluster.dimensions,
        clustering.method = clustering.method,
        rows.distance.method = distance.method,
        cols.distance.method = distance.method,
        xaxis.lab = xaxis.lab,
        xlab.label = xlab.label,
        yaxis.lab = colnames(hm.array),
        print.colour.key = print.colour.key,
        colourkey.labels.at = col.labels,
        colour.scheme = colour.scheme,
        ...
        ));
    }
