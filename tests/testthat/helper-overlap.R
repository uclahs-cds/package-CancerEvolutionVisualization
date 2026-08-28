# Geometric overlap detection for rendered SRCGrob node circles.
#
# The node circles are drawn by add.node.ellipse() as a single 'node.polygons'
# grob whose points are positioned in native units but sized in inches, so the
# only honest way to ask "do these two nodes collide?" is to convert the drawn
# points inside the plot viewport and measure. Everything below reports inches.

get.node.circles <- function(plt) {
    node.grob <- getGrob(plt, 'node.polygons');
    if (is.null(node.grob)) {
        return(NULL);
        }

    polygon.ids <- rep(seq_along(node.grob$id.lengths), node.grob$id.lengths);

    pdf(NULL);
    on.exit(dev.off(), add = TRUE);

    grid.newpage();
    pushViewport(plt$vp);
    x.inches <- convertX(node.grob$x, 'inches', valueOnly = TRUE);
    y.inches <- convertY(node.grob$y, 'inches', valueOnly = TRUE);
    popViewport();

    labels <- getGrob(plt, 'node.labels')$label;

    circles <- lapply(
        unique(polygon.ids),
        FUN = function(i) {
            xs <- x.inches[polygon.ids == i];
            ys <- y.inches[polygon.ids == i];

            data.frame(
                node = if (length(labels) >= i) labels[i] else as.character(i),
                x = mean(range(xs)),
                y = mean(range(ys)),
                # Ellipses here are near-circular; take the larger semi-axis so
                # the test never reports a collision that is not really there.
                radius = max(diff(range(xs)), diff(range(ys))) / 2,
                stringsAsFactors = FALSE
                );
            }
        );

    return(do.call(rbind, circles));
    }

find.node.overlaps <- function(plt, tolerance = 0) {
    circles <- get.node.circles(plt);

    if (is.null(circles) || nrow(circles) < 2) {
        return(NULL);
        }

    overlaps <- list();

    for (i in 1:(nrow(circles) - 1)) {
        for (j in (i + 1):nrow(circles)) {
            distance <- sqrt(
                (circles$x[i] - circles$x[j]) ** 2 +
                (circles$y[i] - circles$y[j]) ** 2
                );
            required <- circles$radius[i] + circles$radius[j];

            if (distance < required - tolerance) {
                overlaps[[length(overlaps) + 1]] <- data.frame(
                    a = circles$node[i],
                    b = circles$node[j],
                    distance = distance,
                    required = required,
                    overlap = required - distance,
                    stringsAsFactors = FALSE
                    );
                }
            }
        }

    if (length(overlaps) == 0) {
        return(NULL);
        }

    return(do.call(rbind, overlaps));
    }

describe.node.overlaps <- function(overlaps) {
    if (is.null(overlaps)) {
        return('no overlapping nodes');
        }

    return(paste(
        sprintf(
            '%s/%s %.4f apart, needs %.4f',
            overlaps$a,
            overlaps$b,
            overlaps$distance,
            overlaps$required
            ),
        collapse = '; '
        ));
    }

# The pathology in issue #127: two nodes rendered at the exact same point, so
# their labels print on top of each other and one node is invisible.
find.coincident.nodes <- function(plt, tolerance = 1e-9) {
    overlaps <- find.node.overlaps(plt);

    if (is.null(overlaps)) {
        return(NULL);
        }

    coincident <- overlaps[overlaps$distance <= tolerance, ];

    if (nrow(coincident) == 0) {
        return(NULL);
        }

    return(coincident);
    }
