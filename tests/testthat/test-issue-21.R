# Regression tests for issue #21 "Account for node size in branch lengths",
# which also covers #107 "Test edge lengths with node size".
#
# length.from.node.edge (SRCGrob.R) defaults to TRUE, so adjust.branch.lengths()
# runs on every plot. It pads each edge by the radius of the node at each end so
# that the part of the edge visible OUTSIDE the node circles equals the branch
# length the user supplied.
#
# The bug: it reserved node.radius * node.size, but add.node.ellipse() draws the
# node at node.radius * node.size * (1 + 0.2 * nchar(label)) so that the label
# fits. Every node therefore overlapped the end of its own edge, "absorbing" it
# (issue #103's screenshots), and the shortfall grew with node.size and with
# label length. Because the reserved radius is expressed in scale1 units, the
# same absolute shortfall reads much larger against the second y-axis whenever
# scale2 < scale1 - which is why #21 was reported as a second-length-column
# problem.
#
# These tests measure the drawn grobs against the drawn node ellipse rather than
# against stored coordinates, so they stay meaningful if the layout changes.

# --- geometry helpers -------------------------------------------------------

# Native units are isotropic: the viewport is (extent * scale1) inches in both
# directions (set.up.plot.area.R), so native = inches / scale1.
get.scales <- function(tree.df, scale1 = 1, scale2 = 1) {
    inputs <- prep.tree(
        tree.df,
        NULL,
        polygon.colour.scheme = gradient.color.scheme('grey', nrow(tree.df)),
        use.radians = FALSE,
        default.node.colour = 'white'
        );

    depth <- max(inputs$in.tree.df$tier);

    list(
        inputs = inputs,
        scale1 = get.branch.length.scale(inputs$tree$length1, depth, scale1),
        scale2 = if (is.null(inputs$tree$length2)) NA else {
            get.branch.length.scale(inputs$tree$length2, depth, scale2);
            }
        );
    }

# Radius of the drawn node ellipse along a given direction. gridExtra's
# ngonGrob stretches the unit polygon by sqrt(ar) in x and 1/sqrt(ar) in y and
# then rotates by pi/2, so the semi-axis along y is size * sqrt(ar) and the one
# along x is size / sqrt(ar).
get.drawn.node.radius <- function(size, ar, theta) {
    semi.y <- size * sqrt(ar);
    semi.x <- size / sqrt(ar);

    1 / sqrt((sin(theta) / semi.x)^2 + (cos(theta) / semi.y)^2);
    }

# For every edge, the length of the segment that is visible outside the node
# circles, expressed in the units of the branch-length column that drew it.
get.visible.edge.lengths <- function(tree.df, column = 1, ...) {
    scales <- get.scales(tree.df, ...);
    edges <- scales$inputs$tree;
    nodes <- scales$inputs$in.tree.df;
    nodes <- nodes[!nodes$excluded, ];

    plt <- suppressWarnings(SRCGrob(tree.df, ...));
    segs <- getGrob(plt, paste0('tree.segs.', column));
    expect_false(is.null(segs));

    labels <- get.node.plot.labels(nodes);
    size <- get.node.ellipse.size(0.1, nodes$node.size, labels);
    ar <- get.node.ellipse.ar(labels);

    lengths.c <- if (1 == column) {
        edges$length1;
    } else {
        edges$length2 / scales$scale1 * scales$scale2;
        }
    max.lengths <- if (is.null(edges$length2)) {
        edges$length1;
    } else {
        pmax(edges$length1, edges$length2 / scales$scale1 * scales$scale2);
        }

    sapply(
        seq_len(nrow(edges)),
        FUN = function(i) {
            x0 <- as.numeric(segs$x0)[i];
            y0 <- as.numeric(segs$y0)[i];
            x1 <- as.numeric(segs$x1)[i];
            y1 <- as.numeric(segs$y1)[i];

            drawn <- sqrt((x1 - x0)^2 + (y1 - y0)^2);
            theta <- atan2(x1 - x0, y1 - y0);

            radius.of <- function(id) {
                k <- which(nodes$id == id);
                if (0 == length(k) || !nodes$draw.node[k]) {
                    return(0);
                    }

                get.drawn.node.radius(size[k], ar[k], theta) / scales$scale1;
                };

            hidden <- if (-1 == edges$parent[i]) 0 else radius.of(edges$parent[i]);
            if (lengths.c[i] >= max.lengths[i]) {
                hidden <- hidden + radius.of(edges$tip[i]);
                }

            visible <- drawn - hidden;

            # Convert back into the units of the axis this column is read on.
            if (1 == column) visible else visible * scales$scale1 / scales$scale2;
            }
        );
    }

make.tree <- function(node.size = 1, length.2 = NULL, label = NULL) {
    df <- data.frame(
        node.id = c('1', '2', '3'),
        parent = c(NA, '1', '1'),
        length.1 = c(NA, 10, 20),
        node.size = node.size,
        stringsAsFactors = FALSE
        );

    if (!is.null(length.2)) {
        df$length.2 <- c(NA, length.2);
        }
    if (!is.null(label)) {
        df$label <- label;
        }

    return(df);
    }

# --- tests ------------------------------------------------------------------

test_that('the node grob and the branch length adjustment agree on node size', {
    # The whole bug was these two disagreeing. Nail the contract down.
    labels <- c('1', 'AB', 'LONGLABEL');

    expect_equal(get.node.label.nchar(labels), c(1, 2, 9));
    expect_equal(
        get.node.ellipse.size(0.1, 1, labels),
        0.1 * (1 + 0.2 * c(1, 2, 9))
        );
    expect_equal(get.node.ellipse.ar(labels), 1 - log2(c(1, 2, 9)) / 10);

    # Single-character labels are drawn as true circles, so the reserved
    # radius is exact there rather than an equal-area approximation.
    expect_equal(get.node.ellipse.ar('1'), 1);
    });

test_that('visible branch length equals the supplied length (issue #21, #107)', {
    # Default one-character labels: the node is a circle, so this is exact.
    visible <- get.visible.edge.lengths(make.tree());

    expect_equal(visible, c(1, 10, 20), tolerance = 1e-6);
    });

test_that('node size does not change the visible branch length (issue #103, #107)', {
    # A bigger node must push its neighbours further away, not eat the edge.
    for (node.size in c(0.3, 1, 2.5)) {
        expect_equal(
            get.visible.edge.lengths(make.tree(node.size = node.size)),
            c(1, 10, 20),
            tolerance = 1e-6,
            info = paste('node.size =', node.size)
            );
        }
    });

test_that('a larger node lengthens the drawn segment (issue #107)', {
    drawn.span <- function(node.size) {
        plt <- suppressWarnings(SRCGrob(make.tree(node.size = node.size)));
        segs <- getGrob(plt, 'tree.segs.1');
        sqrt(
            (as.numeric(segs$x1) - as.numeric(segs$x0))^2 +
            (as.numeric(segs$y1) - as.numeric(segs$y0))^2
            );
        }

    small <- drawn.span(0.5);
    large <- drawn.span(3);

    expect_true(all(large > small));

    # The extra length is exactly the extra node radius at each end. scale1
    # depends only on the branch lengths, so it is identical in both plots.
    scale1 <- get.scales(make.tree())$scale1;
    extra <- get.node.ellipse.size(0.1, 3, '1') - get.node.ellipse.size(0.1, 0.5, '1');
    expect_equal(
        large - small,
        c(1, 2, 2) * extra / scale1,
        tolerance = 1e-6
        );
    });

test_that('the second length column is accurate too (issue #21)', {
    # The original report: lengths are wrong "for the second length column".
    # scale1 / scale2 here is ~8.5, which is what amplified the shortfall.
    tree <- make.tree(length.2 = c(5, 60));

    expect_equal(get.visible.edge.lengths(tree, column = 1), c(1, 10, 20), tolerance = 1e-6);
    expect_equal(get.visible.edge.lengths(tree, column = 2), c(1, 5, 60), tolerance = 1e-6);
    });

test_that('second-column accuracy survives large nodes (issue #21)', {
    tree <- make.tree(node.size = 3, length.2 = c(5, 60));

    expect_equal(get.visible.edge.lengths(tree, column = 1), c(1, 10, 20), tolerance = 1e-6);
    expect_equal(get.visible.edge.lengths(tree, column = 2), c(1, 5, 60), tolerance = 1e-6);
    });

test_that('which column holds the maximum does not matter (issue #21)', {
    # length1 is the longer branch on one edge and length2 on the other, so
    # both sides of the "longest branch reaches the node" rule are exercised.
    tree <- make.tree(length.2 = c(60, 5));

    expect_equal(get.visible.edge.lengths(tree, column = 1), c(1, 10, 20), tolerance = 1e-6);
    expect_equal(get.visible.edge.lengths(tree, column = 2), c(1, 60, 5), tolerance = 1e-6);
    });

test_that('long node labels no longer absorb the edge (issue #21)', {
    # Labels inflate the node ellipse, and it is not a circle any more, so the
    # reserved radius is the equal-area one. Before the fix a six-character
    # label swallowed over 90% of the shortest edge; require it to stay close.
    visible <- get.visible.edge.lengths(
        make.tree(label = c('CLONE1', 'CLONE2', 'CLONE3'))
        );

    expect_equal(visible, c(1, 10, 20), tolerance = 0.05);
    });

test_that('length.from.node.edge = FALSE leaves lengths unpadded (issue #21)', {
    plt <- suppressWarnings(SRCGrob(make.tree(), length.from.node.edge = FALSE));
    segs <- getGrob(plt, 'tree.segs.1');

    drawn <- sqrt(
        (as.numeric(segs$x1) - as.numeric(segs$x0))^2 +
        (as.numeric(segs$y1) - as.numeric(segs$y0))^2
        );

    expect_equal(drawn, c(1, 10, 20), tolerance = 1e-6);
    });
