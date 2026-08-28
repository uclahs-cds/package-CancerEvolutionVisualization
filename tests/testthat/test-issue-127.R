# Issue #127 "Resolve the overlapped node issue" makes two separate claims:
#
#   A. nodes are drawn on top of each other;
#   B. the y-axis does not appear when the input carries a CP column.
#
# The screenshot in the issue shows two nodes at different tiers of a binary
# tree printed at the exact same point, their labels overprinting. Child angles
# are measured from the parent's own branch direction, so an undamped, constant
# per-tier offset accumulates: in a symmetric tree the mirrored paths
# (-a, +a, +a) and (+a, -a, -a) sum to the identical displacement.
#
# See helper-overlap.R for the detector these tests are built on.

balanced.binary.tree <- function(depth, length.1 = 30) {
    parent <- NA;
    ids <- 1;
    next.id <- 2;
    frontier <- 1;

    for (tier in 1:depth) {
        new.frontier <- c();

        for (p in frontier) {
            for (child in 1:2) {
                parent <- c(parent, p);
                ids <- c(ids, next.id);
                new.frontier <- c(new.frontier, next.id);
                next.id <- next.id + 1;
                }
            }

        frontier <- new.frontier;
        }

    return(data.frame(
        parent = parent,
        length.1 = c(NA, rep(length.1, length(ids) - 1)),
        row.names = as.character(ids)
        ));
    }

# The shape drawn in the issue screenshot: two branches off the root, each
# continuing through a single child before rejoining the centre line.
issue.127.tree <- function() {
    return(data.frame(
        parent   = c(NA, 1, 1, 2, 3, 5, 6),
        length.1 = c(NA, 30, 25, 25, 25, 30, 30),
        length.2 = c(NA, 300, 250, 250, 250, 300, 300),
        row.names = as.character(1:7)
        ));
    }

with.cp <- function(tree) {
    tree$CP <- seq(0.95, 0.1, length.out = nrow(tree));
    return(tree);
    }

axis.names <- function(plt) {
    return(plt$childrenOrder[grepl('axis', plt$childrenOrder)]);
    }

# ----------------------------------------------------------------- claim A --

test_that('no two nodes are drawn at the same point', {
    # This is the exact failure in the screenshot. Before the angle damping,
    # nodes 11 and 12 of this tree rendered 0.0000 inches apart.
    for (depth in 2:4) {
        plt <- suppressMessages(SRCGrob(balanced.binary.tree(depth)));
        coincident <- find.coincident.nodes(plt);

        expect_null(
            coincident,
            info = paste(
                'depth', depth, 'binary tree:',
                describe.node.overlaps(coincident)
                )
            );
        }
    });

test_that('a symmetric binary tree renders without overlapping nodes', {
    plt <- suppressMessages(SRCGrob(balanced.binary.tree(3)));
    overlaps <- find.node.overlaps(plt);

    expect_null(overlaps, info = describe.node.overlaps(overlaps));
    });

test_that('sibling subtrees do not interleave', {
    # The two halves of a symmetric tree must stay on their own side of the
    # root. Undamped, the innermost leaves of each half met in the middle: the
    # gap between the two subtrees' x-ranges was exactly 0.
    plt <- suppressMessages(SRCGrob(balanced.binary.tree(3)));
    circles <- get.node.circles(plt);
    rownames(circles) <- circles$node;

    left.subtree <- as.character(c(2, 4, 5, 8, 9, 10, 11));
    right.subtree <- as.character(c(3, 6, 7, 12, 13, 14, 15));

    expect_lt(
        max(circles[left.subtree, 'x']),
        min(circles[right.subtree, 'x'])
        );
    });

test_that('the tree from the issue screenshot renders without overlaps', {
    for (tree in list(issue.127.tree(), with.cp(issue.127.tree()))) {
        plt <- suppressMessages(SRCGrob(
            tree,
            yaxis2.label = 'Distance (# of SNV)',
            add.normal = TRUE
            ));
        overlaps <- find.node.overlaps(plt);

        expect_null(overlaps, info = describe.node.overlaps(overlaps));
        }
    });

test_that('deep single-child chains are not narrowed by the damping', {
    # A chain has nothing to separate, so it must keep the undamped layout.
    chain <- data.frame(
        parent   = c(NA, 1:11),
        length.1 = c(NA, rep(30, 11)),
        row.names = as.character(1:12)
        );

    plt <- suppressMessages(SRCGrob(chain));
    overlaps <- find.node.overlaps(plt);

    expect_null(overlaps, info = describe.node.overlaps(overlaps));
    });

test_that('nested radial branching does not collide', {
    # Three children at every tier forces calculate.angles.radial rather than
    # calculate.angles.fixed; both paths accumulate the same way.
    nested <- data.frame(
        parent = c(NA, rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)),
        length.1 = c(NA, rep(30, 12)),
        row.names = as.character(1:13)
        );

    plt <- suppressMessages(SRCGrob(nested));
    overlaps <- find.node.overlaps(plt);

    expect_null(overlaps, info = describe.node.overlaps(overlaps));
    });

test_that('get.node.wedges leaves the layout alone at damping 0', {
    v <- data.frame(
        id     = c(1, 2, 3, 4, 5),
        parent = c(-1, 1, 1, 2, 2),
        leaves = c(3, 2, 1, 1, 1)
        );

    expect_equal(unname(get.node.wedges(v, damping = 0)), rep(1, 5));
    });

test_that('get.node.wedges splits a parent fan between its subtrees', {
    v <- data.frame(
        id     = c(1, 2, 3, 4, 5),
        parent = c(-1, 1, 1, 2, 2),
        leaves = c(3, 2, 1, 1, 1)
        );

    wedges <- get.node.wedges(v, damping = 1);

    expect_equal(wedges[['1']], 1);
    expect_equal(wedges[['2']], 2 / 3);
    expect_equal(wedges[['3']], 1 / 3);
    # Node 2 owns two thirds of the fan and halves it between its own children.
    expect_equal(wedges[['4']], 1 / 3);
    expect_equal(wedges[['5']], 1 / 3);

    # A partial damping level sits between the two.
    half <- get.node.wedges(v, damping = 0.5);
    expect_true(all(half[c('2', '3')] > wedges[c('2', '3')]));
    expect_true(all(half[c('2', '3')] < 1));
    });

test_that('measure.node.overlap scores encroachment, not distance', {
    v <- data.frame(id = c(1, 2), parent = c(-1, 1), x = c(0, 0), y = c(0, 1));

    expect_equal(measure.node.overlap(v, radii = c(0.25, 0.25)), 0);
    expect_equal(measure.node.overlap(v, radii = c(0.75, 0.75)), 0.5);
    # A node that is not drawn cannot collide with anything, even sitting on
    # top of its neighbour.
    expect_equal(measure.node.overlap(v, radii = c(0, 5)), 0);

    # Each pair counts once, not twice.
    three <- data.frame(id = 1:3, x = c(0, 0, 0), y = c(0, 1, 2));
    expect_equal(measure.node.overlap(three, radii = rep(0.75, 3)), 1);
    });

# ----------------------------------------------------------------- claim B --
#
# "when CP column is in the data.frame, the y.axis isn't working". A CP column
# switches on polygon (fish plot) mode via add.polygons in SRCGrob(); the claim
# is that doing so drops the y-axes. It does not, and has not since PR #191
# restored the axes that PR #190 suppressed -- these tests pin that down for the
# CP case specifically, which test-axis-plotting-direction.R does not cover for
# the default plotting direction.

test_that('a CP column does not remove the y-axes', {
    two.scales <- issue.127.tree();

    expect_setequal(
        axis.names(suppressMessages(SRCGrob(two.scales, yaxis2.label = 'nSNV'))),
        c('axis.left', 'axis.right')
        );

    # Adding CP adds the CCF x-axis; it must not take the y-axes away.
    expect_setequal(
        axis.names(suppressMessages(SRCGrob(with.cp(two.scales), yaxis2.label = 'nSNV'))),
        c('axis.bottom', 'axis.left', 'axis.right')
        );
    });

test_that('a CP column does not remove a single y-axis', {
    one.scale <- issue.127.tree();
    one.scale$length.2 <- NULL;

    expect_setequal(
        axis.names(suppressMessages(SRCGrob(one.scale))),
        'axis.left'
        );

    expect_setequal(
        axis.names(suppressMessages(SRCGrob(with.cp(one.scale)))),
        c('axis.bottom', 'axis.left')
        );
    });

test_that('a missing y-axis is caused by missing branch lengths, not by CP', {
    # get.y.axis.position() picks the y-axes from the length.* columns alone.
    # A tree with no branch lengths has nothing for a y-axis to measure, and
    # that is true with or without CP -- which is the likely explanation for the
    # report, since the issue passes lengths through "scale2" rather than a
    # length.2 column.
    no.lengths <- issue.127.tree();
    no.lengths$length.1 <- NULL;
    no.lengths$length.2 <- NULL;

    expect_length(axis.names(suppressMessages(SRCGrob(no.lengths))), 0);
    expect_setequal(
        axis.names(suppressMessages(SRCGrob(with.cp(no.lengths)))),
        'axis.bottom'
        );

    # Restoring a single length column restores the y-axis, CP or not.
    one.length <- no.lengths;
    one.length$length.1 <- c(NA, 30, 25, 25, 25, 30, 30);

    expect_true('axis.left' %in% axis.names(suppressMessages(SRCGrob(one.length))));
    expect_true('axis.left' %in% axis.names(suppressMessages(SRCGrob(with.cp(one.length)))));
    });

test_that('the y-axes survive CP together with the issue\'s other arguments', {
    plt <- suppressMessages(SRCGrob(
        with.cp(issue.127.tree()),
        scale2 = 2,
        yaxis2.label = 'Distance (# of SNV)',
        add.normal = TRUE
        ));

    expect_true(all(c('axis.left', 'axis.right') %in% axis.names(plt)));
    });
