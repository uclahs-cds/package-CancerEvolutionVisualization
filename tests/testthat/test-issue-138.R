# Regression tests for issue #138 -- "Inconsistent polygon behaviour when
# including CP column".
#
# The reported tree is a single-sample SRC result rendered as a fish plot.
# Node "4" (row 3) is a child of the MRCA (row 5) but was given a CP greater
# than the MRCA's CP, which is biologically impossible: a subclone cannot
# occupy more of the tumour than its own ancestor.

issue.138.tree <- function(CP.4 = 0.990) {
    data.frame(
        parent = c(5, 4, 5, 1, NA),
        label = c('1', '14', '4', '7', 'MRCA'),
        length.1 = c(8, 37, 11, 48, 122),
        CP = c(0.865, 0.560, CP.4, 0.860, 0.930),
        stringsAsFactors = FALSE
        );
    }

# Returns one row per clone polygon, in draw order, keyed by the node label
# the polygon belongs to. Polygon fill colours are unique per node, so the
# fill is used to join the grobs back onto the prepared tree.
get.clone.polygon.summary <- function(plt) {
    polygon.names <- grep(
        '^clone\\.polygon\\.',
        plt$childrenOrder,
        value = TRUE
        );

    node.data <- plt$input.data$tree;

    summary.df <- do.call(rbind, lapply(
        polygon.names,
        FUN = function(polygon.name) {
            polygon.grob <- plt$children[[polygon.name]];
            x <- as.numeric(polygon.grob$x);
            y <- as.numeric(polygon.grob$y);
            fill <- polygon.grob$gp$fill;

            data.frame(
                polygon = polygon.name,
                label = node.data$label.text[match(fill, node.data$color)],
                fill = fill,
                x.min = min(x),
                x.max = max(x),
                y.min = min(y),
                y.max = max(y),
                stringsAsFactors = FALSE
                );
            }
        ));

    rownames(summary.df) <- summary.df$label;
    return(summary.df);
    }

get.node.parents <- function(plt) {
    node.data <- plt$input.data$tree;
    parent.labels <- node.data$label.text[match(node.data$parent, node.data$id)];
    return(setNames(parent.labels, node.data$label.text));
    }

# Numerical slack for coordinates that should coincide exactly but are the
# result of long floating point chains.
polygon.tolerance <- 1e-6;

test_that(
    'CP greater than the parent CP is reported to the user', {
        expect_warning(
            SRCGrob(issue.138.tree(CP.4 = 0.990)),
            'cellular prevalence',
            ignore.case = TRUE
            );
        });

test_that(
    'CP warning names the offending node and its parent', {
        warning.message <- tryCatch(
            {
                SRCGrob(issue.138.tree(CP.4 = 0.990));
                '';
                },
            warning = function(w) conditionMessage(w)
            );

        # Row 3 is the offending node, row 5 is its parent (the MRCA).
        expect_match(warning.message, '3');
        expect_match(warning.message, '5');
        });

test_that(
    'Valid cellular prevalences produce no CP warning', {
        for (CP.4 in c(0.930, 0.800, 0.300)) {
            expect_no_warning(SRCGrob(issue.138.tree(CP.4 = CP.4)));
            }
        });

test_that(
    'No clone polygon extends beyond its parent clone polygon', {
        for (CP.4 in c(0.990, 0.930, 0.800, 0.300)) {
            plt <- suppressWarnings(SRCGrob(issue.138.tree(CP.4 = CP.4)));

            polygons <- get.clone.polygon.summary(plt);
            parents <- get.node.parents(plt);

            for (label in rownames(polygons)) {
                parent.label <- parents[[label]];

                if (is.na(parent.label) || !(parent.label %in% rownames(polygons))) {
                    next;
                    }

                expect_gte(
                    polygons[label, 'x.min'] - polygons[parent.label, 'x.min'],
                    -polygon.tolerance
                    );
                expect_lte(
                    polygons[label, 'x.max'] - polygons[parent.label, 'x.max'],
                    polygon.tolerance
                    );
                }
            }
        });

test_that(
    'Clone polygon widths never exceed the parent clone polygon width', {
        for (CP.4 in c(0.990, 0.930, 0.800, 0.300)) {
            plt <- suppressWarnings(SRCGrob(issue.138.tree(CP.4 = CP.4)));

            polygons <- get.clone.polygon.summary(plt);
            parents <- get.node.parents(plt);
            widths <- polygons$x.max - polygons$x.min;
            names(widths) <- rownames(polygons);

            for (label in names(widths)) {
                parent.label <- parents[[label]];

                if (is.na(parent.label) || !(parent.label %in% names(widths))) {
                    next;
                    }

                expect_lte(
                    widths[[label]] - widths[[parent.label]],
                    polygon.tolerance
                    );
                }
            }
        });

test_that(
    'CP above the parent CP renders identically to CP equal to the parent CP', {
        clamped <- suppressWarnings(SRCGrob(issue.138.tree(CP.4 = 0.990)));
        at.parent <- SRCGrob(issue.138.tree(CP.4 = 0.930));

        expect_equal(
            get.clone.polygon.summary(clamped)[, -1],
            get.clone.polygon.summary(at.parent)[, -1]
            );
        });

test_that(
    'Clone polygon width is monotonic in CP', {
        get.width <- function(CP.4, label) {
            polygons <- get.clone.polygon.summary(
                suppressWarnings(SRCGrob(issue.138.tree(CP.4 = CP.4)))
                );
            return(polygons[label, 'x.max'] - polygons[label, 'x.min']);
            }

        widths <- sapply(
            c(0.990, 0.930, 0.800, 0.300),
            FUN = function(CP.4) get.width(CP.4, '4')
            );

        expect_equal(widths, sort(widths, decreasing = TRUE));
        });

test_that(
    'Clamping cascades from an ancestor to its descendants', {
        # Node "7" (row 4) is a grandchild of the MRCA through node "1".
        # Both are given a CP above the MRCA's, so both must be pulled down.
        tree.df <- issue.138.tree(CP.4 = 0.800);
        tree.df$CP <- c(0.990, 0.560, 0.800, 0.995, 0.930);

        plt <- suppressWarnings(SRCGrob(tree.df));
        node.data <- plt$input.data$tree;
        CP.values <- setNames(node.data$ccf, node.data$label.text);

        expect_equal(CP.values[['1']], 0.930);
        expect_equal(CP.values[['7']], 0.930);
        expect_equal(CP.values[['MRCA']], 0.930);
        });

# The issue also reports that "the colour ordering is changed" at low CP.
# Measurement shows the fill assigned to each node does not change; what
# changes is the left-to-right placement and the draw order of the sibling
# clones, which both follow the descending-CP node ordering applied by
# reorder.nodes.by.CP. The two tests below pin that ordering rule so the
# behaviour cannot drift silently.

test_that(
    'Sibling clones are laid out left to right in descending CP order', {
        for (CP.4 in c(0.990, 0.930, 0.800, 0.300)) {
            plt <- suppressWarnings(SRCGrob(issue.138.tree(CP.4 = CP.4)));

            polygons <- get.clone.polygon.summary(plt);
            node.data <- plt$input.data$tree;
            x.mid <- (polygons$x.min + polygons$x.max) / 2;
            names(x.mid) <- rownames(polygons);

            for (parent.id in unique(node.data$parent)) {
                siblings <- node.data[node.data$parent == parent.id, ];

                if (nrow(siblings) < 2) {
                    next;
                    }

                siblings <- siblings[order(-(siblings$ccf)), ];
                expect_equal(
                    unname(x.mid[siblings$label.text]),
                    sort(unname(x.mid[siblings$label.text]))
                    );
                }
            }
        });

test_that(
    'Clone polygons are drawn ancestors first', {
        for (CP.4 in c(0.990, 0.930, 0.800, 0.300)) {
            plt <- suppressWarnings(SRCGrob(issue.138.tree(CP.4 = CP.4)));

            polygons <- get.clone.polygon.summary(plt);
            node.data <- plt$input.data$tree;
            draw.order <- setNames(
                seq_len(nrow(polygons)),
                polygons$label
                );
            parents <- get.node.parents(plt);

            for (label in names(draw.order)) {
                parent.label <- parents[[label]];

                if (is.na(parent.label) || !(parent.label %in% names(draw.order))) {
                    next;
                    }

                expect_lt(draw.order[[parent.label]], draw.order[[label]]);
                }
            }
        });

test_that(
    'Polygon colours stay bound to the same node as CP changes', {
        colours.by.label <- lapply(
            c(0.990, 0.930, 0.800, 0.300),
            FUN = function(CP.4) {
                polygons <- get.clone.polygon.summary(
                    suppressWarnings(SRCGrob(issue.138.tree(CP.4 = CP.4)))
                    );
                return(polygons[sort(rownames(polygons)), 'fill']);
                }
            );

        for (i in 2:length(colours.by.label)) {
            expect_equal(colours.by.label[[i]], colours.by.label[[1]]);
            }
        });
