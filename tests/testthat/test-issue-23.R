# Issue #23 - "Genes on nodes mode unstable".
#
# Setting `genes.on.nodes` drew every gene name for a node on top of every
# other one. The resolution was to remove the option (07aba5d, 2022-06-24),
# and the parameter was later renamed to `text.on.nodes` (7e06ee3, 2022-09-01).
#
# The layout code behind it was never fixed - it is still present, and still
# broken, in R/add.text.R. It is only harmless because `text.on.nodes` is
# hardcoded to FALSE in SRCGrob and cannot be reached from the public API.
#
# These tests pin that state: the switch stays unreachable, and the surviving
# "text along the branch" layout keeps multiple labels on one node separated.
# If anyone re-exposes the option, the first two tests fail and point here.

collect.text.positions <- function(grob) {
    positions <- list();

    walk <- function(g) {
        if (inherits(g, 'text') && length(g$label) > 0) {
            positions[[length(positions) + 1]] <<- data.frame(
                label = as.character(g$label),
                x = rep(
                    convertX(g$x, 'native', valueOnly = TRUE),
                    length.out = length(g$label)
                    ),
                y = rep(
                    convertY(g$y, 'native', valueOnly = TRUE),
                    length.out = length(g$label)
                    ),
                stringsAsFactors = FALSE
                );
            }

        if (!is.null(g$children)) {
            for (child in g$children) {
                walk(child);
                }
            }
        };

    walk(grob);

    return(do.call(rbind, positions));
    }

issue.23.tree <- function() {
    data.frame(
        parent   = c(NA, 1, 1, 2, 2),
        node.id  = c('1', '2', '3', '4', '5'),
        length.1 = c(NA, 20, 25, 15, 18),
        stringsAsFactors = FALSE
        );
    }

# Five gene labels on a single node - the scenario from the issue.
issue.23.node.text <- function() {
    data.frame(
        node = c(2, 2, 2, 2, 2, 3, 3, 5),
        name = c('TP53', 'BRCA1', 'PTEN', 'MYC', 'EGFR', 'KRAS', 'RB1', 'APC'),
        stringsAsFactors = FALSE
        );
    }

test_that(
    'the genes.on.nodes option is gone', {
        expect_false('genes.on.nodes' %in% names(formals(SRCGrob)));
        expect_false(exists('genes.on.nodes'));
    });

test_that(
    'its successor text.on.nodes is not user-reachable either', {
        # text.on.nodes is a hardcoded local in SRCGrob, not a parameter. The
        # on-node layout it guards still piles every label for a node onto a
        # single point, so it must stay switched off until that is fixed.
        expect_false('text.on.nodes' %in% names(formals(SRCGrob)));

        src <- deparse(SRCGrob);
        expect_length(grep('text\\.on\\.nodes\\s*<-\\s*FALSE', src), 1);
        expect_length(grep('text\\.on\\.nodes\\s*<-\\s*TRUE', src), 0);
    });

test_that(
    'multiple labels on one node are drawn at distinct positions', {
        node.text <- issue.23.node.text();

        plt <- suppressWarnings(SRCGrob(
            issue.23.tree(),
            node.text = node.text
            ));

        positions <- collect.text.positions(plt);
        gene.labels <- positions[positions$label %in% node.text$name, ];

        expect_equal(nrow(gene.labels), nrow(node.text));

        # No two labels may share a position.
        expect_equal(
            sum(duplicated(round(gene.labels[, c('x', 'y')], 6))),
            0
            );
    });

test_that(
    'the five labels on a single node are stacked, not overlaid', {
        node.text <- issue.23.node.text();
        node.2.genes <- node.text$name[node.text$node == 2];

        plt <- suppressWarnings(SRCGrob(
            issue.23.tree(),
            node.text = node.text
            ));

        positions <- collect.text.positions(plt);
        stacked <- positions[positions$label %in% node.2.genes, ];

        expect_equal(nrow(stacked), length(node.2.genes));

        # Every label gets its own row in the stack. Under the broken on-node
        # layout all five collapsed onto one y value.
        expect_equal(length(unique(round(stacked$y, 6))), length(node.2.genes));
        expect_gt(diff(range(stacked$y)), 0);
    });

test_that(
    'each node keeps its own labels together', {
        # The broken on-node layout also flung labels far apart - node 3's two
        # labels landed on the panel edge while node 5's went off the other
        # side. Labels sharing a node must stay in one compact stack.
        node.text <- issue.23.node.text();

        plt <- suppressWarnings(SRCGrob(
            issue.23.tree(),
            node.text = node.text
            ));

        positions <- collect.text.positions(plt);
        gene.labels <- positions[positions$label %in% node.text$name, ];
        rownames(gene.labels) <- gene.labels$label;

        node.of <- setNames(node.text$node, node.text$name);
        total.spread <- diff(range(gene.labels$y));

        for (node in unique(node.text$node)) {
            labels <- names(node.of)[node.of == node];
            if (length(labels) < 2) next;

            expect_lt(diff(range(gene.labels[labels, 'y'])), total.spread);
            }
    });
