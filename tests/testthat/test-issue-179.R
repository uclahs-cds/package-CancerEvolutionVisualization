# Regression tests for issue #179: node.text was unusable for any plotting
# direction other than 'down'.
#
# Originally the reproduction below died with
#
#     Error in strwidth(node.text, units = 'inches', cex = cex) :
#       invalid 'cex' value
#
# because the text-shrinking loops in position.node.text()/axis.overlap() were
# unsatisfiable for those directions and drove cex past zero. Commit ab0e9ff
# added floors to those loops, which stopped the crash but left the underlying
# bug: the labels were still shrunk all the way down to the 0.01/0.05 floor and
# rendered invisible, and numeric right/left angles flung them out of the panel
# entirely (x positions of ~1e14 inches).

issue.179.tree <- function() {
    data.frame(
        node.id = c('1', '2', '3'),
        parent = c(NA, '1', '1'),
        length.1 = 5
        );
    }

issue.179.text <- function() {
    data.frame(
        node = c('1', '1', '2', '3'),
        name = c('CNA', 'hello', '12', 'bird'),
        col = c('red', 'blue', 'green', 'purple'),
        fontface = c('bold', 'italic', NA, 'bold')
        );
    }

issue.179.directions <- list('down', 'up', 'left', 'right', 45, 135, -45, 90);

issue.179.plot <- function(direction, ...) {
    suppressMessages(suppressWarnings(create.phylogenetic.tree(
        issue.179.tree(),
        node.text = issue.179.text(),
        plotting.direction = direction,
        add.normal = TRUE,
        ...
        )));
    }

node.text.children <- function(plt) {
    text.tree <- plt$children[['node.text']];

    if (is.null(text.tree)) {
        return(list());
        }

    text.tree$children;
    }

test_that('node.text renders for every plotting direction (issue #179)', {
    for (direction in issue.179.directions) {
        expect_no_error(issue.179.plot(direction));
        }
    });

test_that('every node.text label is kept for every plotting direction (issue #179)', {
    for (direction in issue.179.directions) {
        labels <- vapply(
            node.text.children(issue.179.plot(direction)),
            FUN = function(grob) as.character(grob$label),
            FUN.VALUE = character(1)
            );

        expect_setequal(labels, issue.179.text()$name);
        }
    });

test_that('node.text is not shrunk into invisibility (issue #179)', {
    # The shrink loops bottom out at cex 0.05 (axis.overlap) and 0.01
    # (position.node.text). Either floor means the label is on the page but
    # far too small to read.
    requested.cex <- 0.85;

    for (direction in issue.179.directions) {
        text.cex <- vapply(
            node.text.children(issue.179.plot(direction, node.text.cex = requested.cex)),
            FUN = function(grob) grob$gp$cex,
            FUN.VALUE = numeric(1)
            );

        expect_length(text.cex, nrow(issue.179.text()));
        expect_true(all(text.cex > requested.cex / 2));
        }
    });

test_that('node.text stays near the panel for every plotting direction (issue #179)', {
    # A near-horizontal branch has a slope of ~0, so back-computing x from y
    # produced coordinates of ~1e14 inches for numeric left/right angles.
    for (direction in issue.179.directions) {
        plt <- issue.179.plot(direction);
        text.grobs <- node.text.children(plt);

        panel.width <- convertWidth(plt$vp$width, 'inches', valueOnly = TRUE);
        panel.height <- convertHeight(plt$vp$height, 'inches', valueOnly = TRUE);

        for (grob in text.grobs) {
            xpos <- convertX(grob$x, 'inches', valueOnly = TRUE);
            ypos <- convertY(grob$y, 'inches', valueOnly = TRUE);

            expect_true(is.finite(xpos));
            expect_true(is.finite(ypos));

            # The normal-node stub sits outside the panel, so labels on the
            # root branch legitimately overhang it. One panel width/height of
            # slack is generous; 1e14 is not a rounding error.
            expect_lt(abs(xpos), 2 * panel.width + 1);
            expect_lt(abs(ypos), 2 * panel.height + 1);
            }
        }
    });

test_that('a numeric right angle matches the named direction (issue #179)', {
    # 90 degrees is 'right'. is.horizontal() in add.text.R only tested the
    # character names, so numeric angles skipped the horizontal text layout.
    describe.text <- function(direction) {
        # Grob names are auto-generated and differ between calls.
        unname(lapply(
            node.text.children(issue.179.plot(direction)),
            FUN = function(grob) {
                list(
                    label = grob$label,
                    x = convertX(grob$x, 'inches', valueOnly = TRUE),
                    y = convertY(grob$y, 'inches', valueOnly = TRUE),
                    just = grob$just,
                    cex = grob$gp$cex
                    );
                }
            ));
        }

    expect_equal(describe.text(90), describe.text('right'));
    expect_equal(describe.text(-90), describe.text('left'));
    });
