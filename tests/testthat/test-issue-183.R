# Regression tests for issue #183 "Disproportionate y-axis to y-axis label spacing".
#
# add.axis.label() positions the rotated axis title just outside the tick
# labels. It measured the tick-label grob with convertX()/grobWidth() while
# that grob was detached from its parent axis gTree, so the gpar(cex = axis.cex)
# that grid applies when the axis is actually drawn was ignored. The tick
# labels therefore render wider (and further from the axis line) than the
# measurement assumed, and the axis title lands inside the intended gap.
#
# The shortfall equals the un-measured cex expansion, which scales with the
# width of the tick-label text. The two y-axes almost never carry equally wide
# tick labels (PGA fractions on the left, SNV counts on the right), so the two
# gaps end up different -- the "disproportionate" spacing in the issue.

issue.183.tree <- function() {
    data.frame(
        parent   = c('0', '1', '2', '3'),
        child    = c('1', '2', '3', '4'),
        label    = c('MRCA', '4', '13', '24'),
        length.1 = c(140, 80, 60, 50),
        length.2 = c(3.5, 2, 1.5, 1.2),
        stringsAsFactors = FALSE
        );
    }

issue.183.plot <- function(...) {
    suppressMessages(SRCGrob(
        issue.183.tree(),
        yaxis1.label = 'yaxis1',
        yaxis2.label = 'yaxis2',
        yat = list(seq(0, 400, 100), seq(0, 10, 2)),
        ...
        ));
    }

# Distance, in inches, between the outer edge of the tick labels and the near
# edge of the rotated axis title. Measured in the graphical context the axis is
# actually drawn in, so the axis gTree's cex is applied to the tick labels.
axis.label.gap <- function(plt, axis.name) {
    axis.grob <- plt$children[[axis.name]];
    title.grob <- getGrob(axis.grob, 'axis.label');
    content.grob <- axis.grob$children[['axis.content']];
    tick.grob <- getGrob(content.grob, 'labels');

    pushViewport(plt$vp);
    pushViewport(axis.grob$vp);

    title.x <- convertX(title.grob$x[1], 'inches', valueOnly = TRUE);

    pushViewport(viewport(gp = content.grob$gp));
    tick.x <- convertX(tick.grob$x[1], 'inches', valueOnly = TRUE);
    tick.width <- convertWidth(
        max(stringWidth(tick.grob$label)),
        'inches',
        valueOnly = TRUE
        );
    popViewport(3);

    if (axis.name == 'axis.left') {
        (tick.x - tick.width) - title.x;
    } else {
        title.x - (tick.x + tick.width);
        }
    }

with.null.device <- function(expr) {
    pdf(NULL, width = 7, height = 7);
    on.exit(dev.off(), add = TRUE);
    grid.newpage();
    force(expr);
    }

test_that('both y-axis labels sit the same distance from their tick labels', {
    with.null.device({
        plt <- issue.183.plot();

        left.gap <- axis.label.gap(plt, 'axis.left');
        right.gap <- axis.label.gap(plt, 'axis.right');

        # Same padding on both sides, so the same gap on both sides.
        expect_equal(left.gap, right.gap, tolerance = 1e-6);
        });
    });

test_that('the y-axis label gap matches the requested padding', {
    with.null.device({
        plt <- issue.183.plot();

        # Default padding is 1, i.e. the historic 1.5 lines.
        expected.gap <- convertWidth(unit(1.5, 'lines'), 'inches', valueOnly = TRUE);

        expect_equal(axis.label.gap(plt, 'axis.left'), expected.gap, tolerance = 1e-6);
        expect_equal(axis.label.gap(plt, 'axis.right'), expected.gap, tolerance = 1e-6);
        });
    });

test_that('ylab.axis.padding scales the gap on both y-axes', {
    with.null.device({
        default.plt <- issue.183.plot();
        wide.plt <- issue.183.plot(ylab.axis.padding = 3);
        tight.plt <- issue.183.plot(ylab.axis.padding = 0);

        for (axis.name in c('axis.left', 'axis.right')) {
            default.gap <- axis.label.gap(default.plt, axis.name);
            wide.gap <- axis.label.gap(wide.plt, axis.name);
            tight.gap <- axis.label.gap(tight.plt, axis.name);

            expect_equal(wide.gap, 3 * default.gap, tolerance = 1e-6);
            expect_equal(tight.gap, 0, tolerance = 1e-6);
            }
        });
    });

test_that('ylab.axis.padding leaves the x-axis label alone', {
    with.null.device({
        get.x.label.y <- function(plt) {
            convertY(
                getGrob(plt$children[['axis.bottom']], 'axis.label')$y[1],
                'inches',
                valueOnly = TRUE
                );
            }

        tree <- issue.183.tree();
        tree$CP <- c(1, 0.8, 0.6, 0.4);
        tree$ccf <- c(1, 0.8, 0.6, 0.4);

        default.plt <- suppressMessages(SRCGrob(tree, yaxis2.label = 'yaxis2'));
        padded.plt <- suppressMessages(SRCGrob(
            tree,
            yaxis2.label = 'yaxis2',
            ylab.axis.padding = 4
            ));

        pushViewport(default.plt$vp);
        expect_equal(get.x.label.y(default.plt), get.x.label.y(padded.plt));
        popViewport();
        });
    });

test_that('ylab.axis.padding rejects invalid values', {
    expect_error(
        suppressMessages(issue.183.plot(ylab.axis.padding = -1)),
        'ylab.axis.padding'
        );
    expect_error(
        suppressMessages(issue.183.plot(ylab.axis.padding = 'wide')),
        'ylab.axis.padding'
        );
    });
