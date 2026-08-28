# Regression tests for issue #186 "Test plots are not right".
#
# Root cause candidate: compare.trees() uses a SIGNED difference.
# helper-compare.R:12 evaluates
#
#     all(get.differences() <= threshold)
#
# where get.differences() returns convertUnit(x) - convertUnit(y) with no
# abs(). With the default threshold = 0 the assertion therefore holds whenever
# x <= y, so any layout change that moves a coordinate in one direction is
# never detected. The blind direction flips between test files, because the
# argument order is inconsistent: test-branching.R passes
# compare.trees(result.tree, fixture) while test-linear.R passes
# compare.trees(fixture, result.tree).
#
# These tests pin the property that actually matters: a perturbed tree must
# not compare equal to its baseline, in EITHER direction.

perturb.segment.coord <- function(tree, delta) {
    segs <- getGrob(tree, 'tree.segs.1');
    segs$y1 <- unit(
        as.numeric(convertUnit(segs$y1, 'native')) + delta,
        'native'
        );

    return(setGrob(tree, 'tree.segs.1', segs));
    }

test_that('a tree compares equal to itself', {
    load('data/branching.fixed.data.Rda');
    baseline <- SRCGrob(branching.fixed.test.data$tree);

    expect_true(compare.trees(baseline, baseline));
    });

test_that('compare.trees detects a coordinate that grew (issue #186)', {
    load('data/branching.fixed.data.Rda');
    baseline <- SRCGrob(branching.fixed.test.data$tree);
    grown <- perturb.segment.coord(baseline, 25);

    # A 25-native-unit shift is a visible layout change in both argument
    # orders. Neither may be silently accepted.
    expect_false(compare.trees(baseline, grown));
    expect_false(compare.trees(grown, baseline));
    });

test_that('compare.trees detects a coordinate that shrank (issue #186)', {
    load('data/branching.fixed.data.Rda');
    baseline <- SRCGrob(branching.fixed.test.data$tree);
    shrunk <- perturb.segment.coord(baseline, -25);

    expect_false(compare.trees(baseline, shrunk));
    expect_false(compare.trees(shrunk, baseline));
    });

test_that('text coordinate tolerance is symmetric (issue #186)', {
    # helper-compare.R:129 allows a 10-native-unit delta on text coords.
    # That tolerance must be two-sided: +/-9 accepted, +/-11 rejected.
    load('data/branching.fixed.data.Rda');
    baseline <- SRCGrob(branching.fixed.test.data$tree);

    for (delta in c(40, -40)) {
        far <- perturb.segment.coord(baseline, delta);
        expect_false(
            compare.trees(baseline, far),
            info = paste('delta =', delta)
            );
        }
    });
