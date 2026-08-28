# Issue #162 - "Column names for length results in inconsistent behaviour".
#
# The user guide used to say "All columns whose names contain `length` will be
# used. (For example, `length.1` and `snv.length` are both valid.)". That was
# never true. The contract the code actually implements - and the one the
# maintainers settled on - is that a branch length column name must START with
# "length" (`length.1`, `length.2`, `length.<edge name>`).
#
# These tests pin that `^length` contract end to end, and pin the four column
# name combinations reported in the issue.

axis.names <- function(grob) {
    grob$childrenOrder[grepl('axis', grob$childrenOrder)];
    }

issue.162.tree <- function(first.colname, second.colname) {
    tree <- data.frame(
        parent = c(NA, 1, 1, 2),
        stringsAsFactors = FALSE
        );

    tree[, first.colname] <- c(NA, 10, 20, 30);
    tree[, second.colname] <- c(NA, 100, 200, 300);

    return(tree);
    }

test_that(
    'a branch length column name must start with "length"', {
        expect_true(validate.branch.colname('length.1'));
        expect_true(validate.branch.colname('length1'));
        expect_true(validate.branch.colname('length.snv'));

        # `<edge name>.length` is NOT a branch length column.
        expect_false(validate.branch.colname('snv.length'));
        expect_false(validate.branch.colname('pga.length'));

        # The prefix is case sensitive.
        expect_false(validate.branch.colname('Length.1'));
    });

test_that(
    'get.branch.names ignores trailing "length" column names', {
        # A `<edge name>.length` column must not manufacture a phantom branch.
        # Before the fix, `snv.length` produced the branch name "snv.", which
        # made prep.tree invent an all-default `length.snv.` column.
        tree.df <- data.frame(snv.length = 1:10, pga.length = 1:10);

        expect_length(get.branch.names(tree.df), 0);
    });

test_that(
    'get.branch.names still recognises the documented prefixes', {
        tree.df <- data.frame(
            length.1 = 1:10,
            edge.col.2 = 'black',
            stringsAsFactors = FALSE
            );

        expect_setequal(get.branch.names(tree.df), c('1', '2'));
    });

test_that(
    'prep.branch.lengths only uses columns starting with "length"', {
        tree.df <- data.frame(snv.length = 1:10, length.2 = 11:20);

        lengths.df <- suppressWarnings(prep.branch.lengths(tree.df));

        expect_named(lengths.df, 'length1');
        expect_equal(lengths.df$length1, 11:20);
    });

test_that(
    'an ignored "<name>.length" column raises a warning naming the column', {
        # The original confusion in #162 was that `snv.length` was dropped in
        # total silence. Tell the user instead.
        tree.df <- data.frame(snv.length = 1:10, length.2 = 11:20);

        expect_warning(
            warn.ignored.length.columns(colnames(tree.df)),
            regexp = 'snv\\.length'
            );
        expect_warning(
            warn.ignored.length.columns(colnames(tree.df)),
            regexp = 'length'
            );
    });

test_that(
    'valid length columns do not raise the ignored-column warning', {
        expect_silent(warn.ignored.length.columns(c('parent', 'length.1', 'length.2')));
        expect_silent(warn.ignored.length.columns(c('parent', 'CP', 'edge.col.1')));
    });

test_that(
    'SRCGrob warns about an ignored "<name>.length" column', {
        expect_warning(
            SRCGrob(issue.162.tree('snv.length', 'length.2')),
            regexp = 'snv\\.length'
            );
    });

# The four combinations from the issue body. Each pins the number of y-axes and
# which column actually supplied the branch lengths.

test_that(
    'issue #162 (a): length.1 + length.2 gives two axes and two scales', {
        tree <- issue.162.tree('length.1', 'length.2');

        expect_equal(get.y.axis.position(colnames(tree)), 'both');

        plt <- suppressWarnings(SRCGrob(tree));
        expect_setequal(axis.names(plt), c('axis.left', 'axis.right'));
    });

test_that(
    'issue #162 (b): snv.length + length.2 gives one axis, from length.2', {
        tree <- issue.162.tree('snv.length', 'length.2');

        expect_equal(get.y.axis.position(colnames(tree)), 'left');

        inputs <- suppressWarnings(prep.tree(
            tree,
            NULL,
            polygon.colour.scheme = gradient.color.scheme('grey', nrow(tree)),
            default.node.colour = 'white'
            ));

        # Exactly one branch length, taken from length.2 - not a phantom
        # second branch invented from snv.length.
        expect_named(inputs$tree, c('parent', 'tip', 'length1'));
        expect_equal(inputs$tree$length1, c(1, 100, 200, 300));

        plt <- suppressWarnings(SRCGrob(tree));
        expect_setequal(axis.names(plt), 'axis.left');
    });

test_that(
    'issue #162 (c): snv.length + length.3 gives one axis, from length.3', {
        tree <- issue.162.tree('snv.length', 'length.3');

        expect_equal(get.y.axis.position(colnames(tree)), 'left');

        inputs <- suppressWarnings(prep.tree(
            tree,
            NULL,
            polygon.colour.scheme = gradient.color.scheme('grey', nrow(tree)),
            default.node.colour = 'white'
            ));

        expect_named(inputs$tree, c('parent', 'tip', 'length1'));
        expect_equal(inputs$tree$length1, c(1, 100, 200, 300));

        plt <- suppressWarnings(SRCGrob(tree));
        expect_setequal(axis.names(plt), 'axis.left');
    });

test_that(
    'issue #162 (d): snv.length + pga.length gives no axes and no scales', {
        tree <- issue.162.tree('snv.length', 'pga.length');

        expect_equal(get.y.axis.position(colnames(tree)), 'none');

        inputs <- suppressWarnings(prep.tree(
            tree,
            NULL,
            polygon.colour.scheme = gradient.color.scheme('grey', nrow(tree)),
            default.node.colour = 'white'
            ));

        # Neither column is a branch length, so every branch falls back to the
        # default length of 1.
        expect_named(inputs$tree, c('parent', 'tip', 'length1'));
        expect_true(all(inputs$tree$length1 == 1));

        plt <- suppressWarnings(SRCGrob(tree));
        expect_length(axis.names(plt), 0);
    });

test_that(
    'both length columns are ignored when neither starts with "length"', {
        tree <- issue.162.tree('snv.length', 'pga.length');

        warnings.raised <- NULL;
        withCallingHandlers(
            SRCGrob(tree),
            warning = function(w) {
                warnings.raised <<- c(warnings.raised, conditionMessage(w));
                invokeRestart('muffleWarning');
                }
            );

        expect_true(any(grepl('snv\\.length', warnings.raised)));
        expect_true(any(grepl('pga\\.length', warnings.raised)));
    });
