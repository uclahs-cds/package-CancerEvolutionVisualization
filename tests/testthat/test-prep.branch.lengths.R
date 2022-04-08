test_that(
    'validate.branch.colname handles valid case', {
        expect_true(validate.branch.colname('length10'));
    });

test_that(
    'validate.branch.colname handles invalid case', {
        expect_false(validate.branch.colname('invalid'));
    });

test_that(
    'validate.branch.length.values handles valid case', {
        length.column <- 1:10;
        
        expect_true(validate.branch.length.values(length.column));
    });

test_that(
    'validate.branch.length.values handles invalid case', {
        length.column <- rep('invalid', 5);
        
        expect_false(validate.branch.length.values(length.column));
    });

test_that(
    'limit.branch.length.columns uses default limit', {
        length.columns <- rep('length', 10);
        
        tryCatch(
            result.colnames <- limit.branch.length.columns(length.columns)
            );
        
        expect_less_than(
            length(result.colnames),
            10
            );
    });

test_that(
    'limit.branch.length.columns reports when length columns have been truncated', {
        length.columns <- rep('length', 5);

        max.branches <- 3;
        
        expect_message(limit.branch.length.columns(length.columns));
    });

test_that(
    'get.branch.length.colnames returns the correct length', {
        expected.length <- 5;

        expect_length(
            get.branch.length.colnames(expected.length),
            expected.length
            );
    });

test_that(
    'get.branch.length.colnames handles length 0', {
        expected.length <- 0;

        expect_length(
            get.branch.length.colnames(expected.length),
            expected.length
            );
    });

test_that(
    'get.default.branch.lengths creates correct size', {
        expected.length <- 10;
        
        expect_equal(
            nrow(get.default.branch.lengths(expected.length)),
            expected.length
            );
    });

test_that(
    'get.default.branch.lengths creates correct value', {
        expected.value <- 1;
        
        expect_true(all(
            apply(
                get.default.branch.lengths(3),
                MARGIN = 1,
                FUN = function(x) {
                    x == expected.value;
                    }
                )
            ));
    });

test_that(
    'prep.branch.lengths handles valid length columns', {
        original.tree <- data.frame(test.length = 1:10);
        branch.lengths <- prep.branch.lengths(original.tree);
        
        expect_true(is.data.frame(branch.lengths));
    });

test_that(
    'prep.branch.lengths handles no length columns', {
        original.tree <- data.frame();
        branch.lengths <- prep.branch.lengths(original.tree);
        
        expect_true(is.data.frame(branch.lengths));
    });
