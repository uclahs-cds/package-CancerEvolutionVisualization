test_that(
    'extract.length.colnames handles valid case', {
        length.columns <- c('length2', '1length');
        
        start.index <- 4;
        
        # Fill all indices before start index with invalid column names
        all.columns <- c(rep('no', start.index - 1), length.columns, 'test', 'col');
    
        expected.output <- start.index:(start.index + length(length.columns) - 1);

        expect_setequal(
              extract.length.colnames(all.columns),
              expected.output
        );
    });

test_that(
    'extract.length.colnames limits number of columns', {
        length.columns <- rep('length', 10);

        max.branches <- 2;
        
        
        tryCatch(
            result.colnames <- extract.length.colnames(length.columns)
            );
        
        expect_length(
            result.colnames,
            max.branches
            );
    });

test_that(
    'extract.length.colnames reports when length columns have been truncated', {
        length.columns <- rep('length', 5);

        max.branches <- 3;
        
        expect_message(extract.length.colnames(length.columns));
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
