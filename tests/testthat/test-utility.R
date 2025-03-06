test_that('get.value.index creates a valid list', {
    expected.length <- 6;

    old.values <- sample.int(expected.length, expected.length);
    new.values <- old.values + 1;

    expect_false(is.null(get.value.index(old.values, new.values)));
    });

test_that('get.value.index handles invalid values', {
    valid.length <- 5;

    expect_error(get.value.index(
        1:valid.length,
        1:(valid.length - 1)
        ));
    });

test_that(
    'reindex.column handles valid values', {
        expected.length <- 5;
        new.value.offset <- 2;

        old.values <- sample.int(expected.length, expected.length);
        new.values <- 1:(expected.length) + new.value.offset;

        value.index <- as.list(new.values);
        names(value.index) <- as.character(1:expected.length);

        reindexed <- reindex.column(old.values, value.index);
        expected.values <- old.values + new.value.offset;

        expect_equal(as.numeric(reindexed), expected.values);
    });

test_that(
    'validate.data.frame.columns handles present columns', {
        expected.columns <- c('x', 'y');

        input.data <- data.frame(a = 1:10, b = 1:10);
        colnames(input.data) <- expected.columns;

        expect_no_error({
            validate.data.frame.columns(
                input.data,
                expected.columns = expected.columns
                );
            });
    });

test_that(
    'validate.data.frame.columns errors on missing columns', {
        expected.columns <- c('x', 'y');

        input.data <- data.frame(a = 1:10, b = 1:10);

        expect_error(
            object = {
                validate.data.frame.columns(
                    input.data,
                    expected.columns = expected.columns
                    );
                },
            regexp = paste(expected.columns, collapse = ', ')
            );
    });

test_that(
    'get.encoded.distance returns valid relative values', {
        # Testing the order, as the absolute values are not as important.
        point.values <- c(-3, 1, 0, 2, -1);
        points <- data.frame(x = point.values, y = point.values);

        result <- get.encoded.distance(points);
        expected.order <- order(point.values);

        expect_equal(order(result), expected.order);
    });

test_that(
    'get.clone.colours returns expected vectors', {
        result <- get.clone.colours(NULL, NULL);
        expect_null(result);
        result <- get.clone.colours(NULL, c('ABC', 'DEF'));
        expect_null(result);
        result <- get.clone.colours(c('red','green'), NULL);
        expect_null(result);
        result <- get.clone.colours(c(), c());
        expect_null(result);
        result <- get.clone.colours(NULL, c());
        expect_null(result);
        result <- get.clone.colours(c(), NULL);
        expect_null(result);
        result <- get.clone.colours(c(), c('ABC', 'DEF'));
        expect_null(result);
        result <- get.clone.colours(c('red', 'green'), c());
        expect_null(result);
        result <- get.clone.colours(c('red','green'), c('ABC','DEF'));
        expect_equal(result, c(ABC = 'red', DEF = 'green'));
        result <- get.clone.colours(c('red'), c('ABC','DEF'));
        expect_equal(names(result), c('ABC', 'DEF'));
        expect_true(result['ABC'] == 'red');
        expect_true(!is.na(result['DEF']) && nzchar(result['DEF']));
        result <- get.clone.colours(c(NULL,'red'), c('ABC','DEF'));
        expect_equal(names(result), c('ABC', 'DEF'));
        expect_true(result['ABC'] == 'red');
        expect_true(!is.na(result['DEF']) && nzchar(result['DEF']));
        result <- get.clone.colours(c('red',NULL), c('ABC','DEF'));
        expect_equal(names(result), c('ABC', 'DEF'));
        expect_true(result['ABC'] == 'red');
        expect_true(!is.na(result['DEF']) && nzchar(result['DEF']));
        result <- get.clone.colours(c('red','green'), c('ABC'));
        expect_equal(result, c(ABC = 'red'));
        result <- get.clone.colours(c('red','green'), c(NULL,'ABC'));
        expect_equal(result, c(ABC = 'red'));
        result <- get.clone.colours(c('red','green'), c('ABC',NULL));
        expect_equal(result, c(ABC = 'red'));
    });

test_that(
    'get.clone.colours returns expected vectors with minimum colours specified', {
        result <- get.clone.colours(NULL, NULL, 3)
        expect_null(result);
        result <- get.clone.colours(c('red', 'green'), NULL, 3)
        expect_null(result);
        result <- get.clone.colours(c('red'), NULL, 3)
        expect_null(result);
        result <- get.clone.colours(c('red', 'green'), c('ABC','DEF'), 3)
        expect_equal(result, c(ABC = 'red', DEF = 'green'));
        result <- get.clone.colours(c('red'), c('ABC','DEF'), 3)
        expect_equal(names(result), c('ABC', 'DEF'));
        expect_true(result['ABC'] == 'red');
        expect_true(!is.na(result['DEF']) && nzchar(result['DEF']));
        result <- get.clone.colours(c('red', 'green'), c('ABC'), 3)
        expect_equal(result, c(ABC = 'red'));
    });

test_that(
    'get.clone.colours.in.order returns expected vectors when order is not specified', {
        # get.clone.colours.in.order has same result as get.clone.colours
        # with the change that a named list is returned with two members
        result <- get.clone.colours.in.order(NULL, NULL)
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = NULL)))
        result <- get.clone.colours.in.order(NULL, c('ABC', 'DEF'))
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = NULL)))
        result <- get.clone.colours.in.order(c('red','green'), NULL)
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = NULL)))
        result <- get.clone.colours.in.order(c(), c())
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = NULL)))
        result <- get.clone.colours.in.order(NULL, c())
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = NULL)))
        result <- get.clone.colours.in.order(c(), NULL)
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = NULL)))
        result <- get.clone.colours.in.order(c(), c('ABC', 'DEF'))
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = NULL)))
        result <- get.clone.colours.in.order(c('red', 'green'), c())
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = NULL)))
        result <- get.clone.colours.in.order(c('red','green'), c('ABC','DEF'))
        expect_true(setequal(result,list(clone.colours = c(ABC = 'red',DEF = 'green'), clone.order = c('ABC','DEF'))))
        result <- get.clone.colours.in.order(c('red'), c('ABC','DEF'))
        expect_equal(names(result), c('clone.colours', 'clone.order'))
        expect_equal(names(result$clone.colours), c('ABC', 'DEF'))
        expect_true(result$clone.colours['ABC'] == 'red')
        expect_true(!is.na(result$clone.colours['DEF']) && nzchar(result$clone.colours['DEF']))
        expect_equal(result$clone.order, c('ABC','DEF'))

        result <- get.clone.colours.in.order(c(NULL,'red'), c('ABC','DEF'))
        expect_equal(names(result), c('clone.colours', 'clone.order'))
        expect_equal(names(result$clone.colours), c('ABC','DEF'))
        expect_true(result$clone.colours['ABC'] == 'red')
        expect_true(!is.na(result$clone.colours['DEF']) && nzchar(result$clone.colours['DEF']))
        expect_equal(result$clone.order, c('ABC','DEF'))

        result <- get.clone.colours.in.order(c('red',NULL), c('ABC','DEF'))
        expect_equal(names(result), c('clone.colours', 'clone.order'))
        expect_equal(names(result$clone.colours), c('ABC','DEF'))
        expect_true(result$clone.colours['ABC'] == 'red')
        expect_true(!is.na(result$clone.colours['DEF']) && nzchar(result$clone.colours['DEF']))
        expect_equal(result$clone.order, c('ABC','DEF'))

        result <- get.clone.colours.in.order(c('red','green'), c('ABC'))
        expect_true(setequal(result,list(clone.colours = c(ABC = 'red'), clone.order = c('ABC'))))
        result <- get.clone.colours.in.order(c('red','green'), c(NULL,'ABC'))
        expect_true(setequal(result,list(clone.colours = c(ABC = 'red'), clone.order = c('ABC'))))
        result <- get.clone.colours.in.order(c('red','green'), c('ABC',NULL))
        expect_true(setequal(result,list(clone.colours = c(ABC = 'red'), clone.order = c('ABC'))))
    });

test_that(
    'get.clone.colours.in.order returns expected vectors when order is specified', {
        # get.clone.colours.in.order with order specified
        result <- get.clone.colours.in.order(NULL, NULL, c('DEF','ABC'))
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = c('DEF','ABC'))))
        result <- get.clone.colours.in.order(NULL, c('ABC', 'DEF'), c('DEF','ABC'))
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = c('DEF','ABC'))))
        result <- get.clone.colours.in.order(c('red','green'), NULL)
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = NULL)))
        result <- get.clone.colours.in.order(c(), c(), c('DEF','ABC'))
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = c('DEF','ABC'))))
        result <- get.clone.colours.in.order(NULL, c(), c('DEF','ABC'))
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = c('DEF','ABC'))))
        result <- get.clone.colours.in.order(c(), NULL, c('DEF','ABC'))
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = c('DEF','ABC'))))
        result <- get.clone.colours.in.order(c(), c('ABC', 'DEF'), c('DEF','ABC'))
        expect_true(setequal(result, list(clone.colours = NULL, clone.order = c('DEF','ABC'))))
        result <- get.clone.colours.in.order(c('red', 'green'), c(), c('DEF','ABC'))
        expect_true(setequal(result, list(clone.colours = c(DEF = 'red',ABC = 'green'), clone.order = c('DEF','ABC'))))
        result <- get.clone.colours.in.order(c('red','green'), c('ABC','DEF'), c('DEF','ABC'))
        expect_true(setequal(result,list(clone.colours = c(DEF = 'red',ABC = 'green'), clone.order = c('DEF','ABC'))))
        result <- get.clone.colours.in.order(c('red'), c('ABC','DEF'), c('DEF','ABC'))
        expect_equal(names(result), c('clone.colours', 'clone.order'))
        expect_equal(names(result$clone.colours), c('DEF', 'ABC'))
        expect_true(result$clone.colours['DEF'] == 'red')
        expect_true(!is.na(result$clone.colours['ABC']) && nzchar(result$clone.colours['ABC']))
        expect_equal(result$clone.order, c('DEF','ABC'))

        result <- get.clone.colours.in.order(c(NULL,'red'), c('ABC','DEF'), c('DEF','ABC'))
        expect_equal(names(result), c('clone.colours', 'clone.order'))
        expect_equal(names(result$clone.colours), c('DEF','ABC'))
        expect_true(result$clone.colours['DEF'] == 'red')
        expect_true(!is.na(result$clone.colours['ABC']) && nzchar(result$clone.colours['ABC']))
        expect_equal(result$clone.order, c('DEF','ABC'))

        result <- get.clone.colours.in.order(c('red',NULL), c('ABC','DEF'), c('DEF','ABC'))
        expect_equal(names(result), c('clone.colours', 'clone.order'))
        expect_equal(names(result$clone.colours), c('DEF','ABC'))
        expect_true(result$clone.colours['DEF'] == 'red')
        expect_true(!is.na(result$clone.colours['ABC']) && nzchar(result$clone.colours['ABC']))
        expect_equal(result$clone.order, c('DEF','ABC'))

        result <- get.clone.colours.in.order(c('red','green'), c('ABC'), c('DEF','ABC'))
        expect_true(setequal(result,list(clone.colours = c(DEF = 'red',ABC = 'green'), clone.order = c('DEF','ABC'))))
        result <- get.clone.colours.in.order(c('red','green'), c(NULL,'ABC'), c('DEF','ABC'))
        expect_true(setequal(result,list(clone.colours = c(DEF = 'red',ABC = 'green'), clone.order = c('DEF','ABC'))))
        result <- get.clone.colours.in.order(c('red','green'), c('ABC',NULL), c('DEF','ABC'))
        expect_true(setequal(result,list(clone.colours = c(DEF = 'red',ABC = 'green'), clone.order = c('DEF','ABC'))))
    });
