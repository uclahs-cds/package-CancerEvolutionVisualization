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
