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
    'oxford.comma.vector.concat returns `empty.value` when input is empty', {
        vec <- NULL;
        expect_equal(oxford.comma.vector.concat(vec), '');
        expect_equal(oxford.comma.vector.concat(vec, 'default'), 'default');
        vec <- c(NULL);
        expect_equal(oxford.comma.vector.concat(vec), '');
        expect_equal(oxford.comma.vector.concat(vec, 'default'), 'default');
        vec <- c(NULL, NULL);
        expect_equal(oxford.comma.vector.concat(vec), '');
        expect_equal(oxford.comma.vector.concat(vec, 'default'), 'default');
        vec <- c();
        expect_equal(oxford.comma.vector.concat(vec), '');
        expect_equal(oxford.comma.vector.concat(vec, 'default'), 'default');
    });

test_that(
    'oxford.comma.vector.concat returns `empty.value` in correct format when input is empty', {
        vec <- NULL;
        expect_equal(oxford.comma.vector.concat(vec, flatten.empty.value = FALSE), '');
        expect_equal(oxford.comma.vector.concat(vec, 'default', flatten.empty.value = FALSE), 'default');
        vec <- c(NULL);
        expect_equal(oxford.comma.vector.concat(vec, flatten.empty.value = FALSE), '');
        expect_equal(oxford.comma.vector.concat(vec, 'default', flatten.empty.value = FALSE), 'default');
        vec <- c(NULL, NULL);
        expect_equal(oxford.comma.vector.concat(vec, flatten.empty.value = FALSE), '');
        expect_equal(oxford.comma.vector.concat(vec, 'default', flatten.empty.value = FALSE), 'default');
        vec <- c();
        expect_equal(oxford.comma.vector.concat(vec, flatten.empty.value = FALSE), '');
        expect_equal(oxford.comma.vector.concat(vec, 'default', flatten.empty.value = FALSE), 'default');

        vec <- c();
        vec.default <- NULL;
        expect_null(oxford.comma.vector.concat(vec, vec.default, flatten.empty.value = FALSE));
        vec <- c();
        vec.default <- c(1, 2, 3);
        expect_equal(oxford.comma.vector.concat(vec, vec.default), '1, 2, and 3');
        expect_equal(oxford.comma.vector.concat(vec, vec.default, flatten.empty.value = FALSE), c(1, 2, 3));
    });

test_that(
    'oxford.comma.vector.concat returns grammatically correct oxford comma', {
        vec <- c(1);
        expect_equal(oxford.comma.vector.concat(vec), '1');
        vec <- c(1, 2);
        expect_equal(oxford.comma.vector.concat(vec), '1 and 2');
        vec <- c(1, 2, 3);
        expect_equal(oxford.comma.vector.concat(vec), '1, 2, and 3');
        vec <- c(1, 2, 3, 4);
        expect_equal(oxford.comma.vector.concat(vec), '1, 2, 3, and 4');
    });

test_that(
    'oxford.comma.vector.concat returns grammatically correct oxford comma for flattened default value', {
        vec.default <- c(1);
        expect_equal(oxford.comma.vector.concat(NULL, vec.default), '1');
        vec.default <- c(1, 2);
        expect_equal(oxford.comma.vector.concat(NULL, vec.default), '1 and 2');
        vec.default <- c(1, 2, 3);
        expect_equal(oxford.comma.vector.concat(NULL, vec.default), '1, 2, and 3');
        vec.default <- c(1, 2, 3, 4);
        expect_equal(oxford.comma.vector.concat(NULL, vec.default), '1, 2, 3, and 4');
    });
