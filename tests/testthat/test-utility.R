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

test_that('column.contains.all handles valid case', {
    reference <- c('1', '2');
    expect_true(column.contains.all(reference, reference));
    });

test_that('column.contains.all errors on non-vector reference input', {
    reference <- NULL;
    checked <- 1:10;

    expect_error(column.contains.all(reference, checked), 'vector');
    });

test_that('column.contains.all errors on non-vector checked input', {
    reference <- c('A', 'C', 'D');
    other <- NULL;

    expect_error(column.contains.all(reference, other), 'vector');
    });
