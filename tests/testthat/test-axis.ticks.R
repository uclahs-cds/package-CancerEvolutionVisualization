test_that('prep.yat handles valid yat', {
    original.yat <- list(1:10, seq(0, 20));
    result.yat <- prep.yat(original.yat);

    expect_equivalent(result.yat, original.yat);
    });

test_that('prep.yat handles NULL input', {
    result.yat <- prep.yat(NULL);
    expected.result <- list();
    
    expect_equivalent(result.yat, expected.result);
    });

test_that('prep.yat warns on invalid input', {
    invalid.yat <- 100;

    expect_warning(prep.yat(invalid.yat));
    });

test_that(
    'get.default.yat values begin at 0', {
        result.yat <- get.default.yat(5, 2);

        expect_equal(min(result.yat), 0);
    });

test_that(
    'get.default.yat limits values to given maximum', {
        conversion.factor <- 0.5;
        max.y <- 10;

        result.yat <- get.default.yat(max.y, conversion.factor);
        scaled.max <- max.y * conversion.factor;

        expect_lte(max(result.yat), scaled.max)
    });