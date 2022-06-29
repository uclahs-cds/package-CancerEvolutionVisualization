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
