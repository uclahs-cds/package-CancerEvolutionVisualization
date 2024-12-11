test_that(
    'gradient.color.scheme creates correct length with multiple colors', 
    {
        color.scheme <- c('blue', 'red');
        n <- 10;
        result <- gradient.color.scheme(color.scheme, n);
        expect_length(result, n);
        }
    );

test_that(
    'gradient.color.scheme creates correct length with single color', 
    {
        color.scheme <- 'blue';
        n <- 10;
        result <- gradient.color.scheme(color.scheme, n);
        expect_length(result, n);
        }
    );

test_that(
    'gradient.color.scheme results are unique with multiple colors', 
    {
        color.scheme <- c('blue', 'red');
        n <- 10;
        result <- gradient.color.scheme(color.scheme, n);
        expect_length(unique(result), n);
        }
    );

test_that(
    'gradient.color.scheme results are unique with single color', 
    {
        color.scheme <- 'blue';
        n <- 10;
        result <- gradient.color.scheme(color.scheme, n);
        expect_length(unique(result), n);
       }
    );
