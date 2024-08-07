test_that(
    'check.lengths does not error if lengths are equal', {
        a <- rep('1', 5);
        b <- rep('2', length(a));

        expect_no_error({
            check.lengths(a, b, a.name = 'a', b.name = 'b');
            });
    });

test_that(
    'check.lengths errors if lengths are not equal', {
        a <- rep('1', 5);
        b <- rep('2', length(a) + 2);

        expect_error({
            check.lengths(a, b, a.name = 'a', b.name = 'b');
            },
            regexp = 'length'
            );
    });

test_that(
    'check.lengths uses provided names in error', {
        a <- rep('1', 5);
        b <- rep('2', length(a) + 2);
        name.a <- 'string A';
        name.b <- 'string B'

        expect_error({
            check.lengths(a, b, a.name = name.a, b.name = name.b);
        },
        regexp = paste(
            paste(name.a, name.b, sep = '.*'),
            paste(name.b, name.a, sep = '.*'),
            sep = '|'
            )
        );
    });
