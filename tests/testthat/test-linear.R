test_that(
    'Linear case values', {
        load('data/linear.Rda');
        expect_true(all(
            test.segments(linear.example, linear.example)
            ));
        }
    );
