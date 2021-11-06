test_that(
    'Linear case values', {
        load('data/linear.Rda');
        expect_true(all(
            test.segment.grobs(linear.example, linear.example),
            test.text.grobs(linear.example, linear.example),
            test.polygon.grobs(linear.example, linear.example)
            ));
        }
    );
