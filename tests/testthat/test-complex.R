test_that(
    'Complex spread case values', {
        load('data/complex.plots.Rda');
        load('data/complex.data.Rda')

        result.tree <- SRCGrob(spread.test.data$tree);
        expect_true(compare.trees(
            result.tree,
            spread.example
            ));
        }
    );

test_that(
    'Complex spread 30-degree plotting direction case values', {
        load('data/complex.plots.Rda');
        load('data/complex.data.Rda')

        result.tree <- SRCGrob(
            spread.test.data$tree,
            plotting.direction = 30
            );
        expect_true(compare.trees(
            result.tree,
            spread.30deg.example
            ));
        }
    );
