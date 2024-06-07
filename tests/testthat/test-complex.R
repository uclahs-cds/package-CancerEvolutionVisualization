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
