test_that(
    'Fixed branching case values', {
        load('data/branching.fixed.plots.Rda');
        load('data/branching.fixed.data.Rda')

        result.tree <- SRCGrob(branching.fixed.test.data$tree);
        expect_true(compare.trees(
            result.tree,
            branching.fixed.example
            ));
        }
    );
