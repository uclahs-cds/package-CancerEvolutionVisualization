test_that(
    'Branching tree values', {
        load('data/branching.plots.Rda');
        load('data/branching.data.Rda')

        tree <- SRCGrob(branching.test.data$tree);
        expect_true(compare.trees(branching.example, tree));
    }
);
