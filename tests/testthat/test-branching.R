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

test_that(
    'Radial branching case values', {
        load('data/branching.radial.plots.Rda');
        load('data/branching.radial.data.Rda')

        result.tree <- SRCGrob(branching.radial.test.data$tree);
        expect_true(compare.trees(
            result.tree,
            branching.radial.example
            ));
        }
    );
