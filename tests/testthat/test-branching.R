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
    'Fixed branching 30-degree plotting direction case values', {
        load('data/branching.fixed.plots.Rda');
        load('data/branching.fixed.data.Rda')

        result.tree <- SRCGrob(
            branching.fixed.test.data$tree,
            plotting.direction = 30
            );
        expect_true(compare.trees(
            result.tree,
            branching.fixed.30deg.example
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


test_that(
    'Radial branching "right" plotting direction case values', {
        load('data/branching.radial.plots.Rda');
        load('data/branching.radial.data.Rda')

        result.tree <- SRCGrob(
            branching.radial.test.data$tree,
            plotting.direction = 'right'
            );
        expect_true(compare.trees(
            result.tree,
            branching.radial.right.example
            ));
        }
    );

test_that(
    'Radial branching 30-degree plotting direction case values', {
        load('data/branching.radial.plots.Rda');
        load('data/branching.radial.data.Rda')

        result.tree <- SRCGrob(
            branching.radial.test.data$tree,
            plotting.direction = 30
            );
        expect_true(compare.trees(
            result.tree,
            branching.radial.30deg.example
            ));
        }
    );
