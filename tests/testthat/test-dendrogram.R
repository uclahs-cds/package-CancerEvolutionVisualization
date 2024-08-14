test_that(
    'Dendrogram case values', {
        load('data/branching.dendrogram.plots.Rda');
        load('data/branching.dendrogram.data.Rda')

        result.tree <- SRCGrob(branching.dendrogram.test.data$tree);
        expect_true(compare.trees(
            result.tree,
            branching.dendrogram.example
        ));
    }
);