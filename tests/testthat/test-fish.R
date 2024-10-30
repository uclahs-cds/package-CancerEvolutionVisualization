test_that(
    'Fishplot case', {
        load('data/fish.plots.Rda');
        load('data/fish.data.Rda')

        expect_true(compare.trees(
            fish.example,
            SRCGrob(
                fish.test.data$tree,
                colour.scheme = fish.test.data$colour.scheme
                )
            ));
        });
