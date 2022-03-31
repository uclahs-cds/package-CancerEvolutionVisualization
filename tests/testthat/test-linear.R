test_that(
    'Linear case values', {
        load('data/linear.plots.Rda');
        load('data/linear.data.Rda')

        expect_true(compare.trees(
            linear.example,
            create.test.tree(
                linear.test.data$tree,
                linear.test.data$cnas,
                linear.test.data$snvs,
                sample.name,
                add_normal = TRUE,
                add_genes = TRUE,
                yaxis_position = 'both'
                )
            ));
        }
    );
