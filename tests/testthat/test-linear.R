test_that(
    'Linear case values', {
        load('data/linear.plots.Rda');
        load('data/linear.data.Rda')

        expect_true(compare.trees(
            linear.example,
            create.test.tree(
                linear.test.data$tree,
                linear.test.data$node.text,
                'WHO003',
                add.normal = TRUE,
                horizontal.padding = -1
                )
            ));
        }
    );
