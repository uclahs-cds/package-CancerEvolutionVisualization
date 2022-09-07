test_that(
    'Linear Example', {
        load('data/linear.data.Rda');
        sample <- 'WHO003';

        text.df <- linear.test.data$node.text;

        expect_true(!is.null(
            create.test.tree(
                linear.test.data$tree,
                text.df,
                sample = sample,
                add.normal = TRUE
                )
            ));

        expect_true(!is.null(
            create.test.tree(
                linear.test.data$tree,
                text.df,
                sample = sample,
                label.nodes = FALSE,
                disable.polygons = TRUE
                )
            ));

        expect_true(!is.null(
            create.test.tree(
                linear.test.data$tree,
                text.df,
                sample = sample
                )
            ));
    });
