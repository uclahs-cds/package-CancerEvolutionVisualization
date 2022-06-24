test_that(
    'Linear Example', {
        load('data/linear.data.Rda');
        sample <- 'WHO003';

        expect_true(!is.null(
            create.test.tree(
                linear.test.data$tree,
                linear.test.data$genes,
                sample = sample,
                add.normal = TRUE,
                yaxis.position = 'both'
                )
            ));

        expect_true(!is.null(
            create.test.tree(
                linear.test.data$tree,
                linear.test.data$genes,
                sample = sample,
                label.nodes = FALSE,
                disable.polygons = TRUE,
                yaxis.position = 'both'
                )
            ));

        expect_true(!is.null(
            create.test.tree(
                linear.test.data$tree,
                linear.test.data$genes,
                sample = sample,
                yaxis.position = 'left'
                )
            ));
    });
