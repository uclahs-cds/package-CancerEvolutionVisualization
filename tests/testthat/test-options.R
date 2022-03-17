test_that(
'Linear Example', {
    load('data/linear.data.Rda');
    
    expect_true(!is.null(
        create.test.tree(
            tree = linear.test.data$tree,
            cnas = linear.test.data$cnas,
            snvs = linear.test.data$snvs,
            sample = sample,
            add_normal = TRUE,
            add_genes = TRUE,
            yaxis_position = 'both'
            )
        ));

    expect_true(!is.null(
        create.test.tree(
            tree = linear.test.data$tree,
            cnas = linear.test.data$cnas,
            snvs = linear.test.data$snvs,
            sample = sample,
            label_nodes = FALSE,
            add_polygons = FALSE,
            yaxis_position = 'both'
            )
        ));

    expect_true(!is.null(
        create.test.tree(
            tree = linear.test.data$tree,
            cnas = linear.test.data$cnas,
            snvs = linear.test.data$snvs,
            sample = sample,
            yaxis_position = 'left'
            )
        ));
    });
