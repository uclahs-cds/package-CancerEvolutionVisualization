test_that(
'Linear Example', {
    load('data/linear.data.Rda');
    sample <- 'WHO003';
    
    expect_true(!is.null(
        create.test.tree(
            linear.test.data$tree,
            linear.test.data$genes,
            sample = sample,
            add_normal = TRUE,
            add_genes = TRUE,
            yaxis_position = 'both'
            )
        ));

    expect_true(!is.null(
        create.test.tree(
            linear.test.data$tree,
            linear.test.data$genes,
            sample = sample,
            label_nodes = FALSE,
            add_polygons = FALSE,
            yaxis_position = 'both'
            )
        ));

    expect_true(!is.null(
        create.test.tree(
            linear.test.data$tree,
            linear.test.data$genes,
            sample = sample,
            yaxis_position = 'left'
            )
        ));
    });
