test_that(
    'Linear case values', {
        load('data/linear.Rda');
        
        sample.name <- 'WHO003';
        sampled <- sample.test.data(sample.name, trees_df, cnas_df, snvs_df)

        expect_true(compare.trees(
            linear.example,
            create.test.tree(
                sampled$tree,
                sampled$cnas,
                sampled$snvs,
                sample.name,
                add_normal = TRUE,
                add_genes = TRUE,
                yaxis_position = 'both'
                )
            ));
        }
    );
