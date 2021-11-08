test_that(
    'Linear case values', {
        load('data/linear.Rda');
        
        expect_true(compare.trees(
            linear.example,
            create.test.tree(trees_df, cnas_df, snvs_df, 'WHO003')
            ));
        }
    );
