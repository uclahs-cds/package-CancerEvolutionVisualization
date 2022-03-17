test_that(
'Linear Example', {
        expect_true(!is.null(
            create.test.tree(
                tree = trees_df,
                cnas = cnas_df,
                snvs = snvs_df,
                sample = sample,
                add_normal = TRUE,
                add_genes = TRUE,
                yaxis_position = 'both'
                )
            ));

        expect_true(!is.null(
            create.test.tree(
                tree = trees_df,
                cnas = cnas_df,
                snvs = snvs_df,
                sample = sample,
                label_nodes = FALSE,
                add_polygons = FALSE,
                yaxis_position = 'both'
                )
            ));

        expect_true(!is.null(
            create.test.tree(
                tree = trees_df,
                cnas = cnas_df,
                snvs = snvs_df,
                sample = sample,
                yaxis_position = 'left'
                )
            ));
        });
