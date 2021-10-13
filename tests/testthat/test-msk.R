test_that(
    'Non-Branching Examples', {
        for (sample in unique(trees_df$Sample)) {
            sampled <- sampled <- test.sample(sample, trees_df, cnas_df, snvs_df);
            
            test.tree(
                tree = sampled$tree,
                cnas = sampled$cnas,
                snvs = sampled$snvs,
                sample = sample,
                add_normal = TRUE,
                add_genes = TRUE,
                yaxis_position = 'both'
                );
            
            test.tree(
                tree = sampled$tree,
                cnas = sampled$cnas,
                snvs = sampled$snvs,
                sample = sample,
                label_nodes = FALSE,
                add_polygons = FALSE,
                yaxis_position = 'both'
                );
            
            test.tree(
                tree = sampled$tree,
                cnas = sampled$cnas,
                snvs = sampled$snvs,
                sample = sample,
                yaxis_position = 'left'
                );
            }
        }
    );
