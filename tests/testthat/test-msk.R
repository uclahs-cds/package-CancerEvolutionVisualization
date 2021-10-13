test_that(
    'Non-Branching Examples', {
        sample <- 'WHO003';
        sampled <- sampled <- test.sample(sample, trees_df, cnas_df, snvs_df);
        test.tree(
            tree = sampled$tree,
            cnas = sampled$cnas,
            snvs = sampled$snvs,
            sample = sample,
            add_polygons = TRUE,
            add_normal = TRUE,
            add_genes = TRUE
            );
        }
    );
