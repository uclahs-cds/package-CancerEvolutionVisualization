test_that(
    'get.branch.length.scale accounts for the given branches', {
        tree.depth <- 2;
        user.scale <- 1;

        shorter.branches <- rep(2, tree.depth);
        longer.branches <- rep(4, tree.depth);
        
        short.scale <- get.branch.length.scale(shorter.branches, tree.depth, user.scale);
        long.scale <- get.branch.length.scale(longer.branches, tree.depth, user.scale);

        # Longer branches should be scaled down
        expect_gt(short.scale, long.scale);
    });

test_that(
    'get.branch.length.scale accounts for tree.depth', {
        user.scale <- 1;

        shallow.tree.depth <- 2;
        deep.tree.depth <- 5;

        branch.lengths <- rep(3, 2);

        shallow.scale <- get.branch.length.scale(branch.lengths, shallow.tree.depth, user.scale);
        deep.scale <- get.branch.length.scale(branch.lengths, deep.tree.depth, user.scale);

        # Tree with more levels should be scaled down
        expect_gt(shallow.scale, deep.scale);
    });

test_that(
    'get.branch.length.scale accounts for user.scale', {
        tree.depth <- 3;
        branch.lengths <- rep(3, 2);

        
        normal.scale <- get.branch.length.scale(branch.lengths, tree.depth, 1);
        scaled.down <- get.branch.length.scale(branch.lengths, tree.depth, 0.8);

        # Tree with more levels should be scaled down
        expect_lt(scaled.down, normal.scale);
    });