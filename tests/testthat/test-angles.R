test_that(
    'calculate.angles.fixed orders nodes correctly', {
        num.children <- 4;
        test.tree <- data.frame(
            parent = c(-1, rep(1, num.children))
            );
        test.tree$tip <- rownames(test.tree);

        test.v <- data.frame(
            id = test.tree$tip,
            parent = test.tree$parent
            );

        total.angle <- pi / 2;

        result <- calculate.angles.radial(
            test.v,
            test.tree,
            spread = 1,
            total.angle = total.angle
            );
        result.order <- order(result);
        child.ids <- as.numeric(test.tree$tip[test.tree$parent == 1]);
        result.order <- result.order[result.order %in% child.ids];

        expect_equal(result.order, child.ids);
        }
    );

test_that(
    'calculate.angles.radial spreads nodes equally', {
        num.children <- 4;
        test.tree <- data.frame(
            parent = c(-1, rep(1, num.children))
            );
        test.tree$tip <- rownames(test.tree)

        test.v <- data.frame(
            id = test.tree$tip,
            parent = test.tree$parent
            );

        total.angle <- pi / 2;

        result <- calculate.angles.radial(
            test.v,
            test.tree,
            spread = 1,
            total.angle = total.angle
            );

        num.digits <- 6;
        deltas <- sapply(
            # Iterate through all children in pairs if i, i + 1.
            # Last index not needed, as i + 1 out of bounds.
            2:(length(result) - 1),
            FUN = function(i) round(abs(result[i + 1] - result[i]), num.digits)
            );
        expected.delta <- round(
            total.angle / (num.children - 1),
            num.digits
            );

        expect_true(all(deltas == expected.delta));
        }
    );

test_that(
    'calculate.angles.radial applies spread to angle range', {
        num.children <- 4;
        test.tree <- data.frame(
            parent = c(-1, rep(1, num.children))
            );
        test.tree$tip <- rownames(test.tree)

        test.v <- data.frame(
            id = test.tree$tip,
            parent = test.tree$parent
            );

        spread <- 2.5;
        total.angle <- pi / 4;

        result <- calculate.angles.radial(
            test.v,
            test.tree,
            spread = spread,
            total.angle = total.angle
            );

        result.range <- range(result);
        expected.range <- c(-1, 1) * (spread * total.angle / 2);

        expect_equal(result.range, expected.range);
        }
    );

test_that(
    'calculate.angles.radial overrides angles', {
        num.children <- 3;
        test.tree <- data.frame(
            parent = c(-1, rep(1, num.children))
            );
        test.tree$tip <- rownames(test.tree)

        angles.to.override <- c(2, 3);
        override.values <- c(-1, 1) * (pi / 2);

        test.v <- data.frame(
            id = test.tree$tip,
            parent = test.tree$parent,
            angle = NA
            );
        test.v[angles.to.override, 'angle'] <- override.values;

        result <- calculate.angles.radial(
            test.v,
            test.tree,
            spread = 1,
            total.angle = pi / 2.5
            );

        expect_equal(result[angles.to.override], override.values);
        }
    );

test_that(
    'calculate.angles.fixed sets angle correctly', {
        test.tree <- data.frame(
            parent = c(-1, 1, 1)
            );
        test.tree$tip <- rownames(test.tree)

        test.v <- data.frame(
            id = test.tree$tip,
            parent = test.tree$parent
            );

        angle <- pi / 2;

        result <- calculate.angles.fixed(
            test.v,
            test.tree,
            fixed.angle = angle
            );
        expected.result <- c(0, -(angle), angle);

        expect_equal(result, expected.result, tolerance = 10 ** -3);
        }
    );

test_that(
    'calculate.angles.fixed overrides angles', {
        num.children <- 2;
        test.tree <- data.frame(
            parent = c(-1, rep(1, num.children))
            );
        test.tree$tip <- rownames(test.tree)

        angles.to.override <- c(2, 3);
        override.values <- c(-1, 1) * (pi / 2);

        test.v <- data.frame(
            id = test.tree$tip,
            parent = test.tree$parent,
            angle = NA
            );
        test.v[angles.to.override, 'angle'] <- override.values;

        result <- calculate.angles.fixed(
            test.v,
            test.tree,
            fixed.angle = pi / 4
            );

        expect_equal(result[angles.to.override], override.values);
        }
);
