test_that(
    'calculate.angles.fixed orders nodes correctly', {
        num.children <- 4;
        test.tree <- data.frame(
            parent = c(-1, rep(1, num.children))
            );
        test.tree$tip <- rownames(test.tree);

        test.v <- data.frame(
            id = test.tree$tip,
            parent = test.tree$parent,
            spread = 1
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
            parent = test.tree$parent,
            spread = 1
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
        spread <- 2.5;

        test.tree <- data.frame(
            parent = c(-1, rep(1, num.children))
            );
        test.tree$tip <- rownames(test.tree)

        spread <- 2.5;
        test.v <- data.frame(
            id = test.tree$tip,
            parent = test.tree$parent,
            spread = spread
            );

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
            angle = NA,
            spread = 1
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
    'calculate.angles.radial handles children of overriden angle', {
        num.children <- 4;
        test.tree <- data.frame(
            parent = c(-1, rep(1, num.children))
            );
        test.tree$tip <- rownames(test.tree)

        test.v <- data.frame(
            id = test.tree$tip,
            parent = test.tree$parent,
            angle = NA
            );
        new.angle <- degrees.to.radians(15);
        test.v[1, 'angle'] <- new.angle;

        result <- calculate.angles.radial(
            test.v,
            test.tree,
            spread = 1,
            total.angle = pi / 2.5
            );

        expect_equal(mean(result[-1]), new.angle);
        }
    );


test_that(
    'calculate.angles.radial applies left-outer spread correctly', {
        # Three child nodes.
        # Leftmost node has larger spread.
        # All other nodes have default spread.
        num.children <- 3;
        test.tree <- data.frame(
            parent = c(-1, rep(1, num.children))
            );
        test.tree$tip <- rownames(test.tree);

        spread <- 1.5;
        test.v <- data.frame(
            id = test.tree$tip,
            parent = test.tree$parent,
            spread = 1
            );
        test.v$spread[2] <- spread;

        total.angle <- pi / 2;

        result <- calculate.angles.radial(
            test.v,
            test.tree,
            spread = 1,
            total.angle = total.angle
            );
        result.angle.increments <- c(
            result[3] - result[2],
            result[4] - result[3]
            );

        # First increment larger, second unaffected
        expected.angle.increments <- c(
            total.angle / 2 * mean(c(1, spread)),
            total.angle / 2
            );

        expect_equal(result.angle.increments, expected.angle.increments);
        }
    );

test_that(
    'calculate.angles.radial applies right-outer spread correctly', {
        # Three child nodes.
        # Rightmost node has larger spread.
        # All other nodes have default spread.
        num.children <- 3;
        test.tree <- data.frame(
            parent = c(-1, rep(1, num.children))
            );
        test.tree$tip <- rownames(test.tree);

        spread <- 1.5;
        test.v <- data.frame(
            id = test.tree$tip,
            parent = test.tree$parent,
            spread = 1
            );
        test.v$spread[4] <- spread;

        total.angle <- pi / 2;

        result <- calculate.angles.radial(
            test.v,
            test.tree,
            spread = 1,
            total.angle = total.angle
            );
        result.angle.increments <- c(
            result[3] - result[2],
            result[4] - result[3]
            );

        # First increment larger, second unaffected
        expected.angle.increments <- c(
            total.angle / 2,
            total.angle / 2 * mean(c(1, spread))
            );

        expect_equal(result.angle.increments, expected.angle.increments);
        }
    );

test_that(
    'calculate.angles.fixed sets angle correctly', {
        test.tree <- data.frame(
            parent = c(-1, 1, 1)
            );
        test.tree$tip <- rownames(test.tree);

        test.v <- data.frame(
            id = test.tree$tip,
            parent = test.tree$parent,
            spread = 1
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
            angle = NA,
            spread = 1
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

test_that(
    'calculate.angles.fixed applies spread to angle range', {
        num.children <- 2;
        test.tree <- data.frame(
            parent = c(-1, rep(1, num.children))
            );
        test.tree$tip <- rownames(test.tree)

        override.values <- c(-1, 1) * (pi / 2);

        spread <- 2.1;
        test.v <- data.frame(
            id = test.tree$tip,
            parent = test.tree$parent,
            angle = NA,
            spread = spread
            );

        fixed.angle <- pi / 4;
        result <- calculate.angles.fixed(
            test.v,
            test.tree,
            fixed.angle = fixed.angle
            );
        result.angle.range <- result[3] - result[2];
        expected.angle.range <- fixed.angle * 2 * spread;

        expect_equal(result.angle.range, expected.angle.range, tolerance = 0.001);
        }
    );
