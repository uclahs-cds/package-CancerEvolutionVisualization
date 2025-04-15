# Test for rotate.coords
test_that('rotate.coords rotates points correctly around origin', {
    # 90 degrees (pi/2 radians) counter-clockwise
    res <- rotate.coords(x = 1, y = 0, rotate.by = pi/2);
    expect_equal(round(res$x), 0);
    expect_equal(round(res$y), 1);

    # 180 degrees (pi radians)
    res <- rotate.coords(x = 1, y = 2, rotate.by = pi);
    expect_equal(round(res$x), -1);
    expect_equal(round(res$y), -2);

    # No rotation
    res <- rotate.coords(x = c(1, 2), y = c(3, 4), rotate.by = 0);
    expect_equal(res$x, c(1, 2));
    expect_equal(res$y, c(3, 4));

    # Rotation around custom origin
    res <- rotate.coords(x = 2, y = 1, rotate.by = pi / 2, x.origin = 1, y.origin = 1);
    expect_equal(round(res$x), 1);
    expect_equal(round(res$y), 2);
});

# Test for rotate.dendrogram
test_that('rotate.dendrogram applies rotate.coords to basex/basey and tipx/tipy', {
    df <- data.frame(
        basex = c(1, 2),
        basey = c(0, 0),
        tipx = c(1, 2),
        tipy = c(1, 1)
        );

    rotated <- rotate.dendrogram(df, rotate.by = pi / 2);

    expect_equal(dim(rotated), dim(df));
    expect_equal(colnames(rotated), colnames(df));

    # First base point (1,0) should rotate to (0,1)
    expect_equal(round(rotated$basex[1]), 0);
    expect_equal(round(rotated$basey[1]), 1);

    # First tip point (1,1) should rotate to (-1,1)
    expect_equal(round(rotated$tipx[1]), -1);
    expect_equal(round(rotated$tipy[1]), 1);
})
