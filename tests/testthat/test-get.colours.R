test_that('get.colours returns a color for each vector element', {
  value.list <- c('ABC', 'DEF', 'DEF', 'GHI');
  colors <- get.colours(value.list);
  expect_equal(names(colors), c('ABC', 'DEF', 'DEF', 'GHI'));
  expect_equal(length(colors), 4);
  for (i in seq_along(colors)) {
    color.value <- colors[i];
    expect_true(!is.na(color.value) && nzchar(color.value));
    }
  });

test_that('get.colours returns a color for each vector element when return.names is FALSE', {
  value.list <- c('ABC', 'DEF', 'DEF', 'GHI');
  colors <- get.colours(value.list, FALSE);
  expect_equal(names(colors), c('ABC', 'DEF', 'DEF', 'GHI'));
  expect_equal(length(colors), 4);
  for (i in seq_along(colors)) {
    color.value <- colors[i];
    expect_true(!is.na(color.value) && nzchar(color.value));
    }
  });

test_that('get.colours returns a color for unique vector elements when return.names is TRUE', {
  value.list <- c('ABC', 'DEF', 'DEF', 'GHI');
  colors <- get.colours(value.list, TRUE);
  expect_equal(names(colors), c('ABC', 'DEF', 'GHI'));
  expect_equal(length(colors), 3);
  unique.colors <- unique(names(colors));
  for (key in unique.colors) {
    expect_true(!is.na(colors[key]) && nzchar(colors[key]));
    }
  });

test_that('get.colours returns an empty color vector when value.list is empty', {
  value.list <- c();
  colors <- get.colours(value.list);
  expect_true(identical(names(colors), character(0)));
  expect_true(identical(colors, setNames(character(0), character(0))));
  expect_equal(length(colors), 0);
  });

test_that('get.colours returns an empty color vector when value.list is empty and return.names is FALSE', {
  value.list <- c();
  colors <- get.colours(value.list, FALSE);
  expect_true(identical(names(colors), character(0)));
  expect_true(identical(colors, setNames(character(0), character(0))));
  expect_equal(length(colors), 0);
  });

test_that('get.colours returns an empty color vector when value.list is empty and return.names is TRUE', {
  value.list <- c();
  colors <- get.colours(value.list, TRUE);
  expect_true(identical(names(colors), character(0)));
  expect_true(identical(colors, setNames(character(0), character(0))));
  expect_equal(length(colors), 0);
  });
