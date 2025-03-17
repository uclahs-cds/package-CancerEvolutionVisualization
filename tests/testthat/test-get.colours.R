expect.character.vector <- function(result) {
    expect_true(identical(names(result), character(0)));
    expect_true(identical(result, setNames(character(0), character(0))));
    expect_equal(length(result), 0);
    }

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

test_that(
    'get.colours returns expected vectors', {
        result <- get.colours(NULL, predetermined.colours = NULL);
        expect.character.vector(result);
        result <- get.colours(c('ABC', 'DEF'), predetermined.colours = NULL);
        expect_equal(names(result), c('ABC', 'DEF'));
        expect_true(!is.na(result['ABC']) && nzchar(result['ABC']));
        expect_true(!is.na(result['DEF']) && nzchar(result['DEF']));
        result <- get.colours(NULL, predetermined.colours = c('red','green'));
        expect.character.vector(result);
        result <- get.colours(c(), predetermined.colours = c());
        expect.character.vector(result);
        result <- get.colours(NULL, predetermined.colours = c());
        expect.character.vector(result);
        result <- get.colours(c(), predetermined.colours = NULL);
        expect.character.vector(result);
        result <- get.colours(c('ABC', 'DEF'), predetermined.colours = c())
        expect_equal(names(result), c('ABC', 'DEF'));
        expect_true(!is.na(result['ABC']) && nzchar(result['ABC']));
        expect_true(!is.na(result['DEF']) && nzchar(result['DEF']));
        result <- get.colours(c(), predetermined.colours = c('red', 'green'));
        expect.character.vector(result);
        result <- get.colours(c('ABC','DEF'), predetermined.colours = c('red','green'));
        expect_equal(result, c(ABC = 'red', DEF = 'green'));
        result <- get.colours(c('ABC','DEF'), predetermined.colours = c('red'));
        expect_equal(names(result), c('ABC', 'DEF'));
        expect_true(result['ABC'] == 'red');
        expect_true(!is.na(result['DEF']) && nzchar(result['DEF']));
        result <- get.colours(c('ABC','DEF'), predetermined.colours = c(NULL,'red'));
        expect_equal(names(result), c('ABC', 'DEF'));
        expect_true(result['ABC'] == 'red');
        expect_true(!is.na(result['DEF']) && nzchar(result['DEF']));
        result <- get.colours(c('ABC','DEF'), predetermined.colours = c('red',NULL));
        expect_equal(names(result), c('ABC', 'DEF'));
        expect_true(result['ABC'] == 'red');
        expect_true(!is.na(result['DEF']) && nzchar(result['DEF']));
        result <- get.colours(c('ABC'), predetermined.colours = c('red','green'));
        expect_equal(result, c(ABC = 'red'));
        result <- get.colours(c(NULL,'ABC'), predetermined.colours = c('red','green'));
        expect_equal(result, c(ABC = 'red'));
        result <- get.colours(c('ABC',NULL), predetermined.colours = c('red','green'));
        expect_equal(result, c(ABC = 'red'));
    });

test_that(
    'get.colours returns expected vectors with minimum colours specified', {
        result <- get.colours(NULL, predetermined.colours = NULL)
        expect.character.vector(result);
        result <- get.colours(NULL, predetermined.colours = c('red', 'green'))
        expect.character.vector(result);
        result <- get.colours(NULL, predetermined.colours = c('red'))
        expect.character.vector(result);
        result <- get.colours(c('ABC','DEF'), predetermined.colours = c('red', 'green'))
        expect_equal(result, c(ABC = 'red', DEF = 'green'));
        result <- get.colours(c('ABC','DEF'), predetermined.colours = c('red'))
        expect_equal(names(result), c('ABC', 'DEF'));
        expect_true(result['ABC'] == 'red');
        expect_true(!is.na(result['DEF']) && nzchar(result['DEF']));
        result <- get.colours(c('ABC'), predetermined.colours = c('red', 'green'))
        expect_equal(result, c(ABC = 'red'));
    });

test_that(
    'get.colours.in.order returns expected vectors when order is not specified', {
        # get.colours.in.order has same result as get.colours
        # with the change that a named list is returned with two members
        result <- get.colours.in.order(NULL, predetermined.colours = NULL)
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = NULL)))
        result <- get.colours.in.order(c('ABC', 'DEF'), predetermined.colours = NULL)
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = NULL)))
        result <- get.colours.in.order(NULL, predetermined.colours = c('red','green'))
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = NULL)))
        result <- get.colours.in.order(c(), predetermined.colours = c())
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = NULL)))
        result <- get.colours.in.order(c(), predetermined.colours = NULL)
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = NULL)))
        result <- get.colours.in.order(NULL, predetermined.colours = c())
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = NULL)))
        result <- get.colours.in.order(c('ABC', 'DEF'), predetermined.colours = c())
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = NULL)))
        result <- get.colours.in.order(c(), predetermined.colours = c('red', 'green'))
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = NULL)))
        result <- get.colours.in.order(c('ABC','DEF'), predetermined.colours = c('red','green'))
        expect_true(setequal(result,list(predetermined.colours = c(ABC = 'red',DEF = 'green'), value.order = c('ABC','DEF'))))
        result <- get.colours.in.order(c('ABC','DEF'), predetermined.colours = c('red'))
        expect_equal(names(result), c('colours', 'value.order'))
        expect_equal(names(result$colours), c('ABC', 'DEF'))
        expect_true(result$colours['ABC'] == 'red')
        expect_true(!is.na(result$colours['DEF']) && nzchar(result$colours['DEF']))
        expect_equal(result$value.order, c('ABC','DEF'))

        result <- get.colours.in.order(c('ABC','DEF'), predetermined.colours = c(NULL,'red'))
        expect_equal(names(result), c('colours', 'value.order'))
        expect_equal(names(result$colours), c('ABC','DEF'))
        expect_true(result$colours['ABC'] == 'red')
        expect_true(!is.na(result$colours['DEF']) && nzchar(result$colours['DEF']))
        expect_equal(result$value.order, c('ABC','DEF'))

        result <- get.colours.in.order(c('ABC','DEF'), predetermined.colours = c('red',NULL))
        expect_equal(names(result), c('colours', 'value.order'))
        expect_equal(names(result$colours), c('ABC','DEF'))
        expect_true(result$colours['ABC'] == 'red')
        expect_true(!is.na(result$colours['DEF']) && nzchar(result$colours['DEF']))
        expect_equal(result$value.order, c('ABC','DEF'))

        result <- get.colours.in.order(c('ABC'), predetermined.colours = c('red','green'))
        expect_true(setequal(result,list(predetermined.colours = c(ABC = 'red'), value.order = c('ABC'))))
        result <- get.colours.in.order(c(NULL,'ABC'), predetermined.colours = c('red','green'))
        expect_true(setequal(result,list(predetermined.colours = c(ABC = 'red'), value.order = c('ABC'))))
        result <- get.colours.in.order(c('ABC',NULL), predetermined.colours = c('red','green'))
        expect_true(setequal(result,list(predetermined.colours = c(ABC = 'red'), value.order = c('ABC'))))
    });

test_that(
    'get.colours.in.order returns expected vectors when order is specified', {
        # get.colours.in.order with order specified
        result <- get.colours.in.order(NULL, c('DEF','ABC'), NULL)
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = c('DEF','ABC'))))
        result <- get.colours.in.order(c('ABC', 'DEF'), c('DEF','ABC'), NULL)
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = c('DEF','ABC'))))
        result <- get.colours.in.order(NULL, predetermined.colours = c('red','green'))
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = NULL)))
        result <- get.colours.in.order(c(), c("DEF", "ABC"), c())
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = c('DEF','ABC'))))
        result <- get.colours.in.order(c(), c('DEF','ABC'), NULL)
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = c('DEF','ABC'))))
        result <- get.colours.in.order(NULL, c("DEF", "ABC"), c())
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = c('DEF','ABC'))))
        result <- get.colours.in.order(c("ABC", "DEF"), c("DEF", "ABC"), c())
        expect_true(setequal(result, list(predetermined.colours = NULL, value.order = c('DEF','ABC'))))
        result <- get.colours.in.order(c(), c('DEF','ABC'), c('red', 'green'))
        expect_true(setequal(result, list(predetermined.colours = c(DEF = 'red',ABC = 'green'), value.order = c('DEF','ABC'))))
        result <- get.colours.in.order(c('ABC','DEF'), c('DEF','ABC'), c('red','green'))
        expect_true(setequal(result,list(predetermined.colours = c(DEF = 'red',ABC = 'green'), value.order = c('DEF','ABC'))))
        result <- get.colours.in.order(c('ABC','DEF'), c('DEF','ABC'), c('red'))
        expect_equal(names(result), c('colours', 'value.order'))
        expect_equal(names(result$colours), c('DEF', 'ABC'))
        expect_true(result$colours['DEF'] == 'red')
        expect_true(!is.na(result$colours['ABC']) && nzchar(result$colours['ABC']))
        expect_equal(result$value.order, c('DEF','ABC'))

        result <- get.colours.in.order(c('ABC','DEF'), c('DEF','ABC'), c(NULL,'red'))
        expect_equal(names(result), c('colours', 'value.order'))
        expect_equal(names(result$colours), c('DEF','ABC'))
        expect_true(result$colours['DEF'] == 'red')
        expect_true(!is.na(result$colours['ABC']) && nzchar(result$colours['ABC']))
        expect_equal(result$value.order, c('DEF','ABC'))

        result <- get.colours.in.order(c('ABC','DEF'), c('DEF','ABC'), c('red',NULL))
        expect_equal(names(result), c('colours', 'value.order'))
        expect_equal(names(result$colours), c('DEF','ABC'))
        expect_true(result$colours['DEF'] == 'red')
        expect_true(!is.na(result$colours['ABC']) && nzchar(result$colours['ABC']))
        expect_equal(result$value.order, c('DEF','ABC'))

        result <- get.colours.in.order(c('ABC'), c('DEF','ABC'), c('red','green'))
        expect_true(setequal(result,list(predetermined.colours = c(DEF = 'red',ABC = 'green'), value.order = c('DEF','ABC'))))
        result <- get.colours.in.order(c(NULL,'ABC'), c('DEF','ABC'), c('red','green'))
        expect_true(setequal(result,list(predetermined.colours = c(DEF = 'red',ABC = 'green'), value.order = c('DEF','ABC'))))
        result <- get.colours.in.order(c('ABC',NULL), c('DEF','ABC'), c('red','green'))
        expect_true(setequal(result,list(predetermined.colours = c(DEF = 'red',ABC = 'green'), value.order = c('DEF','ABC'))))
    });
