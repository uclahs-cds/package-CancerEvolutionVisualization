# create.ccf.densityplot() is exported but had no tests at all, which is how it
# shipped completely broken for ~10 months:
#
#   144ddb7 (2024-11-23) added the function with a `breaks = 100` argument that
#                        was never used.
#   d1f9706 (2025-06-06) removed `breaks = 100` from the signature.
#   c0444c3 (2025-10-24) added `breaks = breaks` to the create.histogram() call,
#                        referencing the argument d1f9706 had deleted.
#
# Every call then failed with "object 'breaks' not found". R CMD check flagged it
# as "no visible binding for global variable 'breaks'", but nothing executed the
# function, so it never surfaced as an error.

make.ccf.data <- function(n.per.clone = 30) {
    set.seed(1);

    data.frame(
        ID       = 'S1',
        SNV.id   = paste0('snv', seq_len(2 * n.per.clone)),
        CCF      = c(
            runif(n.per.clone, 0.8, 1.0),
            runif(n.per.clone, 0.2, 0.5)
            ),
        clone.id = rep(c('1', '2'), each = n.per.clone),
        stringsAsFactors = FALSE
        );
    }

test_that('create.ccf.densityplot returns a plot object', {
    expect_no_error(plt <- create.ccf.densityplot(make.ccf.data()));
    expect_false(is.null(plt));
    });

test_that('create.ccf.densityplot accepts an explicit breaks value', {
    expect_no_error(create.ccf.densityplot(make.ccf.data(), breaks = 20));
    });

test_that('create.ccf.densityplot honours supplied clone colours', {
    expect_no_error(create.ccf.densityplot(
        make.ccf.data(),
        clone.colours = c('1' = 'red', '2' = 'blue')
        ));
    });

test_that('create.ccf.densityplot writes a file when filename is given', {
    out <- file.path(withr::local_tempdir(), 'density.png');

    create.ccf.densityplot(make.ccf.data(), filename = out);

    expect_true(file.exists(out));
    expect_gt(file.size(out), 0);
    });
