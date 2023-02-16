CEV.report <- function(
    output.filename,
    title,
    author,
    date = NULL
    ) {
    report.params <- list(
        title = title,
        author = author,
        date = if (!is.null(date)) date else Sys.Date()
        );

    rmarkdown::render(
        'inst/test.Rmd',
        output_file = output.filename,
        params = report.params
        );
    }
