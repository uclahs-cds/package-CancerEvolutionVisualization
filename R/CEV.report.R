CEV.report <- function(
    phylogeny,
    SNV.assignment,
    SNV.counts,
    CCF.values,
    output.filename,
    title,
    author,
    date = NULL
    ) {
    inputs <- prep.report(
        phylogeny,
        SNV.assignment,
        SNV.counts,
        CCF.values
        );
    
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
