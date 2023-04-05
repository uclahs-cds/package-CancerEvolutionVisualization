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
        date = if (!is.null(date)) date else Sys.Date(),
        summary.tree.data = inputs$summary.tree.input,
        heatmap.data = inputs$heatmap.input
        );

    template.path <- system.file(
        'CEV.report.Rmd',
        package = 'CancerEvolutionVisualization'
        );

    rmarkdown::render(
        template.path,
        output_file = output.filename,
        params = report.params
        );
    }
