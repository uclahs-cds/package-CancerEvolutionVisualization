# How to automate n.breaks?
    # Use 100 if > 1000 SNVs, 50 if > 500 SNVs, 25 if > 100 SNVs, 10 if > 50 SNVs?
# How to automate ymax?

plot.snv.histogram <- function(dpclust.df, sampleID, src.tool, output.dir) {
    num.snv <- nrow(dpclust.df);

    # Automate number of breaks
    # # Freedman-Diaconis Rule method
    # iqr <- IQR(dpclust.df$subclonal.fraction);
    # binwidth <- 2 * iqr / (num.snv^(1 / 3));
    # n.breaks <- ceiling((max(dpclust.df$subclonal.fraction) - min(dpclust.df$subclonal.fraction)) / binwidth);

    # Piecewise method:
    n.breaks <- if (num.snv > 1000) {
        100;
    } else if (num.snv > 500) {
        50;
    } else if (num.snv > 100) {
        25;
    } else if (num.snv > 40) {
        20;
    } else {
        10;
    }

    # Set y-axis max limit
    hist.data <- hist(dpclust.df$subclonal.fraction, breaks = n.breaks, plot = FALSE);
    ymax <- ceiling(max(hist.data$counts) / 10) * 10;
    # Set y-axis increments
    yseq <- if (ymax > 1000) {
        100;
    } else if (ymax > 200) {
        50;
    } else if (ymax > 40) {
        20;
    } else {
        10;
    }

    save.plt <- file.path(
        output.dir,
        paste(sampleID, src.tool, 'clustered_SNVs_histogram.png', sep = '_')
        );

    return(
        BoutrosLab.plotting.general::create.histogram(
            filename = save.plt,
            x = dpclust.df$subclonal.fraction,
            xlab.label = 'CCF',
            ylab.label = 'SNV Count',
            xlab.top.label = paste('Total SNVs Clustered:', num.snv),
            xlab.top.cex = 1.5,
            xlab.top.x = 0.5,
            xlab.top.y = 1,
            xlab.cex = 1.5,
            ylab.cex = 1.5,
            xaxis.cex = 1.2,
            yaxis.cex = 1.2,
            xlimits = c(0, 1.5),
            ylimits = c(0, ymax),
            xat = seq(0, 1.5, 0.25),
            yat = seq(0, ymax, yseq),
            xaxis.tck = c(1, 0),
            yaxis.tck = c(1, 0),
            breaks = n.breaks,
            type = 'count',
            col = 'gray80',
            border.col = 'black',
            lwd = 0.1,
            top.padding = 1,
            right.padding = 1,
            left.padding = 1,
            height = 6,
            width = 6,
            resolution = 800
            )
        );
    }
