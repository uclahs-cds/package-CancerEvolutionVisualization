\name{create.ccf.summary.heatmap}
\alias{create.ccf.summary.heatmap}
\title{CCF Summary Heatmap}
\description{
Creates a heatmap of cancer cell fraction (CCF) distribution across tumour samples with clone IDs as a covariate beneath the heatmap. Subplot parameters controls the appearance of the heatmap and barplots. See \code{BoutrosLab.plotting.general::create.barplot()} or \code{BoutrosLab.plotting.general::create.heatmap()}  for parameter description. Legend parameters are passed to \code{BoutrosLab.plotting.general::legend.grob()}.
}

\usage{
create.ccf.summary.heatmap(
    DF,
    ccf.limits = NULL,
    median.col = 'median.ccf.per.sample',
    clone.order = NULL,
    sample.order = NULL,
    hm.col.scheme = c('white', 'blue'),
    clone.colours = NULL,
    subplot.xlab.cex = 1.2,
    subplot.xaxis.cex = 1,
    subplot.xaxis.fontface = 'bold',
    subplot.xaxis.rot = 90,
    subplot.ylab.cex = 1.2,
    subplot.yaxis.cex = 1,
    subplot.yaxis.fontface = 'bold',
    hm.xaxis.rot = 90,
    legend.size = 3,
    legend.title.cex = 1.2,
    legend.label.cex = 1,
    legend.x = 0.9,
    legend.y = 0.8,
    plot.objects.heights = c(0.3, 1),
    add.median.text = FALSE,
    ...
    );
}
\arguments{
    \item{DF}{A data-frame with the following column names: 'ID', 'SNV.id', 'clone.id', 'CCF'.}
    \item{ccf.limits}{CCF limits to be applied to the heatmap. Must be a vector of length 2 for min and max thresholds. Defaults to \code{NULL}}
    \item{median.col}{Defaults to \dQuote{median.ccf.per.sample}}
    \item{clone.order}{Defaults to \code{NULL}}
    \item{sample.order}{Defaults to \code{NULL}}
    \item{hm.col.scheme}{Heatmap colour scheme. Defaults to \code{c('white', 'blue')}}
    \item{clone.colours}{A named vector specifying the color to use for each clone to generate a covariate heatmap. If \code{NULL}, no covariates will be added.}
    \item{subplot.xlab.cex}{Subplot parameter. Defaults to 1.2}
    \item{subplot.xaxis.cex}{Subplot parameter. Defaults to 1}
    \item{subplot.xaxis.fontface}{Subplot parameter. Defaults to \dQuote{bold}}
    \item{subplot.xaxis.rot}{Subplot parameter. Defaults to 90}
    \item{subplot.ylab.cex}{Subplot parameter. Defaults to 1.2}
    \item{subplot.yaxis.cex}{Subplot parameter. Defaults to 1}
    \item{subplot.yaxis.fontface}{Subplot parameter. Defaults to \dQuote{bold}}
    \item{hm.xaxis.rot}{Subplot parameter. Defaults to 90}
    \item{legend.size}{Legend parameter. Defaults to 3}
    \item{legend.title.cex}{Legend parameter. Defaults to 1.2}
    \item{legend.label.cex}{Legend parameter. Defaults to 1}
    \item{legend.x}{Legend parameter. Defaults to 0.9}
    \item{legend.y}{Legend parameter. Defaults to 0.8}
    \item{plot.objects.heights}{If \code{clone.colours} is specified, this must be an array of length 3. Defaults to \code{c(0.3, 1)}. See BoutrosLab.plotting.general::create.multipanelplot()}
    \item{add.median.text}{If \code{TRUE}, median CCF values will be added to the heatmap. Defaults to \code{FALSE}}
    \item{...}{Pass through argument. See BoutrosLab.plotting.general::create.multipanelplot() for further details.}

}
\value{A `grob` object of the summary plot.}
\author{Helena Winata}
\seealso{\code{\link[BoutrosLab.plotting.general]{legend.grob}}, \code{\link[BoutrosLab.plotting.general]{create.multipanelplot}}, \code{\link[BoutrosLab.plotting.general]{create.heatmap}}}

\examples{
setseed(123);
load(file.path(system.file('data', package = 'CancerEvolutionVisualization'), 'SNV.Rda'));

snv <- do.call(rbind, lapply(split(snv, list(snv$ID, snv$clone.id)), function(df) {
    df$median.ccf.per.sample <- median(df$CCF)
    df$total.nsnv <- length(unique(df$SNV.id))
    df
    }));

create.ccf.summary.heatmap(snv);
}
