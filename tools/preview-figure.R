#!/usr/bin/env Rscript
# Render ONE figure from vignettes/ManuscriptFigures.Rmd, for fast iteration on
# formatting. Runs every chunk up to and including the target (later chunks
# depend on earlier ones mutating snv/phy.dt/single.dt), but skips the
# include_graphics display chunks and any expensive figure that is not the
# target.
#
#   Rscript tools/preview-figure.R timeline-tree
#
# Chunk labels: ccf-hm summary-hm clone-genome-dist multi-tree single-tree
#               single-tree-annot timeline-tree radial dend mixed phy-500
args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop('usage: preview-figure.R <chunk-label>')
target <- args[1]

suppressMessages(pkgload::load_all('/pkg', quiet = TRUE))
library(knitr)

rmd <- readLines('/pkg/vignettes/ManuscriptFigures.Rmd')
starts <- grep('^```\\{r', rmd)
ends <- grep('^```$', rmd)
ends <- sapply(starts, function(s) ends[ends > s][1])

label.of <- function(header) {
    inner <- sub('^```\\{r\\s*', '', sub('\\}\\s*$', '', header))
    first <- trimws(strsplit(inner, ',')[[1]][1])
    if (!nzchar(first) || grepl('=', first)) '' else first
    }
labels <- vapply(rmd[starts], label.of, character(1), USE.NAMES = FALSE)

if (!target %in% labels) {
    stop('unknown chunk "', target, '". available: ',
         paste(labels[nzchar(labels)], collapse = ', '))
    }
last <- max(which(labels == target))

# Figure-producing chunks other than the target are skipped: they only write a
# PNG we are not looking at. Their side effects on shared data live in the
# *-prep chunks, which are not in this list.
heavy <- c('ccf-hm', 'summary-hm', 'clone-genome-dist', 'multi-tree',
           'single-tree', 'single-tree-annot', 'timeline-tree', 'radial',
           'dend', 'mixed', 'phy-500')

setwd('/pkg/vignettes')
dir.create('figures', showWarnings = FALSE)

for (i in seq_len(last)) {
    code <- rmd[(starts[i] + 1):(ends[i] - 1)]
    if (any(grepl('include_graphics|cat\\(', code))) next
    if (labels[i] != target && labels[i] %in% heavy) next
    eval(parse(text = paste(code, collapse = '\n')), envir = globalenv())
    }

src <- list.files('figures', full.names = TRUE)
invisible(file.copy(src, '/out', overwrite = TRUE))
cat('rendered', target, '->', basename(src[length(src)]), '\n')
