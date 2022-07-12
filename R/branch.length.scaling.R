get.global.branch.length.scale <- function(tree.depth) {
    log2(tree.depth);
    }

get.branch.length.scale <- function(branch.lengths, global.scale) {
    1 / (mean(branch.lengths) * global.scale);
    }