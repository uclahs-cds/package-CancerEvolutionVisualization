get.branch.length.scale <- function(branch.lengths, tree.depth, user.scale) {
    smart.scale <- get.smart.branch.length.scale(branch.lengths, tree.depth);

    return(smart.scale * user.scale);
    }

get.smart.branch.length.scale <- function(branch.lengths, tree.depth) {
    tree.depth.modifier <- get.tree.depth.modifier(tree.depth);

    return(1 / (mean(branch.lengths) * tree.depth.modifier));
    }

get.tree.depth.modifier <- function(tree.depth) {
    log2(tree.depth);
    }
