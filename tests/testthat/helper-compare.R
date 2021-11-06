test.segment.grobs <- function(example, test) {
    get.segment.keys <- function(x) {
        x$childrenOrder[c('tree_segs1', 'tree_segs2')];
        }
    
    example.keys <- get.segment.keys(example);
    test.keys <- get.segment.keys(test);
    
    compare.segments <- function(x, y) {
        coords.equal <- sapply(
                c('x0', 'x1', 'y0', 'y1'), 
                FUN = function(k) {
                    units.are.equal(x[[k]], y[[k]]);
                    }
                );
        
        gp.equal <- identical(x$gp, y$gp);
        arrow.equal <- x$arrow == y$arrow;
        
        return(all(
            coords.equal,
            gp.equal,
            arrow.equal
            ));
        }
    
    all(sapply(
        1:(length(example.keys)), 
        FUN = function(i) {
            compare.segments(
                example$children[[example.keys[i]]],
                test$children[[test.keys[i]]]
                );
            }
        ));
    }

test.text.grobs <- function(example, test) {
    compare.text <- function(x, y) {
        labels.equal <- identical(x$label, y$label);
        
        just.equal <- (
            x$just == y$just 
            & identical(x$hjust, y$hjust) 
            & identical(x$vjust, y$vjust)
            );
        
        rot.equal <- x$rot == y$rot;
        gp.equal <- identical(x$gp, y$gp);
        
        coords.equal <- all(sapply(
            c('x', 'y'),
            FUN = function(coord) {
                units.are.equal(x[[coord]], y[[coord]]);
                }
        ));
        
        
        all(
            labels.equal,
            coords.equal,
            just.equal,
            rot.equal,
            gp.equal
            );
        }
    
    get.text.grobs <- function(x) {
        axis.keys <- stringr::str_subset(x$childrenOrder, 'axis');
        
        c(
            getGrob(x, 'gene.text')$children,
            list(getGrob(x, 'node.labels')),
            getGrob(x, 'title.text')$children,
            sapply(
                x$children[axis.keys],
                FUN = function(ax) {
                    c(
                        list(getGrob(ax, 'axis.label')),
                        list(getGrob(ax, gPath('axis.content', 'labels')))
                        )
                    }
                )
            );
        }
    
    example.grobs <- get.text.grobs(example);
    test.grobs <- get.text.grobs(test);

    all(sapply(
        1:(length(example.grobs)),
        FUN = function(i) {
            compare.text(
                example.grobs[[i]],
                test.grobs[[i]]
                );
            }
        ));
    }

test.polygon.grobs <- function(example, test) {
    get.polygon.keys <- function(x) {
        stringr::str_subset(x$childrenOrder, 'polygon')
        }
    
    compare.polygons <- function(x, y) {
        coords.equal <- all(sapply(
            c('x', 'y'),
            FUN = function(coord) {
                units.are.equal(x[[coord]], y[[coord]]);
                }
            ));
        
        gp.equal <- identical(x$gp, y$gp);
        
        all(coords.equal, gp.equal);
        }
    
    example.keys <- get.polygon.keys(example);
    test.keys <- get.polygon.keys(test);
    
    all(sapply(
        1:(length(example.keys)),
        FUN = function(i) {
            compare.polygons(
                getGrob(example, example.keys[[i]]),
                getGrob(test, test.keys[[i]])
                )
            }
        ));
    }

units.are.equal <- function(x, y) {
    length(x) == length(y) & 
        all(unitType(x) == unitType(y)) & 
        all(as.numeric(x - y) == 0)
}
