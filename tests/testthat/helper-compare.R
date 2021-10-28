test.segments <- function(example, test) {
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


units.are.equal <- function(x, y) {
    length(x) == length(y) & 
        all(unitType(x) == unitType(y)) & 
        all(as.numeric(x - y) == 0)
    }