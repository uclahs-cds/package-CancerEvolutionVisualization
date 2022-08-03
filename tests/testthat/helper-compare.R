compare.trees <- function(example, test) {
    # Utilities
    units.are.equal <- function(x, y, threshold = 0) {
        get.differences <- function() {
            round(
                as.numeric(convertUnit(x, 'native') - convertUnit(y, 'native')),
                3
                );
            }
        length(x) == length(y) &
            all(unitType(x) == unitType(y)) &&
            all(get.differences() <= threshold)
        }

    get.axis.keys <- function(x) {
        stringr::str_subset(x$childrenOrder, 'axis');
        }

    # Grob comparisons
    test.segment.grobs <- function(example, test) {
        get.segment.grobs <- function(x) {
            c(
                list(getGrob(x, 'tree.segs.1')),
                list(getGrob(x, 'tree.segs.2')),
                sapply(
                    x$children[get.axis.keys(x)],
                    FUN = function(ax) {
                        list(getGrob(ax, gPath('axis.content', 'ticks')));
                        }
                    )
                );
            }

        example.grobs <- get.segment.grobs(example);
        test.grobs <- get.segment.grobs(test);

        compare.segments <- function(x, y) {
            coords.equal <- sapply(
                    c('x0', 'x1', 'y0', 'y1'),
                    FUN = function(k) {
                        units.are.equal(x[[k]], y[[k]]);
                        }
                    );

            gp.equal <- identical(x$gp, y$gp);
            arrow.equal <- identical(x$arrow, y$arrow);

            return(all(
                coords.equal,
                gp.equal,
                arrow.equal
                ));
            }

        all(sapply(
            1:(length(example.grobs)),
            FUN = function(i) {
                compare.segments(
                    example.grobs[[i]],
                    test.grobs[[i]]
                    );
                }
            ));
        }

    test.line.grobs <- function(example, test) {
        get.line.grobs <- function(x) {
            sapply(
                x$children[get.axis.keys(x)],
                FUN = function(ax) {
                    list(getGrob(ax, gPath('axis.content', 'major')));
                    }
                );
            }

        compare.lines <- function(x, y) {
            coords.equal <- all(sapply(
                c('x', 'y'),
                FUN = function(k) {
                    units.are.equal(x[[k]], y[[k]])
                    }
                ));

            arrow.equal <- identical(x$arrow, y$arrow);

            all(coords.equal, arrow.equal);
            }

        example.grobs <- get.line.grobs(example);
        test.grobs <- get.line.grobs(test);

        all(sapply(
            1:(length(example.grobs)),
            FUN = function(i) {
                compare.lines(
                    example.grobs[[i]],
                    test.grobs[[i]]
                    );
                }
            ));
        }

    test.text.grobs <- function(example, test) {
        compare.text <- function(x, y) {
            labels.equal <- identical(x$label, y$label);

            just.equal <- (
                identical(x$just, y$just)
                && identical(x$hjust, y$hjust)
                && identical(x$vjust, y$vjust)
                );

            rot.equal <- x$rot == y$rot;
            gp.equal <- identical(x$gp, y$gp);

            coords.equal <- all(sapply(
                c('x', 'y'),
                FUN = function(coord) {
                    units.are.equal(x[[coord]], y[[coord]], 10);
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
            c(
                getGrob(x, 'gene.text')$children,
                list(getGrob(x, 'node.labels')),
                getGrob(x, 'main.text')$children,
                sapply(
                    x$children[get.axis.keys(x)],
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

    all(
        test.segment.grobs(example, test),
        test.text.grobs(example, test),
        test.polygon.grobs(example, test),
        test.line.grobs(example, test)
        );
    }
