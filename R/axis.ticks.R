prep.yat <- function(yat) {
    if (!is.list(yat)) {
        if (!is.null(yat)) {
            warning(paste(
                '"yat" should be a list.',
                'Default axis ticks will be used.'
                ));
            }

        yat <- list();
        }

    return(yat);
    }

get.default.yat <- function(max.y, conversion.factor) {
    scaled.max <- max.y * conversion.factor;

    labels <- Filter(
        function(x) {
            x <= scaled.max;
            },
        pretty(seq(0, scaled.max))
        );
    }
