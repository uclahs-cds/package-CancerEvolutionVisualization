get.default.yat <- function(max.y, conversion.factor) {
    scaled.max <- max.y * conversion.factor;

    labels <- Filter(
        function(x) {
            x <= scaled.max;
            },
        pretty(seq(0, scaled.max))
        );
    }
