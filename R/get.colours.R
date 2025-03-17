get.colours <- function(
    value.list,
    return.names = FALSE,
    predetermined.colours = NULL
    ) {
    colours <- grDevices::colors()[grep('(white|gr(a|e)y)', grDevices::colors(), invert = T)];
    unique.values <- unique(value.list);
    n <- length(unique.values);

    col.list <- sample(colours, n);

    if (!is.null(predetermined.colours) && !is.null(value.list)) {
        col.list[seq_along(predetermined.colours)] <- predetermined.colours;
        }

    if (is.null(levels(value.list))) {
        value.list <- factor(value.list, levels = unique.values);
        }

    names(col.list) <- levels(value.list);

    if (return.names) {
        return(col.list);
    } else {
        return(col.list[value.list]);
        }
    }

#' Generate a named vector of colors for every value specified,
#' ordered by the value in `value.order`.
#'
#' Assigns colors to values and ensures they follow a specified order.
#' Any colors specified in `predetermined.colours` are maintained in the order
#' specified and are used as the first colors for the `value.list`.
#'
#' @param predetermined.colours A vector of colors assigned to values. If `NULL`, colors will be generated automatically.
#' @param value.list A vector of values.
#' @param value.order An optional vector specifying the order of values. If `NULL`, value order is not gauranteed.
#'
#' @return A list containing:
#' \describe{
#'   \item{colours}{A named vector of colors assigned to each value.}
#'   \item{value.order}{The ordered values.}
#' }
get.colours.in.order <- function(
    predetermined.colours,
    value.list,
    value.order = NULL
    ) {

    if (is.null(predetermined.colours) && is.null(value.order)) {
        value.list <- NULL;
        }

    if (is.null(value.order) && !is.null(value.list)) {
        value.order <- unique(value.list);
        }

    if (is.null(predetermined.colours) || is.null(value.order)) {
        predetermined.colours <- NULL;
    } else {
        sampled.colours <- get.colours(value.list);
        sampled.colours[seq_along(predetermined.colours)] <- predetermined.colours;
        predetermined.colours <- setNames(
            sampled.colours[seq_along(value.order)],
            value.order
            );
        }

    return(list(
        colours = predetermined.colours,
        value.order = value.order
        ));
    }

get.colour.luminance <- function(colour) {
    # Formulas and values documented in:
    # https://www.w3.org/WAI/GL/wiki/Relative_luminance
    sRGB.values <- col2rgb(colour) / 255;
    sRGB.values <- sapply(
        sRGB.values,
        FUN = function(sRGB.value) {
            if (sRGB.value <= 0.03928) {
                return(sRGB.value / 12.92);
            } else {
                return(((sRGB.value + 0.055 ) / 1.055) ** 2.4);
                }
            }
        );

    luminance.modifiers <- c(0.2126, 0.7152, 0.0722);
    luminance <- sum(sRGB.values * luminance.modifiers);

    return(luminance);
    }

get.contrast.ratio <- function(luminance1, luminance2) {
    # Based on WCAG accessibility standards:
    # https://www.w3.org/TR/2008/REC-WCAG20-20081211/#visual-audio-contrast-contrast
    luminance <- sort(
        c(luminance1, luminance2),
        decreasing = TRUE
        );
    luminance <- luminance + 0.05;

    contrast.ratio <- luminance[1] / luminance[2];
    return(contrast.ratio);
    }
