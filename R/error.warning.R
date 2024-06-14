check.lengths <- function(a, b, a.name, b.name) {
    a.length <- length(a);
    b.length <- length(b);
    if (a.length != b.length) {
        error.message <- paste(
            shQuote(a.name, type = 'cmd'), "and",
            shQuote(b.name, type = 'cmd'), "have differing lengths",
            paste0("(", paste(a.length, "and", b.length), ").")
            );
        stop(error.message);
        }
    }

non.numeric.warning.message <- function(
    argument.name,
    default.value
    ) {
    paste(
        'Non-numeric values found in', shQuote(argument.name, type = 'cmd'),
        paste0('(', 'replacing with default value', default.value, ').')
        );
    }
