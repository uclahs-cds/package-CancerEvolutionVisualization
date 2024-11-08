
update.descendant.property <- function(
    x,
    parent.id,
    property,
    value,
    overwrite = TRUE
    ) {
    # update children property
    children <- x[x$parent %in% parent.id, ];

    # If there are no child rows, return the original data.frame
    if (nrow(children) == 0) {
        return(x);
        }

    # For each child row, add the column and call the function recursively
    for (child.id in children$label) {
        # Add the column for the current child
        if (overwrite) {
            x[x$label == child.id, property] <- value;
        } else {
            x[x$label == child.id & is.na(x[[property]]), property] <- value;
            }

        # Call the function recursively for the current child
        x <- update.descendant.property(
            x,
            child.id,
            property,
            value,
            overwrite
            );
        }
    return(x);
    }
