#' @importFrom magrittr "%>%"
#' @exportPattern "^[[:alpha:]]+"



read_only <- function(name) {
    function(value) {
        if (missing(value)) {
            private$name
        } else {
            stop("variable is read only", call. = FALSE)
        }

    }
}



validate <- function(priv, name) {
    function(value) {
        if (missing(value)) {
            private$priv
        } else {
            private[[sprintf(".validate_%s", name)]](value)
            private$priv <- value
        }

    }
}
