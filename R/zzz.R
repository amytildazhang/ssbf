# https://github.com/stan-dev/shinystan/blob/master/R/zzz.R

.sso_env <- new.env(parent = emptyenv())

# .onAttach <- function(...) {
#     ver <- utils::packageVersion("slice")
#     msg <- paste0("\nThis is slice version ", ver,"\n")
#     packageStartupMessage(msg)
# }

.onLoad <- function(libname, pkgname) {
# TODO: set global options
        op <- options()
    op.shinystan <- list(
        shinystan.rstudio = FALSE
    )
    set_ops <- !(names(op.shinystan) %in% names(op))
    if (any(set_ops)) options(op.shinystan[set_ops])
    invisible()
}
