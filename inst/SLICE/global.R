
if (exists(".SLICE_OBJECT")) {
    title <- .SLICE_OBJECT$title
} else {
    # source("../../data-raw/DPList.R") # for testing purposes only
    title <- get(".SLICE_OBJECT", envir = ssbf:::.sso_env)$title
    # obj <- mget(".SLICE_OBJECT", envir = slice:::.sso_env, ifnotfound = list(sso))
}
