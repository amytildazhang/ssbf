
#' Title
#'
#' @param ssbf_obj
#' @param x
#' @param y
#' @param plottype
#' @param color_relp
#' @param subset
#' @param aes_args
#' @param other_args
#'
#' @return
#' @export
#'
#' @examples
bfp <- function(ssbf_obj, x, y, plottype, color_relp = TRUE, subset = NULL,
                aes_args = list(), other_args = list()) {
    plotdata <- ssbf_obj$gen_df(x, y, subset = subset)

    plotgeom <- switch(
        plottype,
        "density" = ggplot2::geom_density,
        "scatter" = ggplot2::geom_point,
        "heatmap" = ggplot2::geom_tile,
        "contour" = ggplot2::geom_contour
    )




    sym_x <- rlang::enquo(x)
    sym_y <- rlang::enquo(y)

    aes_call <-  as.call(c(list(ggplot2::aes, x = sym_x, y = sym_y), aes_args))

    if (color_relp) {
        aes_call[["fill"]] <- aes_call[["color"]] <- rlang::quo(relationship)
    }


    if (plottype %in% c("heatmap", "contour")) {
        plotdata <- dplyr::left_join(
            plotdata,
            dplyr::group_by_at(plotdata, c(x, y)) %>%
                dplyr::summarise(ssbf = mean(ssbf))
        )
    }
    aes_call$z <- aes_call$fill <- rlang::quo(ssbf)

    base_call <- as.call(list(ggplot2::ggplot, data = plotdata, mapping = aes_call))
    for (nm in other_args) {
        base_call[[nm]] <- other_args[[nm]]
    }
    eval(base_call) + plotgeom()
}






#' @export
#' @describeIn bfp
bfp_borrowfactors <- function(ssbf_obj, ...) {

   bfp(ssbf_obj, "bf", "ssbf", "scatter", color_relp = TRUE, ...)

}


#' @export
#' @describeIn bfp
bfp_davail <- function(ssbf_obj, aes_args = list(), other_args = list()) {


}



#' @export
#' @describeIn bfp
bfp_pssbf <- function(ssbf_obj, ...) {
    bfp(ssbf_obj, x = "pssbf", y = "ssbf", plottype = "scatter", color_relp = TRUE, ...)
}


#' @export
#' @describeIn bfp
bfp_ssbf <- function(ssbf_obj, x, plottype, ...) {
    bfp(ssbf_obj, x = x, y = "ssbf",plottype = plottype, ...)

}



# bf vs ssbf
# size vs ssbf
#
