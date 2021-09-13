



#' @export
SSBF_app <- R6::R6Class(
    "SSBF_app",
    private = list(
        .factors = NULL,
        .sideplots = NULL,
        .errors = NULL,
        .data = NULL,



        .split_W = function(sliceobjs, subsets, .subsetfun) {
            objs <- purrr::map(sliceobjs, ~.$split_W(subsets, .subsetfun)) %>%
                purrr::set_names(names(sliceobjs))

            purrr::map(subsets, function(set) {
                purrr::map(objs, ~.[[set]]) %>% purrr::set_names(names(objs))
            }) %>% purrr::set_names(subsets)
        }

    ),
    public = list(


        initialize = function(data, title = "SSBF plots",
                              factorplots = NULL,
                              sideplots = NULL,
                              errorplots = NULL){

            private$.data <- dplyr::ungroup(data)

            purrr::walk(list(factorplots, sideplots, errorplots), function(pl) {
                purrr::walk(pl, function(ap) {
                    ap$app_df <- data
                })
            })
            self$factorplots <- factorplots
            self$sideplots <- sideplots
            self$errorplots <- errorplots

            invisible(self)

        },

        title = NULL,
        factorplots = NULL,
        sideplots = NULL,
        errorplots = NULL
    ),
    active = list(
        data = pryr::unenclose(read_only(.data))#,
        # factorplots = pryr::unenclose(read_only(.factors)),
        # sideplots = pryr::unenclose(read_only(.sideplots)),
        # errorplots = pryr::unenclose(read_only(.errors))
    )
)


launch <- function(sso, ...) {
    # credit to https://github.com/stan-dev/shinystan/

    checkmate::assert_r6(sso, "SLICE_app")
    .sso_env$.SLICE_OBJECT <- sso

    on.exit(.sso_env$.SLICE_OBJECT <- NULL, add = TRUE)
    shiny::runApp(system.file("SLICE", package = "ssbf"), ...)
}

