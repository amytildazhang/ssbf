ssbf <- function(x) {
    UseMethod("ap_plots")
}





SSBF <- R6::R6Class(
    "SSBF",
    inherit = hatmatrix::HatMatrixCalculator,
    private = list(
        .summary_df = NULL,
        .data = NULL,
        .id_cols = NULL,
        .relp_cols = NULL, # must be a subset of id_cols
        .relp_funs = NULL, # named list of functions
        .relp_grp_idx = NULL,
        .restricted_colnames = c("id__", "l_id__", "cluster__", "l_cluster__",
                                 "shrinkage_factor", "pooling_factor", "ssbf",
                                 "pssbf", "bf_sign"),
        .lender_tag = "l_",
        .same_tag = "ssbf_same_ ",
        .similar_tag = "ssbf_similar_ ",
        .large_tag = "ssbf_large_ ",

        .relp_same = function(col1, col2) {
            col1 == col2
        },
        .relp_similar =  function(bw) {
            function(col1, col2) {
             abs(col1 - col2) <= bw
            }
        },
        .relp_large =   function(threshold) {
            function(col1, col2) {
             abs(col2) > abs(threshold)
            }
        },
        .check_for_id = function(data, id_cols) {
            n_uniq <- purrr::map_dbl(id_cols, ~length(unique(data[[.]])))
            col_has_N_uniq <- (n_uniq == nrow(data))
            if (any(col_has_N_uniq)) {
                ids <- id_cols[col_has_N_uniq]
                message(sprintf(
                    "Identified %s column(s) as potential unique identifiers: %s. Selecting the first as new ID column `id__`.",
                    paste(ids, collapse = ", ")
                ))

                private$data$id__ <- data[[ids[1]]]
            } else {
                private$data$id__ <- 1:nrow(data)
            }
        },
        .tag_relationship = function(match_mask, relp_colnms) {
            matched_cols <- relp_colnms[match_mask]
            same_cols <- stringr::str_subset(matched_cols, sprintf("^%s", private$.same_tag))
            sim_cols <- stringr::str_subset(matched_cols, sprintf("^%s", private$.large_tag))

            tag <- ""
            if (length(same_cols) > 0) {
                tag <- sprintf("%ssame %s", tag, paste(stringr::str_replace(same_cols, private$.same_tag, ""), collapse = ", "))
                if (length(sim_cols) > 0) {
                    tag <- sprintf("%s; ", tag)
                }
            }
            if (length(sim_cols) > 0) {
                tag <- sprintf("%slarge %s", tag, paste(stringr::str_replace(sim_cols, private$.large_tag, ""), collapse = ", "))
            }
            tag

        },
        .def_create_relationship = function(df, relp_colnms = self$relp_cols) {
            grp_idx <- dplyr::group_by_at(df, relp_colnms) %>%
                dplyr::group_indices()
            mat <- as.matrix(dplyr::distinct_at(df, relp_colnms))

            relp_tags <- apply(mat, 1, private$.tag_relationship, relp_colnms)
            relp_tags[grp_idx]
        },

        #' @description Helper function. This
        #'     changes the column names of `data` to be more uniform
        #'     and removes the `id__` column from labeling.
        #' @param data The dataframe used for labeling.
        #' @return A de-duplicated dataframe based on private$.id_cls,
        #'     with column names normalized to be lower
        #'     case and non-alphanumeric characters removed or replaced.
        label_df = function(data = self$data) {
            # de-dupe dataframe based on private$.id_cols
            id_cols_df <- dplyr::group_by_at(data, self$id_cols)
            private$.relp_grp_idx <- dplyr::group_indices(id_cols_df)
            label_df <- dplyr::select_at(data, self$id_cols) %>%
                dplyr::distinct()

            # clean column names
            labels <- private$clean_names(colnames(label_df))
            colnames(label_df) <- labels

            #
            mat <- matrix(0, nrow = nrow(label_df), ncol = nrow(label_df))
            borrowers_label_df <- label_df[as.vector(row(mat)), ]
            lenders_label_df <- label_df[as.vector(col(mat)), ]
            colnames(lenders_label_df) <- sprintf("%s%s", private$.lender_tag, colnames(lenders_label_df))

            dplyr::bind_cols(borrowers_label_df, lenders_label_df)


        },

        clean_names = function(stringvec) {
            stringvec
            # TODO: keep only alphanumeric characters, replace others with _
            #stringr::str_to_lower(stringvec) %>%
             #   stringr::str_replace_all("\\W+", "_")
        },

        #' @description Split columns in private$.data into categorical vs continuous
        set_relp_funs = function(df = self$data) {
            if (is.null(self$relp_cols)) {
                relp_cols <- private$.id_cols
            } else {
                relp_cols <- private$.relp_cols
            }


            continuous <- intersect(colnames(df)[sapply(df, is.numeric)], relp_cols)
            categorical <- intersect(colnames(df)[sapply(df, function(x) is.character(x) || is.factor(x))], relp_cols)
            for (col in relp_cols) {
                if (length(private$.relp_funs) == 0 || !(col %in% names(private$.relp_funs))) {
                    private$.relp_funs[[col]] <- list()
                    if (col %in% continuous) {
                        threshold <- median(abs(df[[col]]))
                        message(sprintf("Labeling as 'large' values of |%s| that are greater than the median, %s.\n", col, threshold))
                        #message(sprintf("Labeling as 'similar' values of %s that are within %s of each other.", col, 2*density(bf[[col]])$bw))
                    }
                    private$.relp_funs[[col]][["label"]] <- ifelse(col %in% categorical, private$.same_tag, private$.large_tag)
                    private$.relp_funs[[col]][["fun"]] <- ifelse(col %in% categorical, private$.relp_same, pryr::unenclose(private$.relp_large(threshold)))
                }
            }
            invisible(NULL)

        } ,

        match_info = function(df, same = NULL, similar = NULL) {
             private$set_relp_funs()
           
            for (col in names(private$.relp_funs)) {
                row_col_name <- col
                col_col_name <- sprintf("%s%s", private$.lender_tag, col)
                new_col_name <- sprintf("%s%s", private$.relp_funs[[col]][["label"]], col)
                if (new_col_name %in% colnames(df)) {
                    warning(sprintf("Over-writing column '%s' with output from self$relp_funs$%s.", col, col))
                }
                df[[new_col_name]] <- private$.relp_funs[[col]][["fun"]](df[[row_col_name]], df[[col_col_name]])
            }

            df
        },
        .validate_data = function(data) {
            clean_names <- private$clean_names(colnames(data))
            restricted_names <- clean_names %in% private$.restricted_colnames
             if (any(restricted_names)) {
                message(sprintf(
                    "Supplied `data` value contains restricted column names. The following columns will be overwritten: %s.",
                    colnames(data)[restricted_names]
                ))
            }
            private$.data <- data
        },
        .validate_id_cols = function(id_cols, data = self$data) {
                        #TODO

            if (!all(id_cols %in% colnames(data))) {

            }
        },
        .validate_relp_cols = function(relp_cols, data = self$data) {
            #TODO
        },
        .validate_relp_funs = function(relp_funs, data = self$data) {
                        #TODO
            relpnames <-purrr::map_chr(names(relp_funs), ~sprintf("%s%s", relp_funs[[.]]$label, .))
            in_id <- relpnames %in% id_cols
            if (any(in_id)) { 
                stop(sprintf("Supplied label for %s creates a name (%s) that matches an entry in id_cols.\n", names(relp_funs)[in_id], relpnames[in_id]), call. = FALSE)
        }
            all(names(relp_funs) %in% relp_cols)
        }      
    ),
    public = list(
        #' @description
        #'
        #' @param data
        #' @param id_cols Character vector of column names which will always be included by
        #'       `$gen_df()` and are used as the basis for setting relationship groups.
        initialize = function(data, Xf, Phi_inv, Xr = NULL, Sigma_inv = NULL,
                              id_cols = colnames(data)) {
            # TODO checks
           
            private$.data <- data
            if (missing(id_cols)) {
                message("No ID columns provided, setting to all columns.")
            }
            private$.id_cols <- id_cols
            super$initialize(Xf, Phi_inv, Xr, Sigma_inv)
        },



        gen_relp_df = function(subset = NULL, exclude_generators = FALSE) {
            relp_df <- private$label_df() %>%
                private$match_info()
            generators <- purrr::map_chr(names(self$relp_funs), ~sprintf("%s%s", self$relp_funs[[.]]$label, .))

            if (is.null(self$label_relationship)) {
                message(sprintf(
                    "Columns used for relationship groups not specified, using all columns in .$id_cols. 
                Set the columns used by setting .$relp_cols <- c(colnames) or by supplying a function which modifies the result of $gen_relp_df()",
                    paste(generators, collapse = ", ")
                ))
                relp_df$relationship <- private$.def_create_relationship(relp_df, generators)
            } else {

                relp_df$relationship <- self$label_relationship(relp_df)
            }
            if (exclude_generators) {
               relp_df <- dplyr::select_at(relp_df, setdiff(colnames(relp_df), generators))
            }
            relp_df
        },


        #' @description Produces a 'long' dataframe of the factors in matrix `W`,
        #'     where each row contains one entry \eqn{W_{pq}} in `W`. The \eqn{p^{th}}
        #'     and \eqn{q^{th}} rows of `data` are also added to the dataframe,
        #'     with prefixes `p_*` and `q_*`, respectively.
        #'
        #'
        #' @details The addition of `p_*` and `q_*` descriptor columns is to
        #'     allow for determining the relationship between `p` and `q`; e.g.
        #'     if they share the same or similar values of a particular covariate.
        #'     In that vein, the parameter `match` allows for the automatic addition
        #'     of columns `same_x` with logical values indicating whether `p`
        #'     and `q` have the same values for column named `x`
        #'
        #' @param match Logical indicating whether or not to automatically create
        #'     columns with names `same_*` indicating whether `p` or `q` have the same values.
        #' @param mask Logical vector of length `N` indicating which datapoints
        #'     to obtain factors for.
        #' @examples
        #' ts <- do.call(BasicSlice$new, testslice)
        #' # columns in resulting df match columns in testslice$data
        #' head(ts$factor_df())
        gen_df = function(subset = NULL, summarise = FALSE) {
    
            relp_df <- gen_relp_df(subset = NULL, exclude_generators = TRUE)
            if (is.null(self$W)) {
                message("No W calculated, calculating using default $calc() arguments.")
                self$calc()
            }

            # to identify those that are in the same cluster
            uniq_rows <- as.numeric(factor(apply(cbind(self$X, self$Phi), 1, paste)))

            

            all_rows_df <- dplyr::bind_cols(
                data.frame(
                    bij = as.vector(self$W),
                    row_idx = as.vector(row(self$W)),
                    col_idx = as.vector(col(self$W))
                ),
                relp_df[private$.relp_grp_idx, ]
            ) %>%
                dplyr::mutate(
                    id__ = self$data$id__[row_idx],
                    l_id__ = self$data$id__[col_idx],
                    cluster__ = uniq_rows[row_idx],
                    l_cluster__ = uniq_rows[col_idx],
                    borrowers = l_cluster__ == cluster__
                ) %>%
                dplyr::rename(bij = val) %>%
                dplyr::select(-row_idx, -col_idx, -dplyr::contains("cluster__")) %>%
                dplyr::group_by(id__) %>%
                dplyr::mutate(
                    shrinkage_factor = sum(bij[borrowers]),
                    pooling_factor = sum(bij[!lenders]),
                    ssbf = sum(bij[!borrowers]^2),
                    relationship = case_when(borrowers ~ "borrowers", TRUE ~ relationship)
                )


            summary_df <- all_rows_df %>%
                    dplyr::mutate(bf_sign = sign(bij)) %>%
                    dplyr::group_by(id__, borrowers, bf_sign, shrinkage_factor, pooling_factor, ssbf, relationship) %>%
                    dplyr::summarise(borrowing_factor = sum(val))
            if (summarise) {
               all_rows_df <- summary_df
            }

            invisible(all_rows_df)

        },

        set_relationship = function(colnms = c(), FUN = NULL) {
            if (is.null(FUN)) {
                private$.relp_cols <- colnms
                private$.label_relationship <- private$.def_create_relationship
            } else {
                private$.label_relationship <- FUN
            }
        }

    ),
    active = list(
        data = pryr::unenclose(validate(.data, "data")),
        id_cols = pryr::unenclose(validate(.id_cols, "id_cols")),
        relp_cols = pryr::unenclose(validate(.relp_cols, "relp_cols")),
        relp_funs = pryr::unenclose(validate(.relp_funs, "relp_funs")),
        restricted_colnames = pryr::unenclose(read_only(.restricted_colnames)),
        summary_df = function(value) {
            if (missing(value)) {
                if (is.null(private$priv)) {
                    self$gen_df(summarise = TRUE)
                }
                private$.summary_df
            } else {
                stop("variable can only be generated by .$gen_df()", call. = FALSE)
            }
        }
    )
)






#####
# To prevent compatibility issues with cross-package inheritance, see http://r6.r-lib.org/articles/Portable.html
#####
objB <- NULL

.onLoad <- function(libname, pkgname) {
    # The namespace is locked after loading; we can still modify objB at this time.
    objB <<- SSBF$new()
}
