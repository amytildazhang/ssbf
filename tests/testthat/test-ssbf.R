get_private <- function(obj) {
    obj$.__enclos_env__$private
}

Xf <- model.matrix(mpg ~ disp, data = mtcars)
Xr <- model.matrix(mpg ~ -1 + factor(cyl), data = mtcars)
Sigma_inv <- diag(rep(3, ncol(Xr)))
Phi_inv <- rep(2, nrow(Xf))

P <- ncol(Xf) + ncol(Xr)
check_sig <- matrix(0, P, P)
check_sig[(1 + ncol(Xf)):P, (1 + ncol(Xf)):P] <- Sigma_inv

mtcars_labeled <- mtcars
mtcars_labeled$Model8_7 <- rownames(mtcars)
mtcars_labeled$cyl <- factor(mtcars_labeled$cyl)
rownames(mtcars_labeled) <- NULL

rc <- c("cyl", "disp")
id_cols <- c("Model8_7", "mpg", "disp", "cyl")
clean_cols <- id_cols

ssbf <- SSBF$new(data = mtcars_labeled, Xf = Xf, Phi_inv = Phi_inv, Xr = Xr, Sigma_inv = Sigma_inv,
                 id_cols = id_cols)
ssbf$calc()
private <- get_private(ssbf)


test_that("Values are set", {
    expect_equal(ssbf$id_cols, id_cols)
    expect_equal(private$.id_cols, ssbf$id_cols)
    expect_equal(ssbf$data, mtcars_labeled)

    expect_equal(ssbf$relp_cols, NULL)
    expect_equal(ssbf$restricted_colnames, private$.restricted_colnames)
})


test_that("Can set values", {
    expect_silent(ssbf$relp_cols <- rc)
    expect_equal(ssbf$relp_cols, rc)

    expect_silent(ssbf$data <- mtcars_labeled)
    expect_equal(ssbf$data, mtcars_labeled)

    expect_silent(ssbf$relp_funs <- list("cyl" = list("tag" = "same", "fun" = function(x, y) x == y)))
    expect_silent(ssbf$relp_funs <- NULL)

})
# test_that("clean_names() functions as expected", {
#     expect_equal(private$clean_names("ABC3"), "abc3")
#     expect_equal(private$clean_names("A$B C^"), "a_b_c_")
#     expect_equal(private$clean_names("A$ %BC"), "a_bc")
# })

test_that("label_df() functions as expected", {
    ldf <- private$label_df()
    expect_equal(nrow(ldf), nrow(unique(mtcars_labeled[, id_cols]))^2)
    expect_equal(ncol(ldf), length(id_cols)*2)
    expect_equal(colnames(ldf), c(clean_cols, sprintf("l_%s", clean_cols)))
    expect_snapshot(private$label_df())
})


test_that("set_relp_funs()", {
    expect_message(private$set_relp_funs(mtcars_labeled))
    expect_snapshot(self$relp_funs)
})



test_that("match_info()", {
    ldf <- private$label_df()
    self <- ssbf
    minfo <- private$match_info(ldf)
    expect_equal(nrow(minfo), nrow(ldf))
    expect_equal(ncol(minfo), 3*length(id_cols))
    coln <- colnames(minfo)
    coln <- stringr::str_replace(coln, sprintf("(%s)|(%s)", private$.same_tag, private$.large_tag), "")
    expect_equal(sort(coln[1:4]), sort(coln[9:12]))
    expect_equal(minfo$`ssbf_large_ mpg`, minfo$l_mpg > 19.2)
    expect_equal(minfo$`ssbf_large_ disp`, minfo$l_disp > 196.3)
})


test_that("gen_relp_df()", {
    minfo <- private$match_info(private$label_df())
    expect_message(relp_df <- ssbf$gen_relp_df())
    expect_snapshot(relp_df)
    expect_equal(nrow(relp_df), nrow(minfo))
    expect_equal(c(colnames(minfo), "relationship"), colnames(relp_df))
    expect_message(relp_nogen <- ssbf$gen_relp_df(exclude_generators = TRUE))
    expect_equal(relp_nogen, relp_df[, -c(9:12)])


    # supply function correclty throws errors
    expect_error(ssbf$relp_funs <- list("disp" = list(label = "", fun = function(x, y) rep(T, length(x)))))

    # supplying a correct relp_funs works
    expect_silent(ssbf$relp_funs <- list("disp" = list(label = "all-", fun = function(x, y) rep(T, length(x)))))
    expect_message(relp_df <- ssbf$gen_relp_df())
    expect_true(all(relp_df$`all-disp`))
})

