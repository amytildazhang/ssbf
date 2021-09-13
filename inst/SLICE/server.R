idcol <- rlang::sym("id__")

if (exists(".SLICE_OBJECT")) {
    obj <- .SLICE_OBJECT
} else {
    obj <- get(".SLICE_OBJECT", envir = ssbf:::.sso_env)
    # obj <- mget(".SLICE_OBJECT", envir = slice:::.sso_env, ifnotfound = list(sso))
}

function(input, output) {

    points <- reactiveValues(pt_idx = c())

    # TODO check out DataTables library for addded functionality


    center <- multiAPServer("center", obj$factorplots,
                            reactive(c(input$button_go, input$button_clear)),
                            points, scale = 4)

    side <- multiAPServer("side", obj$sideplots,
                          reactive(c(input$button_go, input$button_clear)),
                          points, brush = F, scale = 2)


    top <- multiAPServer("top", obj$errorplots,
                         reactive(c(input$button_go, input$button_clear)),
                         points, scale = 2)

    # TODO: move these into one

    observeEvent(top(), {
        observe("points from top plots")
        print(top())
        points$pt_idx <- unique(c(points$pt_idx, top()))
    })

    observeEvent(center(), {
        observe("points from center plots")
        print(center())
        points$pt_idx <- unique(c(points$pt_idx, center()))

    })

    observeEvent(side(), {
        observe("points from left plots")
        print(side())
        points$pt_idx <- unique(c(points$pt_idx, side()))
    })


    observeEvent(input$button_clear, {
        points$pt_idx <- c()
    })


    output$text_counter <- renderText({
        sprintf("%s points selected", length(points$pt_idx))
    })

    output$points_df <-  renderTable({
        # print(points$pt_idx)
        if (length(points$pt_idx) > 0) {
            dplyr::filter(obj$data, id__ %in% points$pt_idx)
        } else {
            obj$data
        }
    })


}
