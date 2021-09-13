library(testthat)
library(slice)

source("../../../data-raw/DPList.R")


# try shinyslice



shiny::testModule(plot_server, {
    print(output$plots)

   expect_silent(output$plot1)
   expect_error(expect_equal(output$plot1, output$plot2))
}, dplist = plots, gobutton = shiny::reactive(1), points = NULL)



testModule(multiplot_server, {
    print(output$multiplots)

    expect_silent(output$plot1)
}, listdps = list(dp1 = plots, dp2 = plots), gobutton = reactive(1), points = NULL)
