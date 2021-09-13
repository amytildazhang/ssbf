fluidPage(titlePanel(title),

          fluidRow(

          column(width = 4, offset = 0,
                 fluidRow(
                     actionButton("button_go", "Go!"),
                     actionButton("button_clear", "Reset"),
                     textOutput("text_counter", inline = T)

                 ),
                 div(style = 'overflow: scroll; height:200px',
                     tableOutput('points_df')),

                 fluidRow(multiAP("side"))


                 ),



              column(width = 8, offset = 0,
                     fluidRow(multiAP("top")),
                     fluidRow(multiAP("center"))

              )
          )
)
