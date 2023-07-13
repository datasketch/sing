library(shiny)
library(shinyinvoer)
library(purrr)
library(shinyWidgets)
library(dplyr)
library(hdbase)
library(sing)



source("read-params.R")

ui <- fluidPage(
  uiOutput("test"),
  verbatimTextOutput("deb")
)

server <- function(input, output, session) {

  bd <-
    isolate({
      reactivePoll(1000,
                   session,
                   checkFunc = function() {
                     df <- "eg/db/"
                     if (file.exists(df))
                       file.info(df)$uid
                     else
                       shinyalert(title = "file",text = "Archivo no encontrado")
                   },

                   valueFunc = function() {
                     load_data <- "data-prep.R"

                     source(load_data)$value
                   }
      )
    })

  sing_values <- reactiveValues(label_iris = "Petalos")
  results <- render_sing(session,
                         input,
                         bd = bd(),
                         input_params = input_params,
                         sing_values = sing_values)

  data_filter <- reactive({
    if (is.null(input$what_table_input)) return()
    data_filter_sing(input, bd(), input_params)
  })

  sing:::sing(session,
              input,
              output,
              output_id = "test",
              results = results)


  output$deb <- renderPrint({
    data_filter()$data_filter
  })


}

shinyApp(ui, server)


