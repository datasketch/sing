library(shiny)
library(shinyinvoer)

ui <- fluidPage(
  uiOutput("numeric_out"),
  uiOutput("slider_out")
)

server <- function(input, output, session) {

  output$numeric_out <- renderUI({
    numberInput("id_test",
                "Number of observations",
                value = 70,
                min = 10,
                max = 100)
  })

  output$slider_out <- renderUI({
    sliderInput("id_slide",
                "Slide value",
                value = 7,
                min = 3,
                max = 15)
  })


  observe({
    print(
      any(purrr::map(c("id_slide", "id_test"), function(id){
        primary_conditions(input, input_id = id, condition = "equals", comparison_value = 10)
      }) |> unlist()
      )
    )
  })


}

shinyApp(ui, server)
