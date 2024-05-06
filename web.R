library(shiny)

ui = fluidPage(
  titlePanel("Abalone Age Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("length", "Length", value = 0.5),
      numericInput("diameter", "Diameter", value = 0.4),
      numericInput("height", "Height", value = 0.1),
      numericInput("whole_weight", "Whole Weight", value = 0.5),
      numericInput("shucked_weight", "Shucked Weight", value = 0.2),
      numericInput("viscera_weight", "Viscera Weight", value = 0.1),
      numericInput("shell_weight", "Shell Weight", value = 0.15),
      actionButton("predict", "Predict Age")
    ),
    mainPanel(
      textOutput("age_prediction")
    )
  )
)

server = function(input, output) {
  observeEvent(input$predict, {
    new_data = data.frame(
      Length = input$length,
      Diameter = input$diameter,
      Height = input$height,
      Whole_weight = input$whole_weight,
      Shucked_weight = input$shucked_weight,
      Viscera_weight = input$viscera_weight,
      Shell_weight = input$shell_weight
    )
    
    predicted_age <- predict(models$rf, new_data)
    
    output$age_prediction <- renderText({
      paste("Predicted age:", round(predicted_age))
    })
  })
}
shinyApp(ui = ui, server = server)