library(shiny)

shinyUI(bootstrapPage(

  fileInput("file.wav", 
            "Choose sound File",
            accept = c(".wav")),
  
  plotOutput(outputId = "pie.result", height = "300px"),
  
  h3(textOutput("txt.result"))
  
  
  # Display this only if the density is shown
#  conditionalPanel(condition = "input.density == true",
#                   sliderInput(inputId = "bw_adjust",
#                               label = "Bandwidth adjustment:",
#                               min = 0.2, max = 2, value = 1, step = 0.2)
#  )
  
))