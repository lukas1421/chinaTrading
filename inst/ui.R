
library(shiny)
shinyUI(fluidPage(
  titlePanel("Hello shiny"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins","num",min=1,max=50,value=30)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
