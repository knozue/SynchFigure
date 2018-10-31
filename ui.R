library(shiny)
fluidPage(
  
  title = "Figure",
    fluidRow(column(width = 5,DT::dataTableOutput("mytable1")),
             column(width = 5,plotOutput("plot",height = "900px")))
)