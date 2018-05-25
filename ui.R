library(shiny)
fluidPage(
  title = "Examples of DataTables",
    fluidRow(column(width = 4,DT::dataTableOutput("mytable1")),column(width = 6,plotOutput("plot",height = "900px")))
)