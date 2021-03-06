# Define server logic required to draw a histogram
library(shiny)
library(ggplot2)  
library(tidyverse)
library(reshape2) # I do not think I use this package anymore.
library(lubridate) # for date/time. Read chap 13 in "R for Data Science" by Wkckham 2016. 
library(directlabels) # for labeling line with name2 by geom_dl https://stackoverflow.com/questions/37667539/add-text-to-geom-line-in-ggplot?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

# Define UI for application that draws a histogram
## two fluid rows (upper for file input and lower for data table (left) and plot (right))
ui <- fluidPage(
          fluidRow(column(width=12,     
                          # input data file
                 fileInput("file1", "Choose CSV File",
                     accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
                         ),
                     tags$hr(),
                     checkboxInput("header", "Header", TRUE)
                  ),
          fluidRow(column(width = 6,
                  # drawing data table (summary)
                  DT::dataTableOutput("mytable1")),
                  # plot
                  column(width = 6,plotOutput("plot",height = "1200px")),
                  # download plot button
                  column(width =3, offset=1,downloadButton("downloadplot",label="Download plot"))
                  )
                  
  )
)

# start function()
server<-function(input, output) {
  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
  # read table from uploaded file with fileInput function in UI (https://shiny.rstudio.com/reference/shiny/0.14/fileInput.html)
  inFile <- input$file1
  if(is.null(inFile))
    return(NULL)
  Fig.data <- read.csv(inFile$datapath,header=input$header)
  # Fig.data <- read.csv("Synchro_Figure_score - record.JO.csv")
  # Fig.data <- read.csv("../Synchro_Figure_score - record.csv")
  # process Fig.data
  ## separate date data
  Fig.data.s<-Fig.data %>% as_tibble() %>% mutate(DateTime2=mdy(DateTime),year=year(DateTime2))
  Fig.data.s<-Fig.data.s %>% mutate(name2=toupper(name),team=toupper(team))
  ## remove unnecessary data (category containing "H" for not belonging that category)
  Fig.data.s <- Fig.data.s %>% filter(!(name2=="MORGAN, RILEY" & year=="2017" & category=="13-15"))
  #Fig.data.s %>% filter(category==str_match(category,pattern=".+H"))
   Fig.data.s %>% filter(str_detect(category,pattern=".+H"))
    Fig.data.s <- Fig.data.s %>% filter(!str_detect(category,pattern=".+H"))
  # Adding "born" information (to calculate age) automatically to Fig.data.s
  # To distinguish 2006 and 2007 based on age group 11-12 in 2017 and 2018
      swimmer2006<-Fig.data.s %>% filter(year %in% c(2017, 2018), category %in% "11-12",competition %in% c("Invitational Figures San Francisco","West Region B championships")) %>% 
      group_by(name2, competition,category) %>% 
      summarise(num=n()) %>% filter(num==2) %>% 
      ungroup() %>% select(name2) %>% unique # attend two times in 11-12 (= born in 2006)
      swimmer2007<-Fig.data.s %>% filter(year==2018,competition %in% c("Invitational Figures San Francisco","West Region B championships")) %>%
      filter(!name2 %in% swimmer2006$name2) %>% select(name2) %>% unique()
    # To asign 2005 based on moving age group 11-12 in 2017 to 13-15 2018
      swimmer2017_11_12 <- Fig.data.s %>% unite(year.category, year, category, remove=FALSE)  %>%        filter(year.category=="2017_11-12") %>% dplyr::select(name2)
      swimmer2018_13_15 <- Fig.data.s %>% unite(year.category, year, category, remove=FALSE)  %>%        filter(year.category=="2018_13-15") %>% dplyr::select(name2)
      swimmer2005<-swimmer2017_11_12 %>% inner_join(swimmer2018_13_15) %>% unique()
    # input born data into Fig.data.s.summary
      Fig.data.s[Fig.data.s$name2 %in% swimmer2005$name2,"born"]<-"2005"
      Fig.data.s[Fig.data.s$name2 %in% swimmer2006$name2,"born"]<-"2006"
      Fig.data.s[Fig.data.s$name2 %in% swimmer2007$name2,"born"]<-"2007"
    # Manual edition of born year of known swimmers
      Fig.data.s[Fig.data.s$name2=="YAMAMOTO, MARI","born"]<-"2002"
    # Manual edition of names
      Fig.data.s<-Fig.data.s %>% mutate(name2=str_replace(name2,"MOORE, EMMA","MOORE, EMILEEN"))
    # save
      save(Fig.data.s, file="Fig.data.s.Rdata")
    # making summary table, calculate average score, and round up the score
  Fig.data.s.summary <- Fig.data.s %>% group_by(name2,team) %>% summarise(average=mean(score)) %>% mutate(average=round(average,2)) 
    # adding born info in Fig.data.s to Fig.data.s.summary
        Fig.data.s.summary<-Fig.data.s.summary %>%  inner_join(Fig.data.s[!is.na(Fig.data.s$born),c("name2","born")],by="name2") %>% unique()
    # save
      save(Fig.data.s.summary,file="Fig.data.s.summary.Rdata")
        DT::datatable(Fig.data.s.summary)
  })
  # plotting graph by ggplot2 package
  pl <- reactive({
    print("input$mytable1_rows_selected is")
    print(input$mytable1_rows_selected)
    # load data created
    load("Fig.data.s.Rdata");load("Fig.data.s.summary.Rdata")
    # combine and calculate age
      Fig.data.ss<-Fig.data.s  %>% select(-born) %>% inner_join(Fig.data.s.summary[,c("name2","born")]) %>% mutate(age=interval(start=parse_date_time(born, orders = "Y"),end=DateTime2)/dyears(1)) %>% select(-name,-team.abb,-DateTime2,-year)  
    # treat name2 and team as one variable (for fixing legend problem with swimmer belong to multiple team)
      Fig.data.ss <- Fig.data.ss %>% unite(name2.team,name2,team,remove=FALSE)
    # selected data  
    Fig.data.ss.selected<- Fig.data.ss %>% filter(name2 %in% as_vector(Fig.data.s.summary[input$mytable1_rows_selected,"name2"]))
    # geom_dl is used for labeling line with names
    ggplot(Fig.data.ss.selected,aes(x=age,y=score))  + geom_point() + geom_line(aes(group=name2.team,color=born,linetype=team),show.legend=c(group=FALSE,color=TRUE,linetype=TRUE))  + 
      geom_dl(aes(label = name2), method = list(dl.trans(x = x + .1), "smart.grid")) +
      #geom_dl(aes(label = name2), method = list(dl.trans(x = x + .1), "first.points",rot=30)) +
      #      expand_limits(x = as.Date("2018-10-01")) +
      theme(legend.position="top") #+ theme_bw()
  })
  output$plot <- renderPlot({pl()})
  output$downloadplot<-downloadHandler(
    filename=function() {"plot.pdf"},
    content=function(x) ggsave(plot=pl(),filename=x,width=15,height=20)
  )
}
# shinyApp function below is necessary for "Run Document" and deploy in shinyapps.io
shinyApp(ui = ui, server = server,options=list(width=1200,height=900))

