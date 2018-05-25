####
##
##
##########
# hitory
# To do
## 1. download fig in pdf file
## 2. more data from other age group
## 2b. slide years to match up age (option)
## <hypothesis>
## 1. Swimmer in SCA became better during summer, while WCA's do not.
## 2. 

library(shiny)
library(ggplot2)  # for the diamonds dataset
library(tidyverse)
library(reshape2)
library(lubridate) # for date/time. Read chap 13 in "R for Data Science" by Wkckham 2016. 
library(directlabels) # for labeling line with name2 by geom_dl https://stackoverflow.com/questions/37667539/add-text-to-geom-line-in-ggplot?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
 
# read data
Fig.data <- read.csv("Synchro_Figure_score - record.csv")
# general function
#Vlookup in R (https://gist.github.com/jnmaloof/7367450)
#Version 0.3 November 12, 2013
#Return senesical results if return column is a factor
#Version 0.2 November 11, 2013
#Require first column of table to be numeric if range lookup is being done
#Change defaults to larger=FALSE
#Julin Maloof
vlookup <- function(ref, #the value or values that you want to look for
                    table, #the table where you want to look for it; will look in first column
                    column, #the column that you want the return data to come from,
                    range=FALSE, #if there is not an exact match, return the closest?
                    larger=FALSE) #if doing a range lookup, should the smaller or larger key be used?)
{
  if(!is.numeric(column) & !column %in% colnames(table)) {
    stop(paste("can't find column",column,"in table"))
  }
  if(range) {
    if(!is.numeric(table[,1])) {
      stop(paste("The first column of table must be numeric when using range lookup"))
    }
    table <- table[order(table[,1]),] 
    index <- findInterval(ref,table[,1])
    if(larger) {
      index <- ifelse(ref %in% table[,1],index,index+1)
    }
    output <- table[index,column]
    output[!index <= dim(table)[1]] <- NA
    
  } else {
    output <- table[match(ref,table[,1]),column]
    output[!ref %in% table[,1]] <- NA #not needed?
  }
  dim(output) <- dim(ref)
  output
}
##
Fig.data.s<-Fig.data %>% as_tibble() %>% dplyr::mutate(DateTime2=mdy(DateTime,tz=NULL),year=year(DateTime2))
Fig.data.s<-Fig.data.s %>% mutate(name2=toupper(name))
Fig.data.s.summary <- Fig.data.s %>% group_by(name2,team) %>% summarise(average=mean(score))

swimmer2006<-Fig.data.s %>% filter(year %in% c(2017, 2018), category %in% "11-12",competition %in% c("Invitational Figures San Francisco","West Region B championships")) %>% 
  group_by(name2, competition,category) %>% 
  summarise(num=n()) %>% filter(num==2) %>% 
  ungroup() %>% select(name2) %>% unique # attend two times in 11-12 (= born in 2006)
swimmer2007<-Fig.data.s %>% filter(year==2018,competition %in% c("Invitational Figures San Francisco","West Region B championships")) %>%
  filter(!name2 %in% swimmer2006$name2) %>% select(name2) %>% unique()
# input born data into Fig.data.s.summary
Fig.data.s.summary$born<-" "
Fig.data.s.summary[Fig.data.s.summary$name2 %in% swimmer2006$name2,"born"]<-"2006"
Fig.data.s.summary[Fig.data.s.summary$name2 %in% swimmer2007$name2,"born"]<-"2007"
Fig.data.s.summary <- Fig.data.s.summary %>% mutate(average=round(average,2))
##
function(input, output) {
  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
   # DT::datatable(Fig.data.s.summary %>% DT::formatRound("average",2)) # did not work
    DT::datatable(Fig.data.s.summary)
  })
   # graph
  pl <- reactive({
    # draw ggpllot
    #ggplot(Fig.data.s,aes(x=DateTime2,y=score,color=name2))  + geom_point() + geom_line(aes(linetype=team))  + theme_bw() + theme(legend.position="none")
    print("input$mytable1_rows_selected is")
    print(input$mytable1_rows_selected)
    Fig.data.s.selected<- Fig.data.s %>% filter(name2 %in% as_vector(Fig.data.s.summary[input$mytable1_rows_selected,"name2"]))
    # geom_dl is used for labeling line with names
    ggplot(Fig.data.s.selected,aes(x=DateTime2,y=score))  + geom_point() + geom_line(aes(group=name2,color=team))  + 
      geom_dl(aes(label = name2), method = list(dl.trans(x = x + .1), "last.points")) +
      expand_limits(x = as.Date("2018-10-01")) +
      theme_bw() + theme(legend.position="none")
      })
  output$plot <- renderPlot({pl()})
}

