
require(shiny)
require(shinydashboard)
require(rhandsontable)
require(sp)
require(data.table)
require(tidyverse)
require(lubridate)
require(reshape2)
require(gdata)
getwd()
setwd("C:/Users/Beach/Documents/GitHub/etm01-TheDenominator/files")
# read matches info
matches=readRDS('df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds')

setDT(matches)

matches_mod=matches[complete.cases(matches)]
matches_mod=matches_mod[!grepl("POSTP.", matches_mod$score)]
matches_mod[,timestamp:=as_datetime(date,tz='Turkey')]
matches_mod=matches_mod[order(home,timestamp)]
matches_mod

matches_modyear=matches_mod[,list(home,away,score,timestamp,year(timestamp))]
matches_modyear2 = format(as.Date(matches_modyear$timestamp,format="%Y-%m-%d"),"%m")
matches_modyear3 = cbind (matches_modyear, matches_modyear2)
colnames(matches_modyear3)[6] <- "month"
matches_modyear3
matches_modyear3$Season <- ifelse(matches_modyear3$month >=7, as.numeric(matches_modyear3$V5)+1, as.numeric(matches_modyear3$V5))
matches_modyear = matches_modyear3

matches_mod1=matches[complete.cases(matches)]
matches_mod1=matches_mod[!grepl("POSTP.", matches_mod$score)]
matches_mod1[,c('score_home','score_away'):=tstrsplit(score,':')]
matches_mod1[,score_home:=as.numeric(score_home)]
matches_mod1[,score_away:=as.numeric(score_away)]
matches_mod1$type = NULL
matches_mod1[,timestamp:=as_datetime(date,tz='Turkey')]
matches_mod1=matches_mod1[order(home,timestamp)]
matches_mod1


matches_modyear1=matches_mod1[,list(home,away,score_home,score_away,timestamp,year(timestamp))]
matches_modyear1

#Average Goals Scored by Home Teams
matches_modyear1_dc=dcast(matches_modyear1,home~V6, value.var = "score_home", fun.aggregate = mean)
#Average Goals Conceded by Home Teams
matches_modyear2_dc=dcast(matches_modyear1,home~V6, value.var = "score_away", fun.aggregate = mean)
#Average Goals Scored by Away Teams
matches_modyear3_dc=dcast(matches_modyear1,away~V6, value.var = "score_away", fun.aggregate = mean)
#Average Goals Conceded by Away Teams
matches_modyear4_dc=dcast(matches_modyear1,away~V6, value.var = "score_home", fun.aggregate = mean)
#Reshapig 
matches_modyear4_dc_melt=melt(matches_modyear4_dc, id.vars = "away", measure.vars = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
matches_modyear3_dc_melt=melt(matches_modyear3_dc, id.vars = "away", measure.vars = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
matches_modyear2_dc_melt=melt(matches_modyear2_dc, id.vars = "home", measure.vars = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
matches_modyear1_dc_melt=melt(matches_modyear1_dc, id.vars = "home", measure.vars = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))

matches_modyear5_dc_melt = cbind(matches_modyear1_dc_melt,matches_modyear2_dc_melt[,3], matches_modyear3_dc_melt[,3], matches_modyear4_dc_melt[,3])
names(matches_modyear5_dc_melt)=c("team","year","scored_at_home", "conceded_at_home", "scored_on_away", "conceded_on_away")
matches_modyear5_dc_melt_melt = melt(matches_modyear5_dc_melt, id.vars=c("team", "year"), measure.vars= c("scored_at_home", "conceded_at_home", "scored_on_away", "conceded_on_away"))
matches_modyear5_dc_melt_melt_DT=data.table(matches_modyear5_dc_melt_melt)
names(matches_modyear5_dc_melt_melt_DT)=c("team","year","Category", "Goals")

ui <- dashboardPage(
  dashboardHeader(title = "Premiere League Statistics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Team Performance By Year", tabName = "list", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "list",
              fluidRow(
                column(width = 5,
                       selectInput(inputId = "team_detail",
                                   label="Enter Team Name:",
                                   choices = unique(matches_modyear$home),
                                   selected = "arsenal"),
                       dataTableOutput("team_table")
                      
                ),
                column(width = 6,
                       selectInput(inputId = "date_detail",
                                   label="Enter Date:",
                                   choices = unique(matches_modyear$Season),
                                   selected = "2010"),
                       dataTableOutput("date_table")
                       
                ),
                column(width = 7,
                       plotOutput("team_plot")
                )
              )
      ))))

server <- function(input, output) {
  
  output$team_table <-renderDataTable({
    
    
    table_not_to_show3 = matches_modyear[home==input$team_detail]
    table_not_to_show2 = matches_modyear[away==input$team_detail]
    table_not_to_show = rbind(table_not_to_show2, table_not_to_show3)
    table_to_show = table_not_to_show[Season==input$date_detail]
    table_to_show$V5 =NULL
    table_to_show$month = NULL
      table_to_show
  })
  
  
  output$team_plot <- renderPlot({
    
    data_not_to_show=matches_modyear5_dc_melt_melt_DT[team==input$team_detail]
    ggplot(data_not_to_show[year==input$date_detail]) + 
      geom_bar(aes(x=Category,y=Goals),stat = "identity")
    
    
  })
 
}


shinyApp(ui, server)
