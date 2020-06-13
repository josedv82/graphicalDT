
#import required libraries
library(tidyverse)
library(htmlwidgets)
library(htmltools)
library(formattable)
library(sparkline)
library(shiny)
library(feather)
library(DT)
######################################################################################

#data not supplied for this example

######################################################################################

server <- function(input, output) {
  
  #let users filter the table by month
  output$month.filter <- renderUI ({
    
  choices <- game_logs %>%
    filter(Season == input$season_filter) %>%
    mutate(Month = lubridate::month(Date)) %>%
    
    mutate(Month = ifelse(Month == 1, 'January',
                   ifelse(Month == 2, 'February',
                   ifelse(Month == 3, 'March',
                   ifelse(Month == 4, 'April',
                   ifelse(Month == 5, 'May',
                   ifelse(Month == 6, 'May',
                   ifelse(Month == 10, 'October',
                   ifelse(Month == 11, 'November', 'December'))))))))) %>% select(Month)
  
  pickerInput(
    inputId = "month_filter",
    label = p("Select Month(s):", style = "padding-right:100px"), 
    choices = choices$Month %>% unique(),
    options = list(`actions-box` = TRUE,  `selected-text-format` = "count > 3"),
    selected = "March",
    multiple = T)
  
  })
  
  # table
  output$Table <- DT::renderDataTable({
    
    weekLoad <- game_logs %>%
      filter(Season == input$season_filter) %>%
      filter(Team == input$team_filter) %>%
      mutate(Month = lubridate::month(Date), Week = lubridate::week(Date)) %>%
      
      mutate(Month = ifelse(Month == 1, 'January',
                     ifelse(Month == 2, 'February',
                     ifelse(Month == 3, 'March',
                     ifelse(Month == 4, 'April',
                     ifelse(Month == 5, 'May',
                     ifelse(Month == 10, 'October',
                     ifelse(Month == 11, 'November', 'December')))))))) %>% 
      
      filter(Month %in% input$month_filter) %>%
      group_by(Team, Player) %>%
      mutate(Date2 = lag(Date)) %>%
      mutate(dates = Date - Date2) %>% 
      mutate(B2B = ifelse(is.na(dates), 0, ifelse(dates == 1, 1, 0))) %>% 
      summarise(Games = n(), 
                totalMins = sum(MINS),
                
                M.Trend = spk_chr(MINS, 
                                  type = "line", 
                                  lineColor = "deeppink", 
                                  fillColor = "transparent", 
                                  minSpotColor = "skyblue",
                                  maxSpotColor = "orangered",
                                  spotColor = F,
                                  chartRangeMinX = 1,
                                  chartRangeMinX = 60,
                                  normalRangeMin = mean(MINS)-(sd(MINS) * 0.5),
                                  normalRangeMax = mean(MINS)+(sd(MINS) * 0.5),
                                  chartRangeClip = T,
                                  tooltipChartTitle = "Minutes Played"),
                
                M.Dist =  spk_chr(MINS, 
                                  type = "box", 
                                  lineColor="deeppink", 
                                  boxFillColor = "pink",
                                  whiskerColor = "transparent",
                                  outlierLineColor = "white",
                                  outlierFillColor = "white",
                                  medianColor = "deeppink",
                                  minValue = 1,
                                  maxValue = 60,
                                  showOutliers = F,
                                  tooltipChartTitle = "Minutes Distribution"),
                
                
                totalLoad = sum(Load), 
                
                L.Trend = spk_chr(Load, 
                                  type = "line", 
                                  lineColor = "deeppink", 
                                  fillColor = "transparent", 
                                  minSpotColor = "skyblue",
                                  maxSpotColor = "orangered",
                                  spotColor = F,
                                  chartRangeMinX = 0,
                                  chartRangeMinX = 100,
                                  normalRangeMin = mean(MINS)-(sd(MINS) * 0.5),
                                  normalRangeMax = mean(MINS)+(sd(MINS) * 0.5),
                                  chartRangeClip = T,
                                  tooltipChartTitle = "Game Load"),
                
                L.Dist =  spk_chr(Load, 
                                  type = "box", 
                                  lineColor="deeppink", 
                                  boxFillColor = "pink",
                                  whiskerColor = "transparent",
                                  outlierLineColor = "white",
                                  outlierFillColor = "white",
                                  medianColor = "deeppink",
                                  minValue = 0,
                                  maxValue = 100,
                                  showOutliers = F,
                                  tooltipChartTitle = "Minutes Distribution"),
                
                B2B = sum(B2B), 
                
                Participation = mean(Participation)) %>%
      
      select(Team, Player, totalLoad, L.Trend, L.Dist, totalMins, M.Trend, M.Dist, Games, B2B) %>%
      
      ungroup()
    
    photo <- game_logs %>% distinct(Player, Photo)
    logs <- df %>% mutate(Team = val) %>% select(-val, -img)
    
    table <- full_join(weekLoad, photo, by = c("Player")) %>% 
      full_join(logs, by = c("Team")) %>%
      na.omit() %>%
      arrange(desc(totalLoad, totalMins, Games)) %>%
      select(-Team) %>%
      select(Photo, Player, totalLoad, L.Trend, L.Dist, totalMins, M.Trend, M.Dist, Games, B2B, Team = img2) %>%
      mutate(Photo = paste('<img src =',' "', Photo,'" ', 'height="45"></img>', sep = ""))
    
    table2 <- table %>% arrange(desc("totalLoad")) %>%
      
      formattable::formattable(
        list(
          totalLoad = color_tile("white", "indianred"),
          totalMins = color_tile("white", "indianred")
        )
      )
    
    #creates final table to be displayed in the app
    formattable::as.datatable(table2, 
                              rownames = FALSE,
                              escape = F,
                              class = 'white-space: nowrap',
                              extensions = 'Responsive',
                              colnames = c("Photo", "Player", "Total Load", "Load Trend", "Load Dist", "Total Mins", "Mins Trend", "Mins Dist","Games", "B2B","Team"),
                              options = list(dom = 'tp',
                                             pageLength = 100,
                                             bSort=F,
                                             drawCallback = htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}'),
                                             columnDefs = list(
                                               list(className = 'dt-center', targets = 0:9),
                                               list(width = '150px', targets = c(1)),
                                               list(width = '70px', targets = c(0)),
                                               list(width = '70px', targets = c(2,3,4,5,6,7)),
                                               list(width = '50px', targets = c(8,9)),
                                               list(width = '200px', targets = c(10))
                                               ),
                                             initComplete = htmlwidgets::JS(
                                                "function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': '#17202a', 'color': 'ivory'});",
                                                "}")
                                              )) %>%
    
      formatStyle('Team', backgroundColor =  'rgb(52,62,72)', color = "white") %>%
      formatStyle('Photo', backgroundColor =  'rgb(52,62,72)', color = "white") %>%
      formatStyle('Player', backgroundColor =  'rgb(52,62,72)', color = "white") %>%
      formatStyle('totalLoad', backgroundColor =  'rgb(52,62,72)', color = "darkslategrey") %>%
      formatStyle('L.Trend', backgroundColor =  'rgb(52,62,72)', color = "white") %>%
      formatStyle('L.Dist', backgroundColor =  'rgb(52,62,72)', color = "white") %>%
      formatStyle('totalMins', backgroundColor =  'rgb(52,62,72)', color = "darkslategrey") %>%
      formatStyle('M.Trend', backgroundColor =  'rgb(52,62,72)', color = "white") %>%
      formatStyle('M.Dist', backgroundColor =  'rgb(52,62,72)', color = "white") %>%
      formatStyle('Games', backgroundColor =  'rgb(52,62,72)', color = "white") %>%
      formatStyle('B2B', backgroundColor =  'rgb(52,62,72)', color = "white") %>%
    
      formatStyle(c(2, 5, 8, 10), `border-right` = "solid 1px", `border-color` = "darkgrey")
    
    
  })
  
}



# UI
ui <- fluidPage(
  
  #this code helps align the team logos in the picker input  and table to display them on the left of the text
  tags$head(tags$style(".jhr{ display: inline;vertical-align: middle;padding-left: 10px;}")),
  
  #bring in sparkline dependencies
  getDependency('sparkline'),
  
  br(),
  
  fluidRow(
    
    column(width = 2,
    pickerInput(
      inputId = "season_filter",
      label = p("Select Season:", style = "padding-right:100px"),
      choices = sche$Season %>% unique() %>% sort(), 
      selected = "2019-20", 
      choicesOpt = list(style = rep(("color: black; background: white"),30)),
      multiple = F)
    ),
    
    column(width = 2,
    pickerInput(
      inputId = "team_filter",
      label = p("Select Team", style = "padding-right:100px"), 
      choices = df$val,
      choicesOpt = list(style = rep(("color: black; background: white"),30), content = df$img),
      multiple = F)
    ),
    
    column(width = 2,
           uiOutput("month.filter")
           )
    
  ),
  
  fluidRow(column(width = 12, DT::dataTableOutput("Table"))),
  
  br()
  
)

shinyApp(ui = ui, server = server)