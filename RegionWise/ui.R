library(shiny)
library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};server=192.168.1.106,1434;database=KheloIndiaSchool;uid=scott;pwd=tiger')
board1<-sqlQuery(dbhandle, 'select BoardName from BoardMaster')
zone1<-sqlQuery(dbhandle, 'select ZoneName from ZoneMaster')
chain1<-sqlQuery(dbhandle, 'select SchoolChainName from SchoolChainMaster')

chain2<-c()
chain2<-c(chain2,"All")
chain2<-c(chain2,unique(chain1))

board2<-c()
board2<-c(board2,"All")
board2<-c(board2,unique(board1))
zone1<-unique(zone1)

zone2<-c()
zone2<-c(zone2,"All")
zone2<-c(zone2,unique(zone1))

shinyUI(fluidPage(
  
  titlePanel("Region Wise"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("board1","Select Board/chain",selected="All",c('board','chain')),
      
      conditionalPanel(
        condition = "input.board1 == 'board'",
        selectInput("board", "Select Board",board2)
        
      ),
      conditionalPanel(
        condition = "input.board1 == 'chain'",
        selectInput("board", "Select Chain",
                    chain2)
      ),
      selectInput("zone","Select Zone",selected="All",zone2),
      selectInput("ageStart","Select Starting Age",selected=5,5:18),
      selectInput("ageEnd","Select Ending Age",selected=5,5:18),
      selectInput("term","Select Term",selected="All",c("All",1,2))
    
    
    ),
    mainPanel(
      plotlyOutput("PLOT")
    )
  ) 
    
    
  
  
))