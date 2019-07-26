library(shiny)
library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};server=192.168.1.106,1434;database=KheloIndiaSchool;uid=scott;pwd=tiger')
board1<-sqlQuery(dbhandle, 'select BoardName from BoardMaster')
zone1<-sqlQuery(dbhandle, 'select ZoneName from ZoneMaster')
region1<-sqlQuery(dbhandle, 'select RegionName from RegionMaster')
state1<-sqlQuery(dbhandle, 'select StateName from StateMaster')
city1<-sqlQuery(dbhandle, 'select CityName from CityMaster')
school1<-sqlQuery(dbhandle, 'select SchoolName from StudentMaster')
chain1<-sqlQuery(dbhandle, 'select SchoolChainName from SchoolChainMaster')
temp<-sqlQuery(dbhandle, 'select SchoolID,SchoolName,BoardID,SchoolChainID,ZoneID,RegionID,StateID,CityID from SchoolMaster')

board2<-c()
board2<-c(board2,"All")
board2<-c(board2,unique(board1))

chain2<-c()
chain2<-c(chain2,"All")
chain2<-c(chain2,unique(chain1))

zone2<-c()
zone2<-c(zone2,"All")
zone2<-c(zone2,unique(zone1))


region2<-c()
region2<-c(region2,"All")
region2<-c(region2,unique(region1))

state2<-c()
state2<-c(state2,"All")
state2<-c(state2,unique(state1))

city2<-c()
city2<-c(city2,"All")
city2<-c(city2,unique(city1))

school2<-c()
school2<-c(school2,"All")
school2<-c(school2,unique(school1))

shinyUI(fluidPage(
  
  titlePanel("Age Wise"),
  
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
      uiOutput("vs"),
      
      uiOutput("vsa"),
      uiOutput("vsb"),
      uiOutput("vsc"),
     
      selectInput("ageStart","Select Starting Age",selected=5,5:18),
      selectInput("ageEnd","Select Ending Age",selected=5,5:18),
      selectInput("term","Select Term",selected="All",c("All",1,2))
      
      
      
    ),
    mainPanel(
      plotlyOutput("PLOT")
    )
  ) 
  
  
  
  
))