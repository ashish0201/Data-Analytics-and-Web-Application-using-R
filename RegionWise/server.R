library(shiny)
library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};server=192.168.1.106,1434;database=KheloIndiaSchool;uid=scott;pwd=tiger')
#board1<-sqlQuery(dbhandle, 'select BoardID,BoardName from BoardMaster')

temp<-sqlQuery(dbhandle, 'select SchoolID,SchoolName,BoardID,SchoolChainID,ZoneID,RegionID,StateID,CityID from SchoolMaster')

shinyServer(
  
  function(input,output){
    
    
    
    output$zone <-renderUI({
      selectInput("zone","select zone",hola())
    })
    
    output$PLOT<-renderPlotly({
      
      
     
      
      
      x1<-input$board
      x2<-input$zone
      x3<-input$ageStart
      x4<-input$ageEnd
      x5<-input$term
      print(x3)
      if(x4!=5){
      TopPerformanceGraph1(x1,x2,x3,x4)}
      
      
    })
   

    
  }
)