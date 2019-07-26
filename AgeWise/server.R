library(shiny)
library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};server=192.168.1.106,1434;database=KheloIndiaSchool;uid=scott;pwd=tiger')
#board1<-sqlQuery(dbhandle, 'select BoardID,BoardName from BoardMaster')
temp<-sqlQuery(dbhandle, 'select SchoolID,SchoolName,BoardID,SchoolChainID,ZoneID,RegionID,StateID,CityID from SchoolMaster')
region1<-sqlQuery(dbhandle, 'select RegionID,RegionName from RegionMaster')
zone1<-sqlQuery(dbhandle, 'select ZoneID,ZoneName from ZoneMaster')
state1<-sqlQuery(dbhandle, 'select StateID,StateName from StateMaster')
city1<-sqlQuery(dbhandle, 'select CityID,CityName from CityMaster')

shinyServer(
  
 
  
  function(input,output){

    vsta<- reactive({
      x2<<-input$zone
      hola(x2)
      
    })
    output$vs <-renderUI({
      
      selectInput("region","SelectRegion",selected="All",vsta())
      
    })
    
    
    
    vst<- reactive({
      {
        x3<<-input$region
        temp1<-temp1[temp1$RegionName==x3,]
        # print(head(temp1))
        temp1<-merge(temp1,state1,by='StateID')
        #print("ifiohoigj;oerjg;oerjgoerjogjerohjoerjhoerjhpejhpo")

       
        temp2<-as.character( (unique(temp1$StateName)))
        ste<-c()
        ste<-c(ste,"All")
        ste<-c(ste,temp2)
      }
      ste
    })
    output$vsa <-renderUI({
      selectInput('pi',"SelectState",selected="All",vst())
    })



    vstb<- reactive({
      
      {
        x4<<-input$pi
        print("sdfsef")
        print(x4)
        temp1<-merge(temp1,state1,by='StateID')
        temp1<-temp1[temp1$StateName==x4,]
      #print(head(temp1))
      temp1<-merge(temp1,city1,by='CityID')

      print(head(temp1$CityName))

      temp2<-as.character( (unique(temp1$CityName)))
      ste<-c()
      ste<-c(ste,"All")
      ste<-c(ste,temp2)}
      ste
    })
    output$vsb <-renderUI({
      selectInput("city","SelecthCity",selected="All",vstb())
    })
    
    
    
    
    
    
    
    vstc<- reactive({
      
      {
        x5<<-input$city
        print("sdfsef")
        print(x5)
        temp1<-merge(temp1,state1,by='StateID')
        temp1<-merge(temp1,city1,by='CityID')
        temp1<-temp1[temp1$CityName==x5,]
        #print(head(temp1))
       
        
        print(head(temp1$SchoolName))
        
       
        temp2<-as.character( (unique(temp1$SchoolName)))
        ste<-c()
        ste<-c(ste,"All")
        ste<-c(ste,temp2)}
      ste
    })
    output$vsc <-renderUI({
      selectInput("school","SelectSchool",selected="All",vstc())
    })

    

    output$PLOT<-renderPlotly({
     
    
      



      


    

        

    

     

      # x2<-input$zone
      # temp1<-temp1[temp1$ZoneName==x2,]
      # temp1<-merge(temp1,region1,by='RegionID')
      # p<-c("All", unique(temp1$RegionName))
      # 
      # output$myregion<-renderDataTable(
      #   p
      #   
      # )
      # print("fsfsdfsdsf")
      # print(p)
      # 
      
     # output$haha<-renderText(input$zone)
      x1<-input$board
      x6<-input$school
      x7<-input$ageStart
      x8<-input$ageEnd
      x9<-input$term
      x4<-input$pi
      x5<-input$city
      x3<-input$region
      x2<-input$zone
      print(x3)
      if(x8!=5){
        TopPerformanceGraph(x1,x2,x3,x4,x5,x6,x7,x8)}
      
      
    })
    
    
    
  }
)