   TopPerformanceGraph<-function(boardName="All",zoneName="All",regionName="All",stateName="All",cityName="All",schoolName="All",Age.start='All',Age.end='All',termName="All")
        {
          library(RODBC)
          dbhandle <- odbcDriverConnect('driver={SQL Server};server=192.168.1.106,1434;database=KheloIndiaSchool;uid=scott;pwd=tiger')
         # print("Age.start")
         
          Age.start<-as.numeric(Age.start)
          Age.end<-as.numeric(Age.end)
         # print(is.numeric(Age.start))
          if(termName!="All")
          {
            mo<-as.numeric(termName)
          }
          if(boardName!="All")
          {
            board1<-sqlQuery(dbhandle, 'select BoardID,BoardName from BoardMaster')
            board1<-board1[board1$BoardName==boardName,]
            temp<-board1$BoardID
            board<-sqlQuery(dbhandle, 'select SchoolID,SchoolName,BoardID,SchoolChainID,ZoneID,RegionID,StateID,CityID from SchoolMaster')
            board<-board[board$BoardID==temp,]
            board<-merge(board, board1, by.x = "BoardID",by.y="BoardID")
            
          }
          else
          {
            board<-sqlQuery(dbhandle, 'select SchoolID,SchoolName,BoardID,SchoolChainID,ZoneID,RegionID,StateID,CityID from SchoolMaster') 
            board1<-sqlQuery(dbhandle, 'select BoardID,BoardName from BoardMaster')
            board<-merge(board, board1, by = "BoardID")
          }
          
          if(zoneName!="All")
          {
            
            zone1<-sqlQuery(dbhandle, 'select ZoneID,ZoneName from ZoneMaster')
            zone1<-zone1[zone1$ZoneName==zoneName,]
            temp<-zone1$ZoneID
            zone<-board[board$ZoneID==temp,]
            zone<-merge(zone, zone1, by = "ZoneID")
           
            
          }
          else
          {
            zone1<-sqlQuery(dbhandle, 'select ZoneID,ZoneName from ZoneMaster')
            zone<-board[,]
            zone<-merge(zone, zone1, by = "ZoneID")
           
          }
          
          if(regionName!="All")
          {
            region<-sqlQuery(dbhandle, 'select RegionID,RegionName from RegionMaster')
            region<-region[region$RegionName==regionName,]
           # print(zone)
            #print(region)
            #print("----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
            region<-merge(zone, region, by = "RegionID")
           # print(region)
            
          }
          else
          {
            region1<-sqlQuery(dbhandle, 'select RegionID,RegionName from RegionMaster')
            region<-zone[,]
            zone<-merge(region, region1, by = "RegionID")
            
          }
          
          if(stateName!="All")
          {
            state<-sqlQuery(dbhandle, 'select StateID,StateName from StateMaster')
            state<-state[state$StateName==stateName,]
            state<-merge(region, state, by = "StateID")
          }
          else
          {
            state1<-sqlQuery(dbhandle, 'select StateID,StateName from StateMaster')
            state<-region[,]
            state<-merge(state, state1, by = "StateID")
            
          }
          #print(state)
          if(cityName!="All")
          {
            city<-sqlQuery(dbhandle, 'select CityID,CityName from CityMaster')
            city<-city[city$CityName==cityName,]
            city<-merge(state, city, by = "CityID")
          }
          else
          {
            city1<-sqlQuery(dbhandle, 'select CityID,CityName from CityMaster')
            city<-state[,]
            print(head(city))
            print(head(city1))
            city<-merge(city1, city, by = "CityID")
            
          }
          #print(city)
          if(schoolName!="All")
          {
            school<-city[city$SchoolName==schoolName,]
          }
          else
          {
            school<-city[,]
          }
         # print(summary(school))
          vec3<-(unique(school$RegionName))
         # print(school)
          #print(length(vec3))
          #print("hahahha")
         # print(school)
          student<-sqlQuery(dbhandle, 'select StudentID,SchoolID,Gender from StudentMaster')
          
          merge1<-merge(school, student, by = "SchoolID")
         # print(merge1)
          results<-sqlQuery(dbhandle, 'select StudentID,Percentile,MONTH(CreatedOn) as Month,Age,TestTypeID from SeniorTestResults')
          #results<-results[results$TestTypeID==59 | results$TestTypeID ==60 | results$TestTypeID==8 | results$TestTypeID==19 | results$TestTypeID==54 |  results$TestTypeID==55 | results$TestTypeID==57,]
          if(termName!="All")
          {
            if(as.numeric(termName)==1){
              results<-results[results$month>=3 & results$month<=8,]}
            else{
              results<-results[results$month<3 & results$month>8,]}
          
        }
          print(results)
          merge2<-merge(merge1,results,by="StudentID")
         # print(merge2)
          conclude1<-merge2[merge2$Gender==0,]
          conclude2<-merge2[merge2$Gender==1,]
         # print(conclude2)
          allregion <-(unique(merge2$RegionID))
          
          conclude3<-aggregate(Percentile ~ StudentID, conclude1, sum)
          conclude4<-aggregate(Percentile ~ StudentID, conclude2, sum)
         # print((conclude3))
          print((conclude4))
          
          conclude5<-merge(x = conclude3, y = conclude1, by = "StudentID")
          conclude6<-merge(x = conclude4, y = conclude2, by = "StudentID")
          
          #print((conclude5))
          #print(nrow(conclude6))
          
          conclude7<-unique(conclude5[c("StudentID", "Percentile.x","RegionID","Gender","Age")])
          conclude8<-unique(conclude6[c("StudentID", "Percentile.x","RegionID","Gender","Age")])
          
          
          index <- conclude7$Age >= 5 & conclude7$Age<=8
          conclude7$Percentile.x[index] <- (conclude7$Percentile.x[index])/2
          
          index <- conclude7$Age >= 8
          conclude7$Percentile.x[index] <- (conclude7$Percentile.x[index])/5
          
          
          index <- conclude8$Age >= 5 & conclude8$Age<=8
          conclude8$Percentile.x[index] <- (conclude8$Percentile.x[index])/2
          
          index <- conclude8$Age >= 8
          conclude8$Percentile.x[index] <- (conclude8$Percentile.x[index])/5
        # print(head(conclude7))
         #print(head(conclude8))
          start=5
          end=20
          if(Age.start!='All' || Age.end != 'All')
          {
            if(Age.start!='All')
            {
              conclude7<-conclude7[conclude7$Age>=Age.start,]
              conclude8<-conclude8[conclude8$Age>=Age.start,]
              start=Age.start
            }
            if(Age.end!='All')
            {
              conclude7<-conclude7[conclude7$Age<=Age.end,]
              conclude8<-conclude8[conclude8$Age<=Age.end,]
              end=Age.end
            }
          }
        
          ans1<-(aggregate(conclude7[, 2], list(conclude7$Age), mean))
          ans2<-(aggregate(conclude8[, 2], list(conclude8$Age), mean))
         # print(ans1)
          #print(ans2)
          vec1<-c()
          vec2<-c()
          vec3<-c()
          for(i in start:end)
          {
            x<-(ans1[ ans1$Group.1==i,]$x)
            if(length(x)==0){
              vec1<-c(vec1,0)}
            else{
              vec1<-c(vec1,as.numeric(x))}
            
            x<-(ans2[ ans2$Group.1==i,]$x)
            if(length(x)==0){
              vec2<-c(vec2,0)}
            else{
              vec2<-c(vec2,as.numeric(x))}
              vec3<-c(vec3,i)
          }
          
          #print(vec1)
          #print(vec2)
          boys<-vec1
          girls<-vec2
          test <- rbind(vec3,boys,girls)
          library(MASS)
          library(reshape2)
          d1 <- as.data.frame(t(test), stringsAsFactors=FALSE)
          
          library(ggplot2)
          d2 <- melt(d1, id = c("vec3"))
        
          library(plotly)
          p<-ggplot(d2, aes(factor(vec3),value)) +   
            geom_bar(aes(fill = variable), position = "dodge", stat="identity")+coord_cartesian(ylim = c(0,100)) +xlab("Age")+ylab("Average percentile")+ggtitle("Average Percentile Of Boys And Girls Agewise")
          p<-ggplotly(p)
          p
          
        }