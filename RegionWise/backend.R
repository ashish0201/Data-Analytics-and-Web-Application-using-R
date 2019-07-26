   TopPerformanceGraph1<-function(boardName="All",zoneName="All",regionName="All",stateName="All",cityName="All",schoolName="All",termName="All")
      {
        library(RODBC)
        dbhandle <- odbcDriverConnect('driver={SQL Server};server=192.168.1.106,1434;database=KheloIndiaSchool;uid=scott;pwd=tiger')
        
        
        if(boardName!="All")
        {
          board<-sqlQuery(dbhandle, 'select SchoolID,SchoolName,BoardID,SchoolChainID,ZoneID,RegionID,StateID,CityID from SchoolMaster where BoardID=boardName')
        }
        else
        {
          board<-sqlQuery(dbhandle, 'select SchoolID,SchoolName,BoardID,SchoolChainID,ZoneID,RegionID,StateID,CityID from SchoolMaster') 
        }
        
        if(zoneName!="All")
        {
          zone<-board[board$ZoneID==zoneName,]
        }
        else
        {
          zone<-board[,]
        }
        
        if(regionName!="All")
        {
          region<-zone[zone$RegionID==regionName,]
        }
        else
        {
          region<-zone[,]
        }
        
        if(stateName!="All")
        {
          state<-region[region$StateID==stateName,]
        }
        else
        {
          state<-region[,]
        }
        
        if(cityName!="All")
        {
          city<-state[state$CityID==cityName,]
        }
        else
        {
          city<-state[,]
        }
        
        if(schoolName!="All")
        {
          school<-city[city$SchoolID==schoolName,]
        }
        else
        {
          school<-city[,]
        }
       
        
         
      }
    
---------------------------------------------------------------------------------------------------------------------------------p
      
      TopPerformanceGraph1<-function(boardName="All",zoneName="All",Age.start="All",Age.end="All",schoolName="All",termName="All")
      {
        library(RODBC)
        dbhandle <- odbcDriverConnect('driver={SQL Server};server=192.168.1.106,1434;database=KheloIndiaSchool;uid=scott;pwd=tiger')
        
        print("Age.start")
        print(Age.start)
        print(is.numeric(Age.start))
        Age.start<-as.numeric(Age.start)
        Age.end<-as.numeric(Age.end)
        print(is.numeric(Age.start))
        mo<-0
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
          
          zone1<-sqlQuery(dbhandle, 'select ZoneID,ZoneName from ZoneMaster where ZoneName=zoneName')
          zone1<-zone1[zone1$ZoneName==zoneName,]
          temp<-zone1$ZoneID
          zone<-board[board$ZoneID==temp,]
          zone<-merge(zone, zone1, by = "ZoneID")
          region<-sqlQuery(dbhandle, 'select RegionID,RegionName from RegionMaster')
          zone<-merge(zone, region, by = "RegionID")
          
        }
        else
        {
          zone1<-sqlQuery(dbhandle, 'select ZoneID,ZoneName from ZoneMaster')
          zone<-board[,]
          zone<-merge(zone, zone1, by = "ZoneID")
          region<-sqlQuery(dbhandle, 'select RegionID,RegionName from RegionMaster')
          zone<-merge(zone, region, by = "RegionID")
        }
        
        
        if(schoolName!="All")
        {
          school<-zone[zone$SchoolID==schoolName,]
        }
        else
        {
          school<-zone[,]
        }
        print(summary(school))
        vec3<-(unique(school$RegionName))
        print(length(vec3))
        print("hahahha")
        student<-sqlQuery(dbhandle, 'select StudentID,SchoolID,Gender from StudentMaster')
        
        merge1<-merge(school, student, by = "SchoolID")
        results<-sqlQuery(dbhandle, 'select StudentID,Percentile,MONTH(CreatedOn) as month,Age,TestTypeID from SeniorTestResults')
        if(termName!="All")
        {
           if(as.numeric(termName)==1){
           results<-results[results$month>=3 & results$month<=8,]}
          else{
            results<-results[results$month<3 & results$month>8,]}
           
        }
       # results<-results[results$TestTypeID==59 | results$TestTypeID ==60 | results$TestTypeID==8 | results$TestTypeID==19 | results$TestTypeID==54 |  results$TestTypeID==55 | results$TestTypeID==57,]
        merge2<-merge(merge1,results,by="StudentID")
        
        conclude1<-merge2[merge2$Gender==0,]
        conclude2<-merge2[merge2$Gender==1,]
        
        allregion <-(unique(merge2$RegionID))
        
        conclude3<-aggregate(Percentile ~ StudentID, conclude1, sum)
        conclude4<-aggregate(Percentile ~ StudentID, conclude2, sum)
        print(nrow(conclude3))
        print(nrow(conclude4))
        
        conclude5<-merge(x = conclude3, y = conclude1, by = "StudentID")
        conclude6<-merge(x = conclude4, y = conclude2, by = "StudentID")
        
        print(nrow(conclude5))
        print(nrow(conclude6))
        
        conclude7<-unique(conclude5[c("StudentID", "Percentile.x","RegionID","Gender","Age","RegionName")])
        conclude8<-unique(conclude6[c("StudentID", "Percentile.x","RegionID","Gender","Age","RegionName")])
        
        if(Age.start!='All' || Age.end != 'All')
        {
           if(Age.start!='All')
           {
             conclude7<-conclude7[conclude7$Age>=Age.start,]
             conclude8<-conclude8[conclude8$Age>=Age.start,]
           }
           if(Age.end!='All')
           {
             conclude7<-conclude7[conclude7$Age<=Age.end,]
             conclude8<-conclude8[conclude8$Age<=Age.end,]
           }
        }
       
        
        index <- conclude7$Age >= 5 & conclude7$Age<=8
        conclude7$Percentile.x[index] <- (conclude7$Percentile.x[index])/2
        
        index <- conclude7$Age >= 8
        conclude7$Percentile.x[index] <- (conclude7$Percentile.x[index])/5
        
        
        index <- conclude8$Age >= 5 & conclude8$Age<=8
        conclude8$Percentile.x[index] <- (conclude8$Percentile.x[index])/2
        
        index <- conclude8$Age >= 8
        conclude8$Percentile.x[index] <- (conclude8$Percentile.x[index])/5
        
        ans1<-(aggregate(conclude7[, 2], list(conclude7$RegionName), mean))
        ans2<-(aggregate(conclude8[, 2], list(conclude8$RegionName), mean))
        print(ans1)
        print(ans2)
        vec1<-c()
        vec2<-c()
      
        for(i in vec3)
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
          
        }
        
        print(vec1)
        print(vec2)
        test <- rbind(vec1,vec2)
        
        
        boys<-vec1
        girls<-vec2
        test <- rbind(boys,girls)
        library(MASS)
        library(reshape2)
        d1 <- as.data.frame(t(test), stringsAsFactors=FALSE)
        print(vec3)
        vec3<-as.data.frame(vec3)
        d1<- cbind(d1, vec3)
        library(ggplot2)
        d2 <- melt(d1, id = c("vec3"))
        library(plotly)
       p<- ggplot(d2, aes((vec3),value)) +   
          geom_bar(aes(fill = variable), position = "dodge", stat="identity")+coord_cartesian(ylim = c(0,100))+theme(axis.text.x = element_text(angle = 90))+xlab("Region")+ylab("Average percentile")+ggtitle("Average Percentile Of Boys And Girls RegionWise")
       p<-ggplotly(p)
       p 
      
       }
