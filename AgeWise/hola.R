hola<-function(zone_name)
{

 temp1<<-merge(temp,zone1,by='ZoneID')
  print("uiuhoijjikljm;ok")
  print(zone_name)
  
  temp1<<-temp1[temp1$ZoneName==zone_name,]
  print(head(temp1))
  temp1<<-merge(temp1,region1,by='RegionID')
  
  #print(head(temp1))
  
  temp2<-as.character( (unique(temp1$RegionName)))
  ste<-c()
  ste<-c(ste,"All")
  ste<-c(ste,temp2)
  return(ste)
}
z
bola<-function(region_name)
{
  
  
 
  print("fiuhsdhfisdlhsd")
  return(stea)
}