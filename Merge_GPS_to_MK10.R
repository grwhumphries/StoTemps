# This code will be used to pull GPS files and then add feeding events
## SAMPLE CHANGE ##


library(sp)
library(rgdal)

WGS84<-" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
SHPname<-substr(A,start=1,stop=nchar(A)-4)
ws<-"C:/Users/Grant/Dropbox/GrantHumphriesBackup/Projects/Albatross/BBAL/Grant_Files/Merged_files"
setwd(ws)

#Alphas<-c("H_","I_","J_","K_","L_","M_","N_","O_","P_")
#Alphas<-c("A_","B_","C_","D_","E_","F_","G_")

Alphas<-c("B_")

ldir<-dir()


for(j in Alphas){
  for(k in ldir){
    GPSname<-paste(j,"GPS",sep="")
    MK10name<-paste(j,"MK10",sep="")
    #STOname<-paste(j,"STO",sep="")
    if(GPSname == substr(k,1,5)){
      A<-k
    }
    #if(STOname == substr(k,1,5)){
      #B<-k
    #}
    if(MK10name == substr(k,1,6)){
      B<-k
    }
  }
  message<-paste("performing task with",A,"and",B,sep=" ")
  print(message)
  SHPname<-substr(A,start=1,stop=nchar(A)-4)
  
  setwd(ws)
  GPS.data<-read.csv(A)
  Temp.data<-read.csv(B)
  
  TD.st<-data.frame(Temp.data$Date,Temp.data$Time,Temp.data$Stomach.Temperature)
  Tframe<-strptime(paste(Temp.data$Date,Temp.data$Time,sep=" "),"%d-%m-%Y %H:%M:%S")
  #Tframe<-strptime(paste(Temp.data$Date,Temp.data$Time,sep=" "),"%m-%d-%Y %H:%M:%S")
  GPS.time<-strptime(paste(GPS.data$Date,GPS.data$Time,sep=" "),"%d-%m-%Y %H:%M:%S")
  
  
  for(i in 1:length(GPS.time)){
    n<-which(abs(Tframe - GPS.time[i])==min(abs(Tframe - GPS.time[i])))
    m<-paste(as.character(Temp.data[n[1],4]),as.character(Temp.data[n[1],2]),GPS.time[i],sep="//")
    GPS.data$StomachTemp[i]<-as.character(Temp.data[n[1],4])
    GPS.data$STtime[i]<-as.character(Temp.data[n[1],2])
    GPS.data$PreyMass[i]<-as.character(Temp.data[n[1],7])
    print(i)
  }
  
  
  coords = cbind(GPS.data$Longitude,GPS.data$Latitude)
  sp = SpatialPoints(coords, proj4string=CRS(WGS84))
  spdf = SpatialPointsDataFrame(coords,GPS.data)
  spdf = SpatialPointsDataFrame(sp, GPS.data)
  
  
  writeOGR(spdf, ".", SHPname, driver="ESRI Shapefile")
}




#A<-"GPS_GAN13_D16_60col2-142912.csv"
#B<-"MK10_13A0459_D16_60col2_avecGAN13_PreyMass.csv"


WGS84<-" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
SHPname<-substr(A,start=1,stop=nchar(A)-4)

setwd(ws)
GPS.data<-read.csv(A)
Temp.data<-read.csv(B)

TD.st<-data.frame(Temp.data$Date,Temp.data$Time,Temp.data$Stomach.Temperature)
Tframe<-strptime(paste(Temp.data$Date,Temp.data$Time,sep=" "),"%d-%m-%Y %H:%M:%S")

GPS.time<-strptime(paste(GPS.data$Date,GPS.data$Time,sep=" "),"%d-%m-%Y %H:%M:%S")


for(i in 1:length(GPS.time)){
  n<-which(abs(Tframe - GPS.time[i])==min(abs(Tframe - GPS.time[i])))
  m<-paste(as.character(Temp.data[n[1],4]),as.character(Temp.data[n[1],2]),GPS.time[i],sep="//")
  GPS.data$StomachTemp[i]<-as.character(Temp.data[n[1],4])
  GPS.data$STtime[i]<-as.character(Temp.data[n[1],2])
  GPS.data$PreyMass[i]<-as.character(Temp.data[n[1],7])
  print(i)
}


coords = cbind(GPS.data$Longitude,GPS.data$Latitude)
sp = SpatialPoints(coords, proj4string=CRS(WGS84))
spdf = SpatialPointsDataFrame(coords,GPS.data)
spdf = SpatialPointsDataFrame(sp, GPS.data)


writeOGR(spdf, ".", SHPname, driver="ESRI Shapefile")






