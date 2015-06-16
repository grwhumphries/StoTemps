#############################
#### Clean date function ####
#############################

Date.Clean<-function(Temp.data){
  DecSub<-gsub("déc","12", Temp.data$Date)
  JanSub<-gsub("janv","01",DecSub)
  NovSub<-gsub("Nov-13","11-2013",JanSub)
  DecSub2<-gsub("déc-13","12-2013",NovSub)
  novSub<-gsub("nov-13","11-2013",DecSub2)
  Temp.data$Date<-novSub
  Temp.data<-subset(Temp.data,Date != "")
  return(Temp.data)
}


############################
#### Prey Mass Function ####
############################

Get.PreyMass<-function(start,end,Temp.data, BaseTemp, m, SHC, Tf){
  
  total<-end-start
  sampling_rate<-1000
  
  
  Tab<-Temp.data[start:end,]
  Time<-strptime(Tab$Time, "%H:%M:%S")          #c(1:total)
  Time<-(unclass(as.POSIXct(Time)) - unclass(as.POSIXct(Time[1]))[1])
  Data<-Tab$Stomach.Temperature
  
  
  divs = (max(Time) - min(Time)) * 1000
  data_fun <- approxfun(Time, Data, method="constant", 0, 0)
  result <- integrate(data_fun, min(Time),max(Time), subdivisions=divs)
  
  Tab$base<-BaseTemp
  base_fun <- approxfun(Time, Tab$base, method="linear", 0, 0)
  result2 <- integrate(base_fun, min(Time), max(Time), subdivisions = divs)
  
  
  INT<-result2$value - result$value
  
  PreyMass = INT/(m*SHC*(BaseTemp-Tf))
  
  message<-paste("Mass of ingested prey estimated at", PreyMass,"grams",sep=" ")
  
  Temp.data$Preymass[start]<-PreyMass
  
  print(message)
  
  return(Temp.data)
  
}

##################################
#### Calculate percent change ####
##################################

pcchange=function(x1,x2) (x2-x1)/x1

##########################################
#### Locate stomach temperature event ####
##########################################

Event.Finder<-function(Start,Temp.data,Windowsize,MovingWindow,BaseTemp,sensitivity){
  
  st<-Start
  for(win in MovingWindow){
    
    Dat<-Temp.data[st:win,]
    
    x1<-Dat$Stomach.Temperature[1]
    x2<-Dat$Stomach.Temperature[Windowsize]
    
    PCHNG<-pcchange(x1,x2)
    
    
    if(PCHNG < -(sensitivity)){
      
      for(i in c(win:nrow(Temp.data))){
        IFind<-BaseTemp - Temp.data$Stomach.Temperature[i]
        
        if(IFind == 0){
          End = i
          break
        }
                  
      }
      if(exists("End")==F){
        stop("error, please change base temp")
      }
      output<-list(st,End)
      return(output)
      break
    }  
    st=win+1
  }  
}


######################################
#### Main Script running controls ####
######################################

Run.Script<-function(st,End,Temp.data){
  
  EndOfFeedings<-Scale()
  
  
  Supercontrol=T
  while(Supercontrol==T){
    plot(ts(Temp.data$Stomach.Temperature[1:EndOfFeedings]))
  
    abline(v=st)
    abline(v=End)
    
    stlab<-paste("Left, ",st,sep="")
    endlab<-paste("Right, ",End,sep="")
    
    text(EndOfFeedings-500,min(Temp.data$Stomach.Temperature),stlab)
    text(EndOfFeedings-500,min(Temp.data$Stomach.Temperature)+10,endlab)
    
    control=T
    while(control==T){
      Bounds<-readline('Are the boundaries for this event correct?(y or n): ')
      print("---------------------------------------------------------------")
      
      if(Bounds=="n"){
        Chng<-readline('What are the new boundaries? (ex. L,2500): ')
        bnd<-strsplit(Chng,",")[[1]][1]    
        dist<-as.numeric(strsplit(Chng,",")[[1]][2])
        
        if(bnd=="L"){
          st<-dist        
        }
        if(bnd=="R"){
          End<-dist
        } 
        plot(ts(Temp.data$Stomach.Temperature[1:EndOfFeedings]))
        abline(v=st)
        abline(v=End)
        stlab<-paste("Left, ",st,sep="")
        endlab<-paste("Right, ",End,sep="")
        text(EndOfFeedings-500,min(Temp.data$Stomach.Temperature),stlab)
        text(EndOfFeedings-500,min(Temp.data$Stomach.Temperature)+10,endlab)
      }
      if(Bounds=="y"){
        control=F
      }
      if(Bounds!="y" & Bounds!="n"){
        print("Sorry, require 'y' or 'n'")
      }
    }
    Analyze<-readline('Would you like to analyze this event? (y or n): ')
    control=T
    while(control==T){
      if(Analyze=="y"){
        print("Analyzing...")
        
        Temp.data<-Get.PreyMass(st,End,Temp.data,BaseTemp,m,SHC,Tf)
        
        
        control=F
      }
      if(Analyze=="n"){
        print("Okay, skipping...")
        control=F
      }
      if(Analyze!="y" & Analyze!="n"){
        print("Sorry, require 'y' or 'n'")
        
      }    
    }
    
    continue<-readline("Would you like to continue? (y or n): ")
    
    control=T
    while(control==T){
      if(continue=="y"){
        print("Continuing...")
        control=F
      }
      if(continue=="n"){
        
        secondarycontrol=T
        while(secondarycontrol==T){
          
          out<-readline("Would you like to write out the CSV? (y or n): ")
          if(out=="y"){
            print("writing CSV...")
            filename<-paste(as.character(strsplit(B,".csv")),"_PreyMass.csv",sep="")
            write.csv(Temp.data,filename,row.names=F)
            secondarycontrol=F
          }
          if(out=="n"){
            print("Okay, skipping...")
            secondarycontrol=F
          }
          if(out!="y" & out!="n"){
            print("Sorry, require 'y' or 'n'")
          }   
          
        }
        
        print("-------------------------------------")
        print("Program completed.. thanks")
        control=F
        Supercontrol=F
      }
      if(continue!="y" & continue!="n"){
        print("Sorry, require 'y' or 'n'")
      }    
    }
    
    
    newWindow<-seq(End+Windowsize,nrow(Temp.data),Windowsize)
    Event<-Event.Finder(End,Temp.data,Windowsize,newWindow,BaseTemp,sensitivity)  
    st<-as.numeric(Event[1])
    End<-as.numeric(Event[2])
    
    if(identical(st,numeric(0))){
      print("There are no more detectable events, starting from the beginning...")
      FirstEvent<-Event.Finder(1,Temp.data,Windowsize,MovingWindow,BaseTemp,sensitivity)  
      st<-as.numeric(FirstEvent[1])
      End<-as.numeric(FirstEvent[2]) 
    }
    
  }
  
}

###################################
#### Set the scale of the plot ####
###################################

Scale<-function(){
  EndOfFeedings<-as.numeric(readline("At what row would you like to draw the cut off?: "))
  return(EndOfFeedings)
}
