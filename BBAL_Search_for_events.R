##############################
#### Parameters to be set ####
##############################
source('C:/Users/Grant/Dropbox/GrantHumphriesBackup/Projects/Albatross/Codes/R/Search_Functions.R')
#ws<-"C:/Users/Grant/Dropbox/GrantHumphriesBackup/Projects/Albatross/BBAL/Grant_Files/"
ws<-"C:/Users/Grant/Dropbox/GrantHumphriesBackup/Projects/Albatross/BBAL/Grant_Files/Incubation/Stomach_Recorders"
setwd(ws)

B<-"STO_KER1_ID1003_F_B30_Nid19_21112013_PROBE_TEMPERATURE.csv"

Temp.data<-read.csv(B)
Temp.data<-Date.Clean(Temp.data)
Temp.data$Preymass<-"NA"


########################################
#### Initial plot for setting scale ####
########################################

plot(ts(Temp.data$Stomach.Temperature[1:15000]),xaxp=c(0,15000,10))



BaseTemp<-38.0    # Ambient temperature of stomach
m<-0.58           # (MEAN value of m from Wilson et al 1995)
SHC<-3.9           # Specific heat constant estimated from Ropert-Couder and Kato 2006
Tf<-6              # Approx temperature of prey items
sensitivity<-0.05  # Sensitivity of percent change in feeding events

Windowsize<-5
MovingWindow<-seq(Windowsize,nrow(Temp.data),Windowsize)
  

#####################
#### Run Scripts ####
#####################

FirstEvent<-Event.Finder(1,Temp.data,Windowsize,MovingWindow,BaseTemp,sensitivity)  
st<-as.numeric(FirstEvent[1])
End<-as.numeric(FirstEvent[2])  

Run.Script(st,End,Temp.data)




      
