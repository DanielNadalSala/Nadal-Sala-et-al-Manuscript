#To process the LDNDC simulated data in Hyytiala
#Daniel Nadal-Sala, Rüdiger Grote, Benjamin Birami, Yakir Preisler, Eyal Rotenberg, Yann Salmon, Fedor Tatarinov, Dan Yakir, Nadine K. Ruehr

# Here we analize the modeled impact of environmental drivers on forest productivity.
library(suncalc)

Data<- read.table("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Hyytiala_Forest_LDNDC_Data_2012_2014.csv",header=T,na="",dec=".",sep=",")

Array<-NULL
for(i in 1:length(Data[,1])){
  Array[i]<-as.numeric(strsplit(as.character(Data$Date[i]),"/")[[1]][3])}

DataNew<-Data[-which(Array==2011),]

Latitude<-61.85 #Hyytiala
Longitude<-24.28 #Hyytiala
Years_cont<-c(2012:2014)

#We correct the outvalues that are in the SWC data.

Data$SWC[which(Data$SWC>38)]<-38

#Data$FSWC30<-Data$SWC
Data$FSWC30<-1-(max(Data$SWC,na.rm=T)-Data$SWC)/(max(Data$SWC,na.rm=T)-min(Data$SWC,na.rm=T))

#We first correct GPP
Data$GPP<-Data$GPP*10000 #In kgC ha-1 day-1

#Data$PAR<-Data$Q* HoursSunDay * 4.5 /1000

Data$PAR<-Data$Q*4.5 #In micromol m-2 s-1