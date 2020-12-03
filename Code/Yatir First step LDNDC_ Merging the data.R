#Reading the simulations of LDNDC for Yatir Forest.
#Daniel Nadal-Sala, Rüdiger Grote, Benjamin Birami, Yakir Preisler, Eyal Rotenberg, Yann Salmon, Fedor Tatarinov, Dan Yakir, Nadine K. Ruehr

# Here we analize the modeled impact of environmental drivers on forest productivity.
library(suncalc)

Data<- read.table("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Yatir_Forest_LDNDC_Data_2013_2015.csv",header=T,na="",dec=".",sep=",")

Latitude<-31.2 #Yatir
Longitude<-35.2 #Yatir
Years_cont<-c(2013:2015)

#Data$FSWC30<-Data$SWC
Data$FSWC30<-1-(max(Data$SWC,na.rm=T)-Data$SWC)/(max(Data$SWC,na.rm=T)-min(Data$SWC,na.rm=T))

#We first correct GPP
Data$GPP<-Data$GPP*10000 #In kgC ha-1 day-1

#We calculate the sun hours
Date<-as.Date(Data$Date)

a<-getSunlightTimes(date = Date, lat = Latitude, lon = Longitude, tz = "CET")

HoursSunDay<-as.numeric(a$sunset-a$sunrise)

#Data$PAR<-Data$Q* HoursSunDay * 4.5 /1000

Data$PAR<-Data$Q*4.5 /100