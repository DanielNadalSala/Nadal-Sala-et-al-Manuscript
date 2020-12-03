#To process the observed data in Hyytiala
#Observed data in Hyytiala
#Daniel Nadal-Sala, Rüdiger Grote, Benjamin Birami, Yakir Preisler, Eyal Rotenberg, Yann Salmon, Fedor Tatarinov, Dan Yakir, Nadine K. Ruehr

#Here we prepare the algorithm to run with the PROFOUND database for Hyytiälä.
library(ProfoundData)
library(suncalc)

setDB("C:/Users/sala-d/Desktop/Yatir Forest Paper/ProfoundData/ProfoundData.sqlite")

#getDB()  To check if we have the DB.

Latitude<-61.85 #Hyytiäla
Longitude<-24.28 #Hyytiäla
Years_cont<-c(2012:2014)

dataSWC <- getData(dataset = "SOILTS", site = "hyytiala", decreasing = TRUE)
dataClimate<- getData(dataset="CLIMATE_LOCAL", site="hyytiala",decreasing =TRUE)
dataFlux<- getData(dataset="FLUX",site="hyytiala",decreasing = TRUE)
dataStand<-getData(dataset="STAND",site="hyytiala",decreasing = TRUE)

#What to do next:

#Select four years of data
#Merge GPP to daily values and convert it to kg per ha
#The target variable will be the GPP calculated from night-time Respiration, 
#with NEE calculated by a Variable Ustar Threshold for each year: gppNtVutRef_umolCO2m2s1

#Merge climate data to daily values also
#Radiation provided in joule per cm2 per day (J cm-2 day-1)
#Equivalence: 1 J cm-2 day-1 = 0.01 MJ m-2 day-1. Therefore, I should convert the units

#Merge SWC to daily values
#SWC data obtained at the 4th soil layer, which is between 22.5 and 56.0 cm

#Proceed equal than with the Yatir forest data

#Take into account that the dataset contains one more day because there is a 366 days' year

SampleYear<-which(dataFlux$year>=min(Years_cont) & dataFlux$year<=max(Years_cont))
SampleYearClimate<-which(dataClimate$year>=min(Years_cont) & dataClimate$year<=max(Years_cont))

Flux<-dataFlux[SampleYear,]
SWCref<-dataSWC[SampleYear,]
Climate<-dataClimate[SampleYearClimate,]

Month<-NA

for(i in 1:(length(Flux$mo)/48)){
  Month[i]<-min(Flux$mo[((i-1)*48+1):(48*i)])
}

#First we will start for the SWC, which is the easyest.

PoorQualitySWC<-which(SWCref$swcFMDS4_qc>1)

#We only use the data with no gapfill or a high quality gapfill
SWCrefQC<-SWCref$swcFMDS4_degC
SWCrefQC[PoorQualitySWC]<-NA

SWC<-NULL

for(i in 1:(length(SWCrefQC)/48)){
  SampleSWC<-SWCrefQC[(48*(i-1)+1):(48*i)]
  #We discard the day if it has more than two values missing
  if(length(which(is.na(SampleSWC)))>3){
    SWC[i]<-NA
  }else{
    SWC[i]<-mean(SampleSWC,na.rm=T)}
}

FSWC30<-1-(max(SWC,na.rm=T)-SWC)/(max(SWC,na.rm=T)-min(SWC,na.rm=T))
#FSWC30<-SWC

#Then we will calculate the GPP, also taking into account only the values with quality of 0 or 1.

PoorQualityGPP<-which(Flux$neeVutRef_qc>1)

GPPrefQC<-Flux$gppNtVutSe_umolCO2m2s1
GPPrefQC[PoorQualityGPP]<-NA

GPP<-NULL

for(i in 1:(length(GPPrefQC)/48)){
  SampleGPP<-GPPrefQC[(48*(i-1)+1):(48*i)]
  #We discard the day if it has more than three values missing (95% of daiyly values)
  if(length(which(is.na(SampleGPP)))>3){
    GPP[i]<-NA
  }else{
    #In case we lack the GPP value but just a few of them, we assing to this value the average GPP value for a given day
    if(length(which(is.na(SampleGPP))>0)){
      a<-which(is.na(SampleGPP))
      SampleGPP[a]<-mean(SampleGPP,na.rm=T)
    }
    GPP[i]<-sum(SampleGPP,na.rm=T)}
}

GPP<-GPP*24*12*3600/1000000*10000/1000

#Correction factor for GPP (in kgC ha-1 day-1): 24*12*3600/1000000*10000/1000

#Finally we will calculate the climate variables i.e. Q (we will call it PAR in order to simplify the coding), 
#Tmean and D (which we will calculate from RH and Tair)
#We will stablish a threshold of quality to 80% of confidence of the data

#first, the PAR

PoorQualityPAR<-which(Climate$rad_qc<0.8)

PARrefQC<-Climate$rad_Jcm2day *10000/(3600*24) #We convert to J cm-2 day-1 to W/m2

#1 W/m2 -> 4.6 micromols/m-2/s-1 (Plant Growth Chamber Handbook,Chapter 1) 
#https://www.controlledenvironments.org/wp-content/uploads/sites/6/2017/06/Ch01.pdf)

#To obtain the number of hours a day:

Date<-as.Date(paste(Climate$year,"-",Climate$mo, "-",Climate$day,sep=""))

a<-getSunlightTimes(date = Date, lat = Latitude, lon = Longitude, tz = "CET")

HoursSunDay<-as.numeric(a$sunset-a$sunrise)

PARrefQC<-PARrefQC * 4.5 #Now we have the PAR in micromols per square meter and per day

if(length(PoorQualityPAR)>1){
  PARrefQC[PoorQualityPAR]<-NA}

PAR<-PARrefQC

#Then the mean Tair

PoorQualityT<-which(Climate$tmean_qc<0.8)

TrefQC<-Climate$tmean_degC

if(length(PoorQualityT)>1){
  TrefQC[PoorQualityT]<-NA}

Tmean<-TrefQC

#Finally the Vapor Pressure Deficit

#First, we check for the quality of RH

Esat<-610.7*10^((7.5*Tmean)/(Tmean+237.3))
D<-Esat*(1-(Climate$relhum_percent/100))/1000

Dataset<-data.frame(D=D,Tmean=Tmean,PAR=PAR,FSWC30=FSWC30,GPP=GPP)

#We delete the values in which GPP is unrealisticly high under very low temperatures

a<-which(Dataset$Tmean<0 & Dataset$GPP>20)

Dataset$GPP[a]<-NA