#Reading the data for Yatir forest and merging it with climate data.
#Daniel Nadal-Sala, Rüdiger Grote, Benjamin Birami, Yakir Preisler, Eyal Rotenberg, Yann Salmon, Fedor Tatarinov, Dan Yakir, Nadine K. Ruehr
#reading flux data , 2 year chunks

Fileflux<-"C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Yatir_Measured_Data.csv"

FluxData<- read.table(Fileflux,header=T,na="",dec=".",sep=",")

FluxDataMod<-FluxData

Na.ID<- which(is.na(FluxData$Carbonflux))#Conservative approach: we remove all data that has been extrapolated. Firstly for carbon

FluxDataMod[Na.ID,3:length(FluxDataMod)]<-NA

Na.ID<- which(is.na(FluxData$Evapotranspiration))#Conservative approach: we remove all data that has been extrapolated. Then for evapotranspiration

FluxDataMod[Na.ID,3:length(FluxDataMod)]<-NA

FluxDataMod$GPP<-(-FluxDataMod$GPP) #Finally we save GPP as a positive value

#Now we have the variables defined.
#GPP: micromols square meter second
#Carbonflux: micromols square meter second
#Ecoresp: micromols square meter second
#Evapotranspiration: g square meter second
#Heatflux: watts per square meter
#Latent heat: watts per square meter

#Here we will convert the dates in days and hours

FluxDataMod$date_mid_hour<-as.character(FluxDataMod$date_mid_hour)

TimeID<-matrix(ncol=2,nrow=length(FluxDataMod$date_mid_hour),NA)

for(i in 1:length(FluxDataMod$date_mid_hour)){
  ID<-unlist(strsplit(FluxDataMod$date_mid_hour[i], " "))
  TimeID[i,1]<-ID[1]; TimeID[i,2]<-ID[2]
}

DMY<-matrix(ncol=3,nrow=length(TimeID[,1]),NA)
Hour<-matrix(ncol=1,nrow=length(TimeID[,1]),NA)

for (i in 1:length(TimeID[,2])){
  YMDreg<-unlist(strsplit(TimeID[i,1], "/"))
  Hourreg<-unlist(strsplit(TimeID[i,2], ":"))
  DMY[i,1]<-as.numeric(YMDreg[1]); DMY[i,2]<-as.numeric(YMDreg[2]); DMY[i,3]<-as.numeric(YMDreg[3])
  Hour[i]<-sum(as.numeric(Hourreg[1]),as.numeric(Hourreg[2])/60)
}

DMYH<-data.frame(DMY,Hour)
names(DMYH)<-c("Month","Day","Year","Hour")

FluxFinal<-cbind(DMYH,FluxDataMod[,3:length(FluxDataMod)]) #Finally we merge the dataframe with the proper month, day and year

FluxDaily<-data.frame(matrix(ncol=length(FluxFinal),nrow=length(FluxFinal[,1])/48,NA))
names(FluxDaily)<-c("ID","Year","Month","Day","NEE","HF","ET","LH","EcoR","GPP")

for(i in 1:length(FluxDaily[,1])){
  Sample<-FluxFinal[((i-1)*48+1):(i*48),]
  FluxDaily$ID[i]<-i
  FluxDaily$Year[i]<-Sample$Year[1]
  FluxDaily$Month[i]<-Sample$Month[1]
  FluxDaily$Day[i]<-Sample$Day[1]
  FluxDaily$NEE[i]<-sum(Sample$Carbonflux)*12/1000000*30*60/1000*10000 #gC per mmol, micromols per mol, seconds per half an hour, kg per g, ha per square meter
  FluxDaily$HF[i]<-sum(Sample$Heatflux)
  FluxDaily$ET[i]<-sum(Sample$Evapotranspiration) /1000*30*60*1 #g to kgH2O, seconds per half an hour,kg H2O to mm H2O
  FluxDaily$LH[i]<-sum(Sample$Latentheat)
  FluxDaily$EcoR[i]<-sum(Sample$Ecoresp)*12/1000000*30*60/1000*10000 #gC per mmol, micromols per mol, seconds per half an hour
  FluxDaily$GPP[i]<-sum(Sample$GPP)*12/1000000*30*60/1000*10000 #gC per mmol, micromols per mol, seconds per half an hour
}

#Transpiration in mmH2O
#Daily carbon fluxes as KgC per ha
#In FluxDaily whe have the flux data merged daily

###############################################################################
#Now we will work on the soil water content data. As flux data only starts at 2010, we only need meteo data from the 2010-2016 period

Filemeteo<-"C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Yatir_Input_Meteo.csv"

MeteoData<-read.table(Filemeteo,header=T,na=" ",dec=".",sep=",")

MeteoDataMod<-MeteoData

#Na.ID<- which(is.na(MeteoDataMod$SWC_5))#Conservative approach: we remove all data that has been extrapolated. Firstly for SWC_5

#MeteoDataMod[Na.ID,3:length(MeteoDataMod)]<-NA

#Na.ID<- which(is.na(MeteoDataMod$SWC_15))#Conservative approach: we remove all data that has been extrapolated. Then for evapotranspiration

#MeteoDataMod[Na.ID,3:length(MeteoDataMod)]<-NA

#Now we have the NA's in the right places within the soil profile.

MeteoDataMod[,1]<-as.character(MeteoDataMod[,1])
names(MeteoDataMod)[1]<-"date_mid_hour"

#Here we correct for the VPD.

Esat<-610.7*10^((7.5*MeteoDataMod$Tair)/(MeteoDataMod$Tair+237.3))
VPD<-Esat*(1-(MeteoDataMod$RH/100))

MeteoDataMod$D_old<-MeteoDataMod$D
MeteoDataMod$D<-VPD


############

TimeID<-matrix(ncol=2,nrow=length(MeteoDataMod$date_mid_hour),NA)

for(i in 1:length(MeteoDataMod$date_mid_hour)){
  ID<-unlist(strsplit(MeteoDataMod$date_mid_hour[i], " "))
  TimeID[i,1]<-ID[1]; TimeID[i,2]<-ID[2]
}

DMY<-matrix(ncol=3,nrow=length(TimeID[,1]),NA)
Hour<-matrix(ncol=1,nrow=length(TimeID[,1]),NA)

for (i in 1:length(TimeID[,2])){
  YMDreg<-unlist(strsplit(TimeID[i,1], "/"))
  Hourreg<-unlist(strsplit(TimeID[i,2], ":"))
  DMY[i,1]<-as.numeric(YMDreg[1]); DMY[i,2]<-as.numeric(YMDreg[2]); DMY[i,3]<-as.numeric(YMDreg[3])
  Hour[i]<-sum(as.numeric(Hourreg[1]),as.numeric(Hourreg[2])/60)
}

DMYH<-data.frame(DMY,Hour)
names(DMYH)<-c("Month","Day","Year","Hour")

MeteoFinal<-cbind(DMYH,MeteoDataMod[,2:length(MeteoDataMod)]) #Finally we merge the dataframe with the proper month, day and year

IDMeteodaily<-c("ID","Year","Month","Day","Rain","PAR","LPhoto","MaxPAR","Tmax","Tmin","Tmean","D","DayD","SWC15","SWC30","SWC70","PARsum")
MeteoDaily<-data.frame(matrix(ncol=length(IDMeteodaily),nrow=length(MeteoFinal[,1])/48,NA))
names(MeteoDaily)<-IDMeteodaily

for(i in 1:length(MeteoDaily[,1])){
  Sample<-MeteoFinal[((i-1)*48+1):(i*48),]
  MeteoDaily$ID[i]<-i
  MeteoDaily$Year[i]<-Sample$Year[1]
  MeteoDaily$Month[i]<-Sample$Month[1]
  MeteoDaily$Day[i]<-Sample$Day[1]
  MeteoDaily$Rain[i]<-sum(Sample$Rain_30min_mm)
  Min<-min(which(Sample$PAR>10))
  Max<-max(which(Sample$PAR>10))
  
  if (Min != "Inf"){
    ifelse(is.na(mean(Sample$PAR)),MeteoDaily$PAR[i]<-NA,MeteoDaily$PAR[i]<-round(mean(Sample$PAR[Min:Max]),2))
    ifelse(length(which(is.na(Sample$D)))>12,MeteoDaily$DayD[i]<-NA,MeteoDaily$DayD[i]<-round(mean(Sample$D[Min:Max],na.rm=T),2)/1000)
    ifelse(is.na(c(Min,Max)),MeteoDaily$LPhoto[i]<-NA,MeteoDaily$LPhoto[i]<-(Max-Min+1)/2)
    ifelse(is.na(mean(Sample$PAR)),MeteoDaily$PARsum[i]<-NA,MeteoDaily$PARsum[i]<-round(sum(Sample$PAR[Min:Max])/(1000*2),2))
  } else {
    MeteoDaily$PAR[i]<-NA
    MeteoDaily$PARsum[i]<-NA
    MeteoDaily$DayD[i]<-NA
    MeteoDaily$LPhoto[i]<-NA
  }
  
  ifelse(is.na(mean(Sample$PAR)),MeteoDaily$MaxPAR[i]<-NA,MeteoDaily$MaxPAR[i]<-max(Sample$PAR))
  ifelse(length(which(is.na(Sample$Tair)))>12,MeteoDaily$Tmin[i]<-NA,MeteoDaily$Tmin[i]<-min(Sample$Tair,na.rm=T))
  ifelse(length(which(is.na(Sample$Tair)))>12,MeteoDaily$Tmax[i]<-NA,MeteoDaily$Tmax[i]<-max(Sample$Tair,na.rm=T))
  ifelse(length(which(is.na(Sample$Tair)))>12,MeteoDaily$Tmean[i]<-NA,MeteoDaily$Tmean[i]<-round(mean(Sample$Tair,na.rm=T),2))
  ifelse(length(which(is.na(Sample$D)))>12,MeteoDaily$D[i]<-NA,MeteoDaily$D[i]<-round(mean(Sample$D,na.rm=T),2)/1000)
  ifelse(length(which(is.na(Sample$SWC_15)))>24,MeteoDaily$SWC15[i]<-NA,MeteoDaily$SWC15[i]<-round(mean(Sample$SWC_15,na.rm=T),2))
  ifelse(length(which(is.na(Sample$SWC_30)))>24,MeteoDaily$SWC30[i]<-NA,MeteoDaily$SWC30[i]<-round(mean(Sample$SWC_30,na.rm=T),2)) 
  ifelse(length(which(is.na(Sample$SWC_70)))>24,MeteoDaily$SWC70[i]<-NA,MeteoDaily$SWC70[i]<-round(mean(Sample$SWC_70,na.rm=T),2)) 
  
}


plot(MeteoDaily$SWC15,col="red",type="l",las=1, ylab="SWC",xlab="Day from 01/01/2016")
points(MeteoDaily$SWC30,col="green",type="l")
points(MeteoDaily$SWC70,col="blue",type="l")


#Rain in mm.day-1
#PAR in microeinstein.m-2.s-1 (mean PAR)
#MaxPAR in microeinstein.m-2.s-1 (max PAR)
#LPhoto in hours Length of the photoperiod
#DayD is the mean daylight D

#I removed SWC50 because of strange data (continued trend in higher values than SWC30)


####################################################################################
############### Now we will merge the two datasets.

Flux<-FluxDaily
Meteo<-MeteoDaily

MatrixJoint<-matrix(nrow=length(Meteo[,1]),ncol=length(Flux)+length(Meteo)-5) #5 because we already have the Day, Month, Year and ID registers.
DataFrameJoint<-data.frame(MatrixJoint)

Names2<-names(Flux)[5:length(Flux)]
Names1<-names(Meteo)[2:length(Meteo)]
NamesJoint<-c(Names1,Names2)

names(DataFrameJoint)<-NamesJoint


for (i in 1:length(Meteo[,1])){
  Sample<-Meteo[i,]
  ID<-which(Flux$Day == Sample$Day & Flux$Month == Sample$Month  & Flux$Year == Sample$Year)
  Join<-cbind(Sample[2:length(Sample)],Flux[ID,5:length(Flux)])
  
  for (j in 1:length(Join)){
    DataFrameJoint[i,j]<-Join[j] 
  }
}

FSWC15<-1-(max(DataFrameJoint$SWC15,na.rm=T)-DataFrameJoint$SWC15)/(max(DataFrameJoint$SWC15,na.rm=T)-min(DataFrameJoint$SWC15,na.rm=T))
FSWC30<-1-(max(DataFrameJoint$SWC30,na.rm=T)-DataFrameJoint$SWC30)/(max(DataFrameJoint$SWC30,na.rm=T)-min(DataFrameJoint$SWC30,na.rm=T))
FSWC70<-1-(max(DataFrameJoint$SWC70,na.rm=T)-DataFrameJoint$SWC70)/(max(DataFrameJoint$SWC70,na.rm=T)-min(DataFrameJoint$SWC70,na.rm=T))