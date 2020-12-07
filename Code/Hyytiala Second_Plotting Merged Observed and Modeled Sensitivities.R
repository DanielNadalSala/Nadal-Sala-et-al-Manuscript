#Obtain the response to the environmental drivers and the individual limitation strength for each day
#Data  in Hyytiala.
#Daniel Nadal-Sala, Rüdiger Grote, Benjamin Birami, Yakir Preisler, Eyal Rotenberg, Yann Salmon, Fedor Tatarinov, Dan Yakir, Nadine K. Ruehr

# Functions

load_object <- function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  tmp[[ls(tmp)[1]]]
}

#################################################################

Observed<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Hyytiala_Observed.rda")
Modeled<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Hyytiala_LDNDC.rda")


library (randomForest)
library(ggplot2)
library(cowplot)
library(caret) #This is used to predict


#We remove the NA's in the entire Dataset. With unique we are removing just one of the registers for each NA (repeated NA's at the same row)
DataFillObs<-Observed
DataFillMod<-Modeled

#First for the observed part.

a<-unique(c(which(is.na(DataFillObs$GPP)),which(is.na(DataFillObs$D)),which(is.na(DataFillObs$PAR)),which(is.na(DataFillObs$Tmean)),which(is.na(DataFillObs$FSWC30))))

ifelse(length(a)>0,DataFillObs<-DataFillObs[-a,],DataFillObs<-DataFillObs)

DataFillObs<-data.frame(Tmean=DataFillObs$Tmean,PAR=DataFillObs$PAR,FSWC30=DataFillObs$FSWC30,GPP=DataFillObs$GPP,D=DataFillObs$D)

#Now we will split the data into training data and validation data.

set.seed(13) #This fixes the random values to a controlled ones

#We use the 75% of data to create the model, and the 25% for validate it.
Sample<-sample(2,nrow(DataFillObs),replace=TRUE,prob=c(0.75,0.25))
Train<-DataFillObs[which(Sample==1),]
Validate<-DataFillObs[which(Sample==2),]

ModelObs<-randomForest(GPP~.,ntree=1000,data=Train,mtry=2, importance=1) 

RefMatrix<-data.frame(matrix(data=NA,nrow=50,ncol=4))
names(RefMatrix)<-c("FSWC30","D","PAR","Tmean")
RefMatrix$FSWC30<-20
RefMatrix$Tmean<-15
RefMatrix$D<-0.5
RefMatrix$PAR<-1300

NonLimitingFSWC30_old<-RefMatrix$FSWC30[1]
NonLimitingT_old<-RefMatrix$Tmean[1]
NonLimitingPAR_old<-RefMatrix$PAR[1]
NonLimitingD_old<-RefMatrix$D[1]

#Here we modify the input values.

Data<-DataFillObs
Model<-ModelObs

Count = 0

repeat{
  
  Count = Count +1
  
  SWCmin<-min(Observed$FSWC30,na.rm=T)
  SWCmax<-max(Observed$FSWC30,na.rm=T)
  SWCstep<-seq(SWCmin,SWCmax,(SWCmax-SWCmin)/(nrow(RefMatrix)-1))
  
  Matrix<-RefMatrix
  Matrix$FSWC30<-SWCstep
  
  PredictedSWC<-predict(Model,Matrix)
  
  SlopeSWC<-NULL
  SlopeSWC[1:2]<-0
  SlopeSWC[length(SWCstep-1):length(SWCstep)]<-1
  
  for(i in 3:length(SWCstep)){
    SlopeSWC[i]<-coef(lm(PredictedSWC[(i-2):(i+2)]~SWCstep[(i-2):(i+2)]))[2]
  }
  
  DeltaSWC<-NULL
  DeltaSWC[1]<-0
  
  for(i in 2:length(SWCstep)){
    DeltaSWC[i]<-PredictedSWC[i]-PredictedSWC[i-1]
  }
  
  ###############
  #First control: We will take only the higher values, so we will sample the 50% of the data
  
  a1<-which(PredictedSWC>quantile(PredictedSWC,0.90))
  
  ###############
  #Second control: the slope shall be positive or close to zero and also the delta
  
  MinSlope<-SlopeSWC/(max(order(SlopeSWC))-order(SlopeSWC))
  a2<-which(DeltaSWC>=0)
  a3<-intersect(a2,which(MinSlope>0))
  
  ###############
  #Third: we will take the register with minimum slope
  
  a4<-intersect(a1,a3) #To find elements that are common in both rows
  MaxValue<-a4[which(MinSlope[a4]==min(MinSlope[a4]))]
  SWCstep[MaxValue]
  
  plot(PredictedSWC~SWCstep)
  abline(v=SWCstep[MaxValue])
  
  MaxSWCValue<-MaxValue
  
  ######Finally we can stablish the SWC value which is non-limiting for GPP
  NonLimitingSWC<-SWCstep[MaxValue]
  RefMatrix$FSWC30<-NonLimitingSWC
  
  #We go to the PAR
  #------------------------------
  
  PARmin<-min(Observed$PAR,na.rm=T)
  PARmax<-max(Observed$PAR,na.rm=T)
  PARstep<-seq(PARmin,PARmax,(PARmax-PARmin)/(nrow(RefMatrix)-1))
  
  Matrix<-RefMatrix
  Matrix$PAR<-PARstep
  
  PredictedPAR<-predict(Model,Matrix)
  
  SlopePAR<-NULL
  SlopePAR[1:2]<-0
  SlopePAR[length(PARstep-1):length(PARstep)]<-1
  
  
  for(i in 3:length(PARstep)){
    SlopePAR[i]<-coef(lm(PredictedPAR[(i-2):(i+2)]~PARstep[(i-2):(i+2)]))[2]
  }
  
  DeltaPAR<-NULL
  DeltaPAR[1]<-0
  
  for(i in 2:length(PARstep)){
    DeltaPAR[i]<-PredictedPAR[i]-PredictedPAR[i-1]
  }
  
  ###############
  #First control: We will take only the higher values, so we will sample the 50% of the data
  
  a1<-which(PredictedPAR>quantile(PredictedPAR,0.9))
  
  ###############
  #Second control: the slope shall be positive or close to zero and also the delta
  
  MinSlope<-SlopePAR/(max(order(SlopePAR))-order(SlopePAR))
  a2<-which(DeltaPAR>=0)
  a3<-intersect(a2,which(MinSlope>0))
  
  ###############
  #Third: we will take the register with minimum slope
  
  a4<-intersect(a1,a3) #To find elements that are common in both rows
  MaxValue<-a4[which(MinSlope[a4]==min(MinSlope[a4]))]
  PARstep[MaxValue]
  
  plot(PredictedPAR~PARstep)
  abline(v=PARstep[MaxValue])
  
  MaxPARValue<-MaxValue
  
  ######Finally we can stablish the SWC value which is non-limiting for GPP
  NonLimitingPAR<-PARstep[MaxValue]
  RefMatrix$PAR<-NonLimitingPAR
  
  #We go to the D
  #------------------------------
  
  Dmin<-min(Observed$D,na.rm=T)
  Dmax<-max(Observed$D,na.rm=T)
  Dstep<-seq(Dmin,Dmax,(Dmax-Dmin)/(nrow(RefMatrix)-1))
  
  Matrix<-RefMatrix
  Matrix$D<-Dstep
  
  PredictedD<-predict(Model,Matrix)
  
  SlopeD<-NULL
  SlopeD[1:2]<-0
  SlopeD[length(Dstep-1):length(Dstep)]<-1
  
  
  for(i in 3:length(Dstep)){
    SlopeD[i]<-(-coef(lm(PredictedD[(i-2):(i+2)]~Dstep[(i-2):(i+2)]))[2])
  }
  
  DeltaD<-NULL
  DeltaD[1]<-0
  
  for(i in 2:length(Dstep)){
    DeltaD[i]<-PredictedD[i-1]-PredictedD[i]
  }
  
  ###############
  #First control: We will take only the higher values, so we will sample the 50% of the data
  
  a1<-which(PredictedD>=quantile(PredictedD,0.90))
  
  ###############
  #Second control: the slope shall be positive or close to zero and also the delta
  
  MinSlope<-SlopeD/(max(order(SlopeD))-order(SlopeD))
  MinSlope[which(is.na(MinSlope))]<-0
  a2<-which(DeltaD>=0)
  a3<-intersect(a2,which(MinSlope>=-0.01))
  
  ###############
  #Third: we will take the register with minimum slope
  
  a4<-intersect(a1,a3) #To find elements that are common in both rows
  MaxValue<-min(a4[which(MinSlope[a4]==min(MinSlope[a4]))])
  Dstep[MaxValue]
  
  plot(PredictedD~Dstep)
  abline(v=Dstep[MaxValue])
  
  MaxDValue<-MaxValue
  
  ######Finally we can stablish the SWC value which is non-limiting for GPP
  NonLimitingD<-Dstep[MaxValue]
  RefMatrix$D<-NonLimitingD
  
  #We go to the Tmean
  #------------------------------
  
  Tmin<-min(Observed$Tmean,na.rm=T)
  Tmax<-max(Observed$Tmean,na.rm=T)
  Tstep<-seq(Tmin,Tmax,(Tmax-Tmin)/(nrow(RefMatrix)-1))
  
  Matrix<-RefMatrix
  Matrix$Tmean<-Tstep
  
  PredictedT<-predict(Model,Matrix)
  
  SlopeT<-NULL
  SlopeT[1:2]<-0
  SlopeT[length(Tstep-1):length(Tstep)]<-1
  
  
  for(i in 3:length(Tstep)){
    SlopeT[i]<-coef(lm(PredictedT[(i-2):(i+2)]~Tstep[(i-2):(i+2)]))[2]
  }
  
  DeltaT<-NULL
  DeltaT[1]<-0
  
  for(i in 2:length(Tstep)){
    DeltaT[i]<-PredictedT[i]-PredictedT[i-1]
  }
  
  ###############
  #First control: We will take only the higher values, so we will sample the 50% of the data
  
  a1<-which(PredictedT>quantile(PredictedT,0.90))
  
  ###############
  #Second control: the slope shall be positive or close to zero and also the delta
  
  MinSlope<-SlopeT/(max(order(SlopeT))-order(SlopeT))
  a2<-which(DeltaT>=0)
  a3<-intersect(a2,which(MinSlope>0))
  
  ###############
  #Third: we will take the register with minimum slope
  
  a4<-intersect(a1,a3) #To find elements that are common in both rows
  MaxValue<-a4[which(MinSlope[a4]==min(MinSlope[a4]))]
  Tstep[MaxValue]
  
  plot(PredictedT~Tstep)
  abline(v=Tstep[MaxValue])
  
  MaxTValue<-MaxValue
  
  ######Finally we can stablish the T value which is non-limiting for GPP
  NonLimitingT<-Tstep[MaxValue]
  RefMatrix$Tmean<-NonLimitingT
  
  ValidateSWC<-(NonLimitingFSWC30_old/NonLimitingSWC)
  ValidateT<-(NonLimitingT_old/NonLimitingT)
  ValidateD<-(NonLimitingD_old/NonLimitingD)
  ValidatePAR<-(NonLimitingPAR_old/NonLimitingPAR)
  
  ObservedResponseToEnvironment<-data.frame(PredictedD=PredictedD,PredictedT=PredictedT,PredictedSWC=PredictedSWC,
                                            PredoctedPAR=PredictedPAR,Dstep=Dstep,Tstep=Tstep,
                                            SWCstep=SWCstep,PARstep=PARstep)
  
  if (mean(ValidateSWC,ValidateT,ValidateD,ValidatePAR)>0.95 & mean(ValidateSWC,ValidateT,ValidateD,ValidatePAR)<1.05){break()}
  
  if (Count >=20)  {break()}
  
  NonLimitingFSWC30_old<-NonLimitingSWC
  NonLimitingT_old<-NonLimitingT
  NonLimitingD_old<-NonLimitingD
  NonLimitingPAR_old<-NonLimitingPAR
  
} # End of the repeat


#Here in RefMatrix we have now the forest growing in ideal conditions.
#We will calculate for each day of the year the difference between ideal and current conditions.

IdealObservedConditions<-data.frame(FSWC30=RefMatrix$FSWC30[1],D=RefMatrix$D[1],
                            PAR=RefMatrix$PAR[1],Tmean=RefMatrix$Tmean[1])

Data<-Observed

for (i in 2:length(Data[,1])){
  IdealObservedConditions<-rbind(IdealObservedConditions,RefMatrix[1,])
}

ModeledIdeal<-predict(Model,IdealObservedConditions)

DLimitation<-IdealObservedConditions
DLimitation$D<-Data$D

ModeledDLimit<-predict(Model,DLimitation)

#We obtain the fraction of limitation due to D
DLimitFraction<-(ModeledIdeal-ModeledDLimit)/ModeledIdeal

TLimitation<-IdealObservedConditions
TLimitation$Tmean<-Data$Tmean

ModeledTLimit<-predict(Model,TLimitation)

#We obtain the fraction of limitation due to T
TLimitFraction<-(ModeledIdeal-ModeledTLimit)/ModeledIdeal

SWCLimitation<-IdealObservedConditions
SWCLimitation$FSWC30<-Data$FSWC30

ModeledSWCLimit<-predict(Model,SWCLimitation)

#We obtain the fraction of limitation due to T
SWCLimitFraction<-(ModeledIdeal-ModeledSWCLimit)/ModeledIdeal

PARLimitation<-IdealObservedConditions
PARLimitation$PAR<-Data$PAR

ModeledPARLimit<-predict(Model,PARLimitation)

#We obtain the fraction of limitation due to T
PARLimitFraction<-(ModeledIdeal-ModeledPARLimit)/ModeledIdeal

plot(SWCLimitFraction,type="n",ylim=c(0,2),las=1,cex.axis=1.5,ylab="",xlab="")
abline(v=365,lty=3)
abline(v=365*2,lty=3)
abline(v=365*3,lty=3)
abline(h=1,lwd=0.75,lty=2)
points(SWCLimitFraction,type="l",col="steelblue",lwd=1,lty=1)
points(PARLimitFraction,type="l",col="yellow",lwd=1,lty=1)
points(DLimitFraction,type="l",col="green",lwd=1,lty=1)
points(TLimitFraction,type="l",col="red",lwd=1,lty=1)
points(c(SWCLimitFraction+PARLimitFraction+DLimitFraction+TLimitFraction)[1:1461],type="l",col="black")

#Wrapping up the Observed values
ObservedResponse<-ObservedResponseToEnvironment
ObservedLimitations<-data.frame(SWCLimitFraction,DLimitFraction,TLimitFraction,PARLimitFraction)

############################################
##########Then the modeled part

a<-unique(c(which(is.na(DataFillMod$GPP)),which(is.na(DataFillMod$D)),which(is.na(DataFillMod$PAR)),which(is.na(DataFillMod$Tmean)),which(is.na(DataFillMod$FSWC30))))

ifelse(length(a)>0,DataFillMod<-DataFillMod[-a,],DataFillMod<-DataFillMod)

DataFillMod<-data.frame(Tmean=DataFillMod$Tmean,PAR=DataFillMod$PAR,FSWC30=DataFillMod$FSWC30,GPP=DataFillMod$GPP,D=DataFillMod$D)

#Now we will split the data into training data and validation data.

set.seed(13) #This fixes the random values to a controlled ones

#We use the 70% of data to create the model, and the 30% for validate it.
Sample<-sample(2,nrow(DataFillMod),replace=TRUE,prob=c(0.7,0.3))
Train<-DataFillMod[which(Sample==1),]
Validate<-DataFillMod[which(Sample==2),]

ModelMod<-randomForest(GPP~.,ntree=1000,data=Train,mtry=2, importance=1) 

RefMatrix<-data.frame(matrix(data=NA,nrow=50,ncol=4))
names(RefMatrix)<-c("FSWC30","D","PAR","Tmean")
RefMatrix$FSWC30<-20
RefMatrix$Tmean<-15
RefMatrix$D<-0.5
RefMatrix$PAR<-1300

NonLimitingFSWC30_old<-RefMatrix$FSWC30[1]
NonLimitingT_old<-RefMatrix$Tmean[1]
NonLimitingPAR_old<-RefMatrix$PAR[1]
NonLimitingD_old<-RefMatrix$D[1]

#Here we modify the input values.

Data<-DataFillMod
Model<-ModelMod

Count = 0

repeat{
  
  Count = Count +1
  
  SWCmin<-min(Modeled$FSWC30,na.rm=T)
  SWCmax<-max(Modeled$FSWC30,na.rm=T)
  SWCstep<-seq(SWCmin,SWCmax,(SWCmax-SWCmin)/(nrow(RefMatrix)-1))
  
  Matrix<-RefMatrix
  Matrix$FSWC30<-SWCstep
  
  PredictedSWC<-predict(Model,Matrix)
  
  SlopeSWC<-NULL
  SlopeSWC[1:2]<-0
  SlopeSWC[length(SWCstep-1):length(SWCstep)]<-1
  
  for(i in 3:length(SWCstep)){
    SlopeSWC[i]<-coef(lm(PredictedSWC[(i-2):(i+2)]~SWCstep[(i-2):(i+2)]))[2]
  }
  
  DeltaSWC<-NULL
  DeltaSWC[1]<-0
  
  for(i in 2:length(SWCstep)){
    DeltaSWC[i]<-PredictedSWC[i]-PredictedSWC[i-1]
  }
  
  ###############
  #First control: We will take only the higher values, so we will sample the 50% of the data
  
  a1<-which(PredictedSWC>quantile(PredictedSWC,0.90))
  
  ###############
  #Second control: the slope shall be positive or close to zero and also the delta
  
  MinSlope<-SlopeSWC/(max(order(SlopeSWC))-order(SlopeSWC))
  a2<-which(DeltaSWC>=0)
  a3<-intersect(a2,which(MinSlope>0))
  
  ###############
  #Third: we will take the register with minimum slope
  
  a4<-intersect(a1,a3) #To find elements that are common in both rows
  MaxValue<-a4[which(MinSlope[a4]==min(MinSlope[a4]))]
  SWCstep[MaxValue]
  
  plot(PredictedSWC~SWCstep)
  abline(v=SWCstep[MaxValue])
  
  MaxSWCValue<-MaxValue
  
  ######Finally we can stablish the SWC value which is non-limiting for GPP
  NonLimitingSWC<-SWCstep[MaxValue]
  RefMatrix$FSWC30<-NonLimitingSWC
  
  #We go to the PAR
  #------------------------------
  
  PARmin<-min(Modeled$PAR,na.rm=T)
  PARmax<-max(Modeled$PAR,na.rm=T)
  PARstep<-seq(PARmin,PARmax,(PARmax-PARmin)/(nrow(RefMatrix)-1))
  
  Matrix<-RefMatrix
  Matrix$PAR<-PARstep
  
  PredictedPAR<-predict(Model,Matrix)
  
  SlopePAR<-NULL
  SlopePAR[1:2]<-0
  SlopePAR[length(PARstep-1):length(PARstep)]<-1
  
  
  for(i in 3:length(PARstep)){
    SlopePAR[i]<-coef(lm(PredictedPAR[(i-2):(i+2)]~PARstep[(i-2):(i+2)]))[2]
  }
  
  DeltaPAR<-NULL
  DeltaPAR[1]<-0
  
  for(i in 2:length(PARstep)){
    DeltaPAR[i]<-PredictedPAR[i]-PredictedPAR[i-1]
  }
  
  ###############
  #First control: We will take only the higher values, so we will sample the 50% of the data
  
  a1<-which(PredictedPAR>quantile(PredictedPAR,0.9))
  
  ###############
  #Second control: the slope shall be positive or close to zero and also the delta
  
  MinSlope<-SlopePAR/(max(order(SlopePAR))-order(SlopePAR))
  a2<-which(DeltaPAR>=0)
  a3<-intersect(a2,which(MinSlope>0))
  
  ###############
  #Third: we will take the register with minimum slope
  
  a4<-intersect(a1,a3) #To find elements that are common in both rows
  MaxValue<-a4[which(MinSlope[a4]==min(MinSlope[a4]))]
  PARstep[MaxValue]
  
  plot(PredictedPAR~PARstep)
  abline(v=PARstep[MaxValue])
  
  MaxPARValue<-MaxValue
  
  ######Finally we can stablish the SWC value which is non-limiting for GPP
  NonLimitingPAR<-PARstep[MaxValue]
  RefMatrix$PAR<-NonLimitingPAR
  
  #We go to the D
  #------------------------------
  
  Dmin<-min(Modeled$D,na.rm=T)
  Dmax<-max(Modeled$D,na.rm=T)
  Dstep<-seq(Dmin,Dmax,(Dmax-Dmin)/(nrow(RefMatrix)-1))
  
  Matrix<-RefMatrix
  Matrix$D<-Dstep
  
  PredictedD<-predict(Model,Matrix)
  
  SlopeD<-NULL
  SlopeD[1:2]<-0
  SlopeD[length(Dstep-1):length(Dstep)]<-1
  
  
  for(i in 3:length(Dstep)){
    SlopeD[i]<-(-coef(lm(PredictedD[(i-2):(i+2)]~Dstep[(i-2):(i+2)]))[2])
  }
  
  DeltaD<-NULL
  DeltaD[1]<-0
  
  for(i in 2:length(Dstep)){
    DeltaD[i]<-PredictedD[i-1]-PredictedD[i]
  }
  
  ###############
  #First control: We will take only the higher values, so we will sample the 50% of the data
  
  a1<-which(PredictedD>=quantile(PredictedD,0.90))
  
  ###############
  #Second control: the slope shall be positive or close to zero and also the delta
  
  MinSlope<-SlopeD/(max(order(SlopeD))-order(SlopeD))
  MinSlope[which(is.na(MinSlope))]<-0
  a2<-which(DeltaD>=0)
  a3<-intersect(a2,which(MinSlope>=-0.01))
  
  ###############
  #Third: we will take the register with minimum slope
  
  a4<-intersect(a1,a3) #To find elements that are common in both rows
  MaxValue<-min(a4[which(MinSlope[a4]==min(MinSlope[a4]))])
  Dstep[MaxValue]
  
  plot(PredictedD~Dstep)
  abline(v=Dstep[MaxValue])
  
  MaxDValue<-MaxValue
  
  ######Finally we can stablish the SWC value which is non-limiting for GPP
  NonLimitingD<-Dstep[MaxValue]
  RefMatrix$D<-NonLimitingD
  
  #We go to the Tmean
  #------------------------------
  
  Tmin<-min(Modeled$Tmean,na.rm=T)
  Tmax<-max(Modeled$Tmean,na.rm=T)
  Tstep<-seq(Tmin,Tmax,(Tmax-Tmin)/(nrow(RefMatrix)-1))
  
  Matrix<-RefMatrix
  Matrix$Tmean<-Tstep
  
  PredictedT<-predict(Model,Matrix)
  
  SlopeT<-NULL
  SlopeT[1:2]<-0
  SlopeT[length(Tstep-1):length(Tstep)]<-1
  
  
  for(i in 3:length(Tstep)){
    SlopeT[i]<-coef(lm(PredictedT[(i-2):(i+2)]~Tstep[(i-2):(i+2)]))[2]
  }
  
  DeltaT<-NULL
  DeltaT[1]<-0
  
  for(i in 2:length(Tstep)){
    DeltaT[i]<-PredictedT[i]-PredictedT[i-1]
  }
  
  ###############
  #First control: We will take only the higher values, so we will sample the 50% of the data
  
  a1<-which(PredictedT>quantile(PredictedT,0.90))
  
  ###############
  #Second control: the slope shall be positive or close to zero and also the delta
  
  MinSlope<-SlopeT/(max(order(SlopeT))-order(SlopeT))
  a2<-which(DeltaT>=0)
  a3<-intersect(a2,which(MinSlope>0))
  
  ###############
  #Third: we will take the register with minimum slope
  
  a4<-intersect(a1,a3) #To find elements that are common in both rows
  MaxValue<-a4[which(MinSlope[a4]==min(MinSlope[a4]))]
  Tstep[MaxValue]
  
  plot(PredictedT~Tstep)
  abline(v=Tstep[MaxValue])
  
  MaxTValue<-MaxValue
  
  ######Finally we can stablish the T value which is non-limiting for GPP
  NonLimitingT<-Tstep[MaxValue]
  RefMatrix$Tmean<-NonLimitingT
  
  ValidateSWC<-(NonLimitingFSWC30_old/NonLimitingSWC)
  ValidateT<-(NonLimitingT_old/NonLimitingT)
  ValidateD<-(NonLimitingD_old/NonLimitingD)
  ValidatePAR<-(NonLimitingPAR_old/NonLimitingPAR)
  
  ModeledResponseToEnvironment<-data.frame(PredictedD=PredictedD,PredictedT=PredictedT,PredictedSWC=PredictedSWC,
                                            PredoctedPAR=PredictedPAR,Dstep=Dstep,Tstep=Tstep,
                                            SWCstep=SWCstep,PARstep=PARstep)
  
  if (mean(ValidateSWC,ValidateT,ValidateD,ValidatePAR)>0.95 & mean(ValidateSWC,ValidateT,ValidateD,ValidatePAR)<1.05){break()}
  
  if (Count >=20)  {break()}
  
  NonLimitingFSWC30_old<-NonLimitingSWC
  NonLimitingT_old<-NonLimitingT
  NonLimitingD_old<-NonLimitingD
  NonLimitingPAR_old<-NonLimitingPAR
  
} # End of the repeat


#Here in RefMatrix we have now the forest growing in ideal conditions.
#We will calculate for each day of the year the difference between ideal and current conditions.

IdealModeledConditions<-data.frame(FSWC30=RefMatrix$FSWC30[1],D=RefMatrix$D[1],
                                    PAR=RefMatrix$PAR[1],Tmean=RefMatrix$Tmean[1])

Data<-Modeled

for (i in 2:length(Data[,1])){
  IdealModeledConditions<-rbind(IdealModeledConditions,RefMatrix[1,])
}

ModeledIdeal<-predict(Model,IdealModeledConditions)

DLimitation<-IdealModeledConditions
DLimitation$D<-Data$D

ModeledDLimit<-predict(Model,DLimitation)

#We obtain the fraction of limitation due to D
DLimitFraction<-(ModeledIdeal-ModeledDLimit)/ModeledIdeal

TLimitation<-IdealModeledConditions
TLimitation$Tmean<-Data$Tmean

ModeledTLimit<-predict(Model,TLimitation)

#We obtain the fraction of limitation due to T
TLimitFraction<-(ModeledIdeal-ModeledTLimit)/ModeledIdeal

SWCLimitation<-IdealModeledConditions
SWCLimitation$FSWC30<-Data$FSWC30

ModeledSWCLimit<-predict(Model,SWCLimitation)

#We obtain the fraction of limitation due to T
SWCLimitFraction<-(ModeledIdeal-ModeledSWCLimit)/ModeledIdeal

PARLimitation<-IdealModeledConditions
PARLimitation$PAR<-Data$PAR

ModeledPARLimit<-predict(Model,PARLimitation)

#We obtain the fraction of limitation due to T
PARLimitFraction<-(ModeledIdeal-ModeledPARLimit)/ModeledIdeal

plot(SWCLimitFraction,type="n",ylim=c(0,2),las=1,cex.axis=1.5,ylab="",xlab="")
abline(v=365,lty=3)
abline(v=365*2,lty=3)
abline(v=365*3,lty=3)
abline(h=1,lwd=0.75,lty=2)
points(SWCLimitFraction,type="l",col="steelblue",lwd=1,lty=1)
points(PARLimitFraction,type="l",col="yellow",lwd=1,lty=1)
points(DLimitFraction,type="l",col="green",lwd=1,lty=1)
points(TLimitFraction,type="l",col="red",lwd=1,lty=1)
points(c(SWCLimitFraction+PARLimitFraction+DLimitFraction+TLimitFraction)[1:1461],type="l",col="black")

#Wrapping up the Observed values
ModeledResponse<-ModeledResponseToEnvironment
ModeledLimitations<-data.frame(SWCLimitFraction,DLimitFraction,TLimitFraction,PARLimitFraction)

#Now we have the information, so we are able to plot this

library(ggplot2)
library(gridExtra)
library(ggExtra)
library(cowplot)
library(dplyr)

ObservedResponse<-ObservedResponseToEnvironment
ModeledResponse<-ModeledResponseToEnvironment
  
Min<-min(Observed$Tmean,na.rm=T)
Max<-max(Observed$Tmean,na.rm=T)

ModeledResponse<-ModeledResponse%>%mutate(RelativePredictedT=PredictedT/max(PredictedT,na.rm=T))
ObservedResponse<-ObservedResponse%>%mutate(RelativePredictedT=PredictedT/max(PredictedT,na.rm=T))

PlotCenter<- ggplot(ModeledResponse, aes(x=Tstep, y=RelativePredictedT)) + 
  geom_line(color="tomato",size=2,linetype=2)+
  theme_bw(30)+
  ylim(0,1)+
  xlim(Min,Max)+
  geom_line(aes(x=ObservedResponse$Tstep,y=ObservedResponse$RelativePredictedT),col="steelblue",size=2)+
  labs(y = expression(GPP~'/'~GPP[max]),
  x = "T (°C)",size=6, family="sans")+
  theme(axis.text.x = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.title = element_text ( size=20,color="black",family="sans"))

PlotUp<-ggplot(Observed, aes(x=Tmean))+
  theme_minimal()+
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="grey80", fill=adjustcolor("grey80",alpha=0.7))+ 
  geom_density(alpha=.2, fill="steelblue",color="transparent")+  # Overlay with transparent density plot
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

A<-plot_grid(PlotUp, PlotCenter, align = "v", nrow = 2, rel_heights = c(1/10, 2/6))

#For SWC

Min<-min(Observed$FSWC30,na.rm=T)
Max<-max(Observed$FSWC30,na.rm=T)

ModeledResponse<-ModeledResponse%>%mutate(RelativePredictedSWC=PredictedSWC/max(PredictedSWC,na.rm=T))
ObservedResponse<-ObservedResponse%>%mutate(RelativePredictedSWC=PredictedSWC/max(PredictedSWC,na.rm=T))

PlotCenter<-ggplot(ModeledResponse, aes(x=SWCstep, y=RelativePredictedSWC)) + 
  geom_line(color="tomato",size=2,linetype=2)+
  theme_bw(30)+
  ylim(0,1)+
  xlim(Min,Max)+
  geom_line(aes(x=ObservedResponse$SWCstep,y=ObservedResponse$RelativePredictedSWC),col="steelblue",size=2)+
  labs(y = expression(GPP~'/'~GPP[max]), 
       x = "REW (fraction)",size=6, family="sans")+
  theme(axis.text.x = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.title = element_text ( size=20,color="black",family="sans"))

PlotUp<-ggplot(Observed, aes(x=FSWC30))+
  theme_minimal()+
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.01,
                 colour="grey80", fill=adjustcolor("grey80",alpha=0.7))+
  geom_density(alpha=.2, fill="steelblue",color="transparent")+  # Overlay with transparent density plot
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

B<-plot_grid(PlotUp, PlotCenter, align = "v", nrow = 2, rel_heights = c(1/10, 2/6))

#For VPD

Min<-min(Observed$D,na.rm=T)
Max<-max(Observed$D,na.rm=T)

ModeledResponse<-ModeledResponse%>%mutate(RelativePredictedD=PredictedD/max(PredictedD,na.rm=T))
ObservedResponse<-ObservedResponse%>%mutate(RelativePredictedD=PredictedD/max(PredictedD,na.rm=T))

PlotCenter<-ggplot(ModeledResponse, aes(x=Dstep, y=RelativePredictedD)) + 
  geom_line(color="tomato",size=2,linetype=2)+
  theme_bw(30)+
  ylim(0,1)+
  xlim(Min,Max)+
  geom_line(aes(x=ObservedResponse$Dstep,y=ObservedResponse$RelativePredictedD),col="steelblue",size=2)+
  labs(y = expression(GPP~'/'~GPP[max]),
       x = "D (kPa)",size=6,family="sans")+
  theme(axis.text.x = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.title = element_text ( size=20,color="black",family="sans"))

PlotUp<-ggplot(Observed, aes(x=D))+
  theme_minimal()+
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.035,
                 colour="grey80", fill=adjustcolor("grey80",alpha=0.7))+
  geom_density(alpha=.2, fill="steelblue",color="transparent")+  # Overlay with transparent density plot
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

CC<-plot_grid(PlotUp, PlotCenter, align = "v", nrow = 2, rel_heights = c(1/10, 2/6))

#For PAR

Min<-min(Observed$PAR,na.rm=T)
Max<-max(Observed$PAR,na.rm=T)

ModeledResponse<-ModeledResponse%>%mutate(RelativePredictedPAR=PredoctedPAR/max(PredoctedPAR,na.rm=T))
ObservedResponse<-ObservedResponse%>%mutate(RelativePredictedPAR=PredoctedPAR/max(PredoctedPAR,na.rm=T))

PlotCenter<-ggplot(ModeledResponse, aes(x=PARstep, y=RelativePredictedPAR)) + 
  geom_line(color="tomato",size=2,linetype=2)+
  theme_bw(30)+
  ylim(0,1)+
  xlim(Min,Max)+
  geom_line(aes(x=ObservedResponse$PARstep,y=ObservedResponse$RelativePredictedPAR),col="steelblue",size=2)+
  labs(y = expression(GPP~'/'~GPP[max]),
  x = expression(paste("PAR (", mu,"E ", m^{-2}, " ", s^{-1}, ")", sep="")),family="sans")+
  #x = expression("PAR "),family="sans")+
  theme(axis.text.x = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.title = element_text ( size=20,color="black",family="sans"))

PlotUp<-ggplot(Observed, aes(x=PAR))+
  theme_minimal()+
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=10,
                 colour="grey80", fill=adjustcolor("grey80",alpha=0.7))+
  geom_density(alpha=.2, fill="steelblue",color="transparent")+  # Overlay with transparent density plot
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

DC<-plot_grid(PlotUp, PlotCenter, align = "v", nrow = 2, rel_heights = c(1/10, 2/6))

#To actually plot the graphs

A #The plot for Temperature
B #The plot for REW
CC # The plot for D
DC # The plot for PAR
