#### Here we perform the optimization of the Alpha coefficients, and we end up obtaining the MLED for each day. (Hyytiala)
#### NOTE there is a breaking point in the middle of the script, in order to avoid running the entire optimization each time the script runs. 

#### Packages:

library (BayesianTools);library (DEoptim); library(dplyr); library(ggplot2) 
library(gridExtra); library(ggExtra);library(cowplot)

##### Functions

load_object <- function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  tmp[[ls(tmp)[1]]]
}

#################################################################

#First part: To obtain the alpha coefficients. If wanted, it can be skipt to the second part
#as we provide the calibrated values used in the analysis in a sepparated file.

Observed<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Hyytiala_Observed_Limitations.rda")
Modeled<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Hyytiala_Modeled_Limitations.rda")

Month<-NULL

for(i in 1:length(Modeled$Date)){
  Month[i]<-as.numeric(strsplit(as.character(Modeled$Date[i]),"/")[[1]][1])
}

Time_step<-15
RefGPP<-Observed$ModeledIdeal
ObsGPP<-Observed$GPP
Results<-matrix(ncol=5,nrow=length(RefGPP))

Tl<-Observed$TLimitFraction #Fraction of reference GPP limited by Temperature
Dl<-Observed$DLimitFraction #Fraction of reference GPP limited by D
SWCl<-Observed$SWCLimitFraction #Fraction of reference GPP limited by SWC
PARl<-Observed$PARLimitFraction #Fraction of reference GPP limited by PAR

Prior<-createUniformPrior(lower=c(0,0,0,0,0.1),upper=c(1,1,1,1,20))

set.seed(13)

for (i in 1:length(ObsGPP)){
  
  Temp<<-Tl[i]
  VPD<<-Dl[i]
  SWC<<-SWCl[i]
  PAR<<-PARl[i]
  GPP<<-ObsGPP[i]
  
  if(is.na(min(c(Temp,VPD,SWC,PAR,GPP)))){
    Results[i,]<-c(NA,NA,NA,NA,NA)
  }else{
    
    likelihood<-function(par){
      
      #Calculate the different GPP/GPPref
      ModeledGPP<- (1-((Temp*par[1])+(VPD*par[2])+(SWC*par[3])+(PAR*par[4])))*RefGPP[1]
      
      LogLikelihood<-dnorm(ModeledGPP,mean=GPP,sd=1,log=T)
      
      return(LogLikelihood)
      
    }
    
    Setup<-createBayesianSetup(likelihood=likelihood,prior=Prior)
    Setup$names<-c("AlphaT","AlphaD","AlphaSWC","AlphaPAR","SD")
    #settings = list(iterations = 45000)
    #out<-runMCMC(bayesianSetup = Setup,sampler="DEzs",settings=settings)
    fn<-function(x)-Setup$posterior$density(x)
    
    out<-DEoptim(fn,lower=c(0,0,0,0,0.1),upper=c(1,1,1,1,20))
    
    Results[i,]<-c(out$optim$bestmem)
    
  } #End of the Else
  
}



GPP_modeled<-NULL
TfactorChain<-NULL
DfactorChain<-NULL
SWCfactorChain<-NULL
PARfactorChain<-NULL
GPP_observed<-NULL


for (i in 1:length(RefGPP)){
  
  par<-Results[i,]
  OriginalGPP<-ObsGPP[i]
  
  Temp<-Tl[i]
  VPD<-Dl[i]
  SWC<-SWCl[i]
  PAR<-PARl[i]
  GPP<-ObsGPP[i]
  
  if(is.na(min(c(Temp,VPD,SWC,PAR,GPP)))){
    GPP_modeled[i]<-NA
    TfactorChain[i]<-NA
    DfactorChain[i]<-NA
    SWCfactorChain[i]<-NA
    PARfactorChain[i]<-NA
    GPP_observed[i]<-ObsGPP[i]
    
  }else{
    
    GPP_modeled[i]<- (1-((Temp*par[1])+(VPD*par[2])+(SWC*par[3])+(PAR*par[4])))*RefGPP[1]
    TfactorChain[i]<-Temp*par[1]
    DfactorChain[i]<-VPD*par[2]
    SWCfactorChain[i]<-SWC*par[3]
    PARfactorChain[i]<-PAR*par[4]
    GPP_observed[i]<-GPP
    
  }
}

#Here we obtained the limitation for all the environmental drivers.

TLimit<-TfactorChain
DLimit<-DfactorChain
SWCLimit<-SWCfactorChain
PARLimit<-PARfactorChain

ObservedLS<-data.frame(TLimitObs=TLimit,DLimitObs=DLimit,SWCLimitObs=SWCLimit,PARLimitObs=PARLimit,GPPMod = GPP_modeled)

#####################
#Now for the modeled part

Time_step<-15
RefGPP<-Modeled$ModeledIdeal
ObsGPP<-Modeled$GPP
Results<-matrix(ncol=5,nrow=length(RefGPP))

Tl<-Modeled$TLimitFraction #Fraction of reference GPP limited by Temperature
Dl<-Modeled$DLimitFraction #Fraction of reference GPP limited by D
SWCl<-Modeled$SWCLimitFraction #Fraction of reference GPP limited by SWC
PARl<-Modeled$PARLimitFraction #Fraction of reference GPP limited by PAR

Prior<-createUniformPrior(lower=c(0,0,0,0,0.1),upper=c(1,1,1,1,20))

set.seed(13)

for (i in 1:length(ObsGPP)){
  
  Temp<<-Tl[i]
  VPD<<-Dl[i]
  SWC<<-SWCl[i]
  PAR<<-PARl[i]
  GPP<<-ObsGPP[i]
  
  if(is.na(min(c(Temp,VPD,SWC,PAR,GPP)))){
    Results[i,]<-c(NA,NA,NA,NA,NA)
  }else{
    
    likelihood<-function(par){
      
      #Calculate the different GPP/GPPref
      ModeledGPP<- (1-((Temp*par[1])+(VPD*par[2])+(SWC*par[3])+(PAR*par[4])))*RefGPP[1]
      
      LogLikelihood<-dnorm(ModeledGPP,mean=GPP,sd=1,log=T)
      
      return(LogLikelihood)
      
    }
    
    Setup<-createBayesianSetup(likelihood=likelihood,prior=Prior)
    Setup$names<-c("AlphaT","AlphaD","AlphaSWC","AlphaPAR","SD")
    #settings = list(iterations = 45000)
    #out<-runMCMC(bayesianSetup = Setup,sampler="DEzs",settings=settings)
    fn<-function(x)-Setup$posterior$density(x)
    
    out<-DEoptim(fn,lower=c(0,0,0,0,0.1),upper=c(1,1,1,1,20))
    
    Results[i,]<-c(out$optim$bestmem)
    
  } #End of the Else
  
}

GPP_modeled<-NULL
TfactorChain<-NULL
DfactorChain<-NULL
SWCfactorChain<-NULL
PARfactorChain<-NULL
GPP_observed<-NULL


for (i in 1:length(RefGPP)){
  
  par<-Results[i,]
  OriginalGPP<-ObsGPP[i]
  
  Temp<-Tl[i]
  VPD<-Dl[i]
  SWC<-SWCl[i]
  PAR<-PARl[i]
  GPP<-ObsGPP[i]
  
  if(is.na(min(c(Temp,VPD,SWC,PAR,GPP)))){
    GPP_modeled[i]<-NA
    TfactorChain[i]<-NA
    DfactorChain[i]<-NA
    SWCfactorChain[i]<-NA
    PARfactorChain[i]<-NA
    GPP_observed[i]<-ObsGPP[i]
    
  }else{
    
    GPP_modeled[i]<- (1-((Temp*par[1])+(VPD*par[2])+(SWC*par[3])+(PAR*par[4])))*RefGPP[1]
    TfactorChain[i]<-Temp*par[1]
    DfactorChain[i]<-VPD*par[2]
    SWCfactorChain[i]<-SWC*par[3]
    PARfactorChain[i]<-PAR*par[4]
    GPP_observed[i]<-GPP
    
  }
}

#Here we obtained the limitation for all the environmental drivers.

TLimit<-TfactorChain
DLimit<-DfactorChain
SWCLimit<-SWCfactorChain
PARLimit<-PARfactorChain

ModeledLS<-data.frame(TLimitObs=TLimit,DLimitObs=DLimit,SWCLimitObs=SWCLimit,PARLimitObs=PARLimit,GPPMod = GPP_modeled)

Data<-list(Observed=ObservedLS,Modeled=ModeledLS,Month=Month)

#save(Data,file="C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Hyytiala_CombinedLimitations.rda")

#############################################
#Here it reads the data already processed via optimization and saved in the previous step.
#It includes both the Observed and the Simulated data.
####################################
##########################
###########To plot

Data<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Hyytiala_CombinedLimitations.rda")

#for this figure I will get the main environmental driver for each day:

#First we calculate which is the most limiting driver each day.
Observed<-Data$Observed
a<-which(is.na(Observed$TLimitObs))
Modeled<-Data$Modeled
Modeled[a,2:5]<-NA

# Here for the observations.
FactorLimiting<-NULL

for(i in 1:length(Observed$GPPMod)){
  Tl<-Observed$TLimitObs[i]
  Dl<-Observed$DLimitObs[i]
  SWCl<-Observed$SWCLimitObs[i]
  PARl<-Observed$PARLimitObs[i]
  
  if(is.na(max(Tl,Dl,SWCl,PARl))){
    FactorLimiting[i]<-NA
  }else{
    FactorLimiting[i]<-
      c("tomato","grey50","steelblue","wheat")[which(c(Tl,Dl,SWCl,PARl)==max(c(Tl,Dl,SWCl,PARl)))]
  }
}

FLDiscrete<-rep(NA,length(FactorLimiting))

for(i in 1:length(FLDiscrete)){
  Ref<-FactorLimiting[i]
  if(is.na(Ref)){FLDiscrete[i]<-NA}else{
    if(Ref=="tomato"){FLDiscrete[i]<-1} #Temperature
    if(Ref=="grey50"){FLDiscrete[i]<-2} #VPD
    if(Ref=="steelblue"){FLDiscrete[i]<-3} #SWC
    if(Ref=="wheat"){FLDiscrete[i]<-4}     #PAR
  }
}

DataJoint<-data.frame(Limiting=FLDiscrete,Month=Data$Month)

LimitFr<-DataJoint%>%group_by(Month)%>%
  summarise(TLimit=length((which(Limiting==1)))/(length(Limiting)-length(which(is.na(Limiting)))),
            DLimit=length((which(Limiting==2)))/(length(Limiting)-length(which(is.na(Limiting)))),
            SWCLimit=length((which(Limiting==3)))/(length(Limiting)-length(which(is.na(Limiting)))),
            PARLimit=length((which(Limiting==4)))/(length(Limiting)-length(which(is.na(Limiting)))))

LimitFrObserved<-LimitFr #Here we save the fractions

DataFeed<-data.frame(Month = as.factor(rep(1:12,4)),
                     Driver= c(rep("Air Tmean",12),rep("PAR",12),rep("SWC",12),rep("D",12)),
                     Limit = c(LimitFr$TLimit,LimitFr$PARLimit,LimitFr$SWCLimit,LimitFr$DLimit))

PlotObserved<-ggplot(data=DataFeed, aes(x=Month, y=Limit, fill=Driver)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("tomato","grey50","wheat","steelblue"))+
  theme_minimal()+
  labs(y = "Days (fraction)", 
       x = "Month",family="sans")+
  theme(axis.text.x = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.title.x = element_text(size=5,color="white",family="sans"))+
  theme(axis.title.y = element_text (size=18,color="black",family="sans"))+ 
  theme(legend.position = "none")

###Here we saved the plot for the observed seasonality. Let's do the modeled:

# Here for the observations.
FactorLimiting<-NULL

for(i in 1:length(Modeled$GPPMod)){
  Tl<-Modeled$TLimitObs[i]
  Dl<-Modeled$DLimitObs[i]
  SWCl<-Modeled$SWCLimitObs[i]
  PARl<-Modeled$PARLimitObs[i]
  
  if(is.na(max(Tl,Dl,SWCl,PARl))){
    FactorLimiting[i]<-NA
  }else{
    FactorLimiting[i]<-
      c("tomato","grey50","steelblue","wheat")[which(c(Tl,Dl,SWCl,PARl)==max(c(Tl,Dl,SWCl,PARl)))]
  }
}

FLDiscrete<-rep(NA,length(FactorLimiting))

for(i in 1:length(FLDiscrete)){
  Ref<-FactorLimiting[i]
  if(is.na(Ref)){FLDiscrete[i]<-NA}else{
    if(Ref=="tomato"){FLDiscrete[i]<-1} #Temperature
    if(Ref=="grey50"){FLDiscrete[i]<-2} #VPD
    if(Ref=="steelblue"){FLDiscrete[i]<-3} #SWC
    if(Ref=="wheat"){FLDiscrete[i]<-4}     #PAR
  }
}

DataJoint<-data.frame(Limiting=FLDiscrete,Month=Data$Month)

LimitFr<-DataJoint%>%group_by(Month)%>%
  summarise(TLimit=length((which(Limiting==1)))/(length(Limiting)-length(which(is.na(Limiting)))),
            DLimit=length((which(Limiting==2)))/(length(Limiting)-length(which(is.na(Limiting)))),
            SWCLimit=length((which(Limiting==3)))/(length(Limiting)-length(which(is.na(Limiting)))),
            PARLimit=length((which(Limiting==4)))/(length(Limiting)-length(which(is.na(Limiting)))))

LimitFrModeled<-LimitFr #Here we save the fractions

DataFeed<-data.frame(Month = as.factor(rep(1:12,4)),
                     Driver= c(rep("Air Tmean",12),rep("PAR",12),rep("SWC",12),rep("D",12)),
                     Limit = c(LimitFr$TLimit,LimitFr$PARLimit,LimitFr$SWCLimit,LimitFr$DLimit))

PlotModeled<-ggplot(data=DataFeed, aes(x=Month, y=Limit, fill=Driver)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("tomato","grey50","wheat","steelblue"))+
  theme_minimal()+
  labs(y = "Days (fraction)", 
       x = "Month",family="sans")+
  theme(axis.text.x = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.title.x = element_text(size=5,color="white",family="sans"))+
  theme(axis.title.y = element_text (size=18,color="black",family="sans"))+ 
  theme(legend.position = "none")

# To calculate the limitation differences.

ObsLim<-LimitFrObserved[,2:5]
ModLim<-LimitFrModeled[,2:5]

PercentDif<-abs(ObsLim-ModLim)/12*100

CumulatedT<-NULL
CumulatedD<-NULL
CumulatedSWC<-NULL
CumulatedPAR<-NULL

for(i in 1:max(LimitFrObserved$Month)){
  CumulatedT[i]<-sum(PercentDif$TLimit[1:i])
  CumulatedD[i]<-sum(PercentDif$DLimit[1:i])
  CumulatedSWC[i]<-sum(PercentDif$SWCLimit[1:i])
  CumulatedPAR[i]<-sum(PercentDif$PARLimit[1:i])
}

ID<-c(rep("Temp",12),rep("D",12),rep("SWC",12),rep("PAR",12))
Month<-c(rep(1:12,4))
Limit<-c(CumulatedT,CumulatedD,CumulatedSWC,CumulatedPAR)

Dataframe<-data.frame(ID,as.factor(Month),Limit)

Diferences<-ggplot(Dataframe,aes(x=Month,y=Limit))+
  theme_minimal()+
  geom_line(aes(color=ID),size=1.2)+
  scale_size_manual(values=c(1))+
  scale_color_manual(values=c("grey50","wheat","steelblue","tomato"))+
  labs(y = "Cum. % Difference", 
       x = "Month",family="sans")+
  ylim(0,15)+
  theme(axis.text.x = element_text(hjust = 1, size=14,color="black",family="sans"))+
  scale_x_continuous(breaks=seq(1,12,1))+
  theme(axis.text.y = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.title.x = element_text(size=18,color="black",family="sans"))+
  theme(axis.title.y = element_text (size=18,color="black",family="sans"))+ 
  theme(legend.position = "none")

plot_grid(PlotObserved, PlotModeled, Diferences, align = "v", nrow = 3, rel_heights = c(1/9.5, 1/9.5,1/10))
