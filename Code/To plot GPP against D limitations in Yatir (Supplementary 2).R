#This plot compares the Observed and Simulated vapor pressure deficit limitations over GPP
#Data  in Yatir, both Observed and Simulated.

#Packages:
library(dplyr);library(ggplot2)

#Functions
load_object <- function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  tmp[[ls(tmp)[1]]]
}

#################################################################

Data<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Yatir_CombinedLimitations.rda")

Observed<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Yatir_Observed.rda")
Modeled<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Yatir_LDNDC.rda")

a<-which(Data$Month>=4 & Data$Month<=5 & Modeled$SWC>18)
Na<-c(which(is.na(Observed$GPP)),which(is.na(Observed$D)))

a<-unique(a,Na)

plot(Observed$GPP[a]~Observed$D[a])
plot(Modeled$GPP[a]~Modeled$D[a],col="red")

DataObs<-data.frame(GPP=Observed$GPP[a],D=Observed$D[a],DLimit=Data$Observed$DLimitObs[a])
DataMod<-data.frame(GPP=Modeled$GPP[a],D=Modeled$D[a],DLimit=Data$Modeled$DLimitObs[a])

Type<-c(rep("Obs",length(a)),rep("Mod",length(a)))
NewData<-rbind(DataObs,DataMod)
NewData$Type<-Type

ggplot(NewData,aes(x=D,y=GPP,shape=Type))+
  geom_point(aes(size=DLimit,fill=Type))+
  scale_fill_manual(values=c("tomato","steelblue"))+
  scale_shape_manual(values=c(21, 24))+
  theme_minimal()+
  labs(y = expression(GPP~(kg~ha^{-1})), 
       x = "D (kPa)",family="sans")+
  theme(axis.text.x = element_text(hjust = 1, size=14,color="black",family="sans"))+
  scale_x_continuous(breaks=seq(1,12,1))+
  theme(axis.text.y = element_text(hjust = 1, size=14,color="black",family="sans"))+
  theme(axis.title.x = element_text(size=18,color="black",family="sans"))+
  theme(axis.title.y = element_text (size=18,color="black",family="sans"))+ 
  theme(legend.position = "none")

