library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggExtra)
library(cowplot)


Yatir<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Yakir_Observed.rda")
Yatir$Month<-rep(c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),
                   rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31)),3)

Hyytiala<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Hyytiala_Observed.rda")

Hyytiala$Month<-c(rep(1,31),rep(2,29),rep(3,31),rep(4,30),rep(5,31),rep(6,30),
           rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31),
         rep(c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),
               rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31)),2))


MonthYatir<-Yatir%>%
  group_by(Month)%>%
  summarise(MeanT = mean(Tmean,na.rm=T),
            SDT = sd(Tmean,na.rm=T),
            MeanSWC = mean(SWC,na.rm=T),
            SDSWC = sd(SWC,na.rm=T),
            MeanD = mean(D,na.rm=T),
            SDD = sd(D,na.rm=T),
            MeanPAR = mean(PAR,na.rm=T),
            SDPAR = sd(PAR,na.rm=T))
                                       
MonthHyytiala<-Hyytiala%>%
  group_by(Month)%>%
  summarise(MeanT = mean(Tmean,na.rm=T),
  SDT = sd(Tmean,na.rm=T),
  MeanSWC = mean(SWC,na.rm=T),
  SDSWC = sd(SWC,na.rm=T),
  MeanD = mean(D,na.rm=T),
  SDD = sd(D,na.rm=T),
  MeanPAR = mean(PAR,na.rm=T),
  SDPAR = sd(PAR,na.rm=T))

CombinedPlot<-rbind(MonthYatir,MonthHyytiala)

CombinedPlot$Plot<-c(rep("Yatir",12),rep("Hyytiala",12))


  Tplot<-ggplot(CombinedPlot,aes(x=Month))+
  theme_minimal()+
  geom_line(aes(y=MeanT,color=Plot),size=1.5)+
  scale_color_manual(values=c("steelblue","tomato"))+
  geom_ribbon(aes(ymin=MeanT-SDT, ymax=MeanT+SDT, color=Plot), alpha=0.2,linetype="blank")+
    labs(y = "Temperature (°C)", 
    x = "Month",family="sans")+
    theme(axis.text.x = element_text(hjust = 1, size=14,color="black",family="sans"))+
    scale_x_continuous(breaks=seq(1,12,1))+
    theme(axis.text.y = element_text(hjust = 1, size=14,color="black",family="sans"))+
    theme(axis.title.x = element_text(size=16,color="black",family="sans"))+
    theme(axis.title.y = element_text (size=16,color="black",family="sans"))+ 
    theme(legend.position = "none")
  
  Dplot<-ggplot(CombinedPlot,aes(x=Month))+
    theme_minimal()+
    geom_line(aes(y=MeanD,color=Plot),size=1.5)+
    geom_ribbon(aes(ymin=MeanD-SDD, ymax=MeanD+SDD, color=Plot), alpha=0.2,linetype="blank")+
    labs(y = "D (kPa)", 
         x = "Month",family="sans")+
    scale_color_manual(values=c("steelblue","tomato"))+
    theme(axis.text.x = element_text(hjust = 1, size=14,color="black",family="sans"))+
    scale_x_continuous(breaks=seq(1,12,1))+
    theme(axis.text.y = element_text(hjust = 1, size=14,color="black",family="sans"))+
    theme(axis.title.x = element_text(size=16,color="black",family="sans"))+
    theme(axis.title.y = element_text (size=16,color="black",family="sans"))+ 
    theme(legend.position = "none")
  
  SWCplot<-ggplot(CombinedPlot,aes(x=Month))+
    theme_minimal()+
    geom_line(aes(y=MeanSWC,color=Plot),size=1.5)+
    geom_ribbon(aes(ymin=MeanSWC-SDSWC, ymax=MeanSWC+SDSWC, color=Plot), alpha=0.2,linetype="blank")+
    labs(y = "SWC (%)", 
         x = "Month",family="sans")+
    scale_color_manual(values=c("steelblue","tomato"))+
    theme(axis.text.x = element_text(hjust = 1, size=14,color="black",family="sans"))+
    scale_x_continuous(breaks=seq(1,12,1))+
    theme(axis.text.y = element_text(hjust = 1, size=14,color="black",family="sans"))+
    theme(axis.title.x = element_text(size=16,color="black",family="sans"))+
    theme(axis.title.y = element_text (size=16,color="black",family="sans"))+ 
    theme(legend.position = "none")
  
  PARplot<-ggplot(CombinedPlot,aes(x=Month))+
    theme_minimal()+
    geom_line(aes(y=MeanPAR,color=Plot),size=1.5)+
    geom_ribbon(aes(ymin=MeanPAR-SDPAR, ymax=MeanPAR+SDPAR, color=Plot), alpha=0.2,linetype="blank")+
    labs(y = expression(paste("PAR (",mu, "E ", m^{-2}, " ", s^{-1},")", sep="")), 
         x = "Month",family="sans")+
    scale_color_manual(values=c("steelblue","tomato"))+
    theme(axis.text.x = element_text(hjust = 1, size=14,color="black",family="sans"))+
    scale_x_continuous(breaks=seq(1,12,1))+
    theme(axis.text.y = element_text(hjust = 1, size=14,color="black",family="sans"))+
    theme(axis.title.x = element_text(size=16,color="black",family="sans"))+
    theme(axis.title.y = element_text (size=16,color="black",family="sans"))+ 
    theme(legend.position = "none")
  
  plot_grid(Tplot, Dplot,SWCplot,PARplot, align = "v", nrow = 2)





  