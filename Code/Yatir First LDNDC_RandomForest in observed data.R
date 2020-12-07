#Apply the Random Forest algorithm, and then to plot the results, both for the time-series and the comparison against measurements
#Observed data in Yatir.

#### Load packages:

library (randomForest);library(ggplot2);library(cowplot);library(caret)

#### Functions:

#Function to account for temporal autocorrelation in linear effects.
LinearRegressionNoTempAutocorrelation<-function(x,y,lag.max=30,iterations=1000){
  
  #Defining the variables to fill
  Coefficients<-NULL
  R2<-NULL
  RMSE<-NULL
  Sequentia<-NULL
  Output<-vector("list")
  
  #We will need Sirad for model evaluation
  require(sirad)
  
  #We firstly calculate the autocorrelation in the observed data
  Autocorrelation<-acf(y,lag.max=lag.max)$acf
  Score<-sqrt((Autocorrelation-(1.96/sqrt(length(y))))^2)
  
  if(min(which(Score<(1.96/sqrt(length(y)))))!="Inf"){
    MinNoAutocorrelation<-1+min(which(Score<(1.96/sqrt(length(y)))))
  }else{
    MinNoAutocorrelation<-1+which(Score==min(Score,na.rm=T))
    print("Warning, there is stil autocorrelation whithin the data analyzed")
  }
  
  #Now we generate the descriptors by sampling the Observed data based on the MinNo Autocorrelation
  for(i in 1:iterations){
    RandomNumber<-sample(size=1,length(y))
    Sample<-c(seq(RandomNumber,length(y),MinNoAutocorrelation),
              seq(RandomNumber,1,-MinNoAutocorrelation))
    LTest<-lm(y[Sample]~x[Sample])
    Coefficients<-rbind(Coefficients,coef(LTest))
    R2<-c(R2,round(summary(LTest)$r.squared,2))
    RMSE<-c(RMSE,round(modeval(y[Sample],x[Sample])$RMSE,2))
  }
  
  #Here we generate a sequentia of numbers focussed on plotting
  CheckSequentia<-seq(min(x,na.rm=T),max(x,na.rm=T),(max(x,na.rm=T)-min(x,na.rm=T))/100)
  
  for(i in 1:iterations){
    RandomCoef1<-sample(size=1,Coefficients[,1])
    RandomCoef2<-sample(size=1,Coefficients[,2])
    LM<-RandomCoef1 + RandomCoef2*CheckSequentia
    Sequentia<-rbind(Sequentia,LM)  
  }
  
  Mean<-colMeans(Sequentia)
  CIDown<-apply(Sequentia, 2, quantile, 0.025)
  CIUP<-apply(Sequentia, 2, quantile, 0.975)
  
  MeanR2<-mean(R2)
  MeanRMSE<-mean(RMSE)
  CIR2<-1.96*sd(R2)/sqrt(length(y))
  CIRMSE<-1.96*sd(RMSE)/sqrt(length(y))
  MeanIntercept<-mean(Coefficients[,1])
  MeanSlope<-mean(Coefficients[,2])
  CIIntercept<-c(quantile(Coefficients[,1],0.025),quantile(Coefficients[,1],0.975))
  CISlope<-c(quantile(Coefficients[,2],0.025),quantile(Coefficients[,2],0.975))
  ToPlot<-vector("list")
  ToPlot[[1]]<-CheckSequentia
  ToPlot[[2]]<-Mean
  ToPlot[[3]]<-CIDown
  ToPlot[[4]]<-CIUP
  names(ToPlot)<-c("x","Mean","CIDown","CIUP")
  Simulated<-x
  Observed<-y
  
  Output<-list(MeanR2=round(MeanR2,2),MeanRMSE=round(MeanRMSE,2),CIR2=round(CIR2,4),cIRMSE=round(CIRMSE,2),
               Intercept=round(MeanIntercept,2),Slope=round(MeanSlope,2),CIIntercept=round(CIIntercept,2),
               CISlope=round(CISlope,2),ToPlot=ToPlot,Simulated=Simulated,Observed=Observed)
  
  return(Output)
}

#To plot the object obtained by the linear detrending.
PlotLinearModel<-function(ToPlot,Minylim=NA,Maxylim=NA,color="tomato"){
  
  Font<<- par(family = "sans")
  
  Max.y<-max(c(ToPlot$Observed,ToPlot$ToPlot$CIUP),na.rm=T)
  Min.y<-min(c(ToPlot$Observed,ToPlot$ToPlot$CIDown),na.rm=T)
  
  if(is.na(Minylim)==TRUE){
    Minylim<-Min.y
  }else{
    Minylim<-Minylim}
  
  if(is.na(Maxylim)==TRUE){
    Maxylim<-Max.y
  }else{
    Maxylim<-Maxylim}
  
  
  plot(ToPlot$Observed~ToPlot$Simulated,pch=21,cex=0.5,las=1,cex.axis=1.5,
       ylim=c(Minylim,Maxylim),xlim=c(Minylim,Maxylim),xlab="Simulated",ylab="Observed",type="n")
  Dummy<-ToPlot$Observed
  abline(lm(ToPlot$Observed~Dummy),lty=3)
  
  XBackandForth<-c(ToPlot$ToPlot$x,rev(ToPlot$ToPlot$x))
  YBackandForth<-c(ToPlot$ToPlot$CIDown,rev(ToPlot$ToPlot$CIUP))
  
  polygon(XBackandForth,YBackandForth,col=adjustcolor("grey80",alpha=0.7),
          adjustcolor("grey80",alpha=0.7))
  points(ToPlot$Observed~ToPlot$Simulated,pch=21,
         col=color,bg=adjustcolor(color,alpha=0.5),cex=1.5)
  points(ToPlot$ToPlot$Mean~ToPlot$ToPlot$x,type="l",col="black",lwd=2)
  legend("topleft",c(paste("R2 = ", ToPlot$MeanR2, sep=""),
                     paste("RMSE = ", ToPlot$MeanRMSE, sep=""),
                     paste("f(x) = ", as.character(ToPlot$Intercept), " [",ToPlot$CIIntercept[1],"-",
                           ToPlot$CIIntercept[2],"] + ",ToPlot$Slope,
                           " [",ToPlot$CISlope[1],"-",ToPlot$CISlope[2],"] x",sep=""),
                     paste("N = ",length(ToPlot$Observed),sep="")),bty="n")
  
  par(Font)
}

load_object <- function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  tmp[[ls(tmp)[1]]]
}

#####################################################

####Load the input data:

DataFrameJoint<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Yatir_LDNDC.rda")

#Here we run the RandomForest on the data and we analyze it.

#firstly we prepare the data

a<-which(DataFrameJoint$Year>=2013&DataFrameJoint$Year<=2015)

Dataset<-data.frame(DataFrameJoint$GPP[a],DataFrameJoint$FSWC30[a],DataFrameJoint$D[a],DataFrameJoint$PAR[a],DataFrameJoint$Tmean[a])
names(Dataset)<-c("GPP","FSWC30","D","PAR","Tmean")

#We remove the NA's in the entire Dataset. With unique we are removing just one of the registers for each NA (repeated NA's at the same row)
a<-unique(c(which(is.na(Dataset$GPP)),which(is.na(Dataset$D)),which(is.na(Dataset$PAR)),which(is.na(Dataset$Tmean)),which(is.na(Dataset$FSWC30))))

if(min(a)!="Inf"){DataFill<-Dataset[-a,]}else{DataFill<-Dataset}

#Now we will split the data into training data and validation data.

set.seed(13) #This fixes the random values to a controlled ones

#We use the 75% of data to create the model, and the 25% for validate it.
Sample<-sample(2,nrow(DataFill),replace=TRUE,prob=c(0.75,0.25))
Train<-DataFill[which(Sample==1),]
Validate<-DataFill[which(Sample==2),]

Model<-randomForest(GPP~.,ntree=1000,data=Train,mtry=2,importance=1) #mtry is the number of variables used for each classification

importance(Model, type=1)
importance(Model)
varImpPlot(Model,type=1)

################################################
################################################

##### Figure 1 a,b

#Representation pannel

#Observed and modeled time-series.

ObservedGPP<-DataFrameJoint$GPP
ModeledGPP<-predict(Model,DataFrameJoint)
ID<-c(1:length(ObservedGPP))

DataFrame<-data.frame(cbind(ObservedGPP,ModeledGPP,ID))
names(DataFrame)<-c("Obs","Mod","ID")

ggplot(data=DataFrame,aes(x=ID,y=Obs))+
  geom_line(aes(y=Obs),color="black")+
  geom_vline(xintercept = (1*365),linetype="dashed")+
  geom_vline(xintercept = (2*365),linetype="dashed")+
  geom_point(aes(y=Mod),color="darkred",type=20)+
  theme_classic() +
  labs(y=expression(GPP~(kg~ha^{-1})), x = "Days since 1/1/2013")+
  theme(axis.text.x = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.title = element_text ( size=20,color="black",family="sans"))+
  annotate("text", label="2013",x=250, y=70, 
           hjust=1, size=7, vjust=1,family="sans")+
  annotate("text", label="2014",x=250+365, y=70, 
           hjust=1, size=7, vjust=1,family="sans")+
  annotate("text", label="2015",x=250+365+365, y=70, 
           hjust=1, size=7, vjust=1,family="sans")+
  annotate("text", label="Observed",x=360+365+365, y=40, 
           hjust=1, size=7, vjust=1,family="sans")+
  annotate("text", label="Modeled",x=360+365+365, y=37, 
           hjust=1, size=7, vjust=1,family="sans")

####################################################################
Predicted<-predict(Model,Validate)

DataPlot<-data.frame(Predicted,Validate$GPP)
names(DataPlot)<-c("Modeled","Observed")

#To obtain the detrended linear regression and to plot it

ToPlot<-LinearRegressionNoTempAutocorrelation(DataPlot$Modeled,
                                              DataPlot$Observed,
                                              lag.max=50,
                                              iterations=1000)
PlotLinearModel(ToPlot,color="steelblue")
