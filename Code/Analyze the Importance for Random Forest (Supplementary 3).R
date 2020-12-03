#Author: Daniel Nadal-Sala

#####Packages:
library (randomForest)
library(ggplot2)
library(cowplot)
library(caret) #This is used to predict

#####Functions:
load_object <- function(file) {
  tmp <- new.env()
  load(file = file, envir = tmp)
  tmp[[ls(tmp)[1]]]
}

#####Code

DataFrameJoint<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Yakir_Observed.rda")

Dataset<-DataFrameJoint

Dataset$SWC<-NULL

#We remove the NA's in the entire Dataset. With unique we are removing just one of the registers for each NA (repeated NA's at the same row)
a<-unique(c(which(is.na(Dataset$GPP)),which(is.na(Dataset$D)),which(is.na(Dataset$PAR)),which(is.na(Dataset$Tmean)),which(is.na(Dataset$FSWC30))))

if(min(a)!="Inf"){DataFill<-Dataset[-a,]}else{DataFill<-Dataset}

#Now we will split the data into training data and validation data.

set.seed(13)

Iterations<-100

Importance_array<-NULL
for (i in 1:Iterations){
  #We take 200 random samples and fit a Random Forest algorithm to each one of them.
Sample<-sample(nrow(DataFill),200,replace=FALSE,prob=NULL)

Train<-DataFill[Sample,]

Model<-randomForest(GPP~.,ntree=1000,data=Train,mtry=2,importance=1) #mtry is the number of variables used for each classification

Importance<-c(importance(Model, type=1))

Importance_array<-rbind(Importance_array,Importance)

}

Names<-row.names(importance(Model, type=1))

Importance_array<-data.frame(Importance_array)
names(Importance_array)<-Names

#Here we will plot the importance of the different variables after 100 random samples.

ggplot(Importance_array)+
  theme_minimal()+
  geom_density(aes(x=FSWC30,y=..density..),alpha=.5, fill="steelblue",color="transparent")+
  xlim(0,130)+
  geom_density(aes(x=PAR,y=..density..),alpha=.5, fill="tan",color="transparent")+
  geom_density(aes(x=Tmean,y=..density..),alpha=.5, fill="tomato",color="transparent")+
  geom_density(aes(x=D,y=..density..),alpha=.5, fill="grey20",color="transparent")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=20,color="black",family="sans"))


##### The same analysis for the LDNDC outputs

DataFrameJoint<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Yakir_LDNDC.rda")

#firstly we prepare the data

a<-which(DataFrameJoint$Year>=2013&DataFrameJoint$Year<=2015)

Dataset<-data.frame(DataFrameJoint$GPP[a],DataFrameJoint$FSWC30[a],DataFrameJoint$D[a],DataFrameJoint$PAR[a],DataFrameJoint$Tmean[a])
names(Dataset)<-c("GPP","FSWC30","D","PAR","Tmean")

#We remove the NA's in the entire Dataset. With unique we are removing just one of the registers for each NA (repeated NA's at the same row)
a<-unique(c(which(is.na(Dataset$GPP)),which(is.na(Dataset$D)),which(is.na(Dataset$PAR)),which(is.na(Dataset$Tmean)),which(is.na(Dataset$FSWC30))))

if(min(a)!="Inf"){DataFill<-Dataset[-a,]}else{DataFill<-Dataset}

#Now we will split the data into training data and validation data.

set.seed(13)

Iterations<-100

Importance_array<-NULL
for (i in 1:Iterations){
  #We take 200 random samples and fit a Random Forest algorithm to each one of them.
  Sample<-sample(nrow(DataFill),200,replace=FALSE,prob=NULL)
  
  Train<-DataFill[Sample,]
  
  Model<-randomForest(GPP~.,ntree=1000,data=Train,mtry=2,importance=1) #mtry is the number of variables used for each classification
  
  Importance<-c(importance(Model, type=1))
  
  Importance_array<-rbind(Importance_array,Importance)
  
}

Names<-row.names(importance(Model, type=1))

Importance_array<-data.frame(Importance_array)
names(Importance_array)<-Names

#Here we will plot the importance of the different variables after 100 random samples.

ggplot(Importance_array)+
  theme_minimal()+
  geom_density(aes(x=FSWC30,y=..density..),alpha=.5, fill="steelblue",color="transparent")+
  xlim(0,130)+
  geom_density(aes(x=PAR,y=..density..),alpha=.5, fill="tan",color="transparent")+
  geom_density(aes(x=Tmean,y=..density..),alpha=.5, fill="tomato",color="transparent")+
  geom_density(aes(x=D,y=..density..),alpha=.5, fill="grey20",color="transparent")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=20,color="black",family="sans"))


###For Hyytiala


DataFrameJoint<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Hyytiala_Observed.rda")

Dataset<-DataFrameJoint

Dataset$SWC<-NULL

#We remove the NA's in the entire Dataset. With unique we are removing just one of the registers for each NA (repeated NA's at the same row)
a<-unique(c(which(is.na(Dataset$GPP)),which(is.na(Dataset$D)),which(is.na(Dataset$PAR)),which(is.na(Dataset$Tmean)),which(is.na(Dataset$FSWC30))))

if(min(a)!="Inf"){DataFill<-Dataset[-a,]}else{DataFill<-Dataset}

#Now we will split the data into training data and validation data.

set.seed(13)

Iterations<-100

Importance_array<-NULL
for (i in 1:Iterations){
  #We take 200 random samples and fit a Random Forest algorithm to each one of them.
  Sample<-sample(nrow(DataFill),200,replace=FALSE,prob=NULL)
  
  Train<-DataFill[Sample,]
  
  Model<-randomForest(GPP~.,ntree=1000,data=Train,mtry=2,importance=1) #mtry is the number of variables used for each classification
  
  Importance<-c(importance(Model, type=1))
  
  Importance_array<-rbind(Importance_array,Importance)
  
}

Names<-row.names(importance(Model, type=1))

Importance_array<-data.frame(Importance_array)
names(Importance_array)<-Names

#Here we will plot the importance of the different variables after 100 random samples.

ggplot(Importance_array)+
  theme_minimal()+
  geom_density(aes(x=FSWC30,y=..density..),alpha=.5, fill="steelblue",color="transparent")+
  xlim(0,80)+
  ylim(0,0.110)+
  geom_density(aes(x=PAR,y=..density..),alpha=.5, fill="tan",color="transparent")+
  geom_density(aes(x=Tmean,y=..density..),alpha=.5, fill="tomato",color="transparent")+
  geom_density(aes(x=D,y=..density..),alpha=.5, fill="grey20",color="transparent")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=20,color="black",family="sans"))


##### The same analysis for the LDNDC outputs

DataFrameJoint<-load_object("C:/Users/sala-d/Desktop/Yatir Forest Paper/Data/Hyytiala_LDNDC.rda")

#firstly we prepare the data

a<-which(DataFrameJoint$Year>=2013&DataFrameJoint$Year<=2015)

Dataset<-data.frame(DataFrameJoint$GPP[a],DataFrameJoint$FSWC30[a],DataFrameJoint$D[a],DataFrameJoint$PAR[a],DataFrameJoint$Tmean[a])
names(Dataset)<-c("GPP","FSWC30","D","PAR","Tmean")

#We remove the NA's in the entire Dataset. With unique we are removing just one of the registers for each NA (repeated NA's at the same row)
a<-unique(c(which(is.na(Dataset$GPP)),which(is.na(Dataset$D)),which(is.na(Dataset$PAR)),which(is.na(Dataset$Tmean)),which(is.na(Dataset$FSWC30))))

if(min(a)!="Inf"){DataFill<-Dataset[-a,]}else{DataFill<-Dataset}

#Now we will split the data into training data and validation data.

set.seed(13)

Iterations<-100

Importance_array<-NULL
for (i in 1:Iterations){
  #We use the 75% of data to create the model, and the 25% for validate it.
  Sample<-sample(nrow(DataFill),200,replace=FALSE,prob=NULL)
  
  Train<-DataFill[Sample,]
  
  Model<-randomForest(GPP~.,ntree=1000,data=Train,mtry=2,importance=1) #mtry is the number of variables used for each classification
  
  Importance<-c(importance(Model, type=1))
  
  Importance_array<-rbind(Importance_array,Importance)
  
}

Names<-row.names(importance(Model, type=1))

Importance_array<-data.frame(Importance_array)
names(Importance_array)<-Names

#Here we will plot the importance of the different variables after 100 random samples.

ggplot(Importance_array)+
  theme_minimal()+
  geom_density(aes(x=FSWC30,y=..density..),alpha=.5, fill="steelblue",color="transparent")+
  xlim(0,80)+
  geom_density(aes(x=PAR,y=..density..),alpha=.5, fill="tan",color="transparent")+
  geom_density(aes(x=Tmean,y=..density..),alpha=.5, fill="tomato",color="transparent")+
  geom_density(aes(x=D,y=..density..),alpha=.5, fill="grey20",color="transparent")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(hjust = 1, size=20,color="black",family="sans"))+
  theme(axis.text.y = element_text(hjust = 1, size=20,color="black",family="sans"))
