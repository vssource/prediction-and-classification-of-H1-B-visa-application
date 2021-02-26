options(scipen = 999)
library(caret)
library(dummies)
library(FNN)
library(dplyr)
library(plyr)


H1b <- read.csv(file = "h1b.csv", header = TRUE)
#considering only year 2016
H1b16 <- subset(H1b, YEAR == 2016)

#remove Column X and YEAR
H1b16 <- H1b16[,-c(1,8)]
#Checking for na
table(is.na.data.frame(H1b16))
H1b16 <- na.omit(H1b16)
#reindex
row.names(H1b16)<-1:nrow(H1b16)
q <- subset(H1b16, SOC_NAME=="N/A")
H1b16$SOC_NAME[as.numeric(row.names(q))] <- "NETWORK AND COMPUTER SYSTEMS"

#Changing status as per the requirement of the project
H1b16$STATUS <- NA
H1b16$STATUS <- ifelse(H1b16$CASE_STATUS =="WITHDRAWN", "CANT_SAY" 
                       ,(ifelse(H1b16$CASE_STATUS == "DENIED", "DENIED",
                                "APPROVED")))
H1b16$FULL_TIME_POSITION <- ifelse(H1b16$FULL_TIME_POSITION== "Y", 1, 0)

#dividing the worksite in cities and states
WORK_STATE <- as.data.frame(matrix(unlist(strsplit(H1b16[,"WORKSITE"], ", ")),ncol=2, byrow=TRUE))
H1b16$CITY <- WORK_STATE[,1]
H1b16$STATE <- WORK_STATE[,2]

#Creating Regions for Exploratory Analysis
H1b16$REGION <- "Other"
H1b16$REGION[H1b16$lon < -66 & H1b16$lon > -80 & H1b16$lat < 48 & H1b16$lat > 37 & H1b16$REGION == "Other"] <- "NorthEast"
H1b16$REGION[H1b16$lon < -72 & H1b16$lon > -102 & H1b16$lat < 37 & H1b16$lat > 24 & H1b16$REGION == "Other"] <- "South"
H1b16$REGION[H1b16$lon < -80 & H1b16$lon > -102 & H1b16$lat < 50 & H1b16$lat > 37 & H1b16$REGION == "Other"] <- "MidWest"
H1b16$REGION[H1b16$lon < -102 & H1b16$lon > -126 & H1b16$lat < 50 & H1b16$lat >25 & H1b16$REGION == "Other"] <- "West"

## creating Categories based on SOC_NAME
#faster string search, remove whitespaces and special char
H1b16$SOC_NAME <- gsub(" ","_",H1b16$SOC_NAME)
H1b16$SOC_NAME <- gsub(",","",H1b16$SOC_NAME)
H1b16$Category = "Other"
H1b16$Category <- ifelse(grepl('ENGINEER|MATHEMATICAL|STATIS', H1b16$SOC_NAME, ignore.case = T), 'Engineering', H1b16$Category)
H1b16$Category <- ifelse(grepl('computer|programmer|software|web developer|database', H1b16$SOC_NAME, ignore.case = T), 'Computer', H1b16$Category)
H1b16$Category <- ifelse(grepl('teacher|school principal|linguist|professor|teach', H1b16$SOC_NAME, ignore.case = T), 'Education', H1b16$Category)
H1b16$Category <- ifelse(grepl('public_relation|manag|operation|chief|executive|plan', H1b16$SOC_NAME, ignore.case = T), 'Management', H1b16$Category)
H1b16$Category <- ifelse(grepl('business|business_analyst|business_systems_analyst|financ|accountant', H1b16$SOC_NAME, ignore.case = T), 'Business', H1b16$Category)
H1b16$Category <- ifelse(grepl('medical|doctor|physician|dentist|health|physical therapists|surgeon|nurse|psychiatr', H1b16$SOC_NAME, ignore.case = T), 'Healthcare', H1b16$Category)
H1b16$Category <- ifelse(grepl('scient|ECONOMISTS|BIOLOGICAL_TECHNICIANS|CHEMIST|NUCLEAR|RESEARCH_ASSISTANTS', H1b16$SOC_NAME, ignore.case = T), 'Science', H1b16$Category)
H1b16$Category <- ifelse(grepl('MUSICIANS_INSTRUMENTAL|ARCHIVISTS|CURATORS|MUSEUM_TECHNICIANS_AND_CONSERVATORS|LIBRARIANS|LIBRARY_TECHNICIANS|AUDIO-VISUAL_AND_MULTIMEDIA_COLLECTIONS|FARM_AND_HOME_MANAGEMENT_ADVISORS|INSTRUCTIONAL_COORDINATORS|INSTRUCTIONAL_DESIGNERS_AND_TECHNOLOGISTS|INSTRUCTIONAL_COORDINATOR|TEACHER_ASSISTANTS|EDUCATION_TRAINING_AND_LIBRARY_WORKERS_ALL|EDUCATION_TRAINING_&_LIBRARY_WORKERS_ALL_OTHER|ART_DIRECTORS|CRAFT_ARTISTS|FINE_ARTISTS_INCLUDING_PAINTERS_SCULPTORS_AND|MULTIMEDIA_ARTISTS_AND_ANIMATORS|ARTISTS_AND_RELATED_WORKERS_ALL_OTHER|COMMERCIAL_AND_INDUSTRIAL_DESIGNERS|COMMERCIAL_AND_INDUSTRIAL_ENGINEERS|COMMERCIAL_AND_INDUSTRIAL_DEISGNERS|FASHION_DESIGNERS|FLORAL_DESIGNERS|GRAPHIC_DESIGNERS|GRAPHIC_DESIGNER|INTERIOR_DESIGNERS|INTERIOR_DESIGNER|SET_AND_EXHIBIT_DESIGNERS|DESIGNERS_ALL_OTHER|ACTORS|PRODUCERS_AND_DIRECTORS|PRODUCERS|ATHLETES_AND_SPORTS_COMPETITORS|COACHES_AND_SCOUTS|DANCERS|CHOREOGRAPHERS|MUSIC_DIRECTORS_AND_COMPOSERS|MUSICIANS_AND_SINGERS|MUSICIANS_INSTRUdata|ENTERTAINERS_AND_PERFORMERS_SPORTS_AND_RELATED|RADIO_AND_TELEVISION_ANNOUNCERS|BROADCAST_NEWS_ANALYSTS|REPORTERS_AND_CORRESPONDENTS|PUBLIC_RELATIONS_SPECIALIST|EDITORS|TECHNICAL_WRITERS|TECHNICAL_WRITER|WRITERS_AND_AUTHORS|POETS_LYRICISTS_AND_CREATIVE_WRITERS|INTERPRETERS_AND_TRANSLATORS|MEDIA_AND_COMMUNICATION_WORKERS_ALL_OTHER|MARKET_RESEARCH|AUDIO_AND_VIDEO_EQUIPMENT_TECHNICIANS|SOUND_ENGINEERING_TECHNICIANS|PHOTOGRAPHERS|CAMERA_OPERATORS_TELEVISION_VIDEO_AND_MOTION|FILM_AND_VIDEO_EDITORS|MEDIA_AND_COMMUNICATION_EQUIPMENT_WORKERS_ALL', H1b16$SOC_NAME, ignore.case = T), 'Arts', H1b16$Category)

#Check categories
table(H1b16$Category)
cat <- table(H1b16$Category)

##......... Pie Chart for Case_Status Distribution.......

library(plotrix)
status_list<- as.vector(table(H1b16$CASE_STATUS))
pieval<-status_list
piepercent<- paste0(round(100*(status_list/sum(status_list)), 2),"%")
pielabels<-
  c("CERTIFIED","CERTIFIED-WITHDRAWN ","DENIED"," WITHDRAWN")
bisectors<-pie3D(pieval,explode=0.1,main="3D PIE OPINIONS",start=45)

pie3D(pieval,radius=0.9,labels=piepercent,explode=0.1,main="CASE_STATUS DISTRIBUTION",
      labelcol=rainbow(length(status_list)),labelcex=1)
legend("topright", pielabels,cex = 0.8, fill = rainbow(length(status_list)))


###...... Creating classification tree
library(rpart)
library(rpart.plot)

## Removing "Can't say" from Status
Data.df <- subset(H1b16, STATUS!="CANT_SAY")
Data.df$Category <- as.factor(Data.df$Category)
#changing status with DENIED = 0, APPROVE = 1
Data.df$STATUS <- ifelse(Data.df$STATUS== "DENIED", 0, 1)
table(Data.df$STATUS)
str(Data.df)


###......OVERSAMPLING.......................
table(Data.df$STATUS)
approved_cases <- Data.df[Data.df$STATUS=="APPROVED",] # creating Dataframe for approved cases
dim(approved_cases)
denied_cases <- Data.df[Data.df$STATUS=="DENIED",]# creating Dataframe for denied cases
dim(denied_cases)

# Randomly slecting 8482 approved cases , same no of records as Denied cases
set.seed(1)
H1b.sample.Approved<- approved_cases[sample(nrow(approved_cases),8482),]
dim(H1b.sample.Approved)

# creating combined dataframe of oversampled data with both Case Status
H1b.sampledata <- rbind(H1b.sample.Approved,denied_cases)
dim(H1b.sampledata) 

## Classification Tree 

#changing status with DENIED = 0, APPROVE = 1
H1b.sampledata$STATUS <- ifelse(H1b.sampledata$STATUS== "DENIED", 0, 1)
table(H1b.sampledata$STATUS)
str(H1b.sampledata$STATUS)
# Selectiong variables for classification tree
H1btree.df.oversample <- H1b.sampledata[,c(5,6,8,9,10,14)]
head(H1btree.df.oversample )
dim(H1btree.df.oversample)
str(H1btree.df.oversample)

## Data partioning
set.seed(1)
train.rows.oversample <- sample(rownames(H1btree.df.oversample), dim(H1btree.df.oversample)[1]*0.6)
train.df.oversample <- H1btree.df.oversample[train.rows.oversample, ]
dim(train.df.oversample )
valid.rows.oversample <- setdiff(rownames(H1btree.df.oversample), train.rows.oversample) 
valid.df.oversample <- H1btree.df.oversample[valid.rows.oversample, ]
dim(valid.df.oversample)

## Creating classification tree with training Data
H1btree.oversample<- rpart( STATUS ~ ., data = train.df.oversample ,minsplit = 1,method = "class")
# plot tree
prp(H1btree.oversample, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

##  Predicting & generating confusion Matrix for validation data
H1btree.oversample.pred.valid <- predict(H1btree.oversample,valid.df.oversample,type = "class")
confusionMatrix(H1btree.oversample.pred.valid, as.factor(valid.df.oversample$STATUS))

## Pruning the tree
H1btree.ct.oversample<- rpart( STATUS ~ ., data = train.df.oversample,cp = 0.00001, minsplit = 10,xval=5,method = "class")
printcp(H1btree.ct.oversample)
# selecting cp = 0.003473588to get better pruned tree
#Find a better parsimonous pruned tree
H1bpruned.ct.oversample <- prune(H1btree.ct.oversample, cp =   0.003473588)

#Ploting the pruned tree
options(scipen=999)
prp(H1bpruned.ct.oversample, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(H1bpruned.ct.oversample$frame$var == "<leaf>", 'gray', 'white')) 

H1bpruned.pred.oversample <- predict(H1bpruned.ct.oversample,valid.df.oversample,type = "class")
## Generating Confusion Matrix for accuracy
confusionMatrix(H1bpruned.pred.oversample , as.factor(valid.df.oversample$STATUS))

## Random Forest
library(randomForest)
## random forest
H1brf.oversample <- randomForest(as.factor(STATUS) ~ ., data = train.df.oversample, ntree = 100, importance = TRUE)  

## variable importance plot
varImpPlot(H1brf.oversample , type = 1)
## Generating Confusion Matrix for accuracy
H1brf.pred.oversample <- predict(H1brf.oversample ,valid.df.oversample,type = "class")
confusionMatrix(H1brf.pred.oversample, as.factor(valid.df.oversample$STATUS))


###### Boosted Trees

library(adabag)
library(rpart) 
library(caret)

train.df.oversample$STATUS <- as.factor(train.df.oversample$STATUS)

set.seed(1)
H1bboost.oversample <- boosting(STATUS ~ ., data = train.df.oversample)

#Predict using Valid data

LoanBoost.pred.oversample<- predict(H1bboost.oversample,valid.df.oversample,type = "class")
# generate confusion matrix for training data
confusionMatrix(as.factor(LoanBoost.pred.oversample$class), as.factor(valid.df.oversample$STATUS))


#NN
library(scales)
library(neuralnet)
train.NN <- train.df.oversample 
valid.NN <- valid.df.oversample
train.NN$STATUS <- as.factor(train.NN$STATUS)
train.NN <- dummy.data.frame(train.NN, sep=".")
#valid.NN$STATUS <- ifelse (valid.NN$STATUS==1, "APPROVED","DENIED")
valid.NN$STATUS <- as.factor(valid.NN$STATUS)
valid.NN <- dummy.data.frame(valid.NN, sep=".")
t(t(names(valid.NN)))
for(i in 2:4){
  train.NN[,i] <- rescale(train.NN[,i])
}

for(i in 2:4){
  valid.NN[,i] <- rescale(valid.NN[,i])
}
summary(valid.NN)
dim(train.NN)
n <- names(train.NN)
n
#f <- as.formula(paste("STATUS.1+STATUS.0 ~", paste(n[!n %in% c("STATUS.APPROVED","STATUS.DENIED")], collapse = " + ")))
#f
datann <- neuralnet(STATUS.0 +STATUS.1 ~ ., data =train.NN,threshold=0.01,stepmax = 1e+05, linear.output = F, hidden = 3)
#datann <- neuralnet(STATUS.APPROVED+STATUS.DENIED ~ ., data =train.NN, linear.output = F, hidden = 3)
plot(datann, rep="best")
library(caret)
Likepredict <- compute(datann, valid.NN[,-c(5,6)])
Likepredict
#predicted.class=apply(Likepredict$net.result,1,which.max)-1 #Predicted denied
#predicted.class 
#confusionMatrix(as.factor(valid.NN$STATUS.DENIED),as.factor(predicted.class))
#class(valid.NN$STATUS.DENIED)
#class(predicted.class)
predict.approved <- ifelse(Likepredict$net.result[,1]>0.5,1,0)
confusionMatrix(as.factor(valid.NN$STATUS.1),as.factor(predict.approved))











