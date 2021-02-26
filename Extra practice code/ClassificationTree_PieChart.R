
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
# Selectiong variables for classification tree
H1btree.df <- Data.df[,c(5,6,8,9,10,14)]
head(H1btree.df )
dim(H1btree.df)
str(H1btree.df)
## Data partioning
set.seed(1)
train.rows1 <- sample(rownames(H1btree.df), dim(H1btree.df)[1]*0.6)
train.df1 <- H1btree.df[train.rows1, ]
dim(train.df1)
valid.rows1 <- setdiff(rownames(H1btree.df), train.rows1) 
valid.df1 <- H1btree.df[valid.rows1, ]
dim(valid.df1)


## Creating classification tree with training Data
H1btree<- rpart( STATUS ~ ., data = train.df1,minsplit = 1,method = "class")
# plot tree
prp(H1btree, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

##  Predicting & generating confusion Matrix for validation data
H1btree.pred.valid <- predict(H1btree,valid.df1,type = "class")
confusionMatrix(H1btree.pred.valid, as.factor(valid.df1$STATUS))

## Pruning the tree
H1btree.ct<- rpart( STATUS ~ ., data = train.df1,cp = 0.00001, minsplit = 10,xval=5,method = "class")
printcp(H1btree.ct)
# selecting cp =0.000593237 to get better pruned tree
#Find a better parsimonous pruned tree
H1bpruned.ct <- prune(H1btree.ct, cp =  0.000593237)

#Ploting the pruned tree
options(scipen=999)
prp(H1bpruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(H1bpruned.ct$frame$var == "<leaf>", 'gray', 'white')) 

H1bpruned.pred.valid <- predict(H1bpruned.ct,valid.df1,type = "class")
## Generating Confusion Matrix for accuracy
confusionMatrix(H1bpruned.pred.valid, as.factor(valid.df1$STATUS))

## Random Forest
library(randomForest)
## random forest
H1brf <- randomForest(as.factor(STATUS) ~ ., data = train.df1, ntree = 100, importance = TRUE)  

## variable importance plot
varImpPlot(H1brf, type = 1)
## Generating Confusion Matrix for accuracy
H1brf.pred.valid <- predict(H1brf,valid.df1,type = "class")
confusionMatrix(H1brf.pred.valid, as.factor(valid.df1$STATUS))

