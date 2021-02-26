options(scipen=999)
library(dummies)
library(forecast)
library(gains)
library(reshape) 
library(leaps)  
library(caret) 
library(FNN) 
library(ROSE)
library(neuralnet)
library(scales)
library(rpart) 
library(rpart.plot)
library(randomForest)
library(dplyr)
library(plyr)
library(treemap)
library(tidyverse)
library(adabag)
library(hrbrthemes)
library(viridis)
library(ggExtra)
options(scipen=999)



H1b <- read.csv(file = "h1b_kaggle.csv", header = TRUE)
H1b16 <- subset(H1b, YEAR == 2016)
dim(H1b16)
H1b16 <- H1b16[,-c(1,8)]
summary(H1b16)

#Checking for na
table(is.na.data.frame(H1b16))
H1b16 <- na.omit(H1b16)
row.names(H1b16) <- 1:nrow(H1b16)

q <- subset(H1b16, SOC_NAME=="N/A")
s <- as.numeric(row.names(q))
H1b16$SOC_NAME[s] <- "NETWORK AND COMPUTER SYSTEMS"


#Changing status as per the requirement of the project
H1b16$STATUS <- ifelse(H1b16$CASE_STATUS =="WITHDRAWN", "CANT_SAY"
                       ,(ifelse(H1b16$CASE_STATUS == "DENIED", "DENIED",
                                "APPROVED")))

H1b16$FULL_TIME_POSITION <- ifelse(H1b16$FULL_TIME_POSITION== "Y", 1, 0)
head(H1b16$FULL_TIME_POSITION)


# Bar Plot for Case Status Distribution
counts <- table(H1b16$STATUS)
barplot(counts, main="Distribution of Status Labels",xlab="Case Status", 
        col = rainbow(3))


#dividing the worksite in cities and states
WORK_STATE <- as.data.frame(matrix(unlist(strsplit(H1b16[,"WORKSITE"], ", ")),ncol=2, byrow=TRUE))
CITY <- WORK_STATE[,1]
STATE <- WORK_STATE[,2]
H1b16 <- cbind(H1b16,STATE,CITY)
head(H1b16)

#Creating regions based on latitudes and longitudes
H1b16$REGION <- "Other"
H1b16$REGION[H1b16$lon < -66 & H1b16$lon > -80 & H1b16$lat < 48 & H1b16$lat > 37 & H1b16$REGION == "Other"] <- "NorthEast"
H1b16$REGION[H1b16$lon < -72 & H1b16$lon > -102 & H1b16$lat < 37 & H1b16$lat > 24 & H1b16$REGION == "Other"] <- "South"
H1b16$REGION[H1b16$lon < -80 & H1b16$lon > -102 & H1b16$lat < 50 & H1b16$lat > 37 & H1b16$REGION == "Other"] <- "MidWest"
H1b16$REGION[H1b16$lon < -102 & H1b16$lon > -126 & H1b16$lat < 50 & H1b16$lat >25 & H1b16$REGION == "Other"] <- "West"

#Creating Categories based on different industries
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

Data <- H1b16

# Pivot tables
summary(H1b16$PREVAILING_WAGE) 

# Creating 3 bins for prevailing wage
H1b16.bin <- H1b16

bin <- c(0, 60000, 100000, Inf)
names <- c("Low", "Medium", "High")
H1b16.bin$PREVAILING_WAGE.bin <- cut(H1b16.bin$PREVAILING_WAGE,breaks = bin,labels = names)

ggplot(data = as_tibble(H1b16.bin$PREVAILING_WAGE.bin), mapping = aes(x=value,fill=value)) + 
  geom_bar(alpha=0.7) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  stat_count(geom="text", aes(label=sprintf("%.4f",..count../length(H1b16.bin$PREVAILING_WAGE.bin))), vjust=-0.5) +
  labs(title ='Distribution of Applicants vs Wage', x='Prevailing Wage', y='No. of Applicants') +
  theme_minimal()



#Our Project is concentrated on suggesting whether an H1B application will be approved only in
#categories: Arts, Business, Computer, Education, Engineering, Healthcare,Others, 
#Management and Science. 
data.df<- subset(Data, STATUS!="CANT_SAY")
dim(data.df)
row.names(data.df) <- 1:nrow(data.df)
data <- data.df
table(data.df$Category)
data.denied <- data[data.df$CASE_STATUS == "DENIED",]



#Distribution of H1B Categories
cat <- c("Business","Computer","Education","Engineering","Healthcare","Management","Other","Science", "Arts")
num <- c(25952,415687,9743,33163,13000,41433,31014,20261,18071)
df <- data.frame("Cat" = cat, "Num" = num)
treemap(dtf = df,
        # data
        index="Cat",
        vSize="Num",
        type="index",
        
        # Main
        title="Distribution of H1B Applications by Industry",
        palette="Dark2",
        
        # Borders:
        border.col=c("black"),             
        border.lwds=1,                         
        
        # Labels
        fontsize.labels=1,
        fontcolor.labels="white",
        fontface.labels=1,            
        bg.labels=c("transparent"),              
        align.labels=c("left", "top"),                                  
        overlap.labels=0.5,
        inflate.labels=T                        # If true, labels are bigger when rectangle is bigger.
)

#For the analysis purposes we are taking Prevailing wage upto 200000K.
data.wage <- subset(data.df, PREVAILING_WAGE <=200000)
table(data.wage$FULL_TIME_POSITION)
data.full <- subset(data.df, FULL_TIME_POSITION == 1)
#Prevailing wage vs Full_Time_Position
ggplot(data = data.wage, aes(x = PREVAILING_WAGE, color = FULL_TIME_POSITION, fill = FULL_TIME_POSITION)) +
  geom_histogram(bins = 30) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 15),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~Category, scale="free_y")+
  geom_vline(data = data.wage, aes(xintercept=median(PREVAILING_WAGE)),color="blue", linetype="dashed", size=1)+
  labs(title = "Wage Distribution by Industry", subtitle = "Grouped by Full-time and Part-time",fill = "Full Time")+
  xlab("Expected Wage")+
  ylab("Number of Applicants")


#Relationship  b/w full_time_position, wages
ggplot(data = data.wage, aes(x = PREVAILING_WAGE, color = FULL_TIME_POSITION, fill = FULL_TIME_POSITION)) +
  geom_boxplot(outlier.shape = NA)+
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 15),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~Category, scale="free_y")+
  labs(title = "Wage Distribution by Industry", subtitle = "Grouped by Full-time and Part-time",fill = "Full Time")+
  xlab("Expected Wage")+
  ylab("Number of Applicants")


#Relationship b/w wages,region and category 
ggplot(data = data.wage, aes(x = PREVAILING_WAGE, color = REGION, fill = REGION)) +
  geom_boxplot(outlier.shape = NA)+
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="bottom",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 15),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~Category, scale="free_y")

#Relationship between category and region
data.reg <- data[data.df$REGION != "Other",]
data.reg <- data.reg[data.reg$Category != "Computer",]
ggplot(data.reg, aes(Category), color = CASE_STATUS)+
  geom_bar(aes(fill=REGION),position = position_dodge(), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart", 
       subtitle="Category vs Region")

#Relationship b/w rate(%) of denial and category
df <- data.frame(STATUS = data.df$STATUS, Category = data.df$Category)
tablature <- as.array(prop.table(table(df)) * 100)
df <- data.frame(prop.table(tablature,2))
df <- df[df$STATUS != "DENIED",]
df <- df[-1]
df$Freq <- (1-df$Freq)*100
g <- ggplot(df, aes(Category, Freq, fill = Category))
g + geom_bar(stat="identity", width = 0.5) + 
  labs(title="Bar Chart", 
       subtitle="Denied % vs Category",
       theme(axis.text.x = element_text(angle=65, vjust=0.6)))
       
       
       
#Relationship Rate of denied vs region
df <- data.frame(STATUS = data.df$STATUS, region = data.df$REGION)
tablature <- as.array(prop.table(table(df)) * 100)
df <- data.frame(prop.table(tablature,2))
df <- df[df$STATUS != "DENIED",]
df <- df[-1]
df$Freq <- (1-df$Freq)*100
g <- ggplot(df, aes(region, Freq,fill=region))
g + geom_bar(stat="identity", width = 0.5) + 
labs(title="Bar Chart", 
subtitle="Denied % vs Region",
theme(axis.text.x = element_text(angle=65, vjust=0.6)))

#Selecting predictor for the models
data.use <- data.df[,c(5,6,8,9,10,14)]
data.use$Category <- as.factor(data.use$Category)


##Creating Models with different Algorithms without Undersampling of the dataset####
#
set.seed(1)
train.rows <- sample(rownames(data.use), dim(data.use)[1]*0.6)
train.df <- data.use[train.rows, ]
valid.rows <- setdiff(rownames(data.use), train.rows) 
valid.df <- data.use[valid.rows, ]


##Logit
train.df2 <-train.df
valid.df2 <- valid.df
train.df2$STATUS <- ifelse(train.df2$STATUS == "APPROVED", 1, 0)
valid.df2$STATUS <- ifelse(valid.df2$STATUS == "APPROVED", 1, 0)

model1 <- glm(formula = STATUS ~ ., family = "binomial"(link = logit), data = train.df2)
model1.pred <- predict(model1, valid.df2[,-5], type = "response")
summary(model1)
confusionMatrix(as.factor(ifelse(model1.pred > 0.9,1,0)), as.factor(valid.df2[,5]))


##KNN
data.knn <- data.use
data.knn <- dummy.data.frame(data.knn, names= "Category", sep=".")
train.df3 <-data.knn[train.rows,]
valid.df3 <- data.knn[valid.rows,]

train.norm.df <- train.df3
valid.norm.df <- valid.df3

norm.values <- preProcess(train.df3[, c(2:4)], method=c("center", "scale"))
train.norm.df[, c(2:4)] <- predict(norm.values, train.df3[,c(2:4)])
valid.norm.df [,c(2:4)]<- predict(norm.values, valid.df3[, c(2:4)])

#find best k for knn
bestk.df <- data.frame(k = seq(1, 25, 1), accuracy = rep(0, 25))
#The loop will need more than 15 minutes to produce result.
for(i in 1:25) {
  bestknn.pred <- knn(train = train.norm.df[,-c(5)], test = valid.norm.df[,-c(5)], 
                      cl = train.norm.df$STATUS, k = i)
  bestk.df[i, 2] <- confusionMatrix(bestknn.pred, as.factor(valid.df3$STATUS))$overall[1] 
}
bestk.df
plot(bestk.df, type="b", xlab="K- Value",ylab="Accuracy level", pch=19)

df.knn <- knn(train = train.norm.df[, -5], test = valid.norm.df[, -5], 
              cl = train.norm.df$STATUS, k = 5)
confusionMatrix(df.knn, as.factor(valid.df3$STATUS))



#Classification tree
data.tree <- data.use
data.tree$STATUS <- as.factor(data.tree$STATUS)
train.df4 <-data.tree[train.rows,]
valid.df4 <- data.tree[valid.rows,]

H1btree<- rpart( STATUS ~ ., data = train.df4,minsplit = 1,method = "class")
prp(H1btree, type = 1, extra = 1,  under= TRUE, split.font = 1, varlen = -10)

##  Predicting & generating confusion Matrix for validation data for default tree
H1btree.pred.valid <- predict(H1btree,valid.df4,type = "class")
confusionMatrix(H1btree.pred.valid, as.factor(valid.df4$STATUS))

## Pruning the tree
H1btree.ct<- rpart( STATUS ~ ., data = train.df4,cp = 0.00001, minsplit = 10,xval=5,method = "class")
printcp(H1btree.ct)
# selecting cp =0.000593237 to get better pruned tree
#Find a better parsimonious pruned tree
H1bpruned.ct <- prune(H1btree.ct, cp =  0.0005)

#Plotting the pruned tree
prp(H1bpruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(H1bpruned.ct$frame$var == "<leaf>", 'gray', 'white')) 

H1bpruned.pred.valid <- predict(H1bpruned.ct,valid.df4,type = "class")
## Generating Confusion Matrix for accuracy
confusionMatrix(H1bpruned.pred.valid, as.factor(valid.df4$STATUS))

## Random Forest
H1brf <- randomForest(as.factor(STATUS) ~ ., data = train.df4, ntree = 100, importance = TRUE)  
varImpPlot(H1brf, type = 1)
H1brf.pred.valid <- predict(H1brf,valid.df4,type = "class")
confusionMatrix(H1brf.pred.valid, as.factor(valid.df4$STATUS))


#NN: It takes more than two hours to train and produce results
train.df1 <- train.df
valid.df1 <- valid.df
train.df1$STATUS <- as.factor(train.df1$STATUS)
train.df1 <- dummy.data.frame(train.df1, sep=".")
valid.df1$STATUS <- as.factor(valid.df1$STATUS)
valid.df1 <- dummy.data.frame(valid.df1, sep=".")

for(i in 2:4){
  train.df1[,i] <- rescale(train.df1[,i])
}

for(i in 2:4){
  valid.df1[,i] <- rescale(valid.df1[,i])
}

n <- names(train.df1)
f <- as.formula(paste("STATUS.APPROVED+STATUS.DENIED ~", paste(n[!n %in% c("STATUS.APPROVED","STATUS.DENIED")], collapse = " + ")))
datann <- neuralnet(f, data =train.df1,threshold=0.01,stepmax = 1e+05, linear.output = F, hidden = 3)
plot(datann, rep="best")

Likepredict <- neuralnet::compute(datann, valid.df1[,-c(5,6)])
predict.approved <- ifelse(Likepredict$net.result[,1]>0.9,1,0)
confusionMatrix(as.factor(valid.df1$STATUS.APPROVED),as.factor(predict.approved))



##Creating Models with different Algorithms with Undersampling of the dataset####
#Under-sampling for Status approved.
newdata.under <- ovun.sample(STATUS~., data.use, method="under", p=0.3,seed=1)$data
table(newdata.under$STATUS)
train.rowsunder <- sample(rownames(newdata.under), dim(newdata.under)[1]*0.6)
train.dfunder <- newdata.under[train.rowsunder, ]
table(train.dfunder$STATUS)
head(train.dfunder)
table(newdata.under$STATUS)
valid.rowsunder <- setdiff(rownames(newdata.under), train.rowsunder) 
valid.dfunder <- newdata.under[valid.rowsunder, ]


#Logit
train.df2under <-train.dfunder
valid.df2under <- valid.dfunder
train.df2under$STATUS <- ifelse(train.df2under$STATUS == "APPROVED", 1, 0)
valid.df2under$STATUS <- ifelse(valid.df2under$STATUS == "APPROVED", 1, 0)
model1under <- glm(formula = STATUS ~ ., family = "binomial"(link = logit), data = train.df2under)
model1.predunder <- predict(model1under, valid.df2under[,-5], type = "response")
summary(model1under)
confusionMatrix(as.factor(ifelse(model1.predunder > 0.5,1,0)), as.factor(valid.df2under[,5]))


##KNN
data.knnunder <- newdata.under
data.knnunder <- dummy.data.frame(data.knnunder, names= "Category", sep=".")
train.df3under <-data.knnunder[train.rowsunder,]
valid.df3under <- data.knnunder[valid.rowsunder,]

train.norm.dfunder <- train.df3under
valid.norm.dfunder <- valid.df3under

norm.valuesunder <- preProcess(train.df3under[, c(2:4)], method=c("center", "scale"))
train.norm.dfunder[, c(2:4)] <- predict(norm.valuesunder, train.df3under[,c(2:4)])
valid.norm.dfunder [,c(2:4)]<- predict(norm.valuesunder, valid.df3under[, c(2:4)])
t(t(names(train.norm.dfunder)))

#find best k for knn
bestk.dfunder <- data.frame(k = seq(1, 25, 1), accuracy = rep(0, 25))
bestk.dfunder 
for(i in 1:25) {
  bestknn.predunder <- knn(train = train.norm.dfunder[,-c(5)], test = valid.norm.dfunder[,-c(5)], 
                           cl = train.norm.dfunder$STATUS, k = i)
  bestk.dfunder[i, 2] <- confusionMatrix(bestknn.predunder, as.factor(valid.df3under$STATUS))$overall[1] 
}
bestk.dfunder
plot(bestk.dfunder, type="b", xlab="K- Value",ylab="Accuracy level", pch=19)

df.knnunder <- knn(train = train.norm.dfunder[, -5], test = valid.norm.dfunder[, -5], 
                   cl = train.norm.dfunder$STATUS, k = 19)
confusionMatrix(df.knnunder, as.factor(valid.df3under$STATUS))


#Classification tree
data.treeunder <- newdata.under
t(t(names(data.treeunder)))
data.treeunder$STATUS <- as.factor(data.treeunder$STATUS)
train.df4under <-data.treeunder[train.rowsunder,]
valid.df4under <- data.treeunder[valid.rowsunder,]

H1btree<- rpart( STATUS ~ ., data = train.df4under,minsplit = 1,method = "class")
# plot tree
prp(H1btree, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

##  Predicting & generating confusion Matrix for validation data
#H1btree.pred.valid <- predict(H1btree,valid.df4under,type = "class")
#confusionMatrix(H1btree.pred.valid, as.factor(valid.df4under$STATUS))

## Pruning the tree
H1btree.ct<- rpart( STATUS ~ ., data = train.df4under,cp = 0.00001, minsplit = 10,xval=5,method = "class")
printcp(H1btree.ct)
# selecting cp =0.000593237 to get better pruned tree

#Find a better parsimonous pruned tree
H1bpruned.ct <- prune(H1btree.ct, cp =  0.0017)
prp(H1bpruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(H1bpruned.ct$frame$var == "<leaf>", 'gray', 'white')) 

H1bpruned.pred.valid <- predict(H1bpruned.ct,valid.df4under,type = "class")
confusionMatrix(H1bpruned.pred.valid, as.factor(valid.df4under$STATUS))

## Random Forest
library(randomForest)
H1brf <- randomForest(as.factor(STATUS) ~ ., data = train.df4under, ntree = 100, importance = TRUE)  
varImpPlot(H1brf, type = 1)

H1brf.pred.valid <- predict(H1brf,valid.df4under,type = "class")
confusionMatrix(H1brf.pred.valid, as.factor(valid.df4under$STATUS))


#NN
train.df1under <- train.dfunder
valid.df1under <- valid.dfunder
train.df1under$STATUS <- as.factor(train.df1under$STATUS)
train.df1under <- dummy.data.frame(train.df1under, sep=".")
valid.df1under$STATUS <- as.factor(valid.df1under$STATUS)
valid.df1under <- dummy.data.frame(valid.df1under, sep=".")

for(i in 2:4){
  train.df1under[,i] <- rescale(train.df1under[,i])
}

for(i in 2:4){
  valid.df1under[,i] <- rescale(valid.df1under[,i])
}

nunder <- names(train.df1under)
funder <- as.formula(paste("STATUS.APPROVED+STATUS.DENIED ~", paste(nunder[!nunder %in% c("STATUS.APPROVED","STATUS.DENIED")], collapse = " + ")))
datannunder <- neuralnet(funder, data =train.df1under,threshold=0.01,stepmax = 1e+05, linear.output = F, hidden = 3)
plot(datannunder, rep="best")
data.under.nn <- datannunder$data
library(caret)

class(datannunder)
Likepredictunder <- neuralnet::compute(datannunder, valid.df1under[,-c(5,6)])
predict.approvedunder <- ifelse(Likepredictunder$net.result[,1]>0.5,1,0)
confusionMatrix(as.factor(valid.df1under$STATUS.APPROVED),as.factor(predict.approvedunder))













