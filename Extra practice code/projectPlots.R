library(ggplot2)
library(dpylr)
library(treemap)
file = 'C:\\Users\\moses\\Desktop\\620\\finalData.csv'
data <- read.csv(file)
data <- data[-1]

data$CASE_STATUS <- as.factor(data$CASE_STATUS)
data$FULL_TIME_POSITION<- as.factor(data$FULL_TIME_POSITION)
data$region <- as.factor(data$region)
data$Category <- as.factor(data$Category)
data$SOC_NAME <- as.factor(data$SOC_NAME)

summary(data$Category)
cat <- c("Business","Computer","Education","Engineering","Healthcare","Management","Other","Science")
num <- c(40853,387168,10972,43592,14771,30566,15771,17170)


data.accept <- data[data$CASE_STATUS != "DENIED",]
data.denied <- data[data$CASE_STATUS == "DENIED",]

summary(data.accept)
summary(data.denied)

# Plot
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


# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggExtra)

data.full <-data[data$FULL_TIME_POSITION == "Y",]
data.part <-data[data$FULL_TIME_POSITION == "N",]
summary(data.full)

ggplot(data = data.denied, aes(x = PREVAILING_WAGE, color = FULL_TIME_POSITION, fill = FULL_TIME_POSITION)) +
  geom_histogram(bins = 30) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 15),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~Category, scale="free_y")+
  geom_vline(data = data.full, aes(xintercept=median(PREVAILING_WAGE)),color="blue", linetype="dashed", size=1)+
  labs(title = "Wage Distribution by Industry", subtitle = "Grouped by Full-time and Part-time",fill = "Full Time")+
  xlab("Expected Wage")+
  ylab("Number of Applicants")

ggplot(data = data, aes(x = PREVAILING_WAGE, color = FULL_TIME_POSITION, fill = FULL_TIME_POSITION)) +
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

ggplot(data = data.denied, aes(x = PREVAILING_WAGE, color = region, fill = region)) +
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

data.reg <- data[data$region != "Other",]
data.reg <- data.reg[data.reg$Category != "Computer",]
ggplot(data.reg, aes(Category), color = CASE_STATUS)+
  geom_bar(aes(fill=region),position = position_dodge(), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart", 
       subtitle="Manufacturer of vehicles", 
       caption="Source: Manufacturers from 'mpg' dataset")


df <- data.frame(CASE_STATUS = data$CASE_STATUS, Category = data$Category)
tablature <- as.array(prop.table(table(df)) * 100)
df <- data.frame(prop.table(tablature,2))
df <- df[df$CASE_STATUS != "DENIED",]
df <- df[-1]
df$Freq <- (1-df$Freq)*100
g <- ggplot(df, aes(Category, Freq, fill=df$Category))
g + geom_bar(stat="identity", width = 0.5) + 
  labs(title="Bar Chart", 
       subtitle="Manufacturer of vehicles", 
       caption="Source: Frequency of Manufacturers from 'mpg' dataset") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

df <- data.frame(CASE_STATUS = data$CASE_STATUS, region = data$region)
tablature <- as.array(prop.table(table(df)) * 100)
df <- data.frame(prop.table(tablature,2))
df <- df[df$CASE_STATUS != "DENIED",]
df <- df[-1]
df$Freq <- (1-df$Freq)*100
g <- ggplot(df, aes(region, Freq,fill=region))
g + geom_bar(stat="identity", width = 0.5) + 
  labs(title="Bar Chart", 
       subtitle="Manufacturer of vehicles", 
       caption="Source: Frequency of Manufacturers from 'mpg' dataset") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

df <- data.frame(CASE_STATUS = data$CASE_STATUS, Category = data$Category, region = data$region)
tablature <- as.array(prop.table(table(df)) * 100)
df <- data.frame(prop.table(tablature,2))
df <- df[df$CASE_STATUS != "DENIED",]
df <- df[-1]
df$Freq <- (1-df$Freq)*100
ggplot(df, aes(Category,Freq))+
  geom_bar(stat = "identity", aes(fill=region),position = position_dodge(), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart", 
       subtitle="Manufacturer of vehicles", 
       caption="Source: Manufacturers from 'mpg' dataset")
