options(scipen = 999)
library(caret)
library(dummies)
library(FNN)
library(dplyr)
library(plyr)
library(reshape) 

H1b <- read.csv(file = "h1b_kaggle.csv", header = TRUE)
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
H1b16$Category <- ifelse(grepl('ANIMAL_TRAINERS|BARBERS|HAIRDRESSERS,_HAIRSTYLISTS,_AND_COSMETOLOGISTS|SKINCARE_SPECIALISTS|TOUR_GUIDES_AND_ESCORTS|TRAVEL_GUIDES|CHILDCARE_WORKERS|FITNESS_TRAINERS_AND_AEROBICS_INSTRUCTORS|RECREATION_WORKERS|RESIDENTIAL_ADVISORS|PERSONAL_CARE_AND_SERVICE_WORKERS', H1b16$SOC_NAME, ignore.case = T), 'Personal', H1b16$Category)
H1b16$Category <- ifelse(grepl('computer|programmer|software|web developer|database', H1b16$SOC_NAME, ignore.case = T), 'Computer', H1b16$Category)
H1b16$Category <- ifelse(grepl('teacher|school principal|linguist|professor|teach', H1b16$SOC_NAME, ignore.case = T), 'Education', H1b16$Category)
H1b16$Category <- ifelse(grepl('public_relation|manag|operation|chief|executive|plan', H1b16$SOC_NAME, ignore.case = T), 'Management', H1b16$Category)
H1b16$Category <- ifelse(grepl('business|business_analyst|business_systems_analyst|financ|accountant', H1b16$SOC_NAME, ignore.case = T), 'Business', H1b16$Category)
H1b16$Category <- ifelse(grepl('medical|doctor|physician|dentist|health|physical therapists|surgeon|nurse|psychiatr', H1b16$SOC_NAME, ignore.case = T), 'Healthcare', H1b16$Category)
H1b16$Category <- ifelse(grepl('scient|ECONOMISTS|BIOLOGICAL_TECHNICIANS|CHEMIST|NUCLEAR|RESEARCH_ASSISTANTS', H1b16$SOC_NAME, ignore.case = T), 'Science', H1b16$Category)
H1b16$Category <- ifelse(grepl('advertis|marketing|market|promotion', H1b16$SOC_NAME, ignore.case = T), 'Marketing', H1b16$Category)
H1b16$Category <- ifelse(grepl('PARALEGALS_AND_LEGAL_ASSISTANTS|LAWYERS|LAWYER|ATTORNEY|JUDICIAL_LAW_CLERKS|ARBITRATORS_MEDIATORS_AND_CONCILIATORS|PARALEGALS_AND_LEGAL_ASSISTANTS*|LEGAL_SUPPORT_WORKERS_ALL_OTHER', H1b16$SOC_NAME, ignore.case = T), 'Legal', H1b16$Category)
H1b16$Category <- ifelse(grepl('COUNSELORS|MENTAL|SUBSTANCE|ABUSE|SOCIAL_WORKERS|BEHAVIORAL|GUIDANCE|THERAPISTS|REHABRELIGIOUS_WORKERS', H1b16$SOC_NAME, ignore.case = T), 'Counselor', H1b16$Category)
H1b16$Category <- ifelse(grepl('SALES|FIRST-LINE|SUPERVISORS|wholesale|commodit|Manufact|TRADER|retail', H1b16$SOC_NAME, ignore.case = T), 'Sales', H1b16$Category)
H1b16$Category <- ifelse(grepl('farming|farm|fishing|agri|ANIMAL_BREEDERS|CROP', H1b16$SOC_NAME, ignore.case = T), 'Farming', H1b16$Category)
H1b16$Category <- ifelse(grepl('transport|airline|flight|pilot|drive|captain|ship_engineers', H1b16$SOC_NAME, ignore.case = T), 'Transportation', H1b16$Category)
H1b16$Category <- ifelse(grepl('Install|REPAIR|SECURITY|MECHANICS|MAINTENANCE', H1b16$SOC_NAME, ignore.case = T), 'Installation', H1b16$Category)
H1b16$Category <- ifelse(grepl('construction|PLUMBERS|helper|PAINTERS', H1b16$SOC_NAME, ignore.case = T), 'Construction', H1b16$Category)
H1b16$Category <- ifelse(grepl('CHEFS|COOKS|Food|LOUNGE', H1b16$SOC_NAME, ignore.case = T), 'Food', H1b16$Category)
H1b16$Category <- ifelse(grepl('MUSICIANS_INSTRUMENTAL|ARCHIVISTS|CURATORS|MUSEUM_TECHNICIANS_AND_CONSERVATORS|LIBRARIANS|LIBRARY_TECHNICIANS|AUDIO-VISUAL_AND_MULTIMEDIA_COLLECTIONS|FARM_AND_HOME_MANAGEMENT_ADVISORS|INSTRUCTIONAL_COORDINATORS|INSTRUCTIONAL_DESIGNERS_AND_TECHNOLOGISTS|INSTRUCTIONAL_COORDINATOR|TEACHER_ASSISTANTS|EDUCATION_TRAINING_AND_LIBRARY_WORKERS_ALL|EDUCATION_TRAINING_&_LIBRARY_WORKERS_ALL_OTHER|ART_DIRECTORS|CRAFT_ARTISTS|FINE_ARTISTS_INCLUDING_PAINTERS_SCULPTORS_AND|MULTIMEDIA_ARTISTS_AND_ANIMATORS|ARTISTS_AND_RELATED_WORKERS_ALL_OTHER|COMMERCIAL_AND_INDUSTRIAL_DESIGNERS|COMMERCIAL_AND_INDUSTRIAL_ENGINEERS|COMMERCIAL_AND_INDUSTRIAL_DEISGNERS|FASHION_DESIGNERS|FLORAL_DESIGNERS|GRAPHIC_DESIGNERS|GRAPHIC_DESIGNER|INTERIOR_DESIGNERS|INTERIOR_DESIGNER|SET_AND_EXHIBIT_DESIGNERS|DESIGNERS_ALL_OTHER|ACTORS|PRODUCERS_AND_DIRECTORS|PRODUCERS|ATHLETES_AND_SPORTS_COMPETITORS|COACHES_AND_SCOUTS|DANCERS|CHOREOGRAPHERS|MUSIC_DIRECTORS_AND_COMPOSERS|MUSICIANS_AND_SINGERS|MUSICIANS_INSTRUdata|ENTERTAINERS_AND_PERFORMERS_SPORTS_AND_RELATED|RADIO_AND_TELEVISION_ANNOUNCERS|BROADCAST_NEWS_ANALYSTS|REPORTERS_AND_CORRESPONDENTS|PUBLIC_RELATIONS_SPECIALIST|EDITORS|TECHNICAL_WRITERS|TECHNICAL_WRITER|WRITERS_AND_AUTHORS|POETS_LYRICISTS_AND_CREATIVE_WRITERS|INTERPRETERS_AND_TRANSLATORS|MEDIA_AND_COMMUNICATION_WORKERS_ALL_OTHER|MARKET_RESEARCH|AUDIO_AND_VIDEO_EQUIPMENT_TECHNICIANS|SOUND_ENGINEERING_TECHNICIANS|PHOTOGRAPHERS|CAMERA_OPERATORS_TELEVISION_VIDEO_AND_MOTION|FILM_AND_VIDEO_EDITORS|MEDIA_AND_COMMUNICATION_EQUIPMENT_WORKERS_ALL', H1b16$SOC_NAME, ignore.case = T), 'Arts', H1b16$Category)

#Check categories
table(H1b16$Category)
cat <- table(H1b16$Category)
# Barplot for Categories with Ratio
cat.count <- as.vector(cat)
bar.plot <- barplot(cat , border=F , names.arg= names(as.vector(cat)), 
                  las=2, 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  ylim=c(0,450)*1000, 
                  main="Category Distribution" )
text(bar.plot, cat.count+500, paste("ratio%=", round((cat.count/sum(cat.count)*100),2), sep="") ,cex=1.4)


# Pivot tables
t(t(names(H1b16)))
summary(H1b16)
summary(H1b16$PREVAILING_WAGE) 

# Creating 3 bins for prevailing wage
H1b16.bin <- H1b16
bin <- c(0, 60000, 100000, Inf)
names <- c("Low", "Medium", "High")
H1b16.bin$PREVAILING_WAGE.bin <- cut(H1b16.bin$PREVAILING_WAGE,breaks = bin,labels = names)

ggplot(data = as_tibble(H1b16.bin$PREVAILING_WAGE.bin), mapping = aes(x=value)) + 
  geom_bar(fill="bisque",color="white",alpha=0.7) + 
  stat_count(geom="text", aes(label=sprintf("%.4f",..count../length(H1b16.bin$PREVAILING_WAGE.bin))), vjust=-0.5) +
  labs(x='Prevailing Wage') +
  theme_minimal() 

# No. of applicants for each combination of (binned) Wage and Case Status
mlt1<- melt(H1b16.bin, id=c("PREVAILING_WAGE.bin", "STATUS"), measure=c("STATUS"))
cast(mlt1, PREVAILING_WAGE.bin ~ STATUS , subset=variable=="STATUS", 
     margins=c("grand_row", "grand_col"),length)

# No. of applicants for each combination of (binned) Wage and Job Category
mlt2<- melt(H1b16.bin, id=c("PREVAILING_WAGE.bin", "Category"), measure=c("STATUS"))
cast(mlt2, PREVAILING_WAGE.bin ~ Category , subset=variable=="STATUS", 
     margins=c("grand_row", "grand_col"),length)

# No. of applicants for each combination of (binned) Wage and Region
mlt3<- melt(H1b16.bin, id=c("PREVAILING_WAGE.bin", "REGION"), measure=c("STATUS"))
cast(mlt3, PREVAILING_WAGE.bin ~ REGION , subset=variable=="STATUS", 
     margins=c("grand_row", "grand_col"),length)


