library(xlsx)
library(dplyr)
library(ggplot2)
library(wordspace)
setwd("C:/Users/jakep/Dropbox/PORQ_dataForUW/evocativeVideoTask")
evoc_trans <- read.xlsx("PORQ_evocativeVideoTask_transcripts.xlsx", sheetIndex = 1)

# read unbinding
setwd("..")
unblinding <- read.xlsx("PORQ_drugUnblinding.xlsx", sheetIndex = 1)

# IDDay
evoc_trans$IDDay <- paste(evoc_trans$participantID, evoc_trans$day, sep="_")
unblinding$IDDay <- paste(unblinding$participantID, unblinding$day, sep="_")

# Join on ID Day
data <- full_join(unblinding, evoc_trans, by="IDDay")
# replace na's for 8 with pl
data$drugCondition <- factor(data$drugCondition, levels = c("PL","OT","HC"))
data[is.na(data$drugCondition),c("drugCondition")] <- "HC"
# filter for only placebo
#data <- data %>% filter(drugCondition == "PL")
# join to go emotions
setwd("C:/Users/jakep/Desktop")
goemo <- read.csv("go_emotions_output.csv")
goemo$Filename <- substr(goemo$file, 17,50) # get filename
goemo$id <- sapply(strsplit(goemo$Filename, "_"), "[", 1)
goemo$emo <- sapply(strsplit(goemo$Filename, "_"), "[", 2)
goemo$thing <- sapply(strsplit(goemo$Filename, "_"), "[", 3)
goemo$feeldescribe <- sapply(strsplit(goemo$Filename, "_"), "[", 4)
goemo <- goemo[grep("feel",goemo$feeldescribe),]
goemo$Filename <- paste(goemo$id,goemo$emo,goemo$thing, sep="_")
# Create variable for joining
data$Filename <- paste(data$participantID.y,data$stimulus,sep="_")
#data$Filename <- paste(data$Filename,".txt",sep="")

emo_unblind <- right_join(goemo,data,by="Filename")
emo_unblind$Case <- substr(emo_unblind$Filename,1,1)
data2 <- emo_unblind

# Define emotions
#emo_unblind <- emo_unblind %>% group_by(participantID.x) %>% summarise_all(funs(mean))
emo_unblind$positive <- (emo_unblind$admiration + emo_unblind$amusement + emo_unblind$approval+
                           emo_unblind$caring + emo_unblind$desire + emo_unblind$excitement + emo_unblind$gratitude + emo_unblind$joy +
                           emo_unblind$love + emo_unblind$optimism + emo_unblind$pride + emo_unblind$relief)/12

emo_unblind$negative <- (emo_unblind$anger + emo_unblind$annoyance + emo_unblind$disappointment +         
                           emo_unblind$disapproval+emo_unblind$disgust+emo_unblind$embarrassment+
                           emo_unblind$fear+emo_unblind$grief+emo_unblind$nervousness +
                           emo_unblind$remorse+emo_unblind$sadness)/11

emo_unblind2 <- emo_unblind[,c("positive","negative","neutral")]
emo_unblind <- emo_unblind[ , -which(names(emo_unblind) %in% c("positive","negative","neutral"))]
emo_unblind2 <- normalize.rows(as.matrix(emo_unblind2),method="manhattan")
emo_unblind_norm <- cbind(emo_unblind,emo_unblind2)

#data4 <- emo_unblind_norm %>% filter(Case == "7")
# (1) average probability across all 1-positive, 2-negative and 3-neutral emotions
emo_unblind_norm$Video <- paste(emo_unblind_norm$emo, emo_unblind_norm$thing, sep="_")
data5 <- emo_unblind_norm %>% group_by(id, Video, drugCondition) %>% summarise_all(funs(mean(., na.rm = TRUE)))


for(i in unique(data5$Video)){
  data6 <- data5 %>% filter(Video == i)
  ggplot(data6, aes(x=positive, color=drugCondition))+geom_density()+ggtitle(i)
}
# joy
data_joy <- data5 %>% filter(emo=="joy")
ggplot(data_joy, aes(x=positive, color=drugCondition)) +
  geom_density()+ggtitle("Density of Joy Videos for 3 Groups")

# neg
data_joy <- data5 %>% filter(emo=="neg")
ggplot(data_joy, aes(x=negative, color=drugCondition)) +
  geom_density()+ggtitle("Density of Neg Videos for 3 Groups")

# neu
data_joy <- data5 %>% filter(emo=="neu")
ggplot(data_joy, aes(x=neutral, color=drugCondition)) +
  geom_density()+ggtitle("Density of Neu Videos for 3 Groups")

