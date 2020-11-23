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
data[is.na(data$drugCondition),c("drugCondition")] <- "PL"
# filter for only placebo
data <- data %>% filter(drugCondition == "PL")
# join to go emotions
setwd("C:/Users/jakep/Desktop")
goemo <- read.csv("go_emotions_output-ekman.csv")
goemo$Filename <- substr(goemo$file, 17,50) # get filename
goemo$id <- sapply(strsplit(goemo$Filename, "_"), "[", 1)
goemo$emo <- sapply(strsplit(goemo$Filename, "_"), "[", 2)
goemo$thing <- sapply(strsplit(goemo$Filename, "_"), "[", 3)
goemo$Filename <- paste(goemo$id,goemo$emo,goemo$thing, sep="_")
# Create variable for joining
data$Filename <- paste(data$participantID.y,data$stimulus,sep="_")
#data$Filename <- paste(data$Filename,".txt",sep="")

emo_unblind <- right_join(goemo,data,by="Filename")
emo_unblind$Case <- substr(emo_unblind$Filename,1,1)
data2 <- emo_unblind

# Define emotions
#emo_unblind <- emo_unblind %>% group_by(participantID.x) %>% summarise_all(funs(mean))
emo_unblind$positive <- emo_unblind$joy

emo_unblind$negative <- emo_unblind$anger + emo_unblind$disgust + emo_unblind$fear +
  emo_unblind$sadness

data4 <- emo_unblind %>% filter(Case == "7")
# (1) average probability across all 1-positive, 2-negative and 3-neutral emotions
data5 <- data4 %>% group_by(id, emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data5 <- data5 %>% group_by(emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
#data4 <- data3 %>% group_by(participantID.y) %>% summarise_all(funs(mean))

# (3) the result should be a 3x3 matrix
data6 <- data5[,c("emo","positive","negative","neutral")]
# (4) row normalize (the rows are videos, the columns emotions)
data7 <- normalize.rows(as.matrix(data6[,2:4]),method="manhattan")
# (5) take the trace (sum of the diagonal)
sum(diag(data7))

# Case == "8"
data4 <- emo_unblind %>% filter(Case == "8")
data4$emo <- gsub(" neg","neg",data4$emo)
# (1) average probability across all 1-positive, 2-negative and 3-neutral emotions
data5 <- data4 %>% group_by(id, emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data5 <- data5 %>% group_by(emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
#data4 <- data3 %>% group_by(participantID.y) %>% summarise_all(funs(mean))

# (3) the result should be a 3x3 matrix
data6 <- data5[,c("emo","positive","negative","neutral")]
# (4) row normalize (the rows are videos, the columns emotions)
data7 <- normalize.rows(as.matrix(data6[,2:4]),method="manhattan")
# (5) take the trace (sum of the diagonal)
sum(diag(data7))


### For OT Cases

# Join on ID Day
data <- full_join(unblinding, evoc_trans, by="IDDay")
# replace na's for 8 with pl
data[is.na(data$drugCondition),c("drugCondition")] <- "PL"
# filter for only placebo
data <- data %>% filter(drugCondition == "OT")
# join to go emotions
setwd("C:/Users/jakep/Desktop")
goemo <- read.csv("go_emotions_output-ekman.csv")
goemo$Filename <- substr(goemo$file, 17,50) # get filename
goemo$id <- sapply(strsplit(goemo$Filename, "_"), "[", 1)
goemo$emo <- sapply(strsplit(goemo$Filename, "_"), "[", 2)
goemo$thing <- sapply(strsplit(goemo$Filename, "_"), "[", 3)
goemo$Filename <- paste(goemo$id,goemo$emo,goemo$thing, sep="_")
# Create variable for joining
data$Filename <- paste(data$participantID.y,data$stimulus,sep="_")
#data$Filename <- paste(data$Filename,".txt",sep="")

emo_unblind <- right_join(goemo,data,by="Filename")
emo_unblind$Case <- substr(emo_unblind$Filename,1,1)
data2 <- emo_unblind

# Define emotions
#emo_unblind <- emo_unblind %>% group_by(participantID.x) %>% summarise_all(funs(mean))
emo_unblind$positive <- emo_unblind$joy

emo_unblind$negative <- emo_unblind$anger + emo_unblind$disgust + emo_unblind$fear +
  emo_unblind$sadness

data4 <- emo_unblind %>% filter(Case == "7")
# (1) average probability across all 1-positive, 2-negative and 3-neutral emotions
data5 <- data4 %>% group_by(id, emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data5 <- data5 %>% group_by(emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
#data4 <- data3 %>% group_by(participantID.y) %>% summarise_all(funs(mean))

# (3) the result should be a 3x3 matrix
data6 <- data5[,c("emo","positive","negative","neutral")]
# (4) row normalize (the rows are videos, the columns emotions)
data7 <- normalize.rows(as.matrix(data6[,2:4]),method="manhattan")
# (5) take the trace (sum of the diagonal)
sum(diag(data7))



