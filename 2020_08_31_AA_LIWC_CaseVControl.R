# (a) read in and merge 
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
# data <- data %>% filter(drugCondition == "OT")

# set wd
setwd("evocativeVideoTask")
# read in liwc
evoc <- read.csv("LIWC2015 Results (emotextfiles (1732 files)).csv")
evoc$Filename <- as.character(evoc$Filename)
evoc$id <- sapply(strsplit(evoc$Filename, "_"), "[", 1)
evoc$emo <- sapply(strsplit(evoc$Filename, "_"), "[", 2)
evoc$thing <- sapply(strsplit(evoc$Filename, "_"), "[", 3)
evoc$IDstimulus <- paste(evoc$id,evoc$emo,evoc$thing, sep="_")

data$IDstimulus <- paste(data$participantID.y,data$stimulus, sep="_")
data2 <- data
# make id stimulus variable
data3 <- right_join(data2, evoc, by="IDstimulus")
# merge liwc with 
data3$Case <- substr(data3$Filename,1,1)


# (2) for a participant, average these probabilities across all their positive, negative and neutral video transcripts
# data3$condition <- substr(data$stimulus, 1,3)
data3$neutralemo <- 100 - data3$posemo - data3$negemo
# (1b) Filter by Case and Control
data4 <- data3 %>% filter(Case == "7")
# (1) average probability across all 1-positive, 2-negative and 3-neutral emotions
data5 <- data4 %>% group_by(id, emo) %>% summarise_all(funs(mean))
data5 <- data5 %>% group_by(emo) %>% summarise_all(funs(mean))
#data4 <- data3 %>% group_by(participantID.y) %>% summarise_all(funs(mean))

# (3) the result should be a 3x3 matrix
data6 <- data5[,c("emo","posemo","negemo","neutralemo")]
# (4) row normalize (the rows are videos, the columns emotions)
data7 <- normalize.rows(as.matrix(data6[,2:4]))
# (5) take the trace (sum of the diagonal)
sum(diag(data7))

# (1b) Filter by Case and Control
data4 <- data3 %>% filter(Case == "8")
# (1) average probability across all 1-positive, 2-negative and 3-neutral emotions
data5 <- data4 %>% group_by(id, emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data5 <- data5 %>% group_by(emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
#data4 <- data3 %>% group_by(participantID.y) %>% summarise_all(funs(mean))

# (3) the result should be a 3x3 matrix
data6 <- data5[,c("emo","posemo","negemo","neutralemo")]
# (4) row normalize (the rows are videos, the columns emotions)
data7 <- normalize.rows(as.matrix(data6[,2:4]))
# (5) take the trace (sum of the diagonal)
sum(diag(data7))