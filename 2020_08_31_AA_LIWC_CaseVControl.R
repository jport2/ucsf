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
#data_oxy <- data %>% filter(drugCondition == "PL")
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
data4 <- data4 %>% filter(drugCondition == "PL")
# (1) average probability across all 1-positive, 2-negative and 3-neutral emotions
data5 <- data4 %>% group_by(id, emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data5 <- data5 %>% group_by(emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
#data4 <- data3 %>% group_by(participantID.y) %>% summarise_all(funs(mean))

# (3) the result should be a 3x3 matrix
data6 <- data5[,c("emo","posemo","negemo","neutralemo")]
# (4) row normalize (the rows are videos, the columns emotions)
data7 <- normalize.rows(as.matrix(data6[,2:4]),method="manhattan")
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
data7 <- normalize.rows(as.matrix(data6[,2:4]),method="manhattan")
# (5) take the trace (sum of the diagonal)
sum(diag(data7))

# For OT Cases
# Join on ID Day
data <- full_join(unblinding, evoc_trans, by="IDDay")
# replace na's for 8 with pl
data[is.na(data$drugCondition),c("drugCondition")] <- "PL"
# filter for only placebo
data <- data %>% filter(drugCondition == "OT")
# data <- data %>% filter(drugCondition == "OT")

# set wd
setwd("C:/Users/jakep/Dropbox/PORQ_dataForUW/evocativeVideoTask")# read in liwc
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
data4 <- data4 %>% filter(drugCondition == "OT")
# (1) average probability across all 1-positive, 2-negative and 3-neutral emotions
data5 <- data4 %>% group_by(id, emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data5 <- data5 %>% group_by(emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
#data4 <- data3 %>% group_by(participantID.y) %>% summarise_all(funs(mean))

# (3) the result should be a 3x3 matrix
data6 <- data5[,c("emo","posemo","negemo","neutralemo")]
# (4) row normalize (the rows are videos, the columns emotions)
data7 <- normalize.rows(as.matrix(data6[,2:4]),method="manhattan")
# (5) take the trace (sum of the diagonal)
sum(diag(data7))


#################################################################################
### T-test for case v control for each emotion output ##################
###########################################################################
case_means <- data.frame()
control_means <- data.frame()
pvalues <- data.frame()
# joy
data4 <- data3[grep("joy",data3$Filename),]
data_case <- data4 %>% filter(Case == 7)
data_case <- data_case %>% filter(drugCondition == "PL")
data_case <- data_case %>% group_by(participantID.x) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_case$positive <- data_case$posemo
data_case$negative <- data_case$negemo
data_case$neutral <- 100 - data_case$posemo - data_case$negemo

case_means[1,c("positive")] <- mean(data_case$positive, na.rm=TRUE)
case_means[1,c("negative")] <-mean(data_case$negative, na.rm=TRUE)
case_means[1,c("neutral")] <-mean(data_case$neutral, na.rm=TRUE)


data4 <- data3[grep("joy",data3$Filename),]
data_control <- data4 %>% filter(Case == 8)
data_control <- data_control %>% group_by(participantID.y) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_control$positive <- data_control$posemo

data_control$negative <- data_control$negemo
data_control$neutral <- 100 - data_control$posemo - data_control$negemo

control_means[1,c("positive")] <- mean(data_control$positive, na.rm=TRUE)
control_means[1,c("negative")] <-mean(data_control$negative, na.rm=TRUE)
control_means[1,c("neutral")] <-mean(data_control$neutral, na.rm=TRUE)

t1 <- t.test(data_case$positive,data_control$positive)
t2 <- t.test(data_case$negative,data_control$negative)
t3 <- t.test(data_case$neutral,data_control$neutral)

pvalues[1,c("positive")] <- t1$p.value
pvalues[1,c("negative")] <- t2$p.value
pvalues[1,c("neutral")] <- t3$p.value

# neg
data4 <- data3[grep("neg",data3$Filename),]
data_case <- data4 %>% filter(Case == 7)
data_case <- data_case %>% filter(drugCondition == "PL")
data_case <- data_case %>% group_by(participantID.x) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_case$positive <- data_case$posemo
data_case$negative <- data_case$negemo
data_case$neutral <- 100 - data_case$posemo - data_case$negemo

case_means[2,c("positive")] <- mean(data_case$positive, na.rm=TRUE)
case_means[2,c("negative")] <-mean(data_case$negative, na.rm=TRUE)
case_means[2,c("neutral")] <-mean(data_case$neutral, na.rm=TRUE)


data4 <- data3[grep("neg",data3$Filename),]
data_control <- data4 %>% filter(Case == 8)
data_control <- data_control %>% group_by(participantID.y) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_control$positive <- data_control$posemo

data_control$negative <- data_control$negemo
data_control$neutral <- 100 - data_control$posemo - data_control$negemo

control_means[2,c("positive")] <- mean(data_control$positive, na.rm=TRUE)
control_means[2,c("negative")] <-mean(data_control$negative, na.rm=TRUE)
control_means[2,c("neutral")] <-mean(data_control$neutral, na.rm=TRUE)

t1 <- t.test(data_case$positive,data_control$positive)
t2 <- t.test(data_case$negative,data_control$negative)
t3 <- t.test(data_case$neutral,data_control$neutral)

pvalues[2,c("positive")] <- t1$p.value
pvalues[2,c("negative")] <- t2$p.value
pvalues[2,c("neutral")] <- t3$p.value

# neu
data4 <- data3[grep("neu",data3$Filename),]
data_case <- data4 %>% filter(Case == 7)
data_case <- data_case %>% filter(drugCondition == "PL")
data_case <- data_case %>% group_by(participantID.x) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_case$positive <- data_case$posemo
data_case$negative <- data_case$negemo
data_case$neutral <- 100 - data_case$posemo - data_case$negemo

case_means[3,c("positive")] <- mean(data_case$positive, na.rm=TRUE)
case_means[3,c("negative")] <-mean(data_case$negative, na.rm=TRUE)
case_means[3,c("neutral")] <-mean(data_case$neutral, na.rm=TRUE)


data4 <- data3[grep("neu",data3$Filename),]
data_control <- data4 %>% filter(Case == 8)
data_control <- data_control %>% group_by(participantID.y) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_control$positive <- data_control$posemo

data_control$negative <- data_control$negemo
data_control$neutral <- 100 - data_control$posemo - data_control$negemo

control_means[3,c("positive")] <- mean(data_control$positive, na.rm=TRUE)
control_means[3,c("negative")] <-mean(data_control$negative, na.rm=TRUE)
control_means[3,c("neutral")] <-mean(data_control$neutral, na.rm=TRUE)

t1 <- t.test(data_case$positive,data_control$positive)
t2 <- t.test(data_case$negative,data_control$negative)
t3 <- t.test(data_case$neutral,data_control$neutral)

pvalues[3,c("positive")] <- t1$p.value
pvalues[3,c("negative")] <- t2$p.value
pvalues[3,c("neutral")] <- t3$p.value

##########################################################################
### Oxy Means and Paired T-test ############################################
##########################################################################
oxy_means <- data.frame()
data3 <- right_join(data2, evoc, by="IDstimulus")
data3$Case <- substr(data3$Filename,1,1)
data3[is.na(data3$drugCondition),c("drugCondition")] <- "OT"

# joy
data4 <- data3[grep("joy",data3$Filename),]
data_case <- data4 %>% filter(Case == 7)
data_case <- data_case %>% filter(drugCondition == "OT")
data_case <- data_case %>% group_by(participantID.x) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_case$positive <- data_case$posemo
data_case$negative <- data_case$negemo
data_case$neutral <- 100 - data_case$posemo - data_case$negemo

oxy_means[1,c("positive")] <- mean(data_case$positive, na.rm=TRUE)
oxy_means[1,c("negative")] <-mean(data_case$negative, na.rm=TRUE)
oxy_means[1,c("neutral")] <-mean(data_case$neutral, na.rm=TRUE)

# neg
data4 <- data3[grep("neg",data3$Filename),]
data_case <- data4 %>% filter(Case == 7)
data_case <- data_case %>% filter(drugCondition == "OT")
data_case <- data_case %>% group_by(participantID.x) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_case$positive <- data_case$posemo
data_case$negative <- data_case$negemo
data_case$neutral <- 100 - data_case$posemo - data_case$negemo

oxy_means[2,c("positive")] <- mean(data_case$positive, na.rm=TRUE)
oxy_means[2,c("negative")] <-mean(data_case$negative, na.rm=TRUE)
oxy_means[2,c("neutral")] <-mean(data_case$neutral, na.rm=TRUE)

# neu
data4 <- data3[grep("neu",data3$Filename),]
data_case <- data4 %>% filter(Case == 7)
data_case <- data_case %>% filter(drugCondition == "OT")
data_case <- data_case %>% group_by(participantID.x) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_case$positive <- data_case$posemo
data_case$negative <- data_case$negemo
data_case$neutral <- 100 - data_case$posemo - data_case$negemo

oxy_means[3,c("positive")] <- mean(data_case$positive, na.rm=TRUE)
oxy_means[3,c("negative")] <-mean(data_case$negative, na.rm=TRUE)
oxy_means[3,c("neutral")] <-mean(data_case$neutral, na.rm=TRUE)