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


data3 <- data2[grep("joy",data2$Filename),]
data_case <- data3 %>% filter(Case == 7)
data_oxy <- data_oxy %>% group_by(participantID.x) %>% summarise_all(funs(mean))
data_oxy$positive <- data_oxy$joy

data_oxy$negative <- data_oxy$anger + data_oxy$disgust + data_oxy$fear +
  data_oxy$sadness
mean(data_oxy$positive)
mean(data_oxy$negative)
mean(data_oxy$neutral)

data3 <- data2[grep("joy",data2$Filename),]
data_case <- data3 %>% filter(Case == 8)
data_pl <- data_case %>% group_by(participantID.y) %>% summarise_all(funs(mean))
data_pl$positive <- data_pl$joy

data_pl$negative <- data_pl$anger + data_pl$disgust + data_pl$fear +
  data_pl$sadness

mean(data_pl$positive)
mean(data_pl$negative)
mean(data_pl$neutral)
a <- data.frame()
t1 <- t.test(data_oxy$positive,data_pl$positive)
t2 <- t.test(data_oxy$negative,data_pl$negative)
t3 <-t.test(data_oxy$neutral,data_pl$neutral)
a[1,c("positive")] <- t1$p.value
a[1,c("negative")] <- t2$p.value
a[1,c("neutral")] <- t3$p.value

# negative
data3 <- data2[grep("neg",data2$Filename),]
data_case <- data3 %>% filter(Case == 7)
data_oxy <- data_case %>% group_by(participantID.x) %>% summarise_all(funs(mean))
data_oxy$positive <- data_oxy$joy

data_oxy$negative <- data_oxy$anger + data_oxy$disgust + data_oxy$fear +
  data_oxy$sadness
mean(data_oxy$positive)
mean(data_oxy$negative)
mean(data_oxy$neutral)

data3 <- data2[grep("neg",data2$Filename),]
data_case <- data3 %>% filter(Case == 8)
data_pl <- data_case %>% group_by(participantID.y) %>% summarise_all(funs(mean))
data_pl$positive <- data_pl$joy

data_pl$negative <- data_pl$anger + data_pl$disgust + data_pl$fear +
  data_pl$sadness

mean(data_pl$positive, na.rm = TRUE)
mean(data_pl$negative, na.rm = TRUE)
mean(data_pl$neutral, na.rm = TRUE)

t1 <- t.test(data_oxy$positive,data_pl$positive)
t2 <- t.test(data_oxy$negative,data_pl$negative)
t3 <-t.test(data_oxy$neutral,data_pl$neutral)
a[2,c("positive")] <- t1$p.value
a[2,c("negative")] <- t2$p.value
a[2,c("neutral")] <- t3$p.value

# neutral
data3 <- data2[grep("neu",data2$Filename),]
data_case <- data3 %>% filter(Case == 7)
data_oxy <- data_case %>% group_by(participantID.x) %>% summarise_all(funs(mean))
data_oxy$positive <- data_oxy$joy

data_oxy$negative <- data_oxy$anger + data_oxy$disgust + data_oxy$fear +
  data_oxy$sadness
mean(data_oxy$positive)
mean(data_oxy$negative)
mean(data_oxy$neutral)
data3 <- data2[grep("neu",data2$Filename),]
data_case <- data3 %>% filter(Case == 8)
data_pl <- data_case %>% group_by(participantID.y) %>% summarise_all(funs(mean))
data_pl$positive <- data_pl$joy

data_pl$negative <- data_pl$anger + data_pl$disgust + data_pl$fear +
  data_pl$sadness

mean(data_pl$positive)
mean(data_pl$negative)
mean(data_pl$neutral)
t1 <- t.test(data_oxy$positive,data_pl$positive)
t2 <- t.test(data_oxy$negative,data_pl$negative)
t3 <-t.test(data_oxy$neutral,data_pl$neutral)
a[3,c("positive")] <- t1$p.value
a[3,c("negative")] <- t2$p.value
a[3,c("neutral")] <- t3$p.value
