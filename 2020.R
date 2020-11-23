library(xlsx)
library(dplyr)
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
#data <- data %>% filter(drugCondition == "PL")
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

### joy #########################
data3 <- data2[grep("joy",data2$Filename),]
data_oxy <- data3 %>% filter(drugCondition == "OT")
data_oxy <- data_oxy %>% group_by(participantID.x) %>% summarise_all(funs(mean))
# group_by id
data_pl <- data3 %>% filter(drugCondition == "PL")
data_pl <- data_pl %>% group_by(participantID.x) %>% summarise_all(funs(mean))

# group_by id
colnames(data_pl) <- paste(colnames(data_pl),"1", sep = ".")
data_oxy <- data_oxy[complete.cases(data_oxy$neutral),]
data_pl <- data_pl[complete.cases(data_pl$neutral.1),]
data_pl$participantID.x <- data_pl$participantID.x.1
# data5 <- inner_join(data_oxy,data_pl, by="participantID.x")


#data_oxy <- data3 %>% filter(drugCondition == "OT")
#data_oxy <- data_oxy %>% group_by(participantID.x) %>% summarise_all(funs(mean))
data_oxy$positive <- data_oxy$joy

data_oxy$negative <- data_oxy$anger + data_oxy$disgust + data_oxy$fear +
  data_oxy$sadness

mean(data_oxy$positive)
mean(data_oxy$negative)
mean(data_oxy$neutral)

data3 <- data2[grep("joy",data2$Filename),]
data_pl <- data3 %>% filter(drugCondition == "PL")
data_pl <- data_pl %>% group_by(participantID.x) %>% summarise_all(funs(mean))
data_pl$positive <- data_pl$joy

data_pl$negative <- data_pl$anger + data_pl$disgust + data_pl$fear +
  data_pl$sadness

mean(data_pl$positive)
mean(data_pl$negative)
mean(data_pl$neutral)
data_paired <- left_join(data_oxy,data_pl, by="participantID.x")
a <- data.frame()
t1 <- t.test(data_paired$positive.x, data_paired$positive.y, paired=TRUE)
t2 <- t.test(data_paired$negative.x, data_paired$negative.y, paired=TRUE)
t3 <-t.test(data_paired$neutral.x, data_paired$neutral.y, paired=TRUE)
a[c("joy"),c("positive")] <- t1$p.value
a[c("joy"),c("negative")] <- t2$p.value
a[c("joy"),c("neutral")] <- t3$p.value

### neg #########################
data3 <- data2[grep("neg",data2$Filename),]
data_oxy <- data3 %>% filter(drugCondition == "OT")
data_oxy <- data_oxy %>% group_by(participantID.x) %>% summarise_all(funs(mean))
# group_by id
data_pl <- data3 %>% filter(drugCondition == "PL")
data_pl <- data_pl %>% group_by(participantID.x) %>% summarise_all(funs(mean))

# group_by id
colnames(data_pl) <- paste(colnames(data_pl),"1", sep = ".")
data_oxy <- data_oxy[complete.cases(data_oxy$neutral),]
data_pl <- data_pl[complete.cases(data_pl$neutral.1),]
data_pl$participantID.x <- data_pl$participantID.x.1
# data5 <- inner_join(data_oxy,data_pl, by="participantID.x")


#data_oxy <- data3 %>% filter(drugCondition == "OT")
#data_oxy <- data_oxy %>% group_by(participantID.x) %>% summarise_all(funs(mean))
data_oxy$positive <- data_oxy$joy

data_oxy$negative <- data_oxy$anger + data_oxy$disgust + data_oxy$fear +
  data_oxy$sadness

mean(data_oxy$positive)
mean(data_oxy$negative)
mean(data_oxy$neutral)

data3 <- data2[grep("neg",data2$Filename),]
data_pl <- data3 %>% filter(drugCondition == "PL")
data_pl <- data_pl %>% group_by(participantID.x) %>% summarise_all(funs(mean))
data_pl$positive <- data_pl$joy

data_pl$negative <- data_pl$anger + data_pl$disgust + data_pl$fear +
  data_pl$sadness

mean(data_pl$positive)
mean(data_pl$negative)
mean(data_pl$neutral)
data_paired <- left_join(data_oxy,data_pl, by="participantID.x")
#a <- data.frame()
t1 <- t.test(data_paired$positive.x, data_paired$positive.y, paired=TRUE)
t2 <- t.test(data_paired$negative.x, data_paired$negative.y, paired=TRUE)
t3 <-t.test(data_paired$neutral.x, data_paired$neutral.y, paired=TRUE)
a[c("neg"),c("positive")] <- t1$p.value
a[c("neg"),c("negative")] <- t2$p.value
a[c("neg"),c("neutral")] <- t3$p.value

### neu #########################
data3 <- data2[grep("neu",data2$Filename),]
data_oxy <- data3 %>% filter(drugCondition == "OT")
data_oxy <- data_oxy %>% group_by(participantID.x) %>% summarise_all(funs(mean))
# group_by id
data_pl <- data3 %>% filter(drugCondition == "PL")
data_pl <- data_pl %>% group_by(participantID.x) %>% summarise_all(funs(mean))

# group_by id
colnames(data_pl) <- paste(colnames(data_pl),"1", sep = ".")
data_oxy <- data_oxy[complete.cases(data_oxy$neutral),]
data_pl <- data_pl[complete.cases(data_pl$neutral.1),]
data_pl$participantID.x <- data_pl$participantID.x.1
# data5 <- inner_join(data_oxy,data_pl, by="participantID.x")


#data_oxy <- data3 %>% filter(drugCondition == "OT")
#data_oxy <- data_oxy %>% group_by(participantID.x) %>% summarise_all(funs(mean))
data_oxy$positive <- data_oxy$joy

data_oxy$negative <- data_oxy$anger + data_oxy$disgust + data_oxy$fear +
  data_oxy$sadness

mean(data_oxy$positive)
mean(data_oxy$negative)
mean(data_oxy$neutral)

data3 <- data2[grep("neu",data2$Filename),]
data_pl <- data3 %>% filter(drugCondition == "PL")
data_pl <- data_pl %>% group_by(participantID.x) %>% summarise_all(funs(mean))
data_pl$positive <- data_pl$joy

data_pl$negative <- data_pl$anger + data_pl$disgust + data_pl$fear +
  data_pl$sadness

mean(data_pl$positive)
mean(data_pl$negative)
mean(data_pl$neutral)
data_paired <- left_join(data_oxy,data_pl, by="participantID.x")
#a <- data.frame()
t1 <- t.test(data_paired$positive.x, data_paired$positive.y, paired=TRUE)
t2 <- t.test(data_paired$negative.x, data_paired$negative.y, paired=TRUE)
t3 <-t.test(data_paired$neutral.x, data_paired$neutral.y, paired=TRUE)
a[c("neu"),c("positive")] <- t1$p.value
a[c("neu"),c("negative")] <- t2$p.value
a[c("neu"),c("neutral")] <- t3$p.value

