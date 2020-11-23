library(xlsx)
library(dplyr)
library(ggplot2)
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
data3$Case <- substr(data3$participantID.y,1,1)
data3 <- data3 %>% filter(Case == "7")

data4 <- data3[grep("joy",data3$Filename),]
data_case <- data4 %>% filter(drugCondition == "PL")
data_case <- data_case %>% group_by(participantID.x) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_case$positive <- data_case$posemo
data_case$negative <- data_case$negemo
data_case$neutral <- 100 - data_case$posemo - data_case$negemo
mean(data_case$positive, na.rm=TRUE)
mean(data_case$negative, na.rm=TRUE)


data4 <- data3[grep("joy",data3$Filename),]
data_control <- data4 %>% filter(drugCondition == "OT")
data_control <- data_control %>% group_by(participantID.y) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_control$positive <- data_control$posemo

data_control$negative <- data_control$negemo
data_control$neutral <- 100 - data_control$posemo - data_control$negemo

mean(data_control$positive, na.rm = TRUE)
mean(data_control$negative, na.rm = TRUE)

data_join <- inner_join(data_case, data_control, by="participantID.x")
liwcp2 <- data.frame()
t1 <- t.test(data_join$positive.x, data_join$positive.y, paired=TRUE)
t2 <- t.test(data_join$negative.x, data_join$negative.y, paired=TRUE)
t3 <- t.test(data_join$neutral.x, data_join$neutral.y, paired=TRUE)
liwcp2[c("joy"),c("posemo")] <- t1$p.value
liwcp2[c("joy"),c("negemo")] <- t2$p.value
liwcp2[c("joy"),c("neuemo")] <- t3$p.value


### neg ################
data4 <- data3[grep("neg",data3$Filename),]
data_case <- data4 %>% filter(drugCondition == "PL")
data_case <- data_case %>% group_by(participantID.x) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_case$positive <- data_case$posemo
data_case$negative <- data_case$negemo
data_case$neutral <- 100 - data_case$posemo - data_case$negemo
mean(data_case$positive, na.rm=TRUE)
mean(data_case$negative, na.rm=TRUE)


data4 <- data3[grep("neg",data3$Filename),]
data_control <- data4 %>% filter(drugCondition == "OT")
data_control <- data_control %>% group_by(participantID.y) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_control$positive <- data_control$posemo

data_control$negative <- data_control$negemo
data_control$neutral <- 100 - data_control$posemo - data_control$negemo

mean(data_control$positive, na.rm = TRUE)
mean(data_control$negative, na.rm = TRUE)

data_join <- inner_join(data_case, data_control, by="participantID.x")
t1 <- t.test(data_join$positive.x, data_join$positive.y, paired=TRUE)
t2 <- t.test(data_join$negative.x, data_join$negative.y, paired=TRUE)
t3 <- t.test(data_join$neutral.x, data_join$neutral.y, paired=TRUE)
liwcp2[c("neg"),c("posemo")] <- t1$p.value
liwcp2[c("neg"),c("negemo")] <- t2$p.value
liwcp2[c("neg"),c("neuemo")] <- t3$p.value

### neu ################
data4 <- data3[grep("neu",data3$Filename),]
data_case <- data4 %>% filter(drugCondition == "PL")
data_case <- data_case %>% group_by(participantID.x) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_case$positive <- data_case$posemo
data_case$negative <- data_case$negemo
data_case$neutral <- 100 - data_case$posemo - data_case$negemo
mean(data_case$positive, na.rm=TRUE)
mean(data_case$negative, na.rm=TRUE)


data4 <- data3[grep("neu",data3$Filename),]
data_control <- data4 %>% filter(drugCondition == "OT")
data_control <- data_control %>% group_by(participantID.y) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_control$positive <- data_control$posemo

data_control$negative <- data_control$negemo
data_control$neutral <- 100 - data_control$posemo - data_control$negemo

mean(data_control$positive, na.rm = TRUE)
mean(data_control$negative, na.rm = TRUE)

data_join <- inner_join(data_case, data_control, by="participantID.x")
t1 <- t.test(data_join$positive.x, data_join$positive.y, paired=TRUE)
t2 <- t.test(data_join$negative.x, data_join$negative.y, paired=TRUE)
t3 <- t.test(data_join$neutral.x, data_join$neutral.y, paired=TRUE)
liwcp2[c("neu"),c("posemo")] <- t1$p.value
liwcp2[c("neu"),c("negemo")] <- t2$p.value
liwcp2[c("neu"),c("neuemo")] <- t3$p.value

