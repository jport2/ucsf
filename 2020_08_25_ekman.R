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

#emo_unblind <- emo_unblind %>% filter(emo == "joy")
temp <- emo_unblind %>% group_by(Case) %>% summarise_all(funs(mean(., na.rm = TRUE)))
a <- data.frame()
for(i in 2:29){
  var1 <- colnames(emo_unblind)[i]
  t <- t.test(emo_unblind[,var1]~emo_unblind$Case, alternative = "two.sided")
  a[i-1,c('p_value')] <- t$p.value
  var <- colnames(emo_unblind)[i]
  a[i-1,c('var')] <- colnames(emo_unblind)[i]
  a[i-1, c("Cases")] <- temp[1,var]
  a[i-1, c('Controls')] <- temp[2,var]
  #   library(broom)
  #   library(dplyr)
  # a <- liwc %>% group_by(Status) %>% do(tidy(t.test(liwc[i])))
}
attach(a)
# sort by p
newdata <- a[order(p_value),]
detach(a)
newdata$p_value <- round(newdata$p_value, digits=3)
newdata$Cases <- round(newdata$Cases, digits=3)
newdata$Controls <- round(newdata$Controls, digits=3)
print(newdata)



### Paired Oxy

# Join on ID Day
data <- full_join(unblinding, evoc_trans, by="IDDay")
# replace na's for 8 with pl
data2 <- data[!is.na(data$drugCondition),]
#data[is.na(data$drugCondition),c("drugCondition")] <- "PL"
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
#emo_unblind <- emo_unblind %>% filter(emo == "neg")
data2 <- emo_unblind

# create ID treatment
# group_by id treatment
#data2 <- data2 %>% group_by(participantID.x) %>% summarise_all(funs(mean))
data_oxy <- data2 %>% filter(drugCondition == "OT")
data_oxy <- data_oxy %>% group_by(participantID.x) %>% summarise_all(funs(mean))
# group_by id
data_pl <- data2 %>% filter(drugCondition == "PL")
data_pl <- data_pl %>% group_by(participantID.x) %>% summarise_all(funs(mean))

# group_by id
colnames(data_pl) <- paste(colnames(data_pl),"1", sep = ".")
data_pl$participantID.x <- data_pl$participantID.x.1
data5 <- left_join(data_oxy,data_pl, by="participantID.x")
emo_unblind_pair <- data5[complete.cases(data5$anger.1),]
emo_unblind_pair <- as.data.frame(emo_unblind_pair)
a <- data.frame()
for(i in 3:9){
  var1 <- colnames(emo_unblind_pair)[i]
  var2 <- colnames(emo_unblind_pair)[i+28]
  t <- t.test(emo_unblind_pair[,var1], emo_unblind_pair[,var2], paired = TRUE, alternative = "two.sided")
  a[i-2,c('p_value')] <- t$p.value
  a[i-2,c('var')] <- colnames(emo_unblind_pair)[i]
  #a[i-6,c("diff")] <- mean(data5[,i+106]) - mean(data5[,i])
  a[i-2,c("mean_ot")] <- mean(emo_unblind_pair[,i],na.rm=TRUE)
  a[i-2,c("mean_pl")] <- mean(emo_unblind_pair[,i+28],na.rm=TRUE)
  #   library(broom)
  #   library(dplyr)
  # a <- liwc %>% group_by(Status) %>% do(tidy(t.test(liwc[i])))
}
attach(a)
# sort by p
newdata <- a[order(p_value),]
detach(a)
newdata$p_value <- round(newdata$p_value, digits=3)
newdata$mean_ot <- round(newdata$mean_ot, digits=3)
newdata$mean_pl <- round(newdata$mean_pl, digits=3)
print(newdata)