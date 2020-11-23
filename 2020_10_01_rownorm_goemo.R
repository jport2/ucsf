# Row normalize on individual level
# mann whitney u/wilcoxon between groups
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

emo_unblind2 <- emo_unblind[,c("positive","negative")]
emo_unblind <- emo_unblind[ , -which(names(emo_unblind) %in% c("positive","negative","neutral"))]
emo_unblind2 <- normalize.rows(as.matrix(emo_unblind2),method="manhattan")
emo_unblind_norm <- cbind(emo_unblind,emo_unblind2)

#data4 <- emo_unblind_norm %>% filter(Case == "7")
# (1) average probability across all 1-positive, 2-negative and 3-neutral emotions
data5 <- emo_unblind_norm %>% group_by(id, emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data5$Case <- as.factor(substr(data5$id,1,1))
data_joy <- data5 %>% filter(emo == "joy")
t.test(positive~Case, data=data_joy)

set.seed(2)
samp <- sample(c(1:88), size=71, replace=FALSE)
train <- data_joy[samp,]
test <- data_joy[-samp,]
glm <- glm(Case~positive,data=train,family=binomial())
test_prob = predict(glm, newdata = test, type = "response")
pred <- prediction( test_prob, test$Case )
perf <- performance( pred, "tpr", "fpr" )
text = performance(pred, "auc")@y.values
plot( perf, col="red")
text(1,0.25,labels=paste("AUC =",text,sep=" "))


data6 <- data5 %>% filter(emo == "joy")
#data6_r <- normalize.rows(as.matrix(data6[,c("positive","negative","neutral")]),method="manhattan")
#data6_r2 <- cbind(data6,data6_r)
data7 <- data5 %>% filter(emo=="neg")
#data7 <- normalize.rows(as.matrix(data7),method="manhattan")

data67 <- inner_join(data6, data7, by="id")
#data67 <- normalize.rows(as.matrix(data67),method="manhattan")

data8 <- data5 %>% filter(emo == "neu")
data678 <- inner_join(data67, data8, by="id")
data678$trace <- data678$positive.x + data678$negative.y 
t.test(trace~Case, data=data678)

set.seed(2)
samp <- sample(c(1:88), size=71, replace=FALSE)
train <- data678[samp,]
test <- data678[-samp,]
glm <- glm(Case~trace,data=train,family=binomial())
test_prob = predict(glm, newdata = test, type = "response")
pred <- prediction( test_prob, test$Case )
perf <- performance( pred, "tpr", "fpr" )
text = performance(pred, "auc")@y.values
plot( perf, col="red")
text(1,0.25,labels=paste("AUC =",text,sep=" "))

set.seed(2)
samp <- sample(c(1:88), size=71, replace=FALSE)
train <- data678[samp,]
test <- data678[-samp,]
glm <- glm(Case~positive.x+negative.y,data=train,family=binomial())
test_prob = predict(glm, newdata = test, type = "response")
pred <- prediction( test_prob, test$Case )
perf <- performance( pred, "tpr", "fpr" )
text = performance(pred, "auc")@y.values
plot( perf, col="red")
text(1,0.25,labels=paste("AUC =",text,sep=" "))



data5 <- data5 %>% group_by(emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
#data4 <- data3 %>% group_by(participantID.y) %>% summarise_all(funs(mean))

# (3) the result should be a 3x3 matrix
data6 <- data5[,c("emo","positive","negative","neutral")]
# (4) row normalize (the rows are videos, the columns emotions)
data7 <- normalize.rows(as.matrix(data6[,2:4]),method="manhattan")
# (5) take the trace (sum of the diagonal)
sum(diag(data7))

# Case == "8"
data4 <- emo_unblind_norm %>% filter(Case == "8")
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

#For OT
# Join on ID Day
data <- full_join(unblinding, evoc_trans, by="IDDay")
# replace na's for 8 with pl
#data[is.na(data$drugCondition),c("drugCondition")] <- "PL"
# filter for only placebo
data$Case <- as.factor(substr(data$participantID.x,1,1))
data <- data %>% filter(Case == "7")
# join to go emotions
setwd("C:/Users/jakep/Desktop")
goemo <- read.csv("go_emotions_output.csv")
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
emo_unblind$positive <- (emo_unblind$admiration + emo_unblind$amusement + emo_unblind$approval+
  emo_unblind$caring + emo_unblind$desire + emo_unblind$excitement + emo_unblind$gratitude + emo_unblind$joy +
  emo_unblind$love + emo_unblind$optimism + emo_unblind$pride + emo_unblind$relief)/12

emo_unblind$negative <- (emo_unblind$anger + emo_unblind$annoyance + emo_unblind$disappointment +  
  emo_unblind$disapproval+emo_unblind$disgust+emo_unblind$embarrassment+emo_unblind$fear+emo_unblind$grief+
  emo_unblind$nervousness + emo_unblind$remorse+emo_unblind$sadness)/11

emo_unblind2 <- emo_unblind[,c("positive","negative","neutral")]
emo_unblind <- emo_unblind[ , -which(names(emo_unblind) %in% c("positive","negative","neutral"))]
emo_unblind2 <- normalize.rows(as.matrix(emo_unblind2),method="manhattan")
emo_unblind_norm <- cbind(emo_unblind,emo_unblind2)

#data4 <- emo_unblind_norm %>% filter(Case == "7")
# (1) average probability across all 1-positive, 2-negative and 3-neutral emotions
data5 <- emo_unblind_norm %>% group_by(id, emo,drugCondition) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_ot <- data5 %>% filter(drugCondition=="OT")
data_pl <- data5 %>% filter(drugCondition=="PL")
data5 <- inner_join(data_ot,data_pl, by="id")
data5$Case <- as.factor(substr(data5$id,1,1))
data_pos <- data5 %>% filter(emo.x == "joy")
t.test(data_pos$positive.x,data_pos$positive.y,paired = TRUE)

data5 <- emo_unblind_norm %>% group_by(id, emo,drugCondition) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data_ot <- data5 %>% filter(drugCondition=="OT")
data_pl <- data5 %>% filter(drugCondition=="PL")
data5 <- inner_join(data_ot,data_pl, by="id")

data6 <- data5 %>% filter(emo.x == "joy")
data7 <- data5 %>% filter(emo.x=="neg")
data67 <- inner_join(data6, data7, by="id")
data8 <- data5 %>% filter(emo.x == "neu")
data678 <- inner_join(data67, data8, by="id")
data678$trace <- data678$positive.x + data678$negative.y + data678$neutral
t.test(data678$trace.x,data678$trace.y, paired=TRUE)


data5 <- data5 %>% group_by(emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
#data4 <- data3 %>% group_by(participantID.y) %>% summarise_all(funs(mean))

# (3) the result should be a 3x3 matrix
data6 <- data5[,c("emo","positive","negative","neutral")]
# (4) row normalize (the rows are videos, the columns emotions)
data7 <- normalize.rows(as.matrix(data6[,2:4]),method="manhattan")
# (5) take the trace (sum of the diagonal)
sum(diag(data7))


