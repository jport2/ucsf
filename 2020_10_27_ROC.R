library(xlsx)
library(dplyr)
library(ggplot2)
library(wordspace)
library(ROCR)
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
data2 <- emo_unblind
emo_unblind2 <- emo_unblind[,c("positive","negative")]
emo_unblind <- emo_unblind[ , -which(names(emo_unblind) %in% c("positive","negative","neutral"))]
emo_unblind2 <- normalize.rows(as.matrix(emo_unblind2),method="manhattan")
emo_unblind_norm <- cbind(emo_unblind,emo_unblind2)
data5 <- emo_unblind_norm %>% group_by(id, emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data5$Case <- as.factor(substr(data5$id,1,1))
data6 <- data5 %>% filter(emo == "joy")
data6[,"pos_max"] <- apply(data6[,c("admiration", "amusement","approval",
                                    "caring", "desire", "excitement", "gratitude", 
                                    "joy", "love","optimism", "pride", "relief")],1,max)
data6[,"neg_max"] <- apply(data6[,c("anger", "annoyance", "disappointment", "disapproval","disgust","embarrassment","fear","grief","nervousness", "remorse","sadness")],1,max)
data6_r <- data6[,c("pos_max","neg_max")]
data6_r <- normalize.rows(as.matrix(data6_r),method="manhattan")
data6 <- data6[ , -which(names(data6) %in% c("pos_max","neg_max"))]
data6_r <- as.data.frame(data6_r)
data6_r2 <- cbind(data6,data6_r)

#glm <- glm(Case~positive,data=train,family=binomial())
#test_prob = predict(data6_r2$positive, data6_r2$Case, type = "response")
pred <- prediction( data6_r2$pos_max, data6_r2$Case )
perf <- performance( pred, "tpr", "fpr" )
text = performance(pred, "auc")@y.values
plot( perf, col="red")
text(1,0.25,labels=paste("AUC =",text,sep=" "), cex=0.5)
pred <- prediction( data6_r2$positive, data6_r2$Case )
perf <- performance( pred, "tpr", "fpr" )
text = performance(pred, "auc")@y.values
plot( perf, col="blue", add=TRUE)

pred <- prediction( data6_r2$negative, data6_r2$Case )
perf <- performance( pred, "tpr", "fpr" )
text = performance(pred, "auc")@y.values
plot( perf, col="purple", add=TRUE)

pred <- prediction( data6_r2$neg_max, data6_r2$Case )
perf <- performance( pred, "tpr", "fpr" )
text = performance(pred, "auc")@y.values
plot( perf, col="orange", add=TRUE)

data2 <- data2[complete.cases(data2$Case),]
data2 <- data2[complete.cases(data2$positive),]
data2 <- data2[complete.cases(data2$negative),]

pred <- prediction( data2$positive, data2$Case )
perf <- performance( pred, "tpr", "fpr" )
text = performance(pred, "auc")@y.values
plot( perf, add=TRUE)

pred <- prediction( data2$negative, data2$Case )
perf <- performance( pred, "tpr", "fpr" )
text = performance(pred, "auc")@y.values
plot( perf, col="yellow", add=TRUE)

data6 <- data5 %>% filter(emo == "joy")
data7 <- data5 %>% filter(emo=="neg")
data67 <- inner_join(data6, data7, by="id")
data8 <- data5 %>% filter(emo == "neu")
data678 <- inner_join(data67, data8, by="id")
data678$trace <- data678$positive.x + data678$negative.y

pred <- prediction(data678$trace, data678$Case)
perf <- performance( pred, "tpr", "fpr" )
text = performance(pred, "auc")@y.values
plot( perf, col="gray", add=TRUE)
text(1,0.15,labels=paste("AUC =",text,sep=" "), cex=0.5)

data6 <- data5 %>% filter(emo == "joy")
data6[,"pos_max"] <- apply(data6[,c("admiration", "amusement","approval",
                                    "caring", "desire", "excitement", "gratitude", 
                                    "joy", "love","optimism", "pride", "relief")],1,max)
data6[,"neg_max"] <- apply(data6[,c("anger", "annoyance", "disappointment", "disapproval","disgust","embarrassment","fear","grief","nervousness", "remorse","sadness")],1,max)
data6_r <- data6[,c("pos_max","neg_max")]
data6_r <- normalize.rows(as.matrix(data6_r),method="manhattan")
data6 <- data6[ , -which(names(data6) %in% c("pos_max","neg_max"))]
data6_r <- as.data.frame(data6_r)
data6_r2 <- cbind(data6,data6_r)

data7 <- data5 %>% filter(emo=="neg")
data7[,"pos_max"] <- apply(data7[,c("admiration", "amusement","approval",
                                    "caring", "desire", "excitement", "gratitude", 
                                    "joy", "love","optimism", "pride", "relief")],1,max)
data7[,"neg_max"] <- apply(data7[,c("anger", "annoyance", "disappointment", "disapproval","disgust","embarrassment","fear","grief","nervousness", "remorse","sadness")],1,max)
data7_r <- data7[,c("pos_max","neg_max")]
data7_r <- normalize.rows(as.matrix(data7_r),method="manhattan")
data7 <- data7[ , -which(names(data7) %in% c("pos_max","neg_max"))]
data7_r <- as.data.frame(data7_r)
data7_r2 <- cbind(data7,data7_r)

data67 <- inner_join(data6_r2, data7_r2, by="id")
data8 <- data5 %>% filter(emo == "neu")
data678 <- inner_join(data67, data8, by="id")
data678$trace <- data678$pos_max.x + data678$neg_max.y 

pred <- prediction(data678$trace, data678$Case)
perf <- performance( pred, "tpr", "fpr" )
text = performance(pred, "auc")@y.values
plot( perf, col="brown", add=TRUE)
text(1,0.2,labels=paste("AUC =",text,sep=" "), cex=0.5)
