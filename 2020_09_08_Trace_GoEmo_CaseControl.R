# Individual Trace Score

#
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
goemo$Filename <- paste(goemo$id,goemo$emo,goemo$thing, sep="_")
# Create variable for joining
data$Filename <- paste(data$participantID.y,data$stimulus,sep="_")
#data$Filename <- paste(data$Filename,".txt",sep="")

emo_unblind <- right_join(goemo,data,by="Filename")
emo_unblind$Case <- substr(emo_unblind$Filename,1,1)
data2 <- emo_unblind
emo_unblind$emo <- gsub(" neg","neg",emo_unblind$emo)

# Define emotions
#emo_unblind <- emo_unblind %>% group_by(participantID.x) %>% summarise_all(funs(mean))
emo_unblind$positive <- emo_unblind$admiration + emo_unblind$amusement + emo_unblind$approval+
  emo_unblind$caring + emo_unblind$desire + emo_unblind$excitement + emo_unblind$gratitude + emo_unblind$joy +
  emo_unblind$love + emo_unblind$optimism + emo_unblind$pride + emo_unblind$relief

emo_unblind$negative <- emo_unblind$anger + emo_unblind$annoyance + emo_unblind$disappointment +         emo_unblind$disapproval+emo_unblind$disgust+emo_unblind$embarrassment+emo_unblind$fear+emo_unblind$grief+
  emo_unblind$nervousness + emo_unblind$remorse+emo_unblind$sadness

#data4 <- emo_unblind %>% filter(Case == "7")
# (1) average probability across all 1-positive, 2-negative and 3-neutral emotions
data5 <- emo_unblind %>% group_by(id, emo) %>% summarise_all(funs(mean(., na.rm = TRUE)))
data6 <- data5 %>% filter(emo == "joy")
data7 <- data5 %>% filter(emo=="neg")
data67 <- inner_join(data6, data7, by="id")
data8 <- data5 %>% filter(emo == "neu")
data678 <- inner_join(data67, data8, by="id")
data678$trace <- data678$positive.x + data678$negative.y + data678$neutral
data678$Case <- substr(data678$id,1,1)
t.test(data678$trace~data678$Case)

data678 <- data678 %>% rename(ID = id)
scales_trace <- inner_join(data678,ratings_scales, by="ID")
scales_trace <- unique( scales_trace[ , c("ID","trace","QLS","AMNART","CAINS",                                                                                             
                                           "PANSSpos" ,                                                                                         
                                           "PANSSneg"  ,                                                                                        
                                          "PANSSdisorganization"  ,                                                                            
                                           "RFS"                    ,                                                                           
                                           "Hinting"                 ,                                                                          
                                          "Defeatist"                ,                                                                         
                                           "ECRRSmom"                  ,                                                                        
                                           "ECRRSdad"                   ,                                                                       
                                           "ECRRSromantic"               ,                                                                      
                                          "ECRRSfriend"                  ,                                                                     
                                           "ECRRS")]) 
scales_trace$Case <- substr(scales_trace$ID,1,1)
ggplot(scales_trace, aes(y=trace,x=CAINS,color=Case))+geom_point()+geom_smooth(method=lm)
ggplot(scales_trace, aes(y=trace,x=PANSSpos,color=Case))+geom_point()+geom_smooth(method=lm)
ggplot(scales_trace, aes(y=trace,x=PANSSneg,color=Case))+geom_point()+geom_smooth(method=lm)
ggplot(scales_trace, aes(y=trace,x=PANSSdisorganization,color=Case))+geom_point()+geom_smooth(method=lm)
ggplot(scales_trace, aes(y=trace,x=RFS,color=Case))+geom_point()+geom_smooth(method=lm)
ggplot(scales_trace, aes(y=trace,x=QLS,color=Case))+geom_point()+geom_smooth(method=lm)
ggplot(scales_trace, aes(y=trace,x=Defeatist,color=Case))+geom_point()+geom_smooth(method=lm)
ggplot(scales_trace, aes(y=trace,x=Hinting,color=Case))+geom_point()+geom_smooth(method=lm)
ggplot(scales_trace, aes(y=trace,x=ECRRS,color=Case))+geom_point()+geom_smooth(method=lm)
ggplot(scales_trace, aes(y=trace,x=ECRRSmom,color=Case))+geom_point()+geom_smooth(method=lm)
ggplot(scales_trace, aes(y=trace,x=ECRRSdad,color=Case))+geom_point()+geom_smooth(method=lm)
ggplot(scales_trace, aes(y=trace,x=ECRRSromantic,color=Case))+geom_point()+geom_smooth(method=lm)
ggplot(scales_trace, aes(y=trace,x=ECRRSfriend,color=Case))+geom_point()+geom_smooth(method=lm)