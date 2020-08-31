# Evocative Videos Unbinding
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

# join to go emotions
setwd("C:/Users/jakep/Desktop/ucsf")
goemo <- read.csv("go_emotions_output.csv")
goemo$Filename <- substr(goemo$file, 17,50)

data$Filename <- paste(data$participantID.y,data$stimulus,sep="_")
data$Filename <- paste(data$Filename,".txt",sep="")

emo_unblind <- left_join(goemo,data,by="Filename")

# PULL OUT NON OXY CASES AND CONTROLS



# separate oxy v non oxy
# Read In emotions from file
setwd("C:/Users/jakep/Dropbox/PORQ_dataForUW/evocativeVideoTask")
evoc <- read.csv("LIWC2015 Results (emotextfiles (1732 files)).csv") #change to evoc
#tom <- tom[1:762,]

# 
evoc <- evoc %>% rename(Oxytocin = drugCondition)
evoc <- evoc %>% filter(Case == '7')
library(tidyr)
evoc <- evoc %>% mutate(IDDrug = paste(Source..A.,Oxytocin, sep="_"))
data2 <- evoc %>% group_by(IDDrug) %>% summarise_all(funs(mean))
data2 <- data2[-c(23,42),]
data2$Oxy <- substr(data2$IDDrug,6,7)
data3 <- data2 %>% filter(Oxy == "PL")
data4 <- data2 %>% filter(Oxy == "OT")
colnames(data4) <- paste(colnames(data4),"1", sep = ".")
data5 <- cbind(data3,data4)