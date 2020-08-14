# Evocative Videos Unbinding

setwd("C:/Users/jakep/Dropbox/PORQ_dataForUW/evocativeVideoTask")
evoc_trans <- read.xlsx("PORQ_evocativeVideoTask_transcripts.xlsx", sheetIndex = 1)

# read unbinding
setwd("..")
unbinding <- read.xlsx("PORQ_drugUnblinding.xlsx", sheetIndex = 1)

# IDDay
evoc_trans$IDDay <- paste(evoc_trans$participantID, evoc_trans$day, sep="_")
unbinding$IDDay <- paste(unbinding$participantID, unbinding$day, sep="_")

# Join on ID Day
data <- left_join(unbinding, evoc_trans, by="IDDay")
