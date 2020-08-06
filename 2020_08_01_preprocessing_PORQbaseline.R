sz <- read.csv("PORQ_demographicAndBaselineDayData_labeled-1.csv")
#View(sz)
#sz <- load("PORQ_demographicAndBaselineDayData.r")
#sz$Item.1..Motivation.for.Close.Family.Spouse.Partner.Relationships <-
 # sub('^[^_]*_(\\d+).*', '\\1',sz$Item.1..Motivation.for.Close.Family.Spouse.Partner.Relationships)
 # as.numeric(gsub("([0-9]+).*$", "\\1", sz$Item.1..Motivation.for.Close.Family.Spouse.Partner.Relationships))
#  gsub("[^0-9.]", "",  sz$Item.1..Motivation.for.Close.Family.Spouse.Partner.Relationships)
for(i in 62:221){
  sz[,i] <- as.numeric(gsub("[^0-9.]", "",sz[,i]))
}
sz[,c("X")] <- as.numeric(gsub("[^0-9.]", "",sz[,"X"]))

# CAINS (NEGATIVE SYMPTOMS)
sz$CAINS <- sz[,62]+sz[,63]+sz[,64]+sz[,65]+sz[,66]+sz[,67]+sz[,68]+
  sz[,69]+sz[,70]+sz[,71]+sz[,72]+sz[,73]+sz[,74]+sz[,75]
# neg symp with dose
ggplot(data=sz, aes(x=CAINS, y=X))+geom_point()+geom_smooth(method=lm)
cor(sz$CAINS, sz$X, use="complete.obs")
# PANSS
sz$PANSS <- sz[,76]+sz[,77]+sz[,78]+sz[,79]+sz[,80]+sz[,81]+sz[,82]+sz[,83]
sz$PANSScoh <- sz[,82]+sz[,83]

# QLS
sz <- sz %>% rename(QLS = Subscale.total.)

# Role functioning scale
sz$RFS <- sz[,88]+sz[,89]+sz[,90]+sz[,91]

# Hinting Task
sz$Hinting <- sz$Total+sz$Total.1+sz$Total.2+sz$Total.3+sz$Total.4+
  sz$Total.5+sz$Total.6+sz$Total.7+sz$Total.8+sz$Total.9

# AMNART
sz <- sz %>% rename(AMNART = Total.score)

# Defeatest Beliefs
sz$Defeatist <- sz[,172]+sz[,173]+sz[,174]+sz[,175]+sz[,176]+sz[,178]+
  sz[,179]+sz[,180]+sz[,181]+sz[,182]+sz[,183]+sz[,184]+sz[,185]

# ECRRS
sz$ECRRS <- sz[,186]+sz[,187]+sz[,188]+sz[,189]+sz[,190]+sz[,191]+sz[,192]+
  sz[,193]+sz[,194]+sz[,195]+sz[,196]+sz[,197]+sz[,198]+sz[,199]+sz[,200]+
  sz[,201]+sz[,202]+sz[,203]+sz[,204]+sz[,205]+sz[,206]+sz[,207]+sz[,208]+
  sz[,209]+sz[,210]+sz[,211]+sz[,212]+sz[,213]+sz[,214]+sz[,215]+sz[,216]+
  sz[,217]+sz[,218]+sz[,219]+sz[,220]+sz[,221]

sz2 <- sz %>% select(one_of(c('CAINS','PANSS','QLS','RFS','AMNART','Hinting',
                              'Defeatist','ECRRS')))
library(corrplot)
sz_cor <- cor(sz2, use="complete.obs")
corrplot(sz_cor, method='number')

tom <- read.csv("LIWC2015_results_PORQ_geometricShapes_transcripts_nosource.csv")
tom <- tom %>% group_by(Source.A.) %>% summarise_all(funs(mean))
write.csv(tom, "tom_liwc.csv")

evoc <- read.csv("LIWC2015 Results (emotextfiles (1732 files)).csv")
evoc$Case <- substr(evoc$Filename,1,1)
evoc$ID <- substr(evoc$Filename,1,4)
evoc <- evoc %>% group_by(ID) %>% summarise_all(funs(mean))
write.csv(evoc,file="evocative_liwc.csv")
cor(evoc$Sixltr, evoc$adverb)

sz_sz <- sz[which(sz$Diagnosis..choice.Schizophrenia.=="Checked"),]
sz_sz$Diagnosis <- 'Schizophrenia'
sz_sza <- sz[which(sz$Diagnosis..choice.Schizoaffective.Disorder.=="Checked"),]
sz_sza$Diagnosis <- 'Schizoaffective'

sz_red <- rbind(sz_sz,sz_sza)
colnames(sz_red)
sz_red2 <- sz_red[,c(222:229)]
table <- sz_red2 %>% group_by(Diagnosis) %>% summarise_all(funs(mean(., na.rm=TRUE)))
