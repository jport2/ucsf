sz <- read.csv("PORQ_demographicAndBaselineDayData_labeled.csv")
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
sz$CPZ <- sz$X

# CAINS (NEGATIVE SYMPTOMS)
sz$CAINS <- sz[,62]+sz[,63]+sz[,64]+sz[,65]+sz[,66]+sz[,67]+sz[,68]+
  sz[,69]+sz[,70]+sz[,71]+sz[,72]+sz[,73]+sz[,74]
# neg symp with dose
#ggplot(data=sz, aes(x=CAINS, y=X))+geom_point()+geom_smooth(method=lm)
#cor(sz$CAINS, sz$X, use="complete.obs")
# PANSS
sz$PANSSpos <- sz[,75]+ sz[,76]+sz[,77]+sz[,78]+sz[,79]
sz$PANSSneg <- sz[,80]
sz$PANSSdisorganization <- sz[,81]+ sz[,82]+sz[,83]

# QLS
sz$QLS <- sz$Subscale.total.

# Role functioning scale
sz$RFS <- sz[,88]+sz[,89]+sz[,90]+sz[,91]

# Hinting Task
sz$Hinting <- sz$Total+sz$Total.1+sz$Total.2+sz$Total.3+sz$Total.4+
  sz$Total.5+sz$Total.6+sz$Total.7+sz$Total.8+sz$Total.9

# AMNART
sz$AMNART <- sz$Total.score

# Defeatest Beliefs
sz$Defeatist <- sz[,172]+sz[,173]+sz[,174]+sz[,175]+sz[,176]+sz[,178]+
  sz[,179]+sz[,180]+sz[,181]+sz[,182]+sz[,183]+sz[,184]+sz[,185]

# ECRRS
sz$ECRRS_Mom <- ((8-sz$It.helps.to.turn.to.this.person.in.times.of.need..R.)+
  (8-sz$I.usually.discuss.my.problems.and.concerns.with.this.person..R.) +
  (8-sz$I.talk.things.over.with.this.person..R.) +
  (8-sz$I.find.it.easy.to.depend.on.this.person..R.)+
  sz$I.don.t.feel.comfortable.opening.up.to.this.person +
  sz$I.prefer.not.to.show.this.person.how.I.feel.deep.down. +
  sz$I.often.worry.that.this.person.doesn.t.really.care.for.me +
  sz$I.m.afraid.that.this.person.may.abandon.me +
  sz$I.worry.that.this.person.won.t.care.about.me.as.much.as.I.care.about.him.or.her)/9
sz$ECRRS_Mom_Attachment <- ((8-sz$It.helps.to.turn.to.this.person.in.times.of.need..R.)+
  (8-sz$I.usually.discuss.my.problems.and.concerns.with.this.person..R.) +
  (8-sz$I.talk.things.over.with.this.person..R.) +
  (8-sz$I.find.it.easy.to.depend.on.this.person..R.))/4
sz$ECRRS_Mom_Avoidance <-
  (sz$I.don.t.feel.comfortable.opening.up.to.this.person +
  sz$I.prefer.not.to.show.this.person.how.I.feel.deep.down. +
  sz$I.often.worry.that.this.person.doesn.t.really.care.for.me +
  sz$I.m.afraid.that.this.person.may.abandon.me +
  sz$I.worry.that.this.person.won.t.care.about.me.as.much.as.I.care.about.him.or.her)/5
sz$ECRRS_Dad <- 
  ((8-sz$It.helps.to.turn.to.this.person.in.times.of.need..R..1)+
  (8-sz$I.usually.discuss.my.problems.and.concerns.with.this.person..R..1) +
  (8-sz$I.talk.things.over.with.this.person..R..1) +
  (8-sz$I.find.it.easy.to.depend.on.this.person..R..1)+
  sz$I.don.t.feel.comfortable.opening.up.to.this.person.1 +
  sz$I.prefer.not.to.show.this.person.how.I.feel.deep.down..1 +
  sz$I.often.worry.that.this.person.doesn.t.really.care.for.me.1 +
  sz$I.m.afraid.that.this.person.may.abandon.me.1 +
  sz$I.worry.that.this.person.won.t.care.about.me.as.much.as.I.care.about.him.or.her.1)/9
sz$ECRRS_Dad_Attachment <- 
  ((8-sz$It.helps.to.turn.to.this.person.in.times.of.need..R..1)+
     (8-sz$I.usually.discuss.my.problems.and.concerns.with.this.person..R..1) +
     (8-sz$I.talk.things.over.with.this.person..R..1) +
     (8-sz$I.find.it.easy.to.depend.on.this.person..R..1))/4
sz$ECRRS_Dad_Avoidance <- 
  (sz$I.don.t.feel.comfortable.opening.up.to.this.person.1 +
  sz$I.prefer.not.to.show.this.person.how.I.feel.deep.down..1 +
  sz$I.often.worry.that.this.person.doesn.t.really.care.for.me.1 +
  sz$I.m.afraid.that.this.person.may.abandon.me.1 +
  sz$I.worry.that.this.person.won.t.care.about.me.as.much.as.I.care.about.him.or.her.1)/5
sz$ECRRS_Friend <- 
  ((8-sz$It.helps.to.turn.to.this.person.in.times.of.need..R..2)+
  (8-sz$I.usually.discuss.my.problems.and.concerns.with.this.person..R..2) +
  (8-sz$I.talk.things.over.with.this.person..R..2) +
  (8-sz$I.find.it.easy.to.depend.on.this.person..R..2)+
  sz$I.don.t.feel.comfortable.opening.up.to.this.person.2 +
  sz$I.prefer.not.to.show.this.person.how.I.feel.deep.down..2 +
  sz$I.often.worry.that.this.person.doesn.t.really.care.for.me.2 +
  sz$I.m.afraid.that.this.person.may.abandon.me.2 +
  sz$I.worry.that.this.person.won.t.care.about.me.as.much.as.I.care.about.him.or.her.2)/9
sz$ECRRS_Friend_Attachment <- 
  ((8-sz$It.helps.to.turn.to.this.person.in.times.of.need..R..2)+
     (8-sz$I.usually.discuss.my.problems.and.concerns.with.this.person..R..2) +
     (8-sz$I.talk.things.over.with.this.person..R..2) +
     (8-sz$I.find.it.easy.to.depend.on.this.person..R..2))/4
sz$ECRRS_Friend_Avoidance <-   
  (sz$I.don.t.feel.comfortable.opening.up.to.this.person.2 +
  sz$I.prefer.not.to.show.this.person.how.I.feel.deep.down..2 +
  sz$I.often.worry.that.this.person.doesn.t.really.care.for.me.2 +
  sz$I.m.afraid.that.this.person.may.abandon.me.2 +
  sz$I.worry.that.this.person.won.t.care.about.me.as.much.as.I.care.about.him.or.her.2)/5
sz$ECRRS_Romantic <- 
  ((8-sz$It.helps.to.turn.to.this.person.in.times.of.need..R..3)+
  (8-sz$I.usually.discuss.my.problems.and.concerns.with.this.person..R..3) +
  (8-sz$I.talk.things.over.with.this.person..R..3) +
  (8-sz$I.find.it.easy.to.depend.on.this.person..R..3)+
  sz$I.don.t.feel.comfortable.opening.up.to.this.person.3 +
  sz$I.prefer.not.to.show.this.person.how.I.feel.deep.down..3 +
  sz$I.often.worry.that.this.person.doesn.t.really.care.for.me.3 +
  sz$I.m.afraid.that.this.person.may.abandon.me.3 +
  sz$I.worry.that.this.person.won.t.care.about.me.as.much.as.I.care.about.him.or.her.3)/9
sz$ECRRS_Romantic_Attachment <- 
  ((8-sz$It.helps.to.turn.to.this.person.in.times.of.need..R..3)+
      (8-sz$I.usually.discuss.my.problems.and.concerns.with.this.person..R..3) +
         (8-sz$I.talk.things.over.with.this.person..R..3) +
            (8-sz$I.find.it.easy.to.depend.on.this.person..R..3))/4
sz$ECRRS_Romantic_Avoidance <- 
  (sz$I.don.t.feel.comfortable.opening.up.to.this.person.3 +
  sz$I.prefer.not.to.show.this.person.how.I.feel.deep.down..3 +
  sz$I.often.worry.that.this.person.doesn.t.really.care.for.me.3 +
  sz$I.m.afraid.that.this.person.may.abandon.me.3 +
  sz$I.worry.that.this.person.won.t.care.about.me.as.much.as.I.care.about.him.or.her.3)/5

sz$ECRRS <- apply(sz[,c("ECRRS_Mom", "ECRRS_Dad", "ECRRS_Friend",
                                   "ECRRS_Romantic")],1,mean, na.rm=TRUE)
sz$ECRRS_Attachment <- apply(sz[,c("ECRRS_Mom_Attachment", "ECRRS_Dad_Attachment", "ECRRS_Friend_Attachment",
            "ECRRS_Romantic_Attachment")],1,mean, na.rm=TRUE)
sz$ECRRS_Avoidance <- apply(sz[,c("ECRRS_Mom_Avoidance", "ECRRS_Dad_Avoidance", "ECRRS_Friend_Avoidance",
                                   "ECRRS_Romantic_Avoidance")],1,mean, na.rm=TRUE)

sz$id <- sz$ï..Record.ID
sz$Case <- substr(sz$id,1,1)

#setwd("C:/Users/jakep/Dropbox/jakeLIWCanalyes")
write.csv(sz, "PORQ_demographicAndBaselineDayData_labeled_scales_updated.csv")

table <- sz[,222:248]
table <- table %>% group_by(Case) %>% summarise_all(funs(mean(., na.rm = TRUE)))
sz2 <- sz %>% select(one_of(c('Case','Age','Years.of.Education','CPZ','CAINS','PANSSpos','QLS','RFS','AMNART','Hinting',
                              'Defeatist','ECRRS_Attachment','ECRRS_Avoidance')))
sz2[,c("Years.of.Education")] <- as.numeric(gsub("[^0-9.]", "",sz2[,c("Years.of.Education")]))
sz2 <- sz2 %>% group_by(Case)%>% summarise_all(funs(mean(., na.rm = TRUE)))
write.csv(sz2, "Demographic_Table_For_Paper.csv")
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

data <- full_join(sz,,by="")