library(readxl)
library(stringr)
library(dplyr)
library(car)
library(assertthat)
library(psych)


rm(list=ls())

subs<-c("001", "002", "003", "004", "006", "007", "008", "009", "010", "011",
        "012", "014", "015", "016", "017", "018", "020", "021", "022", 
        "023", "025", "026", "027", "028", "029", "030", "031", "032", "033", 
        "034", "035", "036", "037", "038", "039", "040", "041")

for(i in 1:length(subs)){
  
file = file.path("/Users/jiajinzhang/Desktop/Mobbs\ Lab/Spider\ Box/Questionnaires/Pre-Lab/Subject",subs[i],".csv",fsep="")

df<-as.data.frame(read.csv(file, header = TRUE)) 

surveys<-as.data.frame(na.omit(ifelse(df$trial_type=="survey-likert", df$responses, NA)))
colnames(surveys)<-c("V1")
surveys1<-str_split_fixed(surveys$V1, ",", 30)
surveys1<-(gsub('}', '', surveys1))

BISBAS<-na.omit(as.data.frame(ifelse(str_detect(surveys1, "BISBAS"), surveys1, NA)))
colnames(BISBAS)<-c("V1") 
BISBAS<-as.data.frame(str_split_fixed(BISBAS$V1, ":", 2))
BISBAS$Q<-seq.int(nrow(BISBAS))
BISBAS$V2<-(as.numeric(BISBAS$V2))
BISBASt<-as.data.frame(t(BISBAS$V2)) #t is for transpose here
BISBAS_DRIVE<-list(sum(BISBASt$V3, BISBASt$V9, BISBASt$V12, BISBASt$V21))

keys <- c(-1)
Fun_V5<- as.data.frame(reverse.code(keys, BISBASt$V5, mini=1, maxi=4))
Fun_V10 <- as.data.frame(reverse.code(keys, BISBASt$V10, mini=1, maxi=4))
Fun_V15 <- as.data.frame(reverse.code(keys, BISBASt$V15, mini=1, maxi=4))
Fun_V20 <- as.data.frame(reverse.code(keys, BISBASt$V20, mini=1, maxi=4))
BISBAS_FUN <- (list(sum(Fun_V5, Fun_V10, Fun_V15, Fun_V20)))  

#BISBAS_FUN<-list(sum(BISBASt$V5, BISBASt$V10, BISBASt$V15, BISBASt$V20))
BISBAS_REWARD<-list(sum(BISBASt$V4, BISBASt$V7, BISBASt$V14, BISBASt$V18, BISBASt$V23))
BISBAS_BIS<-list(sum(BISBASt$V2, BISBASt$V8, BISBASt$V13, BISBASt$V16, BISBASt$V19, BISBASt$V22, BISBASt$V24))
BISAS_Score <- data.frame(BISBAS_DRIVE, BISBAS_FUN, BISBAS_REWARD, BISBAS_BIS)
names(BISAS_Score) <- c("BISBAS_DRIVE", "BISBAS_FUN", "BISBAS_REWARD", "BISBAS_BIS")

#export data
write.table(BISAS_Score, file=sprintf("/Users/jiajinzhang/Desktop/CCT/BISBAS/Subject%s.txt",subs[i]), 
            col.names=TRUE, row.names=FALSE, sep = "\t")
}

######## STAI Questionnaire ###########

rm(list=ls())

subs<-c("001", "002", "003", "004", "006", "007", "008", "009", "010", "011",
        "012", "014", "015", "016", "017", "018", "020", "021", "022", 
        "023", "025", "026", "027", "028", "029", "030", "031", "032", "033", 
        "034", "035", "036", "037", "038", "039", "040", "041")

for(i in 1:length(subs)){
  
file = file.path("/Users/jiajinzhang/Desktop/Mobbs\ Lab/Spider\ Box/Questionnaires/Pre-Lab/Subject",subs[i],".csv",fsep="")
  
df<-as.data.frame(read.csv(file, header = TRUE)) 
surveys<-as.data.frame(na.omit(ifelse(df$trial_type=="survey-likert", df$responses, NA)))
colnames(surveys)<-c("V1")
surveys1<-str_split_fixed(surveys$V1, ",", 30)
surveys1<-(gsub('}', '', surveys1))

STAIT<-na.omit(as.data.frame(ifelse(str_detect(surveys1, "STAIT"), surveys1, NA)),  stringsAsFactors = FALSE)
colnames(STAIT)<-c("V1") 
STAIT<-as.data.frame(str_split_fixed(STAIT$V1, ":", 2))
STAIT$Q<-seq.int(nrow(STAIT))
STAIT$V2<-(as.numeric(STAIT$V2))
STAITt<-as.data.frame(t(STAIT$V2))
keys <- c(-1,1,-1,1,1,-1,-1,1,1,-1,1,1,-1,-1,1,-1,1,1,-1,1)
STAT_Total<- as.data.frame(reverse.code(keys, STAITt, mini=0, maxi=3)) 
STAI_Score <- as.data.frame(list(sum(STAT_Total %>% select(1:20))))  
colnames(STAI_Score)<-c("STAI_Score")

write.table(STAI_Score, file=sprintf("/Users/jiajinzhang/Desktop/Mobbs\ Lab/Spider\ Box/Questionnaires/Prelab_Results/STAI/Subject%s.txt",subs[i]), 
            col.names=TRUE, row.names=FALSE, sep = "\t")

}
##########LA Questionnaire#########
rm(list=ls())

subs<-c("001", "002", "003", "004", "006", "007", "008", "009", "010", "011",
        "012", "014", "015", "016", "017", "018", "020", "021", "022", 
        "023", "025", "026", "027", "028", "029", "030", "031", "032", "033", 
        "034", "035", "036", "037", "038", "039", "040", "041")

for(i in 1:length(subs)){
  
file = file.path("/Users/jiajinzhang/Desktop/Mobbs\ Lab/Spider\ Box/Questionnaires/Pre-Lab/Subject",subs[i],".csv",fsep="")

dfscore <-as.data.frame(read.csv(file, header = TRUE)) 

surveys.LA<-as.data.frame(ifelse(dfscore$trial_type=="survey-multi-select", dfscore$responses, NA))
colnames(surveys.LA)<-c("V1")
surveys1.LA<-str_split_fixed(surveys.LA$V1, "],", 12)
surveys1.LA<-(gsub('}', '', surveys1.LA))

LossAv<-na.omit(as.data.frame(ifelse(str_detect(surveys1.LA, "chance"), surveys1.LA, NA)))
colnames(LossAv)<-c("V1") 
LossAv<-as.data.frame(str_split_fixed(LossAv$V1, ":", 3))
LossAv$V3<-seq.int(nrow(LossAv))
#add a V4 that's 1
LossAv$V4<-ifelse(str_detect(LossAv$V2, "100%"), 1, 0)
LossAv.final<-(LossAv$V4)[1:1]
LA.df <- as.data.frame(LossAv.final)
LossAv$Sum <- sum(LossAv$V4)

##LossAv$V5<-which.min(LossAv$V4*NA^(LossAv$V4 <=0)) (this does not seem right)
##LossAv.final<-(LossAv$V5)[1:1]  #LossAv$V4[1:1] for some pts who didnt ever switch

write.table(LossAv, file=sprintf("/Users/jiajinzhang/Desktop/CCT/LA/Subject%s.txt",subs[i]), 
            col.names=TRUE, row.names=FALSE, sep = "\t")
}


###Toronto Empathy Questionnaire###

#file = file.path("/Users/jiajinzhang/Desktop/Mobbs\ Lab/Spider\ Box/Questionnaires/Pre-Lab/Subject",subs[i],".csv",fsep="")

subs<-c("001", "002", "003", "004", "006", "007", "008", "009", "010", "011",
        "012", "014", "015", "016", "017", "018", "020", "021", "022", 
        "023", "025", "026", "027", "028", "029", "030", "031", "032", "033", 
        "034", "035", "036", "037", "038", "039", "040", "041")

for(i in 1:length(subs)){
file = file.path("/Users/jiajinzhang/Desktop/Mobbs\ Lab/Spider\ Box/Questionnaires/Pre-Lab/Subject",subs[i],".csv",fsep="")
df <- as.data.frame(read.csv(file, header= TRUE)) 
survey.TEQ<-as.data.frame(na.omit(ifelse(df$trial_type=="survey-likert", df$responses, NA)))
colnames(survey.TEQ)<-c("V1")
survey.TEQ<-str_split_fixed(survey.TEQ$V1, ",", 30)
survey.TEQ1<-(gsub('}', '', survey.TEQ))
TEQ<-na.omit(as.data.frame(ifelse(str_detect(survey.TEQ1, "TEQ"), survey.TEQ1, NA)),  stringsAsFactors = FALSE)
colnames(TEQ)<-c("V1") 

TEQ<-as.data.frame(str_split_fixed(TEQ$V1, ":", 2))
TEQ$Q<-seq.int(nrow(TEQ))
TEQ$V2<-(as.numeric(TEQ$V2))
TEQt<-as.data.frame(t(TEQ$V2))
keys <- c(1,-1,1,-1,1,1,-1,1,1,-1,-1,-1,1,-1,-1,1)
TEQ_Total<- as.data.frame(reverse.code(keys, TEQt, mini=0, maxi=4))
TEQ_Score <- as.data.frame(list(sum(TEQ_Total %>% select(1:16))))  
#TEQ$Score <- sum(TEQ$V2)

write.table(TEQ_Score, file=sprintf("/Users/jiajinzhang/Desktop/CCT/TEQ/Subject%s.txt",subs[i]), 
            col.names=TRUE, row.names=FALSE, sep = "\t")

}



