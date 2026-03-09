install.packages("githubinstall")
devtools::install_github('Timag/viewerWindow')
library(haven)
test <- read_sav("test.sav")
View(test)


###############CLEANING DATA##############
#Remove excess information from Qualtrics data export
test$StartDate <- NULL
test$EndDate <- NULL
test$IPAddress <- NULL
test$RecordedDate <- NULL
test$RecipientLastName <- NULL
test$RecipientFirstName <- NULL
test$RecipientEmail <- NULL
test$ExternalReference <- NULL
test$LocationLatitude <- NULL
test$LocationLongitude <- NULL
test$DistributionChannel <- NULL
test$UserLanguage <- NULL
test$Responseld <- NULL
test$CF.infrmd.cnst.stmnt <- NULL
test$Status <- NULL
test$Progress<- NULL

#remove responses where the participant indicated random answering
test <- test[test$Random_ != 0, ]

###Reverse Scoring Items###
#reverse score fundamental social motive items
reverse_cols = c("Fund_Moti_Item_3","Fund_Moti_Item_4","Fund_Moti_Item_5","Fund_Moti_Item_45", "Fund_Moti_Item_52","Fund_Moti_Item_53", "Fund_Moti_Item_54","Fund_Moti_Item_24", "Fund_Moti_Item_14","Fund_Moti_Item_37", "Fund_Moti_Item_38","Fund_Moti_Item_39", "Fund_Moti_Item_70","Fund_Moti_Item_72", "Fund_Moti_Item_57","Fund_Moti_Item_65", "Fund_Moti_Item_66")
test[ ,reverse_cols] = 8 - test[ ,reverse_cols]

#reverse score big5 items
reverse_cols = c("Big5_2","Big5_6","Big5_21","Big5_31", "Big5_12","Big5_27", "Big5_37","Big5_8", "Big5_18","Big5_23", "Big5_43","Big5_9", "Big5_24","Big5_34", "Big5_35","Big5_41")
test[ ,reverse_cols] = 8 - test[ ,reverse_cols]

#reverse score PVD items
reverse_cols = c("PVD2_5","PVD2_12","PVD2_14","PVD2_3", "PVD2_11","PVD2_13")
test[ ,reverse_cols] = 8 - test[ ,reverse_cols]

#reverse score DomPres items
reverse_cols = c("DP_10","DP_12","DP_2","DP_6", "DP_17")
test[ ,reverse_cols] = 8 - test[ ,reverse_cols]

#reverse score BDW items
reverse_cols = c("BDW_2","BDW_7","BDW_9","BDW_5", "BDW_4","BDW_12")
test[ ,reverse_cols] = 8 - test[ ,reverse_cols]

#reverse score N2B items
reverse_cols = c("N2B_1","N2B_3","N2B_7")
test[ ,reverse_cols] = 6 - test[ ,reverse_cols]

#reverse score ECR-R items
reverse_cols = c("ECR_9","ECR_11","ECR_20","ECR_22", "ECR_26","ECR_27", "ECR_28","ECR_29", "ECR_30","ECR_31", "ECR_33","ECR_34", "ECR_35","ECR_36")
test[ ,reverse_cols] = 8 - test[ ,reverse_cols]

#reverse score SOI items
reverse_cols = c("SOI_7","SOI_14","SOI_15","SOI_19")
test[ ,reverse_cols] = 6 - test[ ,reverse_cols]

#reverse score homelife and childhood items
reverse_cols = c("Dem_Q4","Dem_HomeLife_1","Dem_HomeLife_2","Dem_HomeLife_3")
test[ ,reverse_cols] = 8 - test[ ,reverse_cols]

###Create an average for each scale measure to be used in analysis

#FSM averages
test$FSM_MateSeek <- rowMeans(test[ , c(6:11)], na.rm=TRUE)
test$FSM_SelfProtect <- rowMeans(test[ , c(48:53)], na.rm=TRUE)
test$FSM_DiseaseAvoid <- rowMeans(test[ , c(54:59)], na.rm=TRUE)
test$FSM_Affiliation_GROUP <- rowMeans(test[ , c(20,26:29,32)], na.rm=TRUE)
test$FSM_Affiliation_EXCLUDE <- rowMeans(test[ , c(21,30,31,33,34,37)], na.rm=TRUE)
test$FSM_Affiliation_INDEP <- rowMeans(test[ , c(23:25,37:39)], na.rm=TRUE)
test$FSM_Status <- rowMeans(test[ , c(12:19)], na.rm=TRUE)
test$FSM_KinCare_FAMILY <- rowMeans(test[ , c(41:44,46,47)], na.rm=TRUE)
test$FSM_KinCare_CHILD <- rowMeans(test[ , c(73:78)], na.rm=TRUE)
test$FSM_MateRet_BREAKUP <- rowMeans(test[ , c(63:65,67:69)], na.rm=TRUE)
test$FSM_MateRet_GEN <- rowMeans(test[ , c(60:62,70:72)], na.rm=TRUE)

#Big5 averages
test$Big5_EXTVRT <- rowMeans(test[ , c(290,300,305,315,325,295,310,320)], na.rm=TRUE)
test$Big5_AGREE <- rowMeans(test[ , c(296,306,311,321,331,291,301,316,326)], na.rm=TRUE)
test$Big5_CONC <- rowMeans(test[ , c(292,302,317,322,327,297,307,312,332)], na.rm=TRUE)
test$Big5_NEUR <- rowMeans(test[ , c(293,303,308,318,328,298,313,323)], na.rm=TRUE)
test$Big5_OPEN <- rowMeans(test[ , c(294,299,309,314,319,329,333,324,330)], na.rm=TRUE)

#SOI averages
test$SOI_S <- rowMeans(test[ , c(372:381,388)], na.rm=TRUE)
test$SOI_L <- rowMeans(test[ , c(382:387,389,390)], na.rm=TRUE)

#PVD averages
test$TDDS_ONLY <- rowMeans(test[ , c(391:411)], na.rm=TRUE)
test$PVD_ONLY <- rowMeans(test[ , c(412:426)], na.rm=TRUE)
test$PVD_INFECT <- rowMeans(test[ , c(413,416,417,419,421,423,425)], na.rm=TRUE)
test$PVD_GERMS <- rowMeans(test[ , c(412,414,415,418,420,422,424,426)], na.rm=TRUE)
test$TDDS_MORAL <- rowMeans(test[ , c(394,391,406,397,400,409,403)], na.rm=TRUE)
test$TDDS_SEXUAL <- rowMeans(test[ , c(407,401,404,392,410,395,398)], na.rm=TRUE)
test$TDDS_PATHOGEN <- rowMeans(test[ , c(402,405,399,393,411,408,396)], na.rm=TRUE)

#DomPres averages
test$DP_DOM <- rowMeans(test[ , c(429,431,433,435,437,442,436,438)], na.rm=TRUE)
test$DP_PRES <- rowMeans(test[ , c(427,430,434,439,440,441,428,432,443)], na.rm=TRUE)

#BDW average
test$BDW <- rowMeans(test[ , c(491:502)], na.rm=TRUE)

#N2B average
test$N2B <- rowMeans(test[ , c(481:490)], na.rm=TRUE)

#ECR averages
test$ECR_ANXIETY <- rowMeans(test[ , c(445:462)], na.rm=TRUE)
test$ECR_AVOID <- rowMeans(test[ , c(463:480)], na.rm=TRUE)

#Resources Stability averages
test$Resources_CHILDHOOD <- rowMeans(test[ , c(526:529)], na.rm=TRUE)
test$Resources_CURRENT <- rowMeans(test[ , c(530:531)], na.rm=TRUE)
test$CldhdStablty <- rowMeans(test[ , c(534:536)], na.rm=TRUE)

#average age aross participants
mean(test$Dem_AGE)

################## Analyzing the data #####################
# types of analyses:
#     -descriptive statistics and correlations
#     -predict life events (dichotmous/frequency): Neel Table S3
#     -look for demographic predictors of motives: Neel Table 4

library(psych)
library(lsr)
library(car)

#find which life events have enough data (more than 40 for each):
a=which(names(test)=="Life_Data_1")
summary(test[,a:(a+80)])

#find which frequencies have enough variability
describe(test[,(a+81):(a+152)])


##########################################Replicating Tables############################################
################ Descriptive statistics
a=which(names(test)=="FSM_MateSeek")
describe(test[,a:(a+10)])
library(Hmisc)

################ FMI correlations
a=which(names(test)=="FSM_MateSeek")
FMIcorData <- test[,a:(a+10)]
FMIcor<-round(cor(FMIcorData,use="pairwise.complete.obs"),2)
FMIcorSig <- rcorr(as.matrix(FMIcorData))

################ FMI correlation with Big 5
a=which(names(test)=="FSM_MateSeek")
big5FMIdata <- test[,a:(a+15)] 
big5FMI<-round(cor(big5FMIdata,use="pairwise.complete.obs"),2)
big5FMISig <- rcorr(as.matrix(big5FMIdata))
big5FMISig

################ FMI correlation with other scales
FMIscaledata <- test[,c("FSM_SelfProtect","FSM_DiseaseAvoid","FSM_Affiliation_GROUP","FSM_Affiliation_EXCLUDE","FSM_Affiliation_INDEP","FSM_Status","FSM_MateSeek","FSM_MateRet_GEN","FSM_MateRet_BREAKUP","FSM_KinCare_FAMILY","FSM_KinCare_CHILD","SOI_S","SOI_L","BDW","DP_DOM","DP_PRES", "ECR_ANXIETY" , "ECR_AVOID","N2B","TDDS_ONLY","PVD_ONLY")] 
scaleCor<-round(cor(FMIscaledata,use="pairwise.complete.obs"),2)
scaleCorSig <- rcorr(as.matrix(FMIscaledata))
scaleCorSig




test$Relationship <- as.factor(test$Relationship_Stat)

levels(test$Relationship)[levels(test$Relationship)=="5"] <- "-1"
levels(test$Relationship)[levels(test$Relationship)=="1"] <- "1"
levels(test$Relationship)[levels(test$Relationship)=="2"] <- "1"
levels(test$Relationship)[levels(test$Relationship)=="3"] <- "1"
levels(test$Relationship)[levels(test$Relationship)=="4"] <- "0"
levels(test$Relationship)[levels(test$Relationship)=="6"] <- "0"

test
test$Parent <- as.factor(test$Have_children_)
levels(test$Parent)[levels(test$Parent)=="1"] <-"1"
levels(test$Parent)[levels(test$Parent)=="2"] <-"-1"
test$Sex <- as.factor(test$Dem_SEX)
levels(test$Sex)[levels(test$Sex)=="1"] <-"-1"
levels(test$Sex)[levels(test$Sex)=="2"] <-"1"

testLifeHist <- test
testLifeHist <- testLifeHist[testLifeHist$Relationship != 0, ]

#self-protection
SPlifeHist = lm(FSM_SelfProtect~Dem_AGE+Sex+Relationship+Parent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,testLifeHist)
summary(SPlifeHist) 
standardCoefs(SPlifeHist)

#Disease Avoidance
DAlifeHist = lm(FSM_DiseaseAvoid~Dem_AGE+Sex+Relationship+Parent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,testLifeHist)
summary(DAlifeHist) 
standardCoefs(DAlifeHist)

#Affiliation (Group)
AGlifeHist = lm(FSM_Affiliation_GROUP~Dem_AGE+Sex+Relationship+Parent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,testLifeHist)
summary(AGlifeHist)
standardCoefs(AGlifeHist)

#Affiliation (Exclusion concern)
AElifeHist = lm(FSM_Affiliation_EXCLUDE~Dem_AGE+Sex+Relationship+Parent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,testLifeHist)
summary(AElifeHist)
standardCoefs(AElifeHist)

#Affiliation (Independence)
AIlifeHist = lm(FSM_Affiliation_INDEP~Dem_AGE+Sex+Relationship+Parent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,testLifeHist)
summary(AIlifeHist)
standardCoefs(AIlifeHist)

#Status
SlifeHist = lm(FSM_Status~Dem_AGE+Sex+Relationship+Parent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,testLifeHist)
summary(SlifeHist)
standardCoefs(SlifeHist)

#Mate seeking
MSlifeHist = lm(FSM_MateSeek~Dem_AGE+Sex+Relationship+Parent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,testLifeHist)
summary(MSlifeHist)
standardCoefs(MSlifeHist)

#Mate Retention General
MRGlifeHist = lm(FSM_MateRet_GEN~Dem_AGE+Sex+Parent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,testLifeHist)
summary(MRGlifeHist)
standardCoefs(MRGlifeHist)

#Mate Retention Breakup Concern
MRBlifeHist = lm(FSM_MateRet_BREAKUP~Dem_AGE+Sex+Parent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,testLifeHist)
summary(MRBlifeHist)
standardCoefs(MRBlifeHist)

#Kin Care (Family)
KCFlifeHist = lm(FSM_KinCare_FAMILY~Dem_AGE+Sex+Relationship+Parent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,testLifeHist)
summary(KCFlifeHist)
standardCoefs(KCFlifeHist)

#Kin Care (Child)
KCClifeHist = lm(FSM_KinCare_CHILD~Dem_AGE+Sex+Relationship+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,testLifeHist)
summary(KCClifeHist)
standardCoefs(KCClifeHist)


install.packages("sjPlot")
library(sjPlot)

####Tables 
#Self-Protection, Disease Avoidance, Affiliation (Group)
tab_model(SPlifeHist, DAlifeHist, AGlifeHist, 
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Self-Protection","Disease Avoidance","Affiliation (Group)"),
          pred.labels = c("Age","Sex","Relationship status",
                          "Parent status","Childhood stability",
                          "Childhood resources","Current resources"))

#Affiliation (Exclusion Concern), Affiliation (Independence), Status
tab_model(AElifeHist, AIlifeHist, SlifeHist, 
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Affiliation (Exclusion Concern)","Affiliation (Independence)","Status"),
          pred.labels = c("Age","Sex","Relationship status",
                          "Parent status","Childhood stability",
                          "Childhood resources","Current resources"))

#"Mate Seeking","Mate Retention (General)","Mate Retention (Breakup Concern)"
tab_model(MSlifeHist, MRGlifeHist, MRBlifeHist, 
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Mate Seeking","Mate Retention (General)","Mate Retention (Breakup Concern)"),
          pred.labels = c("Age","Sex","Relationship status",
                          "Parent status","Childhood stability",
                          "Childhood resources","Current resources"))

#"Kin Care (Family)","Kin Care (Child)"
tab_model(KCFlifeHist, KCClifeHist, 
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Kin Care (Family)","Kin Care (Child)"),
          pred.labels = c("Age","Sex","Relationship status",
                          "Parent status","Childhood stability",
                          "Childhood resources","Current resources"))
library(fastDummies)

####################################Life Events Odds Ratios###################################
######table S1 replication

###Self protection
#self defense class
test$Life_Data_79 <- as.factor(test$Life_Data_79)
levels(test$Life_Data_79)[levels(test$Life_Data_79)=="2"] <-"0"
SelfdefenseModel= glm(Life_Data_79~FSM_SelfProtect,test,family="binomial")
summary(SelfdefenseModel)
exp(coef(SelfdefenseModel))

#mace
test$Life_Data_12 <- as.factor(test$Life_Data_12)
levels(test$Life_Data_12)[levels(test$Life_Data_12)=="2"] <-"0"
SPmaceModel= glm(Life_Data_12~FSM_SelfProtect,test,family="binomial")
summary(SPmaceModel)
exp(coef(SPmaceModel))
#mace freq
SPmaceFreq= lm(Life_Data_18_WkFreq~FSM_SelfProtect,test[test$ Life_Data_18_WkFreq!=0,]) 
summary(SPmaceFreq)
standardCoefs(SPmaceFreq)

#home security
test$Life_Data_10 <- as.factor(test$Life_Data_10)
levels(test$Life_Data_10)[levels(test$Life_Data_10)=="2"] <-"0"
SPhomeSecModel= glm(Life_Data_10~FSM_SelfProtect,test,family="binomial")
summary(SPhomeSecModel)
exp(coef(SPhomeSecModel))

#weapon public
test$Life_Data_11 <- as.factor(test$Life_Data_11)
levels(test$Life_Data_11)[levels(test$Life_Data_11)=="2"] <-"0"
SPweaponModel= glm(Life_Data_11~FSM_SelfProtect,test,family="binomial")
summary(SPweaponModel) 
exp(coef(SPweaponModel))
#frequency of weapon in public
summary(test$ Life_Data_19_WkFreq!=0) 
SPweaponFreq= lm(Life_Data_19_WkFreq~FSM_SelfProtect,test[test$Life_Data_19_WkFreq!=0,])
summary(SPweaponFreq) 
standardCoefs(SPweaponFreq)

#screamed at someone
test$Life_Data_34 <- as.factor(test$Life_Data_34)
levels(test$Life_Data_34)[levels(test$Life_Data_34)=="2"] <-"0"
SPscreamModel= glm(Life_Data_34~FSM_SelfProtect,test,family="binomial")
summary(SPscreamModel) 
exp(coef(SPscreamModel))

#shoved someone 
test$Life_Data_35 <- as.factor(test$Life_Data_35)
levels(test$Life_Data_35)[levels(test$Life_Data_35)=="2"] <-"0"
SPshoveModel= glm(Life_Data_35~FSM_SelfProtect,test,family="binomial")
summary(SPshoveModel) 
exp(coef(SPshoveModel))

#freq of shoving
summary(test$Life_Data_29_Freq!=0)
SPshoveFreq= lm(Life_Data_29_Freq~FSM_SelfProtect,test[test$Life_Data_29_Freq!=0,])
summary(SPshoveFreq)
standardCoefs(SPshoveFreq)  


###Disease avoidance

#avoided shaking hands with someone who seemed sick
test$Life_Data_4 <- as.factor(test$Life_Data_4)
levels(test$Life_Data_4)[levels(test$Life_Data_4)=="2"] <-"0"
DAhandshakeModel= glm(Life_Data_4~FSM_DiseaseAvoid,test,family="binomial")
summary(DAhandshakeModel) 
exp(coef(DAhandshakeModel))
#freq of avoiding hand shake
DAhandshakeFreq= lm(Life_Data_4_Freq~FSM_DiseaseAvoid,test[test$Life_Data_4_Freq!=0,]) 
summary(DAhandshakeFreq)
standardCoefs(DAhandshakeFreq)

#smoked cigarettes
test$Life_Data_69 <- as.factor(test$Life_Data_69)
levels(test$Life_Data_69)[levels(test$Life_Data_69)=="2"] <-"0"
DAsmokeModel= glm(Life_Data_69~FSM_DiseaseAvoid,test,family="binomial")
summary(DAsmokeModel) 
exp(coef(DAsmokeModel))


###Affiliation group

#playing a team sport
test$Life_Data_14 <- as.factor(test$Life_Data_14)
levels(test$Life_Data_14)[levels(test$Life_Data_14)=="2"] <-"0"
AGteamsportModel= glm(Life_Data_14~FSM_Affiliation_GROUP,test,family="binomial")
summary(AGteamsportModel)
exp(coef(AGteamsportModel))

#attending religious services
test$Life_Data_54 <- as.factor(test$Life_Data_54)
levels(test$Life_Data_54)[levels(test$Life_Data_54)=="2"] <-"0"
AGreligiousModel= glm(Life_Data_54~FSM_Affiliation_GROUP,test,family="binomial")
summary(AGreligiousModel)
exp(coef(AGreligiousModel))

#volunteering
test$Life_Data_15 <- as.factor(test$Life_Data_15)
levels(test$Life_Data_15)[levels(test$Life_Data_15)=="2"] <-"0"
AGvolunteerModel= glm(Life_Data_15~FSM_Affiliation_GROUP,test,family="binomial")
summary(AGvolunteerModel) 
exp(coef(AGvolunteerModel))

#smoked ciagrettes
AGsmokeModel= glm(Life_Data_69~FSM_Affiliation_GROUP,test,family="binomial")
summary(AGsmokeModel) 
exp(coef(AGsmokeModel))


###Affiliation Exclusion Concern

#facebook
test$Life_Data_17 <- as.factor(test$Life_Data_17)
levels(test$Life_Data_17)[levels(test$Life_Data_17)=="2"] <-"0"
AEfacebookModel= glm(Life_Data_17~FSM_Affiliation_EXCLUDE,test,family="binomial")
summary(AEfacebookModel) 
exp(coef(AEfacebookModel))

#twitter
test$Life_Data_75 <- as.factor(test$Life_Data_75)
levels(test$Life_Data_75)[levels(test$Life_Data_75)=="2"] <-"0"
AEtwitterModel= glm(Life_Data_75~FSM_Affiliation_EXCLUDE,test,family="binomial")
summary(AEtwitterModel) 
exp(coef(AEtwitterModel))

#social media frequency 
AEsocialMediaFreq= lm(Life_Data_1_WkFreq_~FSM_Affiliation_EXCLUDE,test,family="binomial")
summary(AEsocialMediaFreq)
standardCoefs(AEsocialMediaFreq)


###Status

#job where other people worked for them
test$Life_Data_70 <- as.factor(test$Life_Data_70)
levels(test$Life_Data_70)[levels(test$Life_Data_70)=="2"] <-"0"
SworkaboveModel= glm(Life_Data_70~FSM_Status,test,family="binomial")
summary(SworkaboveModel) 
exp(coef(SworkaboveModel))

#promotion
test$Life_Data_21 <- as.factor(test$Life_Data_21)
levels(test$Life_Data_21)[levels(test$Life_Data_21)=="2"] <-"0"
SpromotionModel= glm(Life_Data_21~FSM_Status,test,family="binomial")
summary(SpromotionModel) 
exp(coef(SpromotionModel))

#preformed for others
test$Life_Data_46 <- as.factor(test$Life_Data_46)
levels(test$Life_Data_46)[levels(test$Life_Data_46)=="2"] <-"0"
SpreformModel= glm(Life_Data_46~FSM_Status,test,family="binomial")
summary(SpreformModel) 
exp(coef(SpreformModel))

#art
test$Life_Data_42 <- as.factor(test$Life_Data_42)
levels(test$Life_Data_42)[levels(test$Life_Data_42)=="2"] <-"0"
SartModel= glm(Life_Data_42~FSM_Status,test,family="binomial")
summary(SartModel) 
exp(coef(SartModel))


###Mate seeking

#choosen to end relationship
test$Life_Data_36 <- as.factor(test$Life_Data_36)
levels(test$Life_Data_36)[levels(test$Life_Data_36)=="2"] <-"0"
MSdumpModel= glm(Life_Data_36~FSM_MateSeek,test,family="binomial")
summary(MSdumpModel) 
exp(coef(MSdumpModel))

#been broken up with
test$Life_Data_37 <- as.factor(test$Life_Data_37)
levels(test$Life_Data_37)[levels(test$Life_Data_37)=="2"] <-"0"
MSbreakupModel= glm(Life_Data_37~FSM_MateSeek,test,family="binomial")
summary(MSbreakupModel) 
exp(coef(MSbreakupModel))

#asked someone out
test$Life_Data_44 <- as.factor(test$Life_Data_44)
levels(test$Life_Data_44)[levels(test$Life_Data_44)=="2"] <-"0"
MSask4dateModel= glm(Life_Data_44~FSM_MateSeek,test,family="binomial")
summary(MSask4dateModel)
exp(coef(MSask4dateModel))

#been asked out
test$Life_Data_43 <- as.factor(test$Life_Data_43)
levels(test$Life_Data_43)[levels(test$Life_Data_43)=="2"] <-"0"
MSaskedoutModel= glm(Life_Data_43~FSM_MateSeek,test,family="binomial")
summary(MSaskedoutModel)
exp(coef(MSaskedoutModel))

#gone dancing
test$Life_Data_47 <- as.factor(test$Life_Data_47)
levels(test$Life_Data_47)[levels(test$Life_Data_47)=="2"] <-"0"
MSdanceModel= glm(Life_Data_47~FSM_MateSeek,test,family="binomial")
summary(MSdanceModel)
exp(coef(MSdanceModel))
#freq of dancing
summary(test$Life_Data_37_Freq!=0)
MSdanceFreq= lm(Life_Data_37_Freq~FSM_MateSeek,test[test$Life_Data_37_Freq!=0,])
summary(MSdanceFreq) 
standardCoefs(MSdanceFreq)

#sex with condom
test$Life_Data_61 <- as.factor(test$Life_Data_61)
levels(test$Life_Data_61)[levels(test$Life_Data_61)=="2"] <-"0"
MScondomModel= glm(Life_Data_61~FSM_MateSeek,test,family="binomial")
summary(MScondomModel)
exp(coef(MScondomModel))
#freqency of going to concert
summary(test$Life_Data_35_Freq!=0) 
MSconcertFreq= lm(Life_Data_35_Freq~FSM_MateSeek,test[test$Life_Data_35_Freq!=0,]) 
summary(MSconcertFreq)
standardCoefs(MSconcertFreq)


#promotion and new job controling for motives and big 5?
#promotion
MSpromoModel= glm(Life_Data_21~FSM_MateSeek,test,family="binomial")
summary(MSpromoModel)
exp(coef(MSpromoModel))

#controling for motives and big 5
MSpromoControlModel= glm(Life_Data_21~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateRet_GEN+FSM_MateRet_BREAKUP+FSM_KinCare_FAMILY+FSM_KinCare_CHILD+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN,test,family="binomial") #warnings: did not converge
#MSpromoControlModel= step(MSpromoControlModel) #warnings
summary(MSpromoControlModel) 
exp(coef(MSpromoControlModel))

#new job 
test$Life_Data_23 <- as.factor(test$Life_Data_23)
levels(test$Life_Data_23)[levels(test$Life_Data_23)=="2"] <-"0"
MSnewjobModel= glm(Life_Data_23~FSM_MateSeek,test,family="binomial")
summary(MSnewjobModel) 
exp(coef(MSnewjobModel)) 

#falling out with friend and moved within a city controlling for age?
#falling out with friend
test$Life_Data_13 <- as.factor(test$Life_Data_13)
levels(test$Life_Data_13)[levels(test$Life_Data_13)=="2"] <-"0"
MSfalloutModel= glm(Life_Data_13~FSM_MateSeek,test,family="binomial")
summary(MSfalloutModel)
exp(coef(MSfalloutModel))

#moved within a city
test$Life_Data_18 <- as.factor(test$Life_Data_18)
levels(test$Life_Data_18)[levels(test$Life_Data_18)=="2"] <-"0"
MSmoveincityModel= glm(Life_Data_18~FSM_MateSeek,test,family="binomial")
summary(MSmoveincityModel) 
exp(coef(MSmoveincityModel))

#freq of going a day without eating
summary(test$Life_Data_38_Freq!=0) 
MSnoeatFreq= lm(Life_Data_38_Freq~FSM_MateSeek,test[test$Life_Data_38_Freq!=0,]) 
summary(MSnoeatFreq) 
standardCoefs(MSnoeatFreq)

#smoked cigarettes
MScigModel= glm(Life_Data_69~FSM_MateSeek,test,family="binomial")
summary(MScigModel)
exp(coef(MScigModel))

#broken a bone
test$Life_Data_80 <- as.factor(test$Life_Data_80)
levels(test$Life_Data_80)[levels(test$Life_Data_80)=="2"] <-"0"
MSboneModel= glm(Life_Data_80~FSM_MateSeek,test,family="binomial")
summary(MSboneModel)
exp(coef(MSboneModel))

#long term mating orientation
cor.test(~SOI_L+FSM_MateSeek,test[test$Relationship=="1",]) #in committed relationship
cor.test(~SOI_L+FSM_MateSeek,test[test$Relationship=="0",]) #single

#short term mating orientation
cor.test(~SOI_S+FSM_MateSeek,test[test$Relationship=="1",]) #in committed relationship
cor.test(~SOI_S+FSM_MateSeek,test[test$Relationship=="0",]) #single


###Mate retention general
describe(test$FSM_MateRet_GEN) 

#holiday gift
test$Life_Data_51 <- as.factor(test$Life_Data_51)
levels(test$Life_Data_51)[levels(test$Life_Data_51)=="2"] <-"0"
MRGholidaygiftModel= glm(Life_Data_51~FSM_MateRet_GEN,test,family="binomial")
summary(MRGholidaygiftModel) 
exp(coef(MRGholidaygiftModel))

#non holiday gift
test$Life_Data_52 <- as.factor(test$Life_Data_52)
levels(test$Life_Data_52)[levels(test$Life_Data_52)=="2"] <-"0"
MRGnonholidaygiftModel= glm(Life_Data_52~FSM_MateRet_GEN,test,family="binomial")
summary(MRGnonholidaygiftModel) 
exp(coef(MRGnonholidaygiftModel))

#unfaithful to partner
test$Life_Data_71 <- as.factor(test$Life_Data_71)
levels(test$Life_Data_71)[levels(test$Life_Data_71)=="2"] <-"0"
MRGcheatedModel= glm(Life_Data_71~FSM_MateRet_GEN,test,family="binomial")
summary(MRGcheatedModel) 
exp(coef(MRGcheatedModel))


###Mate retention breakup concern
describe(test$FSM_MateRet_BREAKUP)

#relatioship counselor
test$Life_Data_50 <- as.factor(test$Life_Data_50)
levels(test$Life_Data_50)[levels(test$Life_Data_50)=="2"] <-"0"
MRBcounselModel= glm(Life_Data_50~FSM_MateRet_BREAKUP,test,family="binomial")
summary(MRBcounselModel) 
exp(coef(MRBcounselModel))

#unfaithful to partner
MRBcheatedModel= glm(Life_Data_71~FSM_MateRet_BREAKUP,test,family="binomial")
summary(MRBcheatedModel) 
exp(coef(MRBcheatedModel))


###kin care family

#cared for younger relative
test$Life_Data_59 <- as.factor(test$Life_Data_59)
levels(test$Life_Data_59)[levels(test$Life_Data_59)=="2"] <-"0"
KFcaredModel= glm(Life_Data_59~FSM_KinCare_FAMILY,test,family="binomial")
summary(KFcaredModel) 
exp(coef(KFcaredModel))

#skydiving
test$Life_Data_81 <- as.factor(test$Life_Data_81)
levels(test$Life_Data_81)[levels(test$Life_Data_81)=="2"] <-"0"
KFskydiveModel= glm(Life_Data_81~FSM_KinCare_FAMILY,test,family="binomial")
summary(KFskydiveModel) 
exp(coef(KFskydiveModel))

#arrested
test$Life_Data_73 <- as.factor(test$Life_Data_73)
levels(test$Life_Data_73)[levels(test$Life_Data_73)=="2"] <-"0"
KFarrestedModel= glm(Life_Data_73~FSM_KinCare_FAMILY,test,family="binomial")
summary(KFarrestedModel) 
exp(coef(KFarrestedModel))

#moved in same city
KFmoveincityCModel= glm(Life_Data_18~FSM_KinCare_FAMILY,test,family="binomial")
summary(KFmoveincityCModel) 
exp(coef(KFmoveincityCModel))

#moved to diff city
test$Life_Data_19 <- as.factor(test$Life_Data_19)
levels(test$Life_Data_19)[levels(test$Life_Data_19)=="2"] <-"0"
KFmovediffCityModel= glm(Life_Data_19~FSM_KinCare_FAMILY,test,family="binomial")
summary(KFmovediffCityModel) 
exp(coef(KFmovediffCityModel))

#moved to diff country
test$Life_Data_20 <- as.factor(test$Life_Data_20)
levels(test$Life_Data_20)[levels(test$Life_Data_20)=="2"] <-"0"
KFmoveCountryModel= glm(Life_Data_20~FSM_KinCare_FAMILY,test,family="binomial")
summary(KFmoveCountryModel) 
exp(coef(KFmoveCountryModel))

#kept gun at home
test$Life_Data_82 <- as.factor(test$Life_Data_82)
levels(test$Life_Data_82)[levels(test$Life_Data_82)=="2"] <-"0"
KFgunHomeModel= glm(Life_Data_82~FSM_KinCare_FAMILY,test,family="binomial")
summary(KFgunHomeModel) 
exp(coef(KFgunHomeModel))

#cooked meal at home
test$Life_Data_66 <- as.factor(test$Life_Data_66)
levels(test$Life_Data_66)[levels(test$Life_Data_66)=="2"] <-"0"
KFcookModel= glm(Life_Data_66~FSM_KinCare_FAMILY,test,family="binomial")
summary(KFcookModel) 
exp(coef(KFcookModel))

#asked someone out
test$Life_Data_44 <- as.factor(test$Life_Data_44)
levels(test$Life_Data_44)[levels(test$Life_Data_44)=="2"] <-"0"
KFaskoutModel= glm(Life_Data_44~FSM_KinCare_FAMILY,test,family="binomial")
summary(KFaskoutModel) 
exp(coef(KFaskoutModel))

#ended relationship
KFdumpedModel= glm(Life_Data_36~FSM_KinCare_FAMILY,test,family="binomial")
summary(KFdumpedModel) 
exp(coef(KFdumpedModel))


###kin care child 
testCHILD <- test
testCHILD <- testCHILD[testCHILD$Parent!= 0, ]
describe(testCHILD$FSM_KinCare_CHILD)
cor.test(testCHILD$FSM_KinCare_CHILD,test$Big5_AGREE)
cor.test(testCHILD$FSM_KinCare_CHILD,test$DP_PRES)
cor.test(testCHILD$FSM_KinCare_CHILD,testCHILD$DP_DOM)
cor.test(testCHILD$FSM_KinCare_CHILD,testCHILD$SOI_S)

########################ADDITIONAL ANALYSIS##################
#For the added measures, the following analysis will be conducted:
#To examine the extent to which political affiliation predicts disease avoidance, we will perform a spearman’s correlation analysis. 
#To examine the extent to which personal experience with COVID predicts disease avoidance and mate seeking, we will perform a regression analysis. For both motives, we will simultaneously enter individual covid positive ever (no = -1, yes = 1), individual covid hospitalization (no = -1, yes = 1), covid vaccine (no = -1, yes = 1), family covid positive ever (no = -1, yes = 1), family covid hospitalization (no = -1, yes = 1), and family covid death (no = -1, yes = 1) as predictors.
#To examine the extent to which gender identity predicts the fundamental social motives, we will perform a multiple linear regression. The averages of each fundamental social motive will be the dependent variable, with the sex (male = -1, female = 1) being the independent variable, while controlling for gender identity. With gender identity as the confounding variable, we will be able to see if sex is still a significant predictor of the fundamental social motives. 
#To examine the extent to which sexual orientation predicts mate seeking, mate-retention, self-protection, disease avoidance, and kin care, we will perform a welch’s anova for each. We will also perform a welch’s t-test in order to examine if there is a difference more broadly between heterosexuality vs. nonheterosexuality as a predictor of the above fundamental social motives.

