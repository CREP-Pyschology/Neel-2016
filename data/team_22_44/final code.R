install.packages("githubinstall")
devtools::install_github('Timag/viewerWindow')
library(haven)
raw <- read_sav("raw.sav")
View(raw)


###############delete unneeded columns##############
#Remove excess information from Qualtrics data export
raw$StartDate <- NULL
raw$EndDate <- NULL
raw$IPAddress <- NULL
raw$RecordedDate <- NULL
raw$RecipientLastName <- NULL
raw$RecipientFirstName <- NULL
raw$RecipientEmail <- NULL
raw$ExternalReference <- NULL
raw$LocationLatitude <- NULL
raw$LocationLongitude <- NULL
raw$DistributionChannel <- NULL
raw$UserLanguage <- NULL
raw$Responseld <- NULL
raw$CF.infrmd.cnst.stmnt <- NULL
raw$Status <- NULL
raw$Progress<- NULL
raw$Duration__in_seconds_<- NULL
alldata <- raw

#remove responses where the participant indicated random answering
alldata <- alldata[alldata$Finished != 0, ]
alldata <- alldata[alldata$Random_ != 0, ]
alldata$Finished<- NULL
View(alldata)
#####################Reverse Scoring Items##########################
#reverse score fundamental social motive items
reverse_cols = c("FSM_SP_3","FSM_DA_4","FSM_DA_5","FSM_DA_6", "FSM_AG_4","FSM_S_6", "FSM_MS_3","FSM_MS_4", "FSM_MS_5","FSM_MRG_3", "FSM_MRG_4","FSM_MRG_5", "FSM_MRG_6","FSM_KCF_2", "FSM_KCF_3", "FSM_KCF_4", "FSM_KCC_3", "FSM_KCC_5")
alldata[ ,reverse_cols] = 8 - alldata[ ,reverse_cols]

#reverse score big5 items
reverse_cols = c("Big5_2","Big5_6","Big5_21","Big5_31", "Big5_12","Big5_27", "Big5_37","Big5_8", "Big5_18","Big5_23", "Big5_43","Big5_9", "Big5_24","Big5_34", "Big5_35","Big5_41")
alldata[ ,reverse_cols] = 8 - alldata[ ,reverse_cols]

#reverse score PVD items
reverse_cols = c("PVD2_5","PVD2_12","PVD2_14","PVD2_3", "PVD2_11","PVD2_13")
alldata[ ,reverse_cols] = 8 - alldata[ ,reverse_cols]

#reverse score DomPres items
reverse_cols = c("DP_10","DP_12","DP_2","DP_6", "DP_17")
alldata[ ,reverse_cols] = 8 - alldata[ ,reverse_cols]

#reverse score BDW items
reverse_cols = c("BDW_2","BDW_7","BDW_9","BDW_5", "BDW_4","BDW_12")
alldata[ ,reverse_cols] = 8 - alldata[ ,reverse_cols]

#reverse score N2B items
reverse_cols = c("N2B_1","N2B_3","N2B_7")
alldata[ ,reverse_cols] = 6 - alldata[ ,reverse_cols]

#reverse score ECR-R items
reverse_cols = c("ECR_9","ECR_11","ECR_20","ECR_22", "ECR_26","ECR_27", "ECR_28","ECR_29", "ECR_30","ECR_31", "ECR_33","ECR_34", "ECR_35","ECR_36")
alldata[ ,reverse_cols] = 8 - alldata[ ,reverse_cols]

#reverse score SOI items
reverse_cols = c("SOI_7","SOI_14","SOI_15","SOI_19")
alldata[ ,reverse_cols] = 6 - alldata[ ,reverse_cols]

#reverse score homelife and childhood items
reverse_cols = c("Dem_Q4","Dem_HomeLife_1","Dem_HomeLife_2","Dem_HomeLife_3")
alldata[ ,reverse_cols] = 8 - alldata[ ,reverse_cols]

#######################Averages#################################
###Create an average for each scale measure to be used in analysis
###The numbers are column numbers. If the question order changes and this code is being used, ensure that the column numbers match and/or are changed so they line up with the correct scores

#FSM averages 
alldata$FSM_MateSeek <- rowMeans(alldata[ , c("FSM_MS_1", "FSM_MS_2", "FSM_MS_3", "FSM_MS_4", "FSM_MS_5", "FSM_MS_6")], na.rm=TRUE)
alldata$FSM_SelfProtect <- rowMeans(alldata[ , c("FSM_SP_1", "FSM_SP_2", "FSM_SP_3", "FSM_SP_4", "FSM_SP_5", "FSM_SP_6")], na.rm=TRUE)
alldata$FSM_DiseaseAvoid <- rowMeans(alldata[ , c("FSM_DA_1", "FSM_DA_2", "FSM_DA_3", "FSM_DA_4", "FSM_DA_5", "FSM_DA_6")], na.rm=TRUE)
alldata$FSM_Affiliation_GROUP <- rowMeans(alldata[ , c("FSM_AG_1", "FSM_AG_2", "FSM_AG_3", "FSM_AG_4", "FSM_AG_5", "FSM_AG_6")], na.rm=TRUE)
alldata$FSM_Affiliation_EXCLUDE <- rowMeans(alldata[ , c("FSM_AEC_1", "FSM_AEC_2", "FSM_AEC_3", "FSM_AEC_4", "FSM_AEC_5", "FSM_AEC_6")], na.rm=TRUE)
alldata$FSM_Affiliation_INDEP <- rowMeans(alldata[ , c("FSM_AI_1", "FSM_AI_2", "FSM_AI_3", "FSM_AI_4", "FSM_AI_5", "FSM_AI_6")], na.rm=TRUE)
alldata$FSM_Status <- rowMeans(alldata[ , c("FSM_S_1", "FSM_S_2", "FSM_S_3", "FSM_S_4", "FSM_S_5", "FSM_S_6")], na.rm=TRUE)
alldata$FSM_KinCare_FAMILY <- rowMeans(alldata[ , c("FSM_KCF_1", "FSM_KCF_2", "FSM_KCF_3", "FSM_KCF_4", "FSM_KCF_5", "FSM_KCF_6")], na.rm=TRUE)
alldata$FSM_KinCare_CHILD <- rowMeans(alldata[ , c("FSM_KCC_1", "FSM_KCC_2", "FSM_KCC_3", "FSM_KCC_4", "FSM_KCC_5", "FSM_KCC_6")], na.rm=TRUE)
alldata$FSM_MateRet_BREAKUP <- rowMeans(alldata[ , c("FSM_MRBC_1", "FSM_MRBC_2", "FSM_MRBC_3", "FSM_MRBC_4", "FSM_MRBC_5", "FSM_MRBC_6")], na.rm=TRUE)
alldata$FSM_MateRet_GEN <- rowMeans(alldata[ , c("FSM_MRG_1", "FSM_MRG_2", "FSM_MRG_3", "FSM_MRG_4", "FSM_MRG_5", "FSM_MRG_6")], na.rm=TRUE)
#FSM omegas
#omega
library(psych)
install.packages("GPArotation")
library(GPArotation)
SPomega = omega(alldata[ , c("FSM_SP_1", "FSM_SP_2", "FSM_SP_3", "FSM_SP_4", "FSM_SP_5", "FSM_SP_6")])
SPomega #FSM Self Protection omega total of 0.9

DAomega <- alldata[ , c("FSM_DA_1", "FSM_DA_2", "FSM_DA_3", "FSM_DA_4", "FSM_DA_5", "FSM_DA_6")]
omega(DAomega, 4) # 0.88

AGomega <- alldata[ , c("FSM_AG_1", "FSM_AG_2", "FSM_AG_3", "FSM_AG_4", "FSM_AG_5", "FSM_AG_6")]
omega(AGomega, 4) # 0.87

AIomega <- alldata[ , c("FSM_AI_1", "FSM_AI_2", "FSM_AI_3", "FSM_AI_4", "FSM_AI_5", "FSM_AI_6")]
omega(AIomega, 4) #0.80

AEomega <- alldata[ , c("FSM_AEC_1", "FSM_AEC_2", "FSM_AEC_3", "FSM_AEC_4", "FSM_AEC_5", "FSM_AEC_6")]
omega(AEomega) #0.92

Somega <- alldata[ , c("FSM_S_1", "FSM_S_2", "FSM_S_3", "FSM_S_4", "FSM_S_5", "FSM_S_6")]
omega(Somega, 4) # 0.82

MSomega <- alldata[ , c("FSM_MS_1", "FSM_MS_2", "FSM_MS_3", "FSM_MS_4", "FSM_MS_5", "FSM_MS_6")]
omega(MSomega) #0.93

MRGomega <- alldata[ , c("FSM_MRG_1", "FSM_MRG_2", "FSM_MRG_3", "FSM_MRG_4", "FSM_MRG_5", "FSM_MRG_6")]
omega(MRGomega, 4) # 0.87

MRBomega <- alldata[ , c("FSM_MRBC_1", "FSM_MRBC_2", "FSM_MRBC_3", "FSM_MRBC_4", "FSM_MRBC_5", "FSM_MRBC_6")]
omega(MRBomega, 4) # 0.94

KCFomega <- alldata[ , c("FSM_KCF_1", "FSM_KCF_2", "FSM_KCF_3", "FSM_KCF_4", "FSM_KCF_5", "FSM_KCF_6")]
omega(KCFomega) # omega total of 0.93

##not enough data for Kin Care Child motive for analysis
#KCComega <- alldata[ , c("FSM_KCC_1", "FSM_KCC_2", "FSM_KCC_3", "FSM_KCC_4", "FSM_KCC_5", "FSM_KCC_6")]
#omega(KCComega)

#Big5 averages
alldata$Big5_EXTVRT <- rowMeans(alldata[ , c("Big5_1", "Big5_6", "Big5_11", "Big5_16", "Big5_21", "Big5_26", "Big5_31", "Big5_36")], na.rm=TRUE)
alldata$Big5_AGREE <- rowMeans(alldata[ , c("Big5_2", "Big5_7", "Big5_12", "Big5_17", "Big5_22", "Big5_27", "Big5_32", "Big5_37", "Big5_42")], na.rm=TRUE)
alldata$Big5_CONC <- rowMeans(alldata[ , c("Big5_3", "Big5_8", "Big5_13", "Big5_18", "Big5_23", "Big5_28", "Big5_33", "Big5_38", "Big5_43")], na.rm=TRUE)
alldata$Big5_NEUR <- rowMeans(alldata[ , c("Big5_4", "Big5_9", "Big5_14", "Big5_19", "Big5_24", "Big5_29", "Big5_34", "Big5_39")], na.rm=TRUE)
alldata$Big5_OPEN <- rowMeans(alldata[ , c("Big5_5", "Big5_10", "Big5_15", "Big5_20", "Big5_25", "Big5_30", "Big5_35", "Big5_40", "Big5_41", "Big5_44")], na.rm=TRUE)

Eomega <- alldata[ , c("Big5_1", "Big5_6", "Big5_11", "Big5_16", "Big5_21", "Big5_26", "Big5_31", "Big5_36")]
omega(Eomega) 
  # omega total of 0.91
Aomega <- alldata[ , c("Big5_2", "Big5_7", "Big5_12", "Big5_17", "Big5_22", "Big5_27", "Big5_32", "Big5_37", "Big5_42")]
omega(Aomega) 
  # omega total of 0.77
Comega <- alldata[ , c("Big5_3", "Big5_8", "Big5_13", "Big5_18", "Big5_23", "Big5_28", "Big5_33", "Big5_38")]
omega(Comega, 2) 
  # omega total of 0.84
Nomega <- alldata[ , c("Big5_4", "Big5_9", "Big5_14", "Big5_19", "Big5_24", "Big5_29", "Big5_34", "Big5_39")]
omega(Nomega) 
  # omega total of 0.91
Oomega <- alldata[ , c("Big5_5", "Big5_10", "Big5_15", "Big5_20", "Big5_25", "Big5_30", "Big5_35", "Big5_40", "Big5_41", "Big5_44")]
omega(Oomega, 2) 
  # omega total of 0.79


#SOI averages
alldata$SOI_S <- rowMeans(alldata[ , c(372:381,388)], na.rm=TRUE)
alldata$SOI_L <- rowMeans(alldata[ , c(382:387,389,390)], na.rm=TRUE)

SOISomega <- alldata[ , c(372:381,388)]
omega(SOISomega) 
  # omega total of 0.94
SOILomega <- alldata[ , c(382:387,389,390)]
omega(SOILomega, 4) 
  # omega total of 0.96

#PVD averages
alldata$TDDS_ONLY <- rowMeans(alldata[ , c(391:411)], na.rm=TRUE)
alldata$PVD_ONLY <- rowMeans(alldata[ , c(412:426)], na.rm=TRUE)
alldata$PVD_INFECT <- rowMeans(alldata[ , c(413,416,417,419,421,423,425)], na.rm=TRUE)
alldata$PVD_GERMS <- rowMeans(alldata[ , c(412,414,415,418,420,422,424,426)], na.rm=TRUE)
alldata$TDDS_MORAL <- rowMeans(alldata[ , c(394,391,406,397,400,409,403)], na.rm=TRUE)
alldata$TDDS_SEXUAL <- rowMeans(alldata[ , c(407,401,404,392,410,395,398)], na.rm=TRUE)
alldata$TDDS_PATHOGEN <- rowMeans(alldata[ , c(402,405,399,393,411,408,396)], na.rm=TRUE)

PVDIomega <- alldata[ , c(413,416,417,419,421,423,425)]
omega(PVDIomega, 2) # omega total of 0.92
PVDGomega <- alldata[ , c(412,414,415,418,420,422,424,426)]
omega(PVDGomega) # omega total of 0.71

#DomPres averages
alldata$DP_DOM <- rowMeans(alldata[ , c(429,431,433,435,437,442,436,438)], na.rm=TRUE)
alldata$DP_PRES <- rowMeans(alldata[ , c(427,430,434,439,440,441,428,432,443)], na.rm=TRUE)

DOMomega <- alldata[ , c(429,431,433,435,437,442,436,438)]
omega(DOMomega, 2) # omega total of 0.91
PRESomega <- alldata[ , c(427,430,434,439,440,441,428,432,443)]
omega(PRESomega, 2) # omega total of 0.82

#BDW average
alldata$BDW <- rowMeans(alldata[ , c(490:501)], na.rm=TRUE)

BDWomega <- alldata[ , c(490:501)]
omega(BDWomega, 2) # omega total of 0.85

#N2B average
alldata$N2B <- rowMeans(alldata[ , c(480:489)], na.rm=TRUE)

N2Bomega <- alldata[ , c(480:489)]
omega(N2Bomega, 2) # omega total of 0.90
#ECR averages
alldata$ECR_ANXIETY <- rowMeans(alldata[ , c(444:461)], na.rm=TRUE)
alldata$ECR_AVOID <- rowMeans(alldata[ , c(462:479)], na.rm=TRUE)

ECRANXomega <- alldata[ , c(444:461)]
omega(ECRANXomega,2) # omega total of 0.85
ECRAVomega <- alldata[ , c(462:479)]
omega(ECRAVomega, 3) # omega total of 0.85

#Resources Stability averages
alldata$Resources_CHILDHOOD <- rowMeans(alldata[ , c(525:528)], na.rm=TRUE)
alldata$Resources_CURRENT <- rowMeans(alldata[ , c(529:530)], na.rm=TRUE)
alldata$CldhdStablty <- rowMeans(alldata[ , c(533:535)], na.rm=TRUE)
ResourcesCHILDHOODomega <- alldata[ , c(525:528)]
omega(ResourcesCHILDHOODomega, 3) # omega total of 0.88
ResourcesCURRENTomega <- alldata[ , c(529:530)]
omega(ResourcesCURRENTomega, 2) # omega total of 0.7
StabilityCHILDHOODomega <- alldata[ , c(533:535)]
omega(StabilityCHILDHOODomega, 2) # omega total of 0.85

#average age across participants
View(mean(alldata$Dem_AGE))
#Average age: 20.57 years old
sd(alldata$Dem_AGE)
#SD: 5.62
summary(alldata$Dem_AGE)
# Min.  1st Qu.  Median  Mean    3rd Qu.  Max. 
#18.00   18.00   19.00   20.57   21.00   64.00 
View(alldata)


################## Analyzing the data #####################
# types of analyses:
#     -descriptive statistics and correlations
#     -predict life events (dichotmous/frequency): Neel Table S3
#     -look for demographic predictors of motives: Neel Table 4

library(psych)
library(lsr)
library(car)

##find which life events have enough data (more than 40 for each):
a=which(names(alldata)=="Life_Data_1")
summary(alldata[,a:(a+80)])
View(summary(alldata[,a:(a+80)]))
EnoughData <- (alldata[,a:(a+80)])
#1 = yes to the life event, 2 = no to the life event
View(sapply(X = EnoughData,
       FUN = table))

#find which frequencies have enough variability
a=which(names(alldata)=="Life_Data_1")
summary(alldata[,(a+81):(a+160)])
View(summary(alldata[,(a+81):(a+160)]))
EnoughData <- (alldata[,(a+81):(a+160)])
View(sapply(X = EnoughData,
            FUN = table))


##########################################Replicating Tables############################################
################ Descriptive statistics
install.packages("psych")
library(psych)
a=which(names(alldata)=="FSM_MateSeek")
summary(alldata[,a:(a+10)])
# FSM_MateSeek   FSM_SelfProtect FSM_DiseaseAvoid FSM_Affiliation_GROUP
#Min.   :1.000   Min.   :1.833   Min.   :1.000    Min.   :2.333        
#1st Qu.:2.667   1st Qu.:4.333   1st Qu.:3.500    1st Qu.:4.167        
#Median :3.833   Median :5.167   Median :4.167    Median :5.167        
#Mean   :3.576   Mean   :5.052   Mean   :4.197    Mean   :4.911        
#3rd Qu.:4.500   3rd Qu.:5.833   3rd Qu.:4.833    3rd Qu.:5.500        
#Max.   :7.000   Max.   :7.000   Max.   :7.000    Max.   :7.000        

#FSM_Affiliation_EXCLUDE FSM_Affiliation_INDEP   FSM_Status    FSM_KinCare_FAMILY
#Min.   :1.000           Min.   :1.833         Min.   :1.333   Min.   :2.167     
#1st Qu.:3.833           1st Qu.:4.000         1st Qu.:4.000   1st Qu.:5.000     
#Median :4.667           Median :4.667         Median :4.500   Median :5.833     
#Mean   :4.682           Mean   :4.687         Mean   :4.445   Mean   :5.607     
#3rd Qu.:5.333           3rd Qu.:5.333         3rd Qu.:5.000   3rd Qu.:6.333     
#Max.   :7.000           Max.   :6.833         Max.   :6.667   Max.   :7.000     

##FSM_KinCare_CHILD FSM_MateRet_BREAKUP FSM_MateRet_GEN
#Min.   :4.500     Min.   :1.000       Min.   :4.000  
#1st Qu.:5.167     1st Qu.:2.625       1st Qu.:5.833  
#Median :5.833     Median :3.750       Median :6.333  
#Mean   :5.778     Mean   :3.556       Mean   :6.122  
#3rd Qu.:6.417     3rd Qu.:4.667       3rd Qu.:6.667  
#Max.   :7.000     Max.   :6.000       Max.   :7.000  
#NA's   :106       NA's   :61          NA's   :61  

describe(alldata[,a:(a+10)])
#                        vars   n mean   sd median trimmed  mad  min  max range  skew kurtosis  se
#FSM_MateSeek               1 109 3.58 1.53   3.83    3.55 1.73 1.00 7.00  6.00  0.02    -0.65  0.15
#FSM_SelfProtect            2 109 5.05 1.06   5.17    5.13 0.99 1.83 7.00  5.17 -0.68     0.18  0.10
#FSM_DiseaseAvoid           3 109 4.20 1.17   4.17    4.21 0.99 1.00 7.00  6.00 -0.02    -0.26  0.11
#FSM_Affiliation_GROUP      4 109 4.91 0.97   5.17    4.94 0.74 2.33 7.00  4.67 -0.31    -0.35  0.09
#FSM_Affiliation_EXCLUDE    5 109 4.68 1.20   4.67    4.70 1.24 1.00 7.00  6.00 -0.23    -0.08  0.11
#FSM_Affiliation_INDEP      6 109 4.69 0.95   4.67    4.69 0.99 1.83 6.83  5.00 -0.03    -0.21  0.09
#FSM_Status                 7 109 4.44 1.00   4.50    4.48 0.74 1.33 6.67  5.33 -0.39     0.81  0.10
#FSM_KinCare_FAMILY         8 109 5.61 1.09   5.83    5.70 1.24 2.17 7.00  4.83 -0.76     0.18  0.10
#FSM_KinCare_CHILD          9   3 5.78 1.25   5.83    5.78 1.73 4.50 7.00  2.50 -0.04    -2.33  0.72
#FSM_MateRet_BREAKUP       10  48 3.56 1.42   3.75    3.60 1.61 1.00 6.00  5.00 -0.23    -1.08  0.21
#FSM_MateRet_GEN           11  48 6.12 0.81   6.33    6.24 0.49 4.00 7.00  3.00 -1.33     1.10  0.12


library(Hmisc)

################ FMI correlations 
a=which(names(alldata)=="FSM_MateSeek")
FSMcorData <- alldata[,a:(a+10)]
FSMcor<-round(cor(FSMcorData,use="pairwise.complete.obs"),2)
FSMcor

# Hide upper triangle
upperFSMcor<-FSMcor
upperFSMcor[upper.tri(upperFSMcor)]<-""
upperFSMcor<-as.data.frame(upperFSMcor)
View(upperFSMcor)

FSMcorP <- corr.test(FSMcorData)$p    # Apply corr.test function
upperFSMcorP<-FSMcorP
upperFSMcorP[upper.tri(upperFSMcorP)]<-""
upperFSMcorP<-as.data.frame(upperFSMcorP)
View(upperFSMcorP)

##Significant Correlations
#MRBC & MS;  r = 0.35,  p = 0.0162
# MRG & MS;  r = -0.42, p = 0.0029
#  DA & SP;  r = 0.33,  p = 0.0005
# AEC & SP;  r = 0.23,  p = 0.0175
# AEC & DA;  r = 0.25,  p = 0.0085
#   S & DA;  r = 0.2,   p = 0.0367
#   S & AG;  r = 0.2,   p = 0.0388
#MRBC & AEC; r = 0.34,  p = 0.0196
# KCC & KCF; r = 1,     p = 0.0490
# MRG & KCF; r = 0.29,  p = 0.0421


################ FMI correlation with Big 5
a=which(names(alldata)=="FSM_MateSeek")
FSMbig5data <- alldata[,a:(a+15)] 
FSMbig5<-round(cor(FSMbig5data,use="pairwise.complete.obs"),2)
FSMbig5

# Hide upper triangle
upperFSMbig5<-FSMbig5
upperFSMbig5[upper.tri(upperFSMbig5)]<-""
upperFSMbig5<-as.data.frame(upperFSMbig5)
View(upperFSMbig5)
upperFSMbig5P <- corr.test(FSMbig5data)$p    # Apply corr.test function
upperFSMbig5P<-upperFSMbig5P
upperFSMbig5P[upper.tri(upperFSMbig5P)]<-""
upperFSMbig5P<-as.data.frame(upperFSMbig5P)
View(upperFSMbig5P)

##Significant Correlations
# Open & DA;   r = 0.26,  p = 0.0066
#Agree & AG;   r = 0.35,  p = 0.0002
# Conc & AG;   r = 0.23,  p = 0.0171
# Neur & AG;   r = -0.22, p = 0.0205
# Conc & AI;   r = -0.2,  p = 0.0366
# Extr & S;    r = 0.34,  p = 0.0004
# Open & S;    r = 0.22,  p = 0.0222
# Extr & KCF;  r = 0.24,  p = 0.0119
#Agree & KCF;  r = 0.23,  p = 0.0156
# Extr & MRBC; r = -0.36, p = 0.0121
# Conc & MRBC; r = -0.4,  p = 0.0051
# Neur & MRBC; r = 0.35,  p = 0.0134
#Agree & MRG;  r = 0.29,  p = 0.0423
# Conc & MRG;  r = 0.3,   p = 0.0388

#Agree & Extr;  r = 0.21,  p = 0.0301
# Open & Extr;  r = 0.3,   p = 0.0005
# Neur & Extr;  r = 0.07,  p = 0.0244
# Open & Extr;  r = 0.06,  p = 0.0362
# Neur & Agree; r = -0.2,  p = 0.0333
# Open & Agree; r = 0.29,  p = 0.0020
# Neur & Conc;  r = -0.32, p = 0.0006
# Open & Conc;  r = 0.21,  p = 0.0314

################ FSM correlation with other scales
FSMscaledata <- alldata[,c("FSM_SelfProtect","FSM_DiseaseAvoid","FSM_Affiliation_GROUP","FSM_Affiliation_EXCLUDE","FSM_Affiliation_INDEP","FSM_Status","FSM_MateSeek","FSM_MateRet_GEN","FSM_MateRet_BREAKUP","FSM_KinCare_FAMILY","FSM_KinCare_CHILD","SOI_S","SOI_L","BDW","DP_DOM","DP_PRES", "ECR_ANXIETY" , "ECR_AVOID","N2B","TDDS_ONLY","PVD_ONLY")] 
FSMscaleCor<-round(cor(FSMscaledata,use="pairwise.complete.obs"),2)
FSMscaleCor
# Hide upper triangle
upperFSMscaleCor<-FSMscaleCor
upperFSMscaleCor[upper.tri(upperFSMscaleCor)]<-""
upperFSMscaleCor<-as.data.frame(upperFSMscaleCor)
View(upperFSMscaleCor)

upperFSMscaleCorP <- corr.test(FSMscaledata)$p    # Apply corr.test function
upperFSMscaleCorP<-upperFSMscaleCorP
upperFSMscaleCorP[upper.tri(upperFSMscaleCorP)]<-""
upperFSMscaleCorP<-as.data.frame(upperFSMscaleCorP)
View(upperFSMscaleCorP)

##Significant Correlations
#  SOI_S & FSM_SP;   r = -0.31, p = 0.0249
#  SOI_L & FSM_SP;   r = 0.31,  p = 0.0221
#    BDW & FSM_SP;   r = 0.28,  p = 0.0386
#DP_Pres & FSM_SP;   r = 0.3,   p = 0.0272
#    PVD & FSM_DA;   r = 0.45,  p = 0.0006
#DP_Pres & FSM_AG;   r = 0.27,  p = 0.0471
#ECR_Anx & FSM_AEC;  r = 0.36,  p = 0.0074
# DP_Dom & FSM_S;    r = 0.45,  p = 0.0006
#    N2B & FSM_S;    r = 0.38,  p = 0.0042
#    PVD & FSM_S;    r = 0.29,  p = 0.0362
#  SOI_S & FSM_MS;   r = 0.36,  p = 0.0068
# DP_Dom & FSM_MS;   r = 0.34,  p = 0.0123
#  SOI_L & FSM_MRG;  r = 0.64,  p = 0.0003
#DP_Pres & FSM_MRG;  r = 0.48,  p = 0.0164
# ECR_Av & FSM_MRG;  r = -0.44, p = 0.0370
#ECR_Anx & FSM_MRBC; r = 0.42,  p = 0.0457
# ECR_Av & FSM_KCF;  r = -0.35, p = 0.0079
#   TDDS & DA;       r = 0.31,  p = 0.0208



# DP_Dom & SOI_S;    r = 0.41,  p = 0.0023
# DP_Dom & SOI_L;    r = -0.28, p = 0.0372
#DP_Pres & SOI_L;    r = 0.37,  p = 0.0059
# ECR_Av & ECR_Anx;  r = 0.36,  p = 0.0389
#    N2B & ECR_Anx;  r = 0.28,  p = 0.0389
#   TDDS & SOI_S;    r = -0.32, p = 0.0202
#   TDDS & PVD;      r = 0.43,  p = 0.0010

#Make column for relationship status (Excluded is for options left out of analysis ('multiple parnters' or 'other')
alldata$Relationship <- as.factor(alldata$Relationship_Stat)
levels(alldata$Relationship)[levels(alldata$Relationship)=="5"] <- "No"
levels(alldata$Relationship)[levels(alldata$Relationship)=="1"] <- "Yes"
levels(alldata$Relationship)[levels(alldata$Relationship)=="2"] <- "Yes"
levels(alldata$Relationship)[levels(alldata$Relationship)=="3"] <- "Yes"
levels(alldata$Relationship)[levels(alldata$Relationship)=="4"] <- "Excluded"
levels(alldata$Relationship)[levels(alldata$Relationship)=="6"] <- "Excluded"

#Make column for parental status 
alldata$Parent <- as.factor(alldata$Have_children_)
levels(alldata$Parent)[levels(alldata$Parent)=="1"] <-"Yes"
levels(alldata$Parent)[levels(alldata$Parent)=="2"] <-"No"

#make responses for biological sex as categorical and not numerical
alldata$Sex <- as.factor(alldata$Dem_SEX)
levels(alldata$Sex)[levels(alldata$Sex)=="1"] <-"Male"
levels(alldata$Sex)[levels(alldata$Sex)=="2"] <-"Female"

LifeHistdata <- alldata
LifeHistdata <- LifeHistdata[LifeHistdata$Relationship != "Excluded", ]

#dummy code
lifehistParent <- ifelse(LifeHistdata$Parent == "Yes", 1, 0)
lifehistRelationship <- ifelse(LifeHistdata$Relationship == "Yes", 1, 0)
lifehistSex <- ifelse(LifeHistdata$Sex == "Female", 1, 0)


#######Regression Analysis to check if age, relationship status, parental status, biological sex, childhood stability, childhood resources, and current resources are predictors of the FSM

#self-protection
LifeHistSP = lm(FSM_SelfProtect~Dem_AGE+lifehistSex+lifehistRelationship+lifehistParent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeHistdata)
summary(LifeHistSP) 
#Residuals:
#Min      1Q  Median      3Q     Max 
#-3.2927 -0.5620  0.0952  0.7748  2.1730 

#Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#(Intercept)           4.85967    0.76403   6.361 6.43e-09 ***
#Dem_AGE              -0.01858    0.02800  -0.664   0.5084    
#lifehistSex           0.52963    0.22596   2.344   0.0211 *  
#lifehistRelationship  0.15548    0.22329   0.696   0.4879    
#lifehistParent       -0.43409    0.97861  -0.444   0.6583    
#CldhdStablty          0.03969    0.07884   0.503   0.6158    
#Resources_CHILDHOOD  -0.01437    0.09524  -0.151   0.8803    
#Resources_CURRENT     0.03085    0.06779   0.455   0.6500    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.047 on 98 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.0908,	Adjusted R-squared:  0.02585 
#F-statistic: 1.398 on 7 and 98 DF,  p-value: 0.2148

standardCoefs(LifeHistSP)
#                               b        beta
#Dem_AGE              -0.01858435 -0.09963465
#lifehistSex           0.52963058  0.25007016 *
#lifehistRelationship  0.15548103  0.07314854
#lifehistParent       -0.43408580 -0.06817234
#CldhdStablty          0.03968865  0.05994622
#Resources_CHILDHOOD  -0.01437344 -0.01893668
#Resources_CURRENT     0.03085342  0.04948881


#Disease Avoidance
LifeHistDA = lm(FSM_DiseaseAvoid~Dem_AGE+lifehistSex+lifehistRelationship+lifehistParent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeHistdata)
summary(LifeHistDA) 
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-3.06639 -0.69708 -0.02657  0.92360  2.31445 

#Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#(Intercept)           3.380084   0.864425   3.910  0.00017 ***
#Dem_AGE               0.037014   0.031677   1.168  0.24544    
#lifehistSex           0.219035   0.255652   0.857  0.39366    
#lifehistRelationship -0.473326   0.252630  -1.874  0.06397 .  
#lifehistParent       -0.334407   1.107208  -0.302  0.76327    
#CldhdStablty         -0.089299   0.089199  -1.001  0.31923    
#Resources_CHILDHOOD   0.149684   0.107750   1.389  0.16793    
#Resources_CURRENT    -0.005096   0.076704  -0.066  0.94716    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.185 on 98 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.07048,	Adjusted R-squared:  0.004089 
#F-statistic: 1.062 on 7 and 98 DF,  p-value: 0.3941

standardCoefs(LifeHistDA)
#                                b         beta
#Dem_AGE               0.037013891  0.177340217
#lifehistSex           0.219035296  0.092423480
#lifehistRelationship -0.473325907 -0.199006817
#lifehistParent       -0.334407219 -0.046934006
#CldhdStablty         -0.089299488 -0.120537950
#Resources_CHILDHOOD   0.149683660  0.176236893
#Resources_CURRENT    -0.005096107 -0.007305024


#Affiliation (Group)
LifeHistAG = lm(FSM_Affiliation_GROUP~Dem_AGE+lifehistSex+lifehistRelationship+lifehistParent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeHistdata)
summary(LifeHistAG)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.64404 -0.52993  0.08737  0.74331  1.72496 

#Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
#(Intercept)           4.874471   0.695188   7.012 3.04e-10 ***
#Dem_AGE               0.003848   0.025475   0.151   0.8802    
#lifehistSex          -0.215070   0.205601  -1.046   0.2981    
#lifehistRelationship  0.046444   0.203170   0.229   0.8197    
#lifehistParent       -0.614414   0.890439  -0.690   0.4918    
#CldhdStablty          0.182138   0.071736   2.539   0.0127 *  
#Resources_CHILDHOOD  -0.142744   0.086655  -1.647   0.1027    
#Resources_CURRENT    -0.033167   0.061687  -0.538   0.5920    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.9528 on 98 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.1064,	Adjusted R-squared:  0.04255 
#F-statistic: 1.667 on 7 and 98 DF,  p-value: 0.1261
standardCoefs(LifeHistAG)
#                                b        beta
#Dem_AGE               0.003848205  0.02247877
#lifehistSex          -0.215070192 -0.11064207
#lifehistRelationship  0.046443598  0.02380704
#lifehistParent       -0.614413993 -0.10513442
#CldhdStablty          0.182138348  0.29974233 *
#Resources_CHILDHOOD  -0.142744286 -0.20490522
#Resources_CURRENT    -0.033166621 -0.05796371


#Affiliation (Exclusion concern)
LifeHistAE = lm(FSM_Affiliation_EXCLUDE~Dem_AGE+lifehistSex+lifehistRelationship+lifehistParent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeHistdata)
summary(LifeHistAE)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.5808 -0.7065  0.0138  0.8082  2.5560 

#Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
#(Intercept)           3.891450   0.883474   4.405 2.71e-05 ***
#Dem_AGE               0.014795   0.032375   0.457   0.6487    
#lifehistSex           0.515766   0.261286   1.974   0.0512 .  
#lifehistRelationship -0.038017   0.258197  -0.147   0.8832    
#lifehistParent       -0.577099   1.131607  -0.510   0.6112    
#CldhdStablty          0.102029   0.091165   1.119   0.2658    
#Resources_CHILDHOOD   0.007763   0.110125   0.070   0.9439    
#Resources_CURRENT    -0.041373   0.078394  -0.528   0.5989    
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.211 on 98 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.0515,	Adjusted R-squared:  -0.01625 
#F-statistic: 0.7602 on 7 and 98 DF,  p-value: 0.6219
standardCoefs(LifeHistAE)
#                                b         beta
#Dem_AGE               0.014795070  0.070062001
#lifehistSex           0.515765704  0.215101631
#lifehistRelationship -0.038016827 -0.015798163
#lifehistParent       -0.577098630 -0.080054373
#CldhdStablty          0.102028819  0.136119618
#Resources_CHILDHOOD   0.007763079  0.009033987
#Resources_CURRENT    -0.041372707 -0.058616526


#Affiliation (Independence)
LifeHistAI = lm(FSM_Affiliation_INDEP~Dem_AGE+lifehistSex+lifehistRelationship+lifehistParent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeHistdata)
summary(LifeHistAI)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.68840 -0.69158  0.00133  0.59481  2.21316 

#Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
#(Intercept)           4.644636   0.694637   6.686 1.41e-09 ***
#Dem_AGE               0.001514   0.025455   0.059   0.9527    
#lifehistSex           0.084533   0.205438   0.411   0.6816    
#lifehistRelationship -0.068016   0.203009  -0.335   0.7383    
#lifehistParent       -0.439728   0.889733  -0.494   0.6223    
#CldhdStablty         -0.133331   0.071679  -1.860   0.0659 .  
#Resources_CHILDHOOD   0.005048   0.086586   0.058   0.9536    
#Resources_CURRENT     0.125698   0.061638   2.039   0.0441 *  
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.9521 on 98 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.08467,	Adjusted R-squared:  0.01929 
#F-statistic: 1.295 on 7 and 98 DF,  p-value: 0.2608

standardCoefs(LifeHistAI)
#                                b         beta
#Dem_AGE               0.001513874  0.008956980
#lifehistSex           0.084532513  0.044047506
#lifehistRelationship -0.068015602 -0.035313890
#lifehistParent       -0.439728028 -0.076212370
#CldhdStablty         -0.133330859 -0.222246456
#Resources_CHILDHOOD   0.005048369  0.007340115
#Resources_CURRENT     0.125698312  0.222506084 *


#Status
LifeHistS = lm(FSM_Status~Dem_AGE+lifehistSex+lifehistRelationship+lifehistParent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeHistdata)
summary(LifeHistS)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.92802 -0.45545  0.05221  0.58471  2.44898 

#Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
#(Intercept)           3.504933   0.735141   4.768 6.48e-06 ***
#Dem_AGE               0.011590   0.026939   0.430   0.6680    
#lifehistSex           0.384114   0.217417   1.767   0.0804 .  
#lifehistRelationship  0.056010   0.214846   0.261   0.7949    
#lifehistParent        0.064225   0.941613   0.068   0.9458    
#CldhdStablty          0.169278   0.075859   2.231   0.0279 *  
#Resources_CHILDHOOD  -0.068866   0.091635  -0.752   0.4541    
#Resources_CURRENT     0.006819   0.065232   0.105   0.9170    
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.008 on 98 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.06825,	Adjusted R-squared:  0.001697 
#F-statistic: 1.025 on 7 and 98 DF,  p-value: 0.4183
standardCoefs(LifeHistS)
#                                b        beta
#Dem_AGE               0.011590258  0.06537523
#lifehistSex           0.384113667  0.19081196
#lifehistRelationship  0.056010204  0.02772378
#lifehistParent        0.064224811  0.01061188
#CldhdStablty          0.169277829  0.26900015 *
#Resources_CHILDHOOD  -0.068865994 -0.09545636
#Resources_CURRENT     0.006819092  0.01150767


#Mate seeking
LifeHistMS = lm(FSM_MateSeek~Dem_AGE+lifehistSex+lifehistRelationship+lifehistParent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeHistdata)
summary(LifeHistMS)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.4531 -0.7406 -0.0785  0.7618  3.6503 

#Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
#(Intercept)           3.794959   0.811174   4.678 9.27e-06 ***
#Dem_AGE               0.015496   0.029725   0.521   0.6033    
#lifehistSex          -0.369221   0.239903  -1.539   0.1270    
#lifehistRelationship -1.848508   0.237067  -7.797 6.85e-12 ***
#lifehistParent       -1.948138   1.039001  -1.875   0.0638 .  
#CldhdStablty          0.005962   0.083704   0.071   0.9434    
#Resources_CHILDHOOD   0.009727   0.101113   0.096   0.9236    
#Resources_CURRENT     0.105150   0.071978   1.461   0.1473    
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.112 on 98 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.5168,	Adjusted R-squared:  0.4822 
#F-statistic: 14.97 on 7 and 98 DF,  p-value: 3.71e-13
standardCoefs(LifeHistMS)
#                                b         beta
#Dem_AGE               0.015495632  0.057044764
#lifehistSex          -0.369220636 -0.119706619
#lifehistRelationship -1.848507620 -0.597163028 ***
#lifehistParent       -1.948138142 -0.210085327
#CldhdStablty          0.005962096  0.006183548
#Resources_CHILDHOOD   0.009727489  0.008800091
#Resources_CURRENT     0.105150430  0.115813224


##Mate Retention

LifeHistMRdata <- LifeHistdata
LifeHistMRdata <- LifeHistMRdata[LifeHistMRdata$Relationship != "No", ]
lifehistMRParent <- ifelse(LifeHistMRdata$Parent == "Yes", 1, 0)
lifehistMRSex <- ifelse(LifeHistMRdata$Sex == "Female", 1, 0)

#Mate Retention General
LifeHistMRG = lm(FSM_MateRet_GEN~Dem_AGE+lifehistMRSex+lifehistMRParent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeHistMRdata)
summary(LifeHistMRG)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.0136 -0.3241  0.1364  0.4138  0.9932 

#Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          6.85656    0.72673   9.435 1.01e-11 ***
#Dem_AGE             -0.05854    0.03055  -1.916   0.0625 .  
#lifehistMRSex        0.59968    0.25889   2.316   0.0257 *  
#lifehistMRParent     1.56149    0.92926   1.680   0.1007    
#CldhdStablty         0.08048    0.07791   1.033   0.3078    
#Resources_CHILDHOOD -0.12369    0.09549  -1.295   0.2026    
#Resources_CURRENT    0.03703    0.06884   0.538   0.5936    
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.734 on 40 degrees of freedom
#(1 observation deleted due to missingness)
#Multiple R-squared:  0.1868,	Adjusted R-squared:  0.06484 
#F-statistic: 1.532 on 6 and 40 DF,  p-value: 0.1929
standardCoefs(LifeHistMRG)
#                              b        beta
#Dem_AGE             -0.05853630 -0.57768956
#lifehistMRSex        0.59967696  0.36523003 *
#lifehistMRParent     1.56148695  0.50833903
#CldhdStablty         0.08048227  0.18151204
#Resources_CHILDHOOD -0.12368998 -0.24052576
#Resources_CURRENT    0.03703451  0.08736087

#Mate Retention Breakup Concern
LifeHistMRB = lm(FSM_MateRet_BREAKUP~Dem_AGE+lifehistMRSex+lifehistMRParent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeHistMRdata)
summary(LifeHistMRB)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.5920 -0.8295  0.1067  0.8976  2.7026 

#Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)  
#(Intercept)          1.74142    1.36144   1.279   0.2082  
#Dem_AGE              0.03304    0.05723   0.577   0.5669  
#lifehistMRSex        0.16906    0.48500   0.349   0.7292  
#lifehistMRParent    -2.99170    1.74088  -1.718   0.0934 .
#CldhdStablty         0.15508    0.14596   1.062   0.2944  
#Resources_CHILDHOOD  0.01104    0.17889   0.062   0.9511  
#Resources_CURRENT    0.13066    0.12897   1.013   0.3171  
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.375 on 40 degrees of freedom
#(1 observation deleted due to missingness)
#Multiple R-squared:  0.2014,	Adjusted R-squared:  0.08163 
#F-statistic: 1.682 on 6 and 40 DF,  p-value: 0.1507
standardCoefs(LifeHistMRB)
#                              b        beta
#Dem_AGE              0.03304130  0.17248895
#lifehistMRSex        0.16906319  0.05446696
#lifehistMRParent    -2.99169823 -0.51519047
#CldhdStablty         0.15507985  0.18501016
#Resources_CHILDHOOD  0.01104081  0.01135698
#Resources_CURRENT    0.13065693  0.16303386


#Kin Care (Family)
LifeHistKCF = lm(FSM_KinCare_FAMILY~Dem_AGE+lifehistSex+lifehistRelationship+lifehistParent+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeHistdata)
summary(LifeHistKCF)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.8401 -0.5926  0.1802  0.7741  1.9342 

#Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#(Intercept)           4.26689    0.77743   5.488 3.18e-07 ***
#Dem_AGE               0.01683    0.02849   0.591    0.556    
#lifehistSex           0.14179    0.22992   0.617    0.539    
#lifehistRelationship  0.17209    0.22721   0.757    0.451    
#lifehistParent       -0.02225    0.99578  -0.022    0.982    
#CldhdStablty          0.11729    0.08022   1.462    0.147    
#Resources_CHILDHOOD   0.14808    0.09691   1.528    0.130    
#Resources_CURRENT    -0.03478    0.06898  -0.504    0.615    
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.066 on 98 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.08817,	Adjusted R-squared:  0.02304 
#F-statistic: 1.354 on 7 and 98 DF,  p-value: 0.2337
standardCoefs(LifeHistKCF)
#                               b         beta
#Dem_AGE               0.01682987  0.088800628
#lifehistSex           0.14179499  0.065890376
#lifehistRelationship  0.17208845  0.079680547
#lifehistParent       -0.02224887 -0.003438847
#CldhdStablty          0.11728989  0.174352559
#Resources_CHILDHOOD   0.14807610  0.191999593
#Resources_CURRENT    -0.03477556 -0.054897194

##Kin Care Child
#!!!Not enough participants with children (n=3) in order to run analysis!!!
LifeHistKCCdata <- LifeHistdata
LifeHistKCCdata <- LifeHistKCCdata[LifeHistKCCdata$Parent != "No", ]
lifehistKCCRelationship <- ifelse(LifeHistKCCdata$Relationship == "Yes", 1, 0)
lifehistKCCSex <- ifelse(LifeHistKCCdata$Sex == "Female", 1, 0)
#Kin Care (Child)
LifeHistKCC = lm(FSM_KinCare_CHILD~Dem_AGE+lifehistKCCSex+lifehistKCCRelationship+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeHistKCCdata)
summary(LifeHistKCC)
standardCoefs(LifeHistKCC)



install.packages("sjPlot")
library(sjPlot)
library(webshot)
webshot::install_phantomjs()
####Tables 
#Self-Protection, Disease Avoidance, Status
tab_model(LifeHistSP, LifeHistDA, LifeHistS, 
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Self-Protection","Disease Avoidance","Status"),
          pred.labels = c("Age","Sex","Relationship status",
                          "Parent status","Childhood stability",
                          "Childhood resources","Current resources"))

tab_model(LifeHistSP, LifeHistDA, LifeHistS,show.se=T, show.std = T, show.intercept = F,col.order= c("est","se","ci","std.est","p"),string.est = "B", string.std = "β", string.se = "SE",CSS = css_theme("cells"),dv.labels = c("Self-Protection","Disease Avoidance","Status"),
          pred.labels = c("Age","Sex","Relationship status","Parent status","Childhood stability","Childhood resources","Current resources"), file = "LifeHist_SP_DA_S.html")

webshot("LifeHist_SP_DA_S.html", "LifeHist_SP_DA_S.png")

#Affiliation (Exclusion Concern), Affiliation (Independence), Affiliation (Group)
tab_model(LifeHistAE, LifeHistAI, LifeHistAG, 
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Affiliation (Exclusion Concern)","Affiliation (Independence)","Affiliation (Group)"),
          pred.labels = c("Age","Sex","Relationship status",
                          "Parent status","Childhood stability",
                          "Childhood resources","Current resources"), file = "LifeHist_AE_AI_AG.html")
webshot("LifeHist_AE_AI_AG.html", "LifeHist_AE_AI_AG.png")

#"Kin Care (Family)","Mate Seeking"
tab_model(LifeHistKCF, LifeHistMS, 
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Kin Care (Family)","Mate Seeking"),
          pred.labels = c("Age","Sex","Relationship status",
                          "Parent status","Childhood stability",
                          "Childhood resources","Current resources"),file = "LifeHist_KCF_MS.html")
webshot("LifeHist_KCF_MS.html", "LifeHist_KCF_MS.png")


#"Mate Retention (General)","Mate Retention (Breakup Concern)"
tab_model(LifeHistMRG, LifeHistMRB, 
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Mate Retention (General)","Mate Retention (Breakup Concern)"),
          pred.labels = c("Age","Sex","Parent status","Childhood stability",
                          "Childhood resources","Current resources"),file = "LifeHist_MRG_MRB.html")
webshot("LifeHist_MRG_MRB.html", "LifeHist_MRG_MRB.png")

#"Kin Care (Child)"
tab_model(LifeHistKCC, 
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Kin Care (Child)"),
          pred.labels = c("Age","Sex","Relationship status","Childhood stability",
                          "Childhood resources","Current resources"),file = "LifeHist_KCC.html")
webshot("LifeHist_KCC.html", "LifeHist_KCC.png")


####################################Life Events Odds Ratios###################################
######table S1 replication
install.packages("gtsummary")
library(gtsummary)
LifeEventsdata <- alldata
install.packages("aod")
library(aod)
install.packages('epitools')
library(epitools)
install.packages("questionr")
library(questionr)
install.packages("lmtest")
library(lmtest)
library(lsr)


c=which(names(LifeEventsdata)=="Life_Data_1")
summary(LifeEventsdata[,c:(c+80)])
View(summary(LifeEventsdata[,c:(c+80)]))
EnoughData2 <- (LifeEventsdata[,c:(c+80)])
#1 = yes to the life event, 2 = no to the life event
View(sapply(X = EnoughData2,
            FUN = table))

d=which(names(LifeEventsdata)=="Life_Data_1")
summary(LifeEventsdata[,(d+81):(d+160)])
View(summary(LifeEventsdata[,(d+81):(d+160)]))
EnoughData2f <- (LifeEventsdata[,(d+81):(d+160)])
View(sapply(X = EnoughData2f,
            FUN = table))
describe(EnoughData2f)

#############Self protection################

####8 (n=48) been in a physical fight
LifeEventsdata$led8 <- as.factor(LifeEventsdata$Life_Data_8)
levels(LifeEventsdata$led8)[levels(LifeEventsdata$led8)=="2"] <-"0"
SPled8Model <- glm(led8~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled8Model)
exp(coef(SPled8Model))
waldtest(SPled8Model)
odds.ratio(SPled8Model, level=0.95)
#frequency
LifeEventsdata$led8freq <- LifeEventsdata$Life_Data_8_Freq
SPled8freq= lm(led8freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led8freq!=0,]) 
summary(SPled8freq)
standardCoefs(SPled8freq)
confint.lm(SPled8freq)

##Model 2
LifeEventsdata$Parent10 <- ifelse(LifeEventsdata$Parent == "Yes", 1, 0)
LifeEventsdata$Relationship10 <- ifelse(LifeEventsdata$Relationship == "Yes", 1, 0)
LifeEventsdata$Sex10 <- ifelse(LifeEventsdata$Sex == "Female", 1, 0)
LifeEventsdata$SexNA <- is.na(LifeEventsdata$Sex10)
LifeEventsdata <- LifeEventsdata[LifeEventsdata$SexNA != TRUE, ]
LifeEventsdata$Parent10 <- as.factor(LifeEventsdata$Parent10)
LifeEventsdata$Relationship10 <- as.factor(LifeEventsdata$Relationship10)
LifeEventsdata$Sex10 <- as.factor(LifeEventsdata$Sex10)

SPled8Model2 <- glm(led8~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SPled8Model2)
exp(coef(SPled8Model2))
waldtest(SPled8Model2)
odds.ratio(SPled8Model2, level=0.95)
#frequency
SPled8freq2 = lm(led8freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata[LifeEventsdata$led8freq!=0,]) 
summary(SPled8freq2)
standardCoefs(SPled8freq2)
confint.lm(SPled8freq2)

----------------------------
####10  used a home security system
LifeEventsdata$led10 <- as.factor(LifeEventsdata$Life_Data_10)
levels(LifeEventsdata$led10)[levels(LifeEventsdata$led10)=="2"] <-"0"
SPled10Model <- glm(led10~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled10Model)
exp(coef(SPled10Model))
waldtest(SPled10Model)
odds.ratio(SPled10Model, level=0.95)
##frequency
LifeEventsdata$led10freq <- LifeEventsdata$Life_Data_10_Freq
SPled10freq= lm(led10freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led10freq!=0,]) 
summary(SPled10freq)
standardCoefs(SPled10freq)
confint.lm(SPled10freq)

#Model 2
SPled10Model2 <- glm(led10~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SPled10Model2)
exp(coef(SPled10Model2))
waldtest(SPled10Model2)
odds.ratio(SPled10Model2, level=0.95)
#frequency
SPled10freq2 = lm(led10freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata[LifeEventsdata$led10freq!=0,]) 
summary(SPled10freq2)
standardCoefs(SPled10freq2)
confint.lm(SPled10freq2)

--------------------
####12 carried mace/pepper spray to protect yourself
LifeEventsdata$led12 <- as.factor(LifeEventsdata$Life_Data_12)
levels(LifeEventsdata$led12)[levels(LifeEventsdata$led12)=="2"] <-"0"
SPled12Model <- glm(led12~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled12Model)
exp(coef(SPled12Model))
waldtest(SPled12Model)
odds.ratio(SPled12Model, level=0.95)
##frequency
LifeEventsdata$led12freq <- LifeEventsdata$Life_Data_18_WkFreq
SPled12freq= lm(led12freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led12freq!=0,]) 
summary(SPled12freq)
standardCoefs(SPled12freq)
confint.lm(SPled12freq)

#Model 2
SPled12Model2 <- glm(led12~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SPled12Model2)
exp(coef(SPled12Model2))
waldtest(SPled12Model2)
odds.ratio(SPled12Model2, level=0.95)
#frequency
led12freqdata <- LifeEventsdata
led12freqdata$led12freqNA <- is.na(led12freqdata$led12freq)
led12freqdata <- led12freqdata[led12freqdata$led12freqNA != TRUE, ]
SPled12freq2 = lm(led12freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata) 
summary(SPled12freq2)
standardCoefs(SPled12freq2)
confint.lm(SPled12freq2)

---------------------------
####34  screamed at someone
LifeEventsdata$led34 <- as.factor(LifeEventsdata$Life_Data_34)
levels(LifeEventsdata$led34)[levels(LifeEventsdata$led34)=="2"] <-"0"
SPled34Model <- glm(led34~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled34Model)
exp(coef(SPled34Model))
waldtest(SPled34Model)
odds.ratio(SPled34Model, level=0.95)
#frequency
LifeEventsdata$led34freq <- LifeEventsdata$Life_Data_28_Freq
SPled34freq= lm(led34freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led34freq!=0,]) 
summary(SPled34freq)
standardCoefs(SPled34freq)
confint.lm(SPled34freq)

#Model 2
SPled34Model2 <- glm(led34~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD,LifeEventsdata,family="binomial")
summary(SPled34Model2)
exp(coef(SPled34Model2))
waldtest(SPled34Model2)
odds.ratio(SPled34Model2, level=0.95)
#frequency
SPled34freq2 = lm(led34freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata[LifeEventsdata$led34freq!=0,]) 
summary(SPled34freq2)
standardCoefs(SPled34freq2)
confint.lm(SPled34freq2)

----------------------
####35  punched or forcefully shoved someone
LifeEventsdata$led35 <- as.factor(LifeEventsdata$Life_Data_35)
levels(LifeEventsdata$led35)[levels(LifeEventsdata$led35)=="2"] <-"0"
SPled35Model <- glm(led35~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled35Model)
exp(coef(SPled35Model))
waldtest(SPled35Model)
odds.ratio(SPled35Model, level=0.95)
#frequency
LifeEventsdata$led35freq <- LifeEventsdata$Life_Data_29_Freq
SPled35freq= lm(led35freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led35freq!=0,]) 
summary(SPled35freq)
standardCoefs(SPled35freq)
confint.lm(SPled35freq)

#Model 2
SPled35Model2 <- glm(led35~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SPled35Model2)
exp(coef(SPled35Model2))
waldtest(SPled35Model2)
odds.ratio(SPled35Model2, level=0.95)
#frequency
led35freqdata <- LifeEventsdata
led35freqdata$led35freqNA <- is.na(led35freqdata$led35freq)
led35freqdata <- led35freqdata[led35freqdata$led35freqNA != TRUE, ]
SPled35freq2 = lm(led35freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata) 
summary(SPled35freq2)
standardCoefs(SPled35freq2)
confint.lm(SPled35freq2)
View(led35freqdata)

---------------------------
####11 carried a weapon in public 
LifeEventsdata$led11 <- as.factor(LifeEventsdata$Life_Data_11)
levels(LifeEventsdata$led11)[levels(LifeEventsdata$led11)=="2"] <-"0"
SPled11Model <- glm(led11~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled11Model)
exp(coef(SPled11Model))
waldtest(SPled11Model)
odds.ratio(SPled11Model, level=0.95)
#frequency
LifeEventsdata$led11freq <- LifeEventsdata$Life_Data_19_WkFreq
SPled11freq= lm(led11freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led11freq!=0,]) 
summary(SPled11freq)
standardCoefs(SPled11freq)
confint.lm(SPled11freq)

#Model 2
SPled11Model2 <- glm(led35~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SPled11Model2)
exp(coef(SPled11Model2))
waldtest(SPled11Model2)
odds.ratio(SPled11Model2, level=0.95)
#frequency
led11freqdata <- LifeEventsdata
led11freqdata$led11freqNA <- is.na(led11freqdata$led11freq)
View(led11freqdata)
led11freqdata <- led11freqdata[led11freqdata$led11freqNA != TRUE, ]
SPled11freq2 <- lm(led11freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led11freqdata) 
summary(SPled11freq2)
standardCoefs(SPled11freq2)
confint.lm(SPled11freq2)

-----------------
####9 purchased a gun
LifeEventsdata$led9 <- as.factor(LifeEventsdata$Life_Data_9)
levels(LifeEventsdata$led9)[levels(LifeEventsdata$led9)=="2"] <-"0"
SPled9Model <- glm(led9~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled9Model)
exp(coef(SPled9Model))
waldtest(SPled9Model)
odds.ratio(SPled9Model, level=0.95)
#frequency
LifeEventsdata$led9freq <- LifeEventsdata$Life_Data_9_Freq
SPled9freq= lm(led9freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led9freq!=0,]) 
summary(SPled9freq)
standardCoefs(SPled9freq)
confint.lm(SPled9freq)

#Model 2
SPled9Model2 <- glm(led9~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Relationship10,LifeEventsdata,family="binomial")
summary(SPled9Model2)
exp(coef(SPled9Model2))
waldtest(SPled9Model2)
odds.ratio(SPled9Model2, level=0.95)


-------------
####82 kept a gun in your home
LifeEventsdata$led82 <- as.factor(LifeEventsdata$Life_Data_82)
levels(LifeEventsdata$led82)[levels(LifeEventsdata$led82)=="2"] <-"0"
SPled82Model <- glm(led82~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled82Model)
exp(coef(SPled82Model))
waldtest(SPled82Model)
odds.ratio(SPled82Model, level=0.95)
#frequency
LifeEventsdata$led82freq <- LifeEventsdata$Life_Data_8_Yr_Y_N
SPled82freq= lm(led82freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led82freq!=0,]) 
summary(SPled82freq)
standardCoefs(SPled82freq)
confint.lm(SPled82freq)

#Model 2
SPled82Model2 <- glm(led82~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SPled82Model2)
exp(coef(SPled82Model2))
waldtest(SPled82Model2)
odds.ratio(SPled82Model2, level=0.95)
#frequency
led82freqdata <- LifeEventsdata
led82freqdata$led82freqNA <- is.na(led82freqdata$led82freq)
View(led82freqdata)
led82freqdata <- led82freqdata[led82freqdata$led82freqNA != TRUE, ]
SPled82freq2 <- lm(led82freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led82freqdata) 
summary(SPled82freq2)
standardCoefs(SPled82freq2)
confint.lm(SPled82freq2)


-------------
####79 taken a self-defense class
LifeEventsdata$led79 <- as.factor(LifeEventsdata$Life_Data_79)
levels(LifeEventsdata$led79)[levels(LifeEventsdata$led79)=="2"] <-"0"
SPled79Model <- glm(led79~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled79Model)
exp(coef(SPled79Model))
waldtest(SPled79Model)
odds.ratio(SPled79Model, level=0.95)
#frequency
LifeEventsdata$led79freq <- LifeEventsdata$Life_Data_7_Yr_Y_N_
SPled79freq= lm(led79freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led79freq!=0,]) 
summary(SPled79freq)
standardCoefs(SPled79freq)
confint.lm(SPled79freq)

#Model 2
SPled79Model2 <- glm(led79~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SPled79Model2)
exp(coef(SPled79Model2))
waldtest(SPled79Model2)
odds.ratio(SPled79Model2, level=0.95)
#frequency
led79freqdata <- LifeEventsdata
led79freqdata$led79freqNA <- is.na(led79freqdata$led79freq)
View(led79freqdata)
led79freqdata <- led79freqdata[led79freqdata$led79freqNA != TRUE, ]
SPled79freq2 <- lm(led79freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led79freqdata) 
summary(SPled79freq2)
standardCoefs(SPled79freq2)
confint.lm(SPled79freq2)

---------------------------------------
########Disease avoidance##############
##69 smoked cigarettes
LifeEventsdata$led69 <- as.factor(LifeEventsdata$Life_Data_69)
levels(LifeEventsdata$led69)[levels(LifeEventsdata$led69)=="2"] <-"0"
DAled69Model <- glm(led69~FSM_DiseaseAvoid,LifeEventsdata,family="binomial")
summary(DAled69Model)
exp(coef(DAled69Model))
waldtest(DAled69Model)
odds.ratio(DAled69Model, level=0.95)
#frequency
LifeEventsdata$led69freq <- LifeEventsdata$Life_Data_14_WkFreq
DAled69freq <- lm(led69freq~FSM_DiseaseAvoid,LifeEventsdata[LifeEventsdata$led69freq!=0,]) 
summary(DAled69freq)
standardCoefs(DAled69freq)
confint.lm(DAled69freq)

#Model 2
DAled69Model2 <- glm(led69~FSM_DiseaseAvoid+FSM_SelfProtect+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(DAled69Model2)
exp(coef(DAled69Model2))
waldtest(DAled69Model2)
odds.ratio(DAled69Model2, level=0.95)
#frequency
led69freqdata <- LifeEventsdata
led69freqdata$led69freqNA <- is.na(led69freqdata$led69freq)
View(led69freqdata)
led69freqdata <- led69freqdata[led69freqdata$led69freqNA != TRUE, ]
DAled69freq2 <- lm(led69freq~FSM_DiseaseAvoid+FSM_SelfProtect+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led69freqdata) 
summary(DAled69freq2)
standardCoefs(DAled69freq2)
confint.lm(DAled69freq2)

-------------
####4 avoided shaking hands with someone who seemed sick
LifeEventsdata$led4 <- as.factor(LifeEventsdata$Life_Data_4)
levels(LifeEventsdata$led4)[levels(LifeEventsdata$led4)=="2"] <-"0"
DAled4Model <- glm(led4~FSM_DiseaseAvoid,LifeEventsdata,family="binomial")
summary(DAled4Model)
exp(coef(DAled4Model))
waldtest(DAled4Model)
odds.ratio(DAled4Model, level=0.95)
##frequency
LifeEventsdata$led4freq <- LifeEventsdata$Life_Data_4_Freq
DAled4freq= lm(led4freq~FSM_DiseaseAvoid,LifeEventsdata[LifeEventsdata$led4freq!=0,]) 
summary(DAled4freq)
standardCoefs(DAled4freq)
confint.lm(DAled4freq)

#Model 2
DAled4Model2 <- glm(led4~FSM_DiseaseAvoid+FSM_SelfProtect+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(DAled4Model2)
exp(coef(DAled4Model2))
waldtest(DAled4Model2)
odds.ratio(DAled4Model2, level=0.95)
#frequency
led4freqdata <- LifeEventsdata
led4freqdata$led4freqNA <- is.na(led4freqdata$led4freq)
View(led4freqdata)
led4freqdata <- led4freqdata[led4freqdata$led4freqNA != TRUE, ]
DAled4freq2 <- lm(led4freq~FSM_DiseaseAvoid+FSM_SelfProtect+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led4freqdata) 
summary(DAled4freq2)
standardCoefs(DAled4freq2)
confint.lm(DAled4freq2)


-------------
####76 gotten a flu shot
LifeEventsdata$led76 <- as.factor(LifeEventsdata$Life_Data_76)
levels(LifeEventsdata$led76)[levels(LifeEventsdata$led76)=="2"] <-"0"
DAled76Model <- glm(led76~FSM_DiseaseAvoid,LifeEventsdata,family="binomial")
summary(DAled76Model)
exp(coef(DAled76Model))
waldtest(DAled76Model)
odds.ratio(DAled76Model, level=0.95)
#frequency
LifeEventsdata$led76freq <- LifeEventsdata$Life_Data_6_Yr_Y_N
DAled76freq= lm(led76freq~FSM_DiseaseAvoid,LifeEventsdata[LifeEventsdata$led76freq!=0,]) 
summary(DAled76freq)
standardCoefs(DAled76freq)
confint.lm(DAled76freq)

#Model 2
DAled76Model2 <- glm(led76~FSM_DiseaseAvoid+FSM_SelfProtect+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(DAled76Model2)
exp(coef(DAled76Model2))
waldtest(DAled76Model2)
odds.ratio(DAled76Model2, level=0.95)
#frequency
led76freqdata <- LifeEventsdata
led76freqdata$led76freqNA <- is.na(led76freqdata$led76freq)
View(led76freqdata)
led76freqdata <- led76freqdata[led76freqdata$led76freqNA != TRUE, ]
DAled76freq2 <- lm(led76freq~FSM_DiseaseAvoid+FSM_SelfProtect+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led76freqdata) 
summary(DAled76freq2)
standardCoefs(DAled76freq2)
confint.lm(DAled76freq2)

-------------
####39 gone to a gym or exercise class
LifeEventsdata$led39 <- as.factor(LifeEventsdata$Life_Data_39)
levels(LifeEventsdata$led39)[levels(LifeEventsdata$led39)=="2"] <-"0"
DAled39Model <- glm(led39~FSM_DiseaseAvoid,LifeEventsdata,family="binomial")
summary(DAled39Model)
exp(coef(DAled39Model))
waldtest(DAled39Model)
odds.ratio(DAled39Model, level=0.95)
##frequency
LifeEventsdata$led39freq <- LifeEventsdata$Life_Data_4_WkFreq
DAled39freq= lm(led39freq~FSM_DiseaseAvoid,LifeEventsdata[LifeEventsdata$led39freq!=0,]) 
summary(DAled39freq)
standardCoefs(DAled39freq)
confint.lm(DAled39freq)

#Model 2
DAled39Model2 <- glm(led39~FSM_DiseaseAvoid+FSM_SelfProtect+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(DAled39Model2)
exp(coef(DAled39Model2))
waldtest(DAled39Model2)
odds.ratio(DAled39Model2, level=0.95)
#frequency
led39freqdata <- LifeEventsdata
led39freqdata$led39freqNA <- is.na(led39freqdata$led39freq)
View(led39freqdata)
led39freqdata <- led39freqdata[led39freqdata$led39freqNA != TRUE, ]
DAled39freq2 <- lm(led39freq~FSM_DiseaseAvoid+FSM_SelfProtect+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led39freqdata) 
summary(DAled39freq2)
standardCoefs(DAled39freq2)
confint.lm(DAled39freq2)

-------------
####3 visited someone in the hospital
LifeEventsdata$led3 <- as.factor(LifeEventsdata$Life_Data_3)
levels(LifeEventsdata$led3)[levels(LifeEventsdata$led3)=="2"] <-"0"
DAled3Model <- glm(led3~FSM_DiseaseAvoid,LifeEventsdata,family="binomial")
summary(DAled3Model)
exp(coef(DAled3Model))
waldtest(DAled3Model)
odds.ratio(DAled3Model, level=0.95)
##frequency
LifeEventsdata$led3freq <- LifeEventsdata$Life_Data_3_Freq
DAled3freq= lm(led3freq~FSM_DiseaseAvoid,LifeEventsdata[LifeEventsdata$led3freq!=0,]) 
summary(DAled3freq)
standardCoefs(DAled3freq)
confint.lm(DAled3freq)

#Model 2
DAled3Model2 <- glm(led3~FSM_DiseaseAvoid+FSM_SelfProtect+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(DAled3Model2)
exp(coef(DAled3Model2))
waldtest(DAled3Model2)
odds.ratio(DAled3Model2, level=0.95)
#frequency
led3freqdata <- LifeEventsdata
led3freqdata$led39freqNA <- is.na(led3freqdata$led3freq)
View(led3freqdata)
led3freqdata <- led3freqdata[led3freqdata$led39freqNA != TRUE, ]
DAled3freq2 <- lm(led3freq~FSM_DiseaseAvoid+FSM_SelfProtect+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led3freqdata) 
summary(DAled3freq2)
standardCoefs(DAled3freq2)
confint.lm(DAled3freq2)

---------------------------------------
########Affiliation Group#################

####14 played a team sport
LifeEventsdata$led14 <- as.factor(LifeEventsdata$Life_Data_14)
levels(LifeEventsdata$led14)[levels(LifeEventsdata$led14)=="2"] <-"0"
AGled14Model <- glm(led14~FSM_Affiliation_GROUP,LifeEventsdata,family="binomial")
summary(AGled14Model)
exp(coef(AGled14Model))
waldtest(AGled14Model)
odds.ratio(AGled14Model, level=0.95)
#frequency
LifeEventsdata$led14freq <- LifeEventsdata$Life_Data_12_WkFreq
AGled14freq= lm(led14freq~FSM_Affiliation_GROUP,LifeEventsdata[LifeEventsdata$led14freq!=0,]) 
summary(AGled14freq)
standardCoefs(AGled14freq)
confint.lm(AGled14freq)

#Model 2
AGled14Model2 <- glm(led14~FSM_Affiliation_GROUP+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(AGled14Model2)
exp(coef(AGled14Model2))
waldtest(AGled14Model2)
odds.ratio(AGled14Model2, level=0.95)
#frequency
led14freqdata <- LifeEventsdata
led14freqdata$led14freqNA <- is.na(led14freqdata$led14freq)
View(led14freqdata)
led14freqdata <- led14freqdata[led14freqdata$led14freqNA != TRUE, ]
AGled14freq2 <- lm(led14freq~FSM_Affiliation_GROUP+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led14freqdata) 
summary(AGled14freq2)
standardCoefs(AGled14freq2)
confint.lm(AGled14freq2)

-------------
####15 volunteered your time for an organization
LifeEventsdata$led15 <- as.factor(LifeEventsdata$Life_Data_15)
levels(LifeEventsdata$led15)[levels(LifeEventsdata$led15)=="2"] <-"0"
AGled15Model <- glm(led15~FSM_Affiliation_GROUP,LifeEventsdata,family="binomial")
summary(AGled15Model)
exp(coef(AGled15Model))
waldtest(AGled15Model)
odds.ratio(AGled15Model, level=0.95)
##frequency
LifeEventsdata$led15freq <- LifeEventsdata$Life_Data_12_Freq
AGled15freq= lm(led15freq~FSM_Affiliation_GROUP,LifeEventsdata[LifeEventsdata$led15freq!=0,]) 
summary(AGled15freq)
standardCoefs(AGled15freq)
confint.lm(AGled15freq)

#Model 2
AGled15Model2 <- glm(led15~FSM_Affiliation_GROUP+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(AGled15Model2)
exp(coef(AGled15Model2))
waldtest(AGled15Model2)
odds.ratio(AGled15Model2, level=0.95)
#frequency
led15freqdata <- LifeEventsdata
led15freqdata$led15freqNA <- is.na(led15freqdata$led15freq)
View(led15freqdata)
led15freqdata <- led15freqdata[led15freqdata$led15freqNA != TRUE, ]
AGled15freq2 <- lm(led15freq~FSM_Affiliation_GROUP+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led15freqdata) 
summary(AGled15freq2)
standardCoefs(AGled15freq2)
confint.lm(AGled15freq2)

-------------
####54 attended a religious service
LifeEventsdata$led54 <- as.factor(LifeEventsdata$Life_Data_54)
levels(LifeEventsdata$led54)[levels(LifeEventsdata$led54)=="2"] <-"0"
AGled54Model <- glm(led54~FSM_Affiliation_GROUP,LifeEventsdata,family="binomial")
summary(AGled54Model)
exp(coef(AGled54Model))
waldtest(AGled54Model)
odds.ratio(AGled54Model, level=0.95)
#frequency
LifeEventsdata$led54freq <- LifeEventsdata$Life_Data_16_WkFreq
AGled54freq= lm(led54freq~FSM_Affiliation_GROUP,LifeEventsdata[LifeEventsdata$led54freq!=0,]) 
summary(AGled54freq)
standardCoefs(AGled54freq)
confint.lm(AGled54freq)

#Model 2
AGled54Model2 <- glm(led54~FSM_Affiliation_GROUP+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(AGled54Model2)
exp(coef(AGled54Model2))
waldtest(AGled54Model2)
odds.ratio(AGled54Model2, level=0.95)
#frequency
led54freqdata <- LifeEventsdata
led54freqdata$led54freqNA <- is.na(led54freqdata$led54freq)
View(led54freqdata)
led54freqdata <- led54freqdata[led54freqdata$led54freqNA != TRUE, ]
AGled54freq2 <- lm(led54freq~FSM_Affiliation_GROUP+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led54freqdata) 
summary(AGled54freq2)
standardCoefs(AGled54freq2)
confint.lm(AGled54freq2)


-------------
##69 smoked cigarettes
AGled69Model <- glm(led69~FSM_Affiliation_GROUP,LifeEventsdata,family="binomial")
summary(AGled69Model)
exp(coef(AGled69Model))
waldtest(AGled69Model)
odds.ratio(AGled69Model, level=0.95)
#frequency
AGled69freq= lm(led69freq~FSM_Affiliation_GROUP,LifeEventsdata[LifeEventsdata$led69freq!=0,]) 
summary(AGled69freq)
standardCoefs(AGled69freq)
confint.lm(AGled69freq)

#Model 2
AGled69Model2 <- glm(led69~FSM_Affiliation_GROUP+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(AGled69Model2)
exp(coef(AGled69Model2))
waldtest(AGled69Model2)
odds.ratio(AGled69Model2, level=0.95)
#frequency
AGled69freq2 <- lm(led69freq~FSM_Affiliation_GROUP+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led69freqdata) 
summary(AGled69freq2)
standardCoefs(AGled69freq2)
confint.lm(AGled69freq2)

---------------------------------------
########Affiliation Exclusion Concern##################

####75 used twitter 
LifeEventsdata$led75 <- as.factor(LifeEventsdata$Life_Data_75)
levels(LifeEventsdata$led75)[levels(LifeEventsdata$led75)=="2"] <-"0"
AECled75Model <- glm(led75~FSM_Affiliation_EXCLUDE,LifeEventsdata,family="binomial")
summary(AECled75Model)
exp(coef(AECled75Model))
waldtest(AECled75Model)
odds.ratio(AECled75Model, level=0.95)
#frequency
LifeEventsdata$led75freq <- LifeEventsdata$Life_Data_17_WkFreq
AECled75freq= lm(led75freq~FSM_Affiliation_EXCLUDE,LifeEventsdata[LifeEventsdata$led75freq!=0,]) 
summary(AECled75freq)
standardCoefs(AECled75freq)
confint.lm(AECled75freq)

#Model 2
AECled75Model2 <- glm(led75~FSM_Affiliation_EXCLUDE+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(AECled75Model2)
exp(coef(AECled75Model2))
waldtest(AECled75Model2)
odds.ratio(AECled75Model2, level=0.95)
#frequency
led75freqdata <- LifeEventsdata
led75freqdata$led75freqNA <- is.na(led75freqdata$led75freq)
View(led75freqdata)
led75freqdata <- led75freqdata[led75freqdata$led75freqNA != TRUE, ]
AECled75freq2 <- lm(led75freq~FSM_Affiliation_EXCLUDE+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led75freqdata) 
summary(AECled75freq2)
standardCoefs(AECled75freq2)
confint.lm(AECled75freq2)

-------------
##17  used a social networking website (like Facebook)
LifeEventsdata$led17 <- as.factor(LifeEventsdata$Life_Data_17)
levels(LifeEventsdata$led17)[levels(LifeEventsdata$led17)=="2"] <-"0"
AECled17Model <- glm(led17~FSM_Affiliation_EXCLUDE,LifeEventsdata,family="binomial")
summary(AECled17Model)
exp(coef(AECled17Model))
waldtest(AECled17Model)
odds.ratio(AECled17Model, level=0.95)
#frequency
LifeEventsdata$led17freq <- LifeEventsdata$Life_Data_1_WkFreq_
AECled17freq= lm(led17freq~FSM_Affiliation_EXCLUDE,LifeEventsdata[LifeEventsdata$led17freq!=0,]) 
summary(AECled17freq)
standardCoefs(AECled17freq)
confint.lm(AECled17freq)

#Model 2
AECled17Model2 <- glm(led17~FSM_Affiliation_EXCLUDE+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10,LifeEventsdata,family="binomial")
summary(AECled17Model2)
exp(coef(AECled17Model2))
waldtest(AECled17Model2)
odds.ratio(AECled17Model2, level=0.95)
#frequency
led17freqdata <- LifeEventsdata
led17freqdata$led17freqNA <- is.na(led17freqdata$led17freq)
View(led17freqdata)
led17freqdata <- led17freqdata[led17freqdata$led17freqNA != TRUE, ]
AECled17freq2 <- lm(led17freq~FSM_Affiliation_EXCLUDE+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led17freqdata) 
summary(AECled17freq2)
standardCoefs(AECled17freq2)
confint.lm(AECled17freq2)

---------------------------------------
########Affiliation Independence############
  
---------------------------------------
########Status#############

####70 had a job where other people worked for you
LifeEventsdata$led70 <- as.factor(LifeEventsdata$Life_Data_70)
levels(LifeEventsdata$led70)[levels(LifeEventsdata$led70)=="2"] <-"0"
SSled70Model <- glm(led70~FSM_Status,LifeEventsdata,family="binomial")
summary(SSled70Model)
exp(coef(SSled70Model))
waldtest(SSled70Model)
odds.ratio(SSled70Model, level=0.95)
#frequency
LifeEventsdata$led70freq <- LifeEventsdata$Life_Data_5_Yr_Y_N
SSled70freq= lm(led70freq~FSM_Status,LifeEventsdata[LifeEventsdata$led70freq!=0,]) 
summary(SSled70freq)
standardCoefs(SSled70freq)
confint.lm(SSled70freq)

#Model 2
SSled70Model2 <- glm(led70~FSM_Status+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SSled70Model2)
exp(coef(SSled70Model2))
waldtest(SSled70Model2)
odds.ratio(SSled70Model2, level=0.95)
#frequency
led70freqdata <- LifeEventsdata
led70freqdata$led70freqNA <- is.na(led70freqdata$led70freq)
View(led70freqdata)
led70freqdata <- led70freqdata[led70freqdata$led70freqNA != TRUE, ]
SSled70freq2 <- lm(led70freq~FSM_Status+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led70freqdata) 
summary(SSled70freq2)
standardCoefs(SSled70freq2)
confint.lm(SSled70freq2)

-------------
####21 gotten a promotion
LifeEventsdata$led21 <- as.factor(LifeEventsdata$Life_Data_21)
levels(LifeEventsdata$led21)[levels(LifeEventsdata$led21)=="2"] <-"0"
SSled21Model <- glm(led21~FSM_Status,LifeEventsdata,family="binomial")
summary(SSled21Model)
exp(coef(SSled21Model))
waldtest(SSled21Model)
odds.ratio(SSled21Model, level=0.95)
#frequency
LifeEventsdata$led21freq <- LifeEventsdata$Life_Data_17_Freq
SSled21freq= lm(led21freq~FSM_Status,LifeEventsdata[LifeEventsdata$led21freq!=0,]) 
summary(SSled21freq)
standardCoefs(SSled21freq)
confint.lm(SSled21freq)

#Model 2
SSled21Model2 <- glm(led21~FSM_Status+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SSled21Model2)
exp(coef(SSled21Model2))
waldtest(SSled21Model2)
odds.ratio(SSled21Model2, level=0.95)
#frequency
led21freqdata <- LifeEventsdata
led21freqdata$led21freqNA <- is.na(led21freqdata$led21freq)
View(led21freqdata)
led21freqdata <- led21freqdata[led21freqdata$led21freqNA != TRUE, ]
SSled21freq2 <- lm(led21freq~FSM_Status+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led21freqdata) 
summary(SSled21freq2)
standardCoefs(SSled21freq2)
confint.lm(SSled21freq2)
-------------
##46 played music, sang, or performed for others
LifeEventsdata$led46 <- as.factor(LifeEventsdata$Life_Data_46)
levels(LifeEventsdata$led46)[levels(LifeEventsdata$led46)=="2"] <-"0"
SSled46Model <- glm(led46~FSM_Status,LifeEventsdata,family="binomial")
summary(SSled46Model)
exp(coef(SSled46Model))
waldtest(SSled46Model)
odds.ratio(SSled46Model, level=0.95)
#frequency
LifeEventsdata$led46freq <- LifeEventsdata$Life_Data_36_Freq
SSled46freq= lm(led46freq~FSM_Status,LifeEventsdata[LifeEventsdata$led46freq!=0,]) 
summary(SSled46freq)
standardCoefs(SSled46freq)
confint.lm(SSled46freq)

#Model 2
SSled46Model2 <- glm(led46~FSM_Status+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SSled46Model2)
exp(coef(SSled46Model2))
waldtest(SSled46Model2)
odds.ratio(SSled46Model2, level=0.95)
#frequency
led46freqdata <- LifeEventsdata
led46freqdata$led46freqNA <- is.na(led46freqdata$led46freq)
View(led46freqdata)
led46freqdata <- led46freqdata[led46freqdata$led46freqNA != TRUE, ]
SSled46freq2 <- lm(led46freq~FSM_Status+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led46freqdata) 
summary(SSled46freq2)
standardCoefs(SSled46freq2)
confint.lm(SSled46freq2)
-------------
####42  made a piece of art (e.g. painting, sculpture, drawing)
LifeEventsdata$led42 <- as.factor(LifeEventsdata$Life_Data_42)
levels(LifeEventsdata$led42)[levels(LifeEventsdata$led42)=="2"] <-"0"
SSled42Model <- glm(led42~FSM_Status,LifeEventsdata,family="binomial")
summary(SSled42Model)
exp(coef(SSled42Model))
waldtest(SSled42Model)
odds.ratio(SSled42Model, level=0.95)
#frequency
LifeEventsdata$led42freq <- LifeEventsdata$Life_Data_32_Freq
SSled42freq= lm(led42freq~FSM_Status,LifeEventsdata[LifeEventsdata$led42freq!=0,]) 
summary(SSled42freq)
standardCoefs(SSled42freq)
confint.lm(SSled42freq)

#Model 2
SSled42Model2 <- glm(led42~FSM_Status+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SSled42Model2)
exp(coef(SSled42Model2))
waldtest(SSled42Model2)
odds.ratio(SSled42Model2, level=0.95)
#frequency
led42freqdata <- LifeEventsdata
led42freqdata$led42freqNA <- is.na(led42freqdata$led42freq)
View(led42freqdata)
led42freqdata <- led42freqdata[led42freqdata$led42freqNA != TRUE, ]
SSled42freq2 <- lm(led42freq~FSM_Status+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led42freqdata) 
summary(SSled42freq2)
standardCoefs(SSled42freq2)
confint.lm(SSled42freq2)
---------------------------------------
########Mate seeking#############

####36 chosen to end a relationship
LifeEventsdata$led36 <- as.factor(LifeEventsdata$Life_Data_36)
levels(LifeEventsdata$led36)[levels(LifeEventsdata$led36)=="2"] <-"0"
MSled36Model <- glm(led36~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled36Model)
exp(coef(MSled36Model))
waldtest(MSled36Model)
odds.ratio(MSled36Model, level=0.95)
#frequency
LifeEventsdata$led36freq <- LifeEventsdata$Life_Data_30_Freq
MSled36freq= lm(led36freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led36freq!=0,]) 
summary(MSled36freq)
standardCoefs(MSled36freq)
confint.lm(MSled36freq)

#Model 2
MSled36Model2 <- glm(led36~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled36Model2)
exp(coef(MSled36Model2))
waldtest(MSled36Model2)
odds.ratio(MSled36Model2, level=0.95)
#frequency
led36freqdata <- LifeEventsdata
led36freqdata$led36freqNA <- is.na(led36freqdata$led36freq)
View(led36freqdata)
led36freqdata <- led36freqdata[led36freqdata$led36freqNA != TRUE, ]
MSled36freq2 <- lm(led36freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led36freqdata) 
summary(MSled36freq2)
standardCoefs(MSled36freq2)
confint.lm(MSled36freq2)
-------------
##37 had someone break up with you/end your relationship
LifeEventsdata$led37 <- as.factor(LifeEventsdata$Life_Data_37)
levels(LifeEventsdata$led37)[levels(LifeEventsdata$led37)=="2"] <-"0"
MSled37Model <- glm(led37~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled37Model)
exp(coef(MSled37Model))
waldtest(MSled37Model)
odds.ratio(MSled37Model, level=0.95)
#frequency
LifeEventsdata$led37freq <- LifeEventsdata$Life_Data_31_Freq
MSled37freq= lm(led37freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led37freq!=0,]) 
summary(MSled37freq)
standardCoefs(MSled37freq)
confint.lm(MSled37freq)

#Model 2
MSled37Model2 <- glm(led37~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled37Model2)
exp(coef(MSled37Model2))
waldtest(MSled37Model2)
odds.ratio(MSled37Model2, level=0.95)
#frequency
led37freqdata <- LifeEventsdata
led37freqdata$led37freqNA <- is.na(led37freqdata$led37freq)
View(led37freqdata)
led37freqdata <- led37freqdata[led37freqdata$led37freqNA != TRUE, ]
MSled37freq2 <- lm(led37freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led37freqdata) 
summary(MSled37freq2)
standardCoefs(MSled37freq2)
confint.lm(MSled37freq2)
-------------
##43  been asked out on a date
LifeEventsdata$led43 <- as.factor(LifeEventsdata$Life_Data_43)
levels(LifeEventsdata$led43)[levels(LifeEventsdata$led43)=="2"] <-"0"
MSled43Model <- glm(led43~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled43Model)
exp(coef(MSled43Model))
waldtest(MSled43Model)
odds.ratio(MSled43Model, level=0.95)
#frequency
LifeEventsdata$led43freq <- LifeEventsdata$Life_Data_33_Freq
MSled43freq= lm(led43freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led43freq!=0,]) 
summary(MSled43freq)
standardCoefs(MSled43freq)
confint.lm(MSled43freq)

#Model 2
MSled43Model2 <- glm(led43~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled43Model2)
exp(coef(MSled43Model2))
waldtest(MSled43Model2)
odds.ratio(MSled43Model2, level=0.95)
#frequency
led43freqdata <- LifeEventsdata
led43freqdata$led43freqNA <- is.na(led43freqdata$led43freq)
View(led43freqdata)
led43freqdata <- led43freqdata[led43freqdata$led43freqNA != TRUE, ]
MSled43freq2 <- lm(led43freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led43freqdata) 
summary(MSled43freq2)
standardCoefs(MSled43freq2)
confint.lm(MSled43freq2)
-------------
####44 asked someone to go out on a date
LifeEventsdata$led44 <- as.factor(LifeEventsdata$Life_Data_44)
levels(LifeEventsdata$led44)[levels(LifeEventsdata$led44)=="2"] <-"0"
MSled44Model <- glm(led44~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled44Model)
exp(coef(MSled44Model))
waldtest(MSled44Model)
odds.ratio(MSled44Model, level=0.95)
#frequency
LifeEventsdata$led44freq <- LifeEventsdata$Life_Data_34_Freq
MSled44freq= lm(led44freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led44freq!=0,]) 
summary(MSled44freq)
standardCoefs(MSled44freq)
confint.lm(MSled44freq)

#Model 2
MSled44Model2 <- glm(led44~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled44Model2)
exp(coef(MSled44Model2))
waldtest(MSled44Model2)
odds.ratio(MSled44Model2, level=0.95)
#frequency
led44freqdata <- LifeEventsdata
led44freqdata$led44freqNA <- is.na(led44freqdata$led44freq)
View(led44freqdata)
led44freqdata <- led44freqdata[led44freqdata$led44freqNA != TRUE, ]
MSled44freq2 <- lm(led44freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led44freqdata) 
summary(MSled44freq2)
standardCoefs(MSled44freq2)
confint.lm(MSled44freq2)
-------------
####47  gone out dancing
LifeEventsdata$led47 <- as.factor(LifeEventsdata$Life_Data_47)
levels(LifeEventsdata$led47)[levels(LifeEventsdata$led47)=="2"] <-"0"
MSled47Model <- glm(led47~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled47Model)
exp(coef(MSled47Model))
waldtest(MSled47Model)
odds.ratio(MSled47Model, level=0.95)
#frequency
LifeEventsdata$led47freq <- LifeEventsdata$Life_Data_37_Freq
MSled47freq= lm(led47freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led47freq!=0,]) 
summary(MSled47freq)
standardCoefs(MSled47freq)
confint.lm(MSled47freq)

#Model 2
MSled47Model2 <- glm(led47~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled47Model2)
exp(coef(MSled47Model2))
waldtest(MSled47Model2)
odds.ratio(MSled47Model2, level=0.95)
#frequency
led47freqdata <- LifeEventsdata
led47freqdata$led47freqNA <- is.na(led47freqdata$led47freq)
View(led47freqdata)
led47freqdata <- led47freqdata[led47freqdata$led47freqNA != TRUE, ]
MSled47freq2 <- lm(led47freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led47freqdata) 
summary(MSled47freq2)
standardCoefs(MSled47freq2)
confint.lm(MSled47freq2)
-------------
####45 gone to a music concert
LifeEventsdata$led45 <- as.factor(LifeEventsdata$Life_Data_45)
levels(LifeEventsdata$led45)[levels(LifeEventsdata$led45)=="2"] <-"0"
MSled45Model <- glm(led45~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled45Model)
exp(coef(MSled45Model))
waldtest(MSled45Model)
odds.ratio(MSled45Model, level=0.95)
#frequency
LifeEventsdata$led45freq <- LifeEventsdata$Life_Data_35_Freq
MSled45freq= lm(led45freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led45freq!=0,]) 
summary(MSled45freq)
standardCoefs(MSled45freq)
confint.lm(MSled45freq)

#Model 2
MSled45Model2 <- glm(led45~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled45Model2)
exp(coef(MSled45Model2))
waldtest(MSled45Model2)
odds.ratio(MSled45Model2, level=0.95)
#frequency
led45freqdata <- LifeEventsdata
led45freqdata$led45freqNA <- is.na(led45freqdata$led45freq)
View(led45freqdata)
led45freqdata <- led45freqdata[led45freqdata$led45freqNA != TRUE, ]
MSled45freq2 <- lm(led45freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led45freqdata) 
summary(MSled45freq2)
standardCoefs(MSled45freq2)
confint.lm(MSled45freq2)
-------------
####60 had sex without a condom
LifeEventsdata$led60 <- as.factor(LifeEventsdata$Life_Data_60)
levels(LifeEventsdata$led60)[levels(LifeEventsdata$led60)=="2"] <-"0"
MSled60Model <- glm(led60~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled60Model)
exp(coef(MSled60Model))
waldtest(MSled60Model)
odds.ratio(MSled60Model, level=0.95)
#frequency
LifeEventsdata$led60freq <- LifeEventsdata$Life_Data_6_WkFreq
MSled60freq= lm(led60freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led60freq!=0,]) 
summary(MSled60freq)
standardCoefs(MSled60freq)
confint.lm(MSled60freq)

#Model 2
MSled60Model2 <- glm(led60~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled60Model2)
exp(coef(MSled60Model2))
waldtest(MSled60Model2)
odds.ratio(MSled60Model2, level=0.95)
#frequency
led60freqdata <- LifeEventsdata
led60freqdata$led60freqNA <- is.na(led60freqdata$led60freq)
View(led60freqdata)
led60freqdata <- led60freqdata[led60freqdata$led60freqNA != TRUE, ]
MSled60freq2 <- lm(led60freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led60freqdata) 
summary(MSled60freq2)
standardCoefs(MSled60freq2)
confint.lm(MSled60freq2)
-------------
####61 had sex with a condom
LifeEventsdata$led61 <- as.factor(LifeEventsdata$Life_Data_61)
levels(LifeEventsdata$led61)[levels(LifeEventsdata$led61)=="2"] <-"0"
MSled61Model <- glm(led61~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled61Model)
exp(coef(MSled61Model))
waldtest(MSled61Model)
odds.ratio(MSled61Model, level=0.95)
#frequency
LifeEventsdata$led61freq <- LifeEventsdata$Life_Data_7_WkFreq
MSled61freq= lm(led61freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led61freq!=0,]) 
summary(MSled61freq)
standardCoefs(MSled61freq)
confint.lm(MSled61freq)

#Model 2
MSled61Model2 <- glm(led61~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled61Model2)
exp(coef(MSled61Model2))
waldtest(MSled61Model2)
odds.ratio(MSled61Model2, level=0.95)
#frequency
led61freqdata <- LifeEventsdata
led61freqdata$led61freqNA <- is.na(led61freqdata$led61freq)
View(led61freqdata)
led61freqdata <- led61freqdata[led61freqdata$led61freqNA != TRUE, ]
MSled61freq2 <- lm(led61freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led61freqdata) 
summary(MSled61freq2)
standardCoefs(MSled61freq2)
confint.lm(MSled61freq2)
-------------
####70 had a job where other people worked for you
MSled70Model <- glm(led70~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled70Model)
exp(coef(MSled70Model))
waldtest(MSled70Model)
odds.ratio(MSled70Model, level=0.95)
#frequency
MSled70freq= lm(led70freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led70freq!=0,]) 
summary(MSled70freq)
standardCoefs(MSled70freq)
confint.lm(MSled70freq)

#Model 2
MSled70Model2 <- glm(led70~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled70Model2)
exp(coef(MSled70Model2))
waldtest(MSled70Model2)
odds.ratio(MSled70Model2, level=0.95)
#frequency
led70freqdata <- LifeEventsdata
led70freqdata$led70freqNA <- is.na(led70freqdata$led70freq)
View(led70freqdata)
led70freqdata <- led70freqdata[led70freqdata$led70freqNA != TRUE, ]
MSled70freq2 <- lm(led70freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led70freqdata) 
summary(MSled70freq2)
standardCoefs(MSled70freq2)
confint.lm(MSled70freq2)
-------------
####21 gotten a promotion
MSled21Model <- glm(led21~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled21Model)
exp(coef(MSled21Model))
waldtest(MSled21Model)
odds.ratio(MSled21Model, level=0.95)
#frequency
MSled21freq= lm(led21freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led21freq!=0,]) 
summary(MSled21freq)
standardCoefs(MSled21freq)
confint.lm(MSled21freq)

#Model 2
MSled21Model2 <- glm(led21~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled21Model2)
exp(coef(MSled21Model2))
waldtest(MSled21Model2)
odds.ratio(MSled21Model2, level=0.95)
#frequency
MSled21freq2 <- lm(led21freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led21freqdata) 
summary(MSled21freq2)
standardCoefs(MSled21freq2)
confint.lm(MSled21freq2)
-------------
####23 started a new job
LifeEventsdata$led23 <- as.factor(LifeEventsdata$Life_Data_23)
levels(LifeEventsdata$led23)[levels(LifeEventsdata$led23)=="2"] <-"0"
MSled23Model <- glm(led23~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled23Model)
exp(coef(MSled23Model))
waldtest(MSled23Model)
odds.ratio(MSled23Model, level=0.95)
#frequency
LifeEventsdata$led23freq <- LifeEventsdata$Life_Data_19_Freq
MSled23freq= lm(led23freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led23freq!=0,]) 
summary(MSled23freq)
standardCoefs(MSled23freq)
confint.lm(MSled23freq)

#Model 2
MSled23Model2 <- glm(led23~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled23Model2)
exp(coef(MSled23Model2))
waldtest(MSled23Model2)
odds.ratio(MSled23Model2, level=0.95)
#frequency
led23freqdata <- LifeEventsdata
led23freqdata$led23freqNA <- is.na(led23freqdata$led23freq)
View(led23freqdata)
led23freqdata <- led23freqdata[led23freqdata$led23freqNA != TRUE, ]
MSled23freq2 <- lm(led23freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led23freqdata) 
summary(MSled23freq2)
standardCoefs(MSled23freq2)
confint.lm(MSled23freq2)
-------------
##13 had a falling out with a friend
LifeEventsdata$led13 <- as.factor(LifeEventsdata$Life_Data_13)
levels(LifeEventsdata$led13)[levels(LifeEventsdata$led13)=="2"] <-"0"
MSled13Model <- glm(led13~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled13Model)
exp(coef(MSled13Model))
waldtest(MSled13Model)
odds.ratio(MSled13Model, level=0.95)
#frequency
LifeEventsdata$led13freq <- LifeEventsdata$Life_Data_11_Freq
MSled13freq= lm(led13freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led13freq!=0,]) 
summary(MSled13freq)
standardCoefs(MSled13freq)
confint.lm(MSled13freq)

#Model 2
MSled13Model2 <- glm(led13~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled13Model2)
exp(coef(MSled13Model2))
waldtest(MSled13Model2)
odds.ratio(MSled13Model2, level=0.95)
#frequency
led13freqdata <- LifeEventsdata
led13freqdata$led13freqNA <- is.na(led13freqdata$led13freq)
View(led13freqdata)
led13freqdata <- led13freqdata[led13freqdata$led13freqNA != TRUE, ]
MSled13freq2 <- lm(led13freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led13freqdata) 
summary(MSled13freq2)
standardCoefs(MSled13freq2)
confint.lm(MSled13freq2)
-------------
####18 moved within the same town/city
LifeEventsdata$led18 <- as.factor(LifeEventsdata$Life_Data_18)
levels(LifeEventsdata$led18)[levels(LifeEventsdata$led18)=="2"] <-"0"
MSled18Model <- glm(led18~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled18Model)
exp(coef(MSled18Model))
waldtest(MSled18Model)
odds.ratio(MSled18Model, level=0.95)
#frequency
LifeEventsdata$led18freq <- LifeEventsdata$Life_Data_14_Freq
MSled18freq= lm(led18freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led18freq!=0,]) 
summary(MSled18freq)
standardCoefs(MSled18freq)
confint.lm(MSled18freq)

#Model 2
MSled18Model2 <- glm(led18~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled18Model2)
exp(coef(MSled18Model2))
waldtest(MSled18Model2)
odds.ratio(MSled18Model2, level=0.95)
#frequency
led18freqdata <- LifeEventsdata
led18freqdata$led18freqNA <- is.na(led18freqdata$led18freq)
View(led18freqdata)
led18freqdata <- led18freqdata[led18freqdata$led18freqNA != TRUE, ]
MSled18freq2 <- lm(led18freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led18freqdata) 
summary(MSled18freq2)
standardCoefs(MSled18freq2)
confint.lm(MSled18freq2)
-------------
####48 gone a full day without eating
LifeEventsdata$led48 <- as.factor(LifeEventsdata$Life_Data_48)
levels(LifeEventsdata$led48)[levels(LifeEventsdata$led48)=="2"] <-"0"
MSled48Model <- glm(led48~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled48Model)
exp(coef(MSled48Model))
waldtest(MSled48Model)
odds.ratio(MSled48Model, level=0.95)
#frequency
LifeEventsdata$led48freq <- LifeEventsdata$Life_Data_38_Freq
MSled48freq= lm(led48freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led48freq!=0,]) 
summary(MSled48freq)
standardCoefs(MSled48freq)
confint.lm(MSled48freq)

#Model 2
MSled48Model2 <- glm(led48~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled48Model2)
exp(coef(MSled48Model2))
waldtest(MSled48Model2)
odds.ratio(MSled48Model2, level=0.95)
#frequency
led48freqdata <- LifeEventsdata
led48freqdata$led48freqNA <- is.na(led48freqdata$led48freq)
View(led48freqdata)
led48freqdata <- led48freqdata[led48freqdata$led48freqNA != TRUE, ]
MSled48freq2 <- lm(led48freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led48freqdata) 
summary(MSled48freq2)
standardCoefs(MSled48freq2)
confint.lm(MSled48freq2)
-------------
####52 bought a gift for a romantic partner when it wasn’t a holiday or birthday
LifeEventsdata$led52 <- as.factor(LifeEventsdata$Life_Data_52)
levels(LifeEventsdata$led52)[levels(LifeEventsdata$led52)=="2"] <-"0"
MSled52Model <- glm(led52~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled52Model)
exp(coef(MSled52Model))
waldtest(MSled52Model)
odds.ratio(MSled52Model, level=0.95)
#frequency
LifeEventsdata$led52freq <- LifeEventsdata$Life_Data_41_Freq
MSled52freq= lm(led52freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led52freq!=0,]) 
summary(MSled52freq)
standardCoefs(MSled52freq)
confint.lm(MSled52freq)

#Model 2
MSled52Model2 <- glm(led52~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled52Model2)
exp(coef(MSled52Model2))
waldtest(MSled52Model2)
odds.ratio(MSled52Model2, level=0.95)
#frequency
led52freqdata <- LifeEventsdata
led52freqdata$led52freqNA <- is.na(led52freqdata$led52freq)
View(led52freqdata)
led52freqdata <- led52freqdata[led52freqdata$led52freqNA != TRUE, ]
MSled52freq2 <- lm(led52freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led52freqdata) 
summary(MSled52freq2)
standardCoefs(MSled52freq2)
confint.lm(MSled52freq2)
-------------
##69 smoked cigarettes
MSled69Model <- glm(led69~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled69Model)
exp(coef(MSled69Model))
waldtest(MSled69Model)
odds.ratio(MSled69Model, level=0.95)
#frequency
MSled69freq= lm(led69freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led69freq!=0,]) 
summary(MSled69freq)
standardCoefs(MSled69freq)
confint.lm(MSled69freq)

#Model 2
MSled69Model2 <- glm(led69~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled69Model2)
exp(coef(MSled69Model2))
waldtest(MSled69Model2)
odds.ratio(MSled69Model2, level=0.95)
#frequency
MSled69freq2 <- lm(led69freq~FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE,led69freqdata) 
summary(MSled69freq2)
standardCoefs(MSled69freq2)
confint.lm(MSled69freq2)
-------------
##80 broken a bone
LifeEventsdata$led80 <- as.factor(LifeEventsdata$Life_Data_80)
levels(LifeEventsdata$led80)[levels(LifeEventsdata$led80)=="2"] <-"0"
MSled80Model <- glm(led80~FSM_MateSeek,LifeEventsdata,family="binomial")
summary(MSled80Model)
exp(coef(MSled80Model))
waldtest(MSled80Model)
odds.ratio(MSled80Model, level=0.95)
#frequency
LifeEventsdata$led80freq <- LifeEventsdata$Life_Data_51_Freq
MSled80freq= lm(led80freq~FSM_MateSeek,LifeEventsdata[LifeEventsdata$led80freq!=0,]) 
summary(MSled80freq)
standardCoefs(MSled80freq)
confint.lm(MSled80freq)

#Model 2
MSled80Model2 <- glm(led80~FSM_MateSeek+FSM_KinCare_FAMILY+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(MSled80Model2)
exp(coef(MSled80Model2))
waldtest(MSled80Model2)
odds.ratio(MSled80Model2, level=0.95)
#frequency
led80freqdata <- LifeEventsdata
led80freqdata$led80freqNA <- is.na(led80freqdata$led80freq)
View(led80freqdata)
led80freqdata <- led80freqdata[led80freqdata$led80freqNA != TRUE, ]
MSled80freq2 <- lm(led80freq~FSM_MateSeek+FSM_KinCare_FAMILY+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led80freqdata) 
summary(MSled80freq2)
standardCoefs(MSled80freq2)
confint.lm(MSled80freq2)
---------------------------------------
###########################Kin Care (Family)###################

##59 babysat or cared for a younger relative
LifeEventsdata$led59 <- as.factor(LifeEventsdata$Life_Data_59)
levels(LifeEventsdata$led59)[levels(LifeEventsdata$led59)=="2"] <-"0"
KCFled59Model <- glm(led59~FSM_KinCare_FAMILY,LifeEventsdata,family="binomial")
summary(KCFled59Model)
exp(coef(KCFled59Model))
waldtest(KCFled59Model)
odds.ratio(KCFled59Model, level=0.95)
#frequency
LifeEventsdata$led59freq <- LifeEventsdata$Life_Data_8_WkFreq
KCFled59freq= lm(led59freq~FSM_KinCare_FAMILY,LifeEventsdata[LifeEventsdata$led59freq!=0,]) 
summary(KCFled59freq)
standardCoefs(KCFled59freq)
confint.lm(KCFled59freq)

#Model 2
KCFled59Model2 <- glm(led59~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(KCFled59Model2)
exp(coef(KCFled59Model2))
waldtest(KCFled59Model2)
odds.ratio(KCFled59Model2, level=0.95)
#frequency
led59freqdata <- LifeEventsdata
led59freqdata$led59freqNA <- is.na(led59freqdata$led59freq)
View(led59freqdata)
led59freqdata <- led59freqdata[led59freqdata$led59freqNA != TRUE, ]
KCFled59freq2 <- lm(led59freq~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led59freqdata) 
summary(KCFled59freq2)
standardCoefs(KCFled59freq2)
confint.lm(KCFled59freq2)
-------------
##57 had a family member have a child
LifeEventsdata$led57 <- as.factor(LifeEventsdata$Life_Data_57)
levels(LifeEventsdata$led57)[levels(LifeEventsdata$led57)=="2"] <-"0"
KCFled57Model <- glm(led57~FSM_KinCare_FAMILY,LifeEventsdata,family="binomial")
summary(KCFled57Model)
exp(coef(KCFled57Model))
waldtest(KCFled57Model)
odds.ratio(KCFled57Model, level=0.95)
#frequency
LifeEventsdata$led57freq <- LifeEventsdata$Life_Data_44_Freq
KCFled57freq= lm(led57freq~FSM_KinCare_FAMILY,LifeEventsdata[LifeEventsdata$led57freq!=0,]) 
summary(KCFled57freq)
standardCoefs(KCFled57freq)
confint.lm(KCFled57freq)

#Model 2
KCFled57Model2 <- glm(led57~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(KCFled57Model2)
exp(coef(KCFled57Model2))
waldtest(KCFled57Model2)
odds.ratio(KCFled57Model2, level=0.95)
#frequency
led57freqdata <- LifeEventsdata
led57freqdata$led57freqNA <- is.na(led57freqdata$led57freq)
View(led57freqdata)
led57freqdata <- led57freqdata[led57freqdata$led57freqNA != TRUE, ]
KCFled57freq2 <- lm(led57freq~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led57freqdata) 
summary(KCFled57freq2)
standardCoefs(KCFled57freq2)
confint.lm(KCFled57freq2)
-------------
##55 had a family member die
LifeEventsdata$led55 <- as.factor(LifeEventsdata$Life_Data_55)
levels(LifeEventsdata$led55)[levels(LifeEventsdata$led55)=="2"] <-"0"
KCFled55Model <- glm(led55~FSM_KinCare_FAMILY,LifeEventsdata,family="binomial")
summary(KCFled55Model)
exp(coef(KCFled55Model))
waldtest(KCFled55Model)
odds.ratio(KCFled55Model, level=0.95)
#frequency
LifeEventsdata$led55freq <- LifeEventsdata$Life_Data_42_Freq
KCFled55freq= lm(led55freq~FSM_KinCare_FAMILY,LifeEventsdata[LifeEventsdata$led55freq!=0,]) 
summary(KCFled55freq)
standardCoefs(KCFled55freq)
confint.lm(KCFled55freq)

#Model 2
KCFled55Model2 <- glm(led55~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(KCFled55Model2)
exp(coef(KCFled55Model2))
waldtest(KCFled55Model2)
odds.ratio(KCFled55Model2, level=0.95)
#frequency
led55freqdata <- LifeEventsdata
led55freqdata$led55freqNA <- is.na(led55freqdata$led55freq)
View(led55freqdata)
led55freqdata <- led55freqdata[led55freqdata$led55freqNA != TRUE, ]
KCFled55freq2 <- lm(led55freq~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led55freqdata) 
summary(KCFled55freq2)
standardCoefs(KCFled55freq2)
confint.lm(KCFled55freq2)
-------------
##81 been skydiving
LifeEventsdata$led81 <- as.factor(LifeEventsdata$Life_Data_81)
levels(LifeEventsdata$led81)[levels(LifeEventsdata$led81)=="2"] <-"0"
KCFled81Model <- glm(led81~FSM_KinCare_FAMILY,LifeEventsdata,family="binomial")
summary(KCFled81Model)
exp(coef(KCFled81Model))
waldtest(KCFled81Model)
odds.ratio(KCFled81Model, level=0.95)
#frequency
LifeEventsdata$led81freq <- LifeEventsdata$Life_Data_52_Freq
KCFled81freq= lm(led81freq~FSM_KinCare_FAMILY,LifeEventsdata[LifeEventsdata$led81freq!=0,]) 
summary(KCFled81freq)
standardCoefs(KCFled81freq)
confint.lm(KCFled81freq)

#Model 2
KCFled81Model2 <- glm(led81~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(KCFled81Model2)
exp(coef(KCFled81Model2))
waldtest(KCFled81Model2)
odds.ratio(KCFled81Model2, level=0.95)
#frequency
led81freqdata <- LifeEventsdata
led81freqdata$led81freqNA <- is.na(led81freqdata$led81freq)
View(led81freqdata)
led81freqdata <- led81freqdata[led81freqdata$led81freqNA != TRUE, ]
KCFled81freq2 <- lm(led81freq~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led81freqdata) 
summary(KCFled81freq2)
standardCoefs(KCFled81freq2)
confint.lm(KCFled81freq2)
-------------
##73 been arrested
LifeEventsdata$led73 <- as.factor(LifeEventsdata$Life_Data_73)
levels(LifeEventsdata$led73)[levels(LifeEventsdata$led73)=="2"] <-"0"
KCFled73Model <- glm(led73~FSM_KinCare_FAMILY,LifeEventsdata,family="binomial")
summary(KCFled73Model)
exp(coef(KCFled73Model))
waldtest(KCFled73Model)
odds.ratio(KCFled73Model, level=0.95)
#frequency
LifeEventsdata$led73freq <- LifeEventsdata$Life_Data_49_Freq
KCFled73freq= lm(led73freq~FSM_KinCare_FAMILY,LifeEventsdata[LifeEventsdata$led73freq!=0,]) 
summary(KCFled73freq)
standardCoefs(KCFled73freq)
confint.lm(KCFled73freq)

#Model 2
KCFled73Model2 <- glm(led73~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(KCFled73Model2)
exp(coef(KCFled73Model2))
waldtest(KCFled73Model2)
odds.ratio(KCFled73Model2, level=0.95)
#frequency
led73freqdata <- LifeEventsdata
led73freqdata$led73freqNA <- is.na(led73freqdata$led73freq)
View(led73freqdata)
led73freqdata <- led73freqdata[led73freqdata$led73freqNA != TRUE, ]
KCFled73freq2 <- lm(led73freq~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led73freqdata) 
summary(KCFled73freq2)
standardCoefs(KCFled73freq2)
confint.lm(KCFled73freq2)
-------------
  ##18 moved within the same town/city
  LifeEventsdata$led18 <- as.factor(LifeEventsdata$Life_Data_18)
levels(LifeEventsdata$led18)[levels(LifeEventsdata$led18)=="2"] <-"0"
KCFled18Model <- glm(led18~FSM_KinCare_FAMILY,LifeEventsdata,family="binomial")
summary(KCFled18Model)
exp(coef(KCFled18Model))
waldtest(KCFled18Model)
odds.ratio(KCFled18Model, level=0.95)
#frequency
LifeEventsdata$led18freq <- LifeEventsdata$Life_Data_15_Freq
KCFled18freq= lm(led18freq~FSM_KinCare_FAMILY,LifeEventsdata[LifeEventsdata$led19freq!=0,]) 
summary(KCFled18freq)
standardCoefs(KCFled18freq)
confint.lm(KCFled18freq)

#Model 2
KCFled18Model2 <- glm(led18~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(KCFled18Model2)
exp(coef(KCFled18Model2))
waldtest(KCFled18Model2)
odds.ratio(KCFled18Model2, level=0.95)
#frequency
led18freqdata <- LifeEventsdata
led18freqdata$led18freqNA <- is.na(led18freqdata$led18freq)
View(led18freqdata)
led18freqdata <- led18freqdata[led18freqdata$led18freqNA != TRUE, ]
KCFled18freq2 <- lm(led18freq~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led18freqdata) 
summary(KCFled18freq2)
standardCoefs(KCFled18freq2)
confint.lm(KCFled18freq2)
-------------
##19 moved to a different town/city
LifeEventsdata$led19 <- as.factor(LifeEventsdata$Life_Data_19)
levels(LifeEventsdata$led19)[levels(LifeEventsdata$led19)=="2"] <-"0"
KCFled19Model <- glm(led19~FSM_KinCare_FAMILY,LifeEventsdata,family="binomial")
summary(KCFled19Model)
exp(coef(KCFled19Model))
waldtest(KCFled19Model)
odds.ratio(KCFled19Model, level=0.95)
#frequency
LifeEventsdata$led19freq <- LifeEventsdata$Life_Data_15_Freq
KCFled19freq= lm(led19freq~FSM_KinCare_FAMILY,LifeEventsdata[LifeEventsdata$led19freq!=0,]) 
summary(KCFled19freq)
standardCoefs(KCFled19freq)
confint.lm(KCFled19freq)

#Model 2
KCFled19Model2 <- glm(led19~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(KCFled19Model2)
exp(coef(KCFled19Model2))
waldtest(KCFled19Model2)
odds.ratio(KCFled19Model2, level=0.95)
#frequency
led19freqdata <- LifeEventsdata
led19freqdata$led19freqNA <- is.na(led19freqdata$led19freq)
View(led19freqdata)
led19freqdata <- led19freqdata[led19freqdata$led19freqNA != TRUE, ]
KCFled19freq2 <- lm(led19freq~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led19freqdata) 
summary(KCFled19freq2)
standardCoefs(KCFled19freq2)
confint.lm(KCFled19freq2)
-------------
##20 moved to a different country
LifeEventsdata$led20 <- as.factor(LifeEventsdata$Life_Data_20)
levels(LifeEventsdata$led20)[levels(LifeEventsdata$led20)=="2"] <-"0"
KCFled20Model <- glm(led20~FSM_KinCare_FAMILY,LifeEventsdata,family="binomial")
summary(KCFled20Model)
exp(coef(KCFled20Model))
waldtest(KCFled20Model)
odds.ratio(KCFled20Model, level=0.95)
#frequency
LifeEventsdata$led20freq <- LifeEventsdata$Life_Data_16_Freq
KCFled20freq= lm(led20freq~FSM_KinCare_FAMILY,LifeEventsdata[LifeEventsdata$led20freq!=0,]) 
summary(KCFled20freq)
standardCoefs(KCFled20freq)
confint.lm(KCFled20freq)

#Model 2
KCFled20Model2 <- glm(led20~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(KCFled20Model2)
exp(coef(KCFled20Model2))
waldtest(KCFled20Model2)
odds.ratio(KCFled20Model2, level=0.95)
#frequency
led20freqdata <- LifeEventsdata
led20freqdata$led20freqNA <- is.na(led20freqdata$led20freq)
View(led20freqdata)
led20freqdata <- led20freqdata[led20freqdata$led20freqNA != TRUE, ]
KCFled20freq2 <- lm(led20freq~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led20freqdata) 
summary(KCFled20freq2)
standardCoefs(KCFled20freq2)
confint.lm(KCFled20freq2)
-------------
##82 kept a gun in your home
KCFled82Model <- glm(led82~FSM_KinCare_FAMILY,LifeEventsdata,family="binomial")
summary(KCFled82Model)
exp(coef(KCFled82Model))
waldtest(KCFled82Model)
odds.ratio(KCFled82Model, level=0.95)
#frequency
KCFled82freq= lm(led82freq~FSM_KinCare_FAMILY,LifeEventsdata[LifeEventsdata$led82freq!=0,]) 
summary(KCFled82freq)
standardCoefs(KCFled82freq)
confint.lm(KCFled82freq)

#Model 2
KCFled82Model2 <- glm(led82~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(KCFled82Model2)
exp(coef(KCFled82Model2))
waldtest(KCFled82Model2)
odds.ratio(KCFled82Model2, level=0.95)
#frequency
led82freqdata <- LifeEventsdata
led82freqdata$led82freqNA <- is.na(led82freqdata$led82freq)
View(led82freqdata)
led82freqdata <- led82freqdata[led82freqdata$led82freqNA != TRUE, ]
KCFled82freq2 <- lm(led82freq~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led82freqdata) 
summary(KCFled82freq2)
standardCoefs(KCFled82freq2)
confint.lm(KCFled82freq2)
-------------
##66 cooked a meal at home
LifeEventsdata$led66 <- as.factor(LifeEventsdata$Life_Data_66)
levels(LifeEventsdata$led66)[levels(LifeEventsdata$led66)=="2"] <-"0"
KCFled66Model <- glm(led66~FSM_KinCare_FAMILY,LifeEventsdata,family="binomial")
summary(KCFled66Model)
exp(coef(KCFled66Model))
waldtest(KCFled66Model)
odds.ratio(KCFled66Model, level=0.95)
#frequency
LifeEventsdata$led66freq <- LifeEventsdata$Life_Data_11_WkFreq
KCFled66freq= lm(led66freq~FSM_KinCare_FAMILY,LifeEventsdata[LifeEventsdata$led66freq!=0,]) 
summary(KCFled66freq)
standardCoefs(KCFled66freq)
confint.lm(KCFled66freq)

#Model 2
KCFled66Model2 <- glm(led66~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(KCFled66Model2)
exp(coef(KCFled66Model2))
waldtest(KCFled66Model2)
odds.ratio(KCFled66Model2, level=0.95)
#frequency
led66freqdata <- LifeEventsdata
led66freqdata$led66freqNA <- is.na(led66freqdata$led66freq)
View(led66freqdata)
led66freqdata <- led66freqdata[led66freqdata$led66freqNA != TRUE, ]
KCFled66freq2 <- lm(led66freq~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led66freqdata) 
summary(KCFled66freq2)
standardCoefs(KCFled66freq2)
confint.lm(KCFled66freq2)
-------------
##44  asked someone to go out on a date
KCFled44Model <- glm(led44~FSM_KinCare_FAMILY,LifeEventsdata,family="binomial")
summary(KCFled44Model)
exp(coef(KCFled44Model))
waldtest(KCFled44Model)
odds.ratio(KCFled44Model, level=0.95)
#frequency
KCFled44freq= lm(led44freq~FSM_KinCare_FAMILY,LifeEventsdata[LifeEventsdata$led44freq!=0,]) 
summary(KCFled44freq)
standardCoefs(KCFled44freq)
confint.lm(KCFled44freq)

#Model 2
KCFled44Model2 <- glm(led44~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(KCFled44Model2)
exp(coef(KCFled44Model2))
waldtest(KCFled44Model2)
odds.ratio(KCFled44Model2, level=0.95)
#frequency
led44freqdata <- LifeEventsdata
led44freqdata$led44freqNA <- is.na(led44freqdata$led44freq)
View(led44freqdata)
led44freqdata <- led44freqdata[led44freqdata$led44freqNA != TRUE, ]
KCFled44freq2 <- lm(led44freq~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led44freqdata) 
summary(KCFled44freq2)
standardCoefs(KCFled44freq2)
confint.lm(KCFled44freq2)
-------------
###36  chosen to end a relationship
KCFled36Model <- glm(led36~FSM_KinCare_FAMILY,LifeEventsdata,family="binomial")
summary(KCFled36Model)
exp(coef(KCFled36Model))
waldtest(KCFled36Model)
odds.ratio(KCFled36Model, level=0.95)
#frequency
KCFled36freq= lm(led36freq~FSM_KinCare_FAMILY,LifeEventsdata[LifeEventsdata$led36freq!=0,]) 
summary(KCFled36freq)
standardCoefs(KCFled36freq)
confint.lm(KCFled36freq)

#Model 2
KCFled36Model2 <- glm(led36~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(KCFled36Model2)
exp(coef(KCFled36Model2))
waldtest(KCFled36Model2)
odds.ratio(KCFled36Model2, level=0.95)
#frequency
led36freqdata <- LifeEventsdata
led36freqdata$led36freqNA <- is.na(led36freqdata$led36freq)
View(led36freqdata)
led36freqdata <- led36freqdata[led36freqdata$led36freqNA != TRUE, ]
KCFled36freq2 <- lm(led36freq~FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led18freqdata) 
summary(KCFled36freq2)
standardCoefs(KCFled36freq2)
confint.lm(KCFled36freq2)
---------------------------------------
#########Mate retention general
  ##NOTE: not enough data for anlaysis
  MateRetdata <- LifeEventsdata
MateRetdata$MR_NA <- is.na(MateRetdata$FSM_MateRet_GEN)
View(MateRetdata)
MateRetdata <- MateRetdata[MateRetdata$MR_NA != TRUE, ]
  
  
e=which(names(MateRetdata)=="Life_Data_1")
summary(MateRetdata[,e:(e+80)])
View(summary(MateRetdata[,e:(c+80)]))
EnoughData3 <- (MateRetdata[,e:(c+80)])
#1 = yes to the life event, 2 = no to the life event
View(sapply(X = EnoughData3,
            FUN = table))

f=which(names(MateRetdata)=="Life_Data_1")
summary(MateRetdata[,(f+81):(f+160)])
View(summary(MateRetdata[,(f+81):(f+160)]))
EnoughData3f <- (MateRetdata[,(f+81):(f+160)])
View(sapply(X = EnoughData3f,
            FUN = table))
describe(EnoughData2f)  
  
  
####71 been unfaithful to a romantic/sexual partner
MateRetdata$led71 <- as.factor(MateRetdata$Life_Data_71)
levels(MateRetdata$led71)[levels(MateRetdata$led71)=="2"] <-"0"
MRGled71Model <- glm(led71~FSM_MateRet_GEN,MateRetdata,family="binomial")
summary(MRGled71Model)
exp(coef(MRGled71Model))
waldtest(MRGled71Model)
odds.ratio(MRGled71Model, level=0.95)
#frequency
MateRetdata$led71freq <- MateRetdata$Life_Data_48_Freq
MRGled71freq= lm(led71freq~FSM_MateRet_GEN,MateRetdata[MateRetdata$led71freq!=0,]) 
summary(MRGled71freq)
standardCoefs(MRGled71freq)
confint.lm(MRGled71freq)

#Model 2
MRGled71Model2 <- glm(led71~FSM_MateRet_GEN+FSM_MateRet_BREAKUP+FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD,MateRetdata,family="binomial")
summary(MRGled71Model2)
exp(coef(MRGled71Model2))
waldtest(MRGled71Model2)
odds.ratio(MRGled71Model2, level=0.95)
#frequency
led71freqdata <- MateRetdata
led71freqdata$led71freqNA <- is.na(led71freqdata$led71freq)
View(led71freqdata)
led71freqdata <- led71freqdata[led71freqdata$led71freqNA != TRUE, ]
MRGled71freq2 <- lm(led71freq~FSM_MateRet_GEN+FSM_MateRet_BREAKUP+FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led71freqdata) 
summary(MRGled71freq2)
standardCoefs(MRGled71freq2)
confint.lm(MRGled71freq2)
-------------
  ##51  bought a holiday/birthday gift for a romantic partner
  MateRetdata$led51 <- as.factor(MateRetdata$Life_Data_51)
levels(MateRetdata$led51)[levels(MateRetdata$led51)=="2"] <-"0"
MRGled51Model <- glm(led51~FSM_MateRet_GEN,MateRetdata,family="binomial")
summary(MRGled51Model)
exp(coef(MRGled51Model))
waldtest(MRGled51Model)
odds.ratio(MRGled51Model, level=0.95)
#frequency
MateRetdata$led51freq <- MateRetdata$Life_Data_40_Freq
MRGled51freq= lm(led51freq~FSM_MateRet_GEN,MateRetdata[MateRetdata$led51freq!=0,]) 
summary(MRGled51freq)
standardCoefs(MRGled51freq)
confint.lm(MRGled51freq)

-------------
  ##52  bought a gift for a romantic partner when it wasn’t a holiday or birthday
  MRGled52Model <- glm(led52~FSM_MateRet_GEN,MateRetdata,family="binomial")
summary(MRGled52Model)
exp(coef(MRGled52Model))
waldtest(MRGled52Model)
odds.ratio(MRGled52Model, level=0.95)
#frequency
MRGled52freq= lm(led52freq~FSM_MateRet_GEN,MateRetdata[MateRetdata$led52freq!=0,]) 
summary(MRGled52freq)
standardCoefs(MRGled52freq)
confint.lm(MRGled52freq)

#Model 2
MRGled52Model2 <- glm(led52~FSM_MateRet_GEN+FSM_MateRet_BREAKUP+FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD,MateRetdata,family="binomial")
summary(MRGled52Model2)
exp(coef(MRGled52Model2))
waldtest(MRGled52Model2)
odds.ratio(MRGled52Model2, level=0.95)
#frequency
led52freqdata <- MateRetdata
led52freqdata$led52freqNA <- is.na(led52freqdata$led52freq)
View(led52freqdata)
led52freqdata <- led52freqdata[led52freqdata$led52freqNA != TRUE, ]
MRGled52freq2 <- lm(led52freq~FSM_MateRet_GEN+FSM_MateRet_BREAKUP+FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led52freqdata) 
summary(MRGled52freq2)
standardCoefs(MRGled52freq2)
confint.lm(MRGled52freq2)
---------------------------------------
  ##########Mate Retebtion breakup concern
  
  ##71 been unfaithful to a romantic/sexual partner
  MRBCled71Model <- glm(led71~FSM_MateRet_BREAKUP,MateRetdata,family="binomial")
summary(MRBCled71Model)
exp(coef(MRBCled71Model))
waldtest(MRBCled71Model)
odds.ratio(MRBCled71Model, level=0.95)
#frequency
MRBCled71freq= lm(led71freq~FSM_MateRet_BREAKUP,MateRetdata[MateRetdata$led71freq!=0,]) 
summary(MRBCled71freq)
standardCoefs(MRBCled71freq)
confint.lm(MRBCled71freq)

#Model 2
MRBCled71Model2 <- glm(led71~FSM_MateRet_BREAKUP+FSM_MateRet_GEN+FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD,MateRetdata,family="binomial")
summary(MRBCled71Model2)
exp(coef(MRBCled71Model2))
waldtest(MRBCled71Model2)
odds.ratio(MRBCled71Model2, level=0.95)
#frequency
led71freqdata <- MateRetdata
led71freqdata$led71freqNA <- is.na(led71freqdata$led71freq)
View(led71freqdata)
led71freqdata <- led71freqdata[led71freqdata$led71freqNA != TRUE, ]
MRGled71freq2 <- lm(led71freq~FSM_MateRet_BREAKUP+FSM_MateRet_GEN+FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led71freqdata) 
summary(MRGled71freq2)
standardCoefs(MRGled71freq2)
confint.lm(MRGled71freq2)
-------------
  ##50 been to a relationship counselor
  MateRetdata$led50 <- as.factor(MateRetdata$Life_Data_50)
levels(MateRetdata$led50)[levels(MateRetdata$led50)=="2"] <-"0"
MRBCled50Model <- glm(led50~FSM_MateRet_BREAKUP,MateRetdata,family="binomial")
summary(MRBCled50Model)
exp(coef(MRBCled50Model))
waldtest(MRBCled50Model)
odds.ratio(MRBCled50Model, level=0.95)
##frequency
MateRetdata$led50freq <- MateRetdata$Life_Data_5_WkFreq
MRBCled50freq= lm(led50freq~FSM_MateRet_BREAKUP,MateRetdata[MateRetdata$led50freq!=0,]) 
summary(MRBCled50freq)
standardCoefs(MRBCled50freq)
confint.lm(MRBCled50freq)

#Model 2
MRBCled50Model2 <- glm(led50~FSM_MateRet_BREAKUP+FSM_MateRet_GEN+FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_INDEP+FSM_Affiliation_EXCLUDE+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD,MateRetdata,family="binomial")
summary(MRBCled50Model2)
exp(coef(MRBCled50Model2))
waldtest(MRBCled50Model2)
odds.ratio(MRBCled50Model2, level=0.95)
#frequency
led50freqdata <- MateRetdata
led50freqdata$led50freqNA <- is.na(led50freqdata$led50freq)
View(led50freqdata)
led50freqdata <- led50freqdata[led50freqdata$led50freqNA != TRUE, ]
MRGled50freq2 <- lm(led50freq~FSM_MateRet_BREAKUP+FSM_MateRet_GEN+FSM_KinCare_FAMILY+FSM_MateSeek+FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led50freqdata) 
summary(MRGled50freq2)
standardCoefs(MRGled50freq2)
confint.lm(MRGled50freq2)
---------------------------------------
#######kin care child

##only 3 participants indicated that they had children, so there isn't enough data for analysis

########################ADDITIONAL ANALYSIS##################

#####Correlation between political affiliation and disease avoidance##### 
#Average of the three political affiliation scale questions
alldata$PoliticalAff <- rowMeans(alldata[ , c(536:538)], na.rm=TRUE)
colnames(alldata[ , c(536:538)])
#Correlation
PolAff_DisAv_cor <- alldata[,c("FSM_DiseaseAvoid","PoliticalAff")] 
PolAffDisAvCor <-round(cor(PolAff_DisAv_cor,use="pairwise.complete.obs"),2)
View(PolAffDisAvCor)
upperPolAff_DisAv_cor<-PolAffDisAvCor
upperPolAff_DisAv_cor[upper.tri(upperPolAff_DisAv_cor)]<-""
upperPolAff_DisAv_cor<-as.data.frame(upperPolAff_DisAv_cor)
View(upperPolAff_DisAv_cor)
upperPolAff_DisAv_corP <- corr.test(PolAff_DisAv_cor)$p    # Apply corr.test function
upperPolAff_DisAv_corP<-upperPolAff_DisAv_corP
upperPolAff_DisAv_corP[upper.tri(upperPolAff_DisAv_corP)]<-""
upperPolAff_DisAv_corP<-as.data.frame(upperPolAff_DisAv_corP)
View(upperPolAff_DisAv_corP)

colnames(alldata)
#personal experience with COVID as predictor of disease avoidance and mate seeking, 


alldata$IndvCovid <- as.factor(alldata$Individual_Covid_)
levels(alldata$IndvCovid)[levels(alldata$IndvCovid)=="1"] <- "Yes"
levels(alldata$IndvCovid)[levels(alldata$IndvCovid)=="2"] <- "Yes"
levels(alldata$IndvCovid)[levels(alldata$IndvCovid)=="3"] <- "No"
levels(alldata$IndvCovid)[levels(alldata$IndvCovid)=="4"] <- "Exclude"

alldata$Vaccine <- as.factor(alldata$Vaccine_)
levels(alldata$Vaccine)[levels(alldata$Vaccine)=="1"] <- "Yes"
levels(alldata$Vaccine)[levels(alldata$Vaccine)=="2"] <- "Yes"
levels(alldata$Vaccine)[levels(alldata$Vaccine)=="3"] <- "No"
levels(alldata$Vaccine)[levels(alldata$Vaccine)=="4"] <- "Exclude"

alldata$IndvHospital <- as.factor(alldata$Indv_Hospital_)
levels(alldata$IndvHospital)[levels(alldata$IndvHospital)=="1"] <- "Yes"
levels(alldata$IndvHospital)[levels(alldata$IndvHospital)=="2"] <- "No"
levels(alldata$IndvHospital)[levels(alldata$IndvHospital)=="4"] <- "Exclude"

alldata$FamCovid <- as.factor(alldata$Fam_COVID)
levels(alldata$FamCovid)[levels(alldata$FamCovid)=="1"] <- "Yes"
levels(alldata$FamCovid)[levels(alldata$FamCovid)=="2"] <- "No"
levels(alldata$FamCovid)[levels(alldata$FamCovid)=="3"] <- "Exclude"

alldata$FamHospital <- as.factor(alldata$Fam_HOSPITAL)
levels(alldata$FamHospital)[levels(alldata$FamHospital)=="1"] <- "Yes"
levels(alldata$FamHospital)[levels(alldata$FamHospital)=="2"] <- "No"
levels(alldata$FamHospital)[levels(alldata$FamHospital)=="3"] <- "Exclude"

alldata$FamDieCovid <- as.factor(alldata$Fam_DIE_COVID)
levels(alldata$FamDieCovid)[levels(alldata$FamDieCovid)=="1"] <- "Yes"
levels(alldata$FamDieCovid)[levels(alldata$FamDieCovid)=="2"] <- "No"
levels(alldata$FamDieCovid)[levels(alldata$FamDieCovid)=="3"] <- "Exclude"


#remove responses that indicated 'prefer not to answer' for any of the questions
CovDisAvMateSeekdata <- alldata
CovDisAvMateSeekdata <- CovDisAvMateSeekdata [CovDisAvMateSeekdata$IndvCovid != "Exclude", ]
CovDisAvMateSeekdata <- CovDisAvMateSeekdata [CovDisAvMateSeekdata$Vaccine != "Exclude", ]
CovDisAvMateSeekdata <- CovDisAvMateSeekdata [CovDisAvMateSeekdata$FamCovid != "Exclude", ]
CovDisAvMateSeekdata <- CovDisAvMateSeekdata [CovDisAvMateSeekdata$FamDieCovid != "Exclude", ]

#Disease Avoidance
lr_CovidDisAv = lm(FSM_DiseaseAvoid~FamDieCovid+FamCovid+Vaccine+IndvCovid,CovDisAvMateSeekdata)
summary(lr_CovidDisAv) 

#Mate Seeking
lr_CovidMateSeek = lm(FSM_MateSeek~FamDieCovid+FamCovid+Vaccine+IndvCovid,CovDisAvMateSeekdata)
summary(lr_CovidMateSeek) 


#"Disease Avoidance","Mate Seeking"
tab_model(lr_CovidDisAv, lr_CovidMateSeek, 
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Disease Avoidance","Mate Seeking"),
          pred.labels = c("Family Death from COVID","Family had COVID","Vaccine","Individual had COVID"),file = "DAMSPolCor.html")
webshot("DAMSPolCor.html", "DAMSPolCor.png")


#####COULDNT COMPLETE BECAUSE ONLY 2 PARTICIPANTS INDICATED A GENDER IDENTITY THAT DIFFERED FROM THEIR SEX
#analysis to see if gender identity predicts the fundamental social motives, we will perform a multiple linear regression. 
#The averages of each fundamental social motive will be the dependent variable, 
#with the sex (male = -1, female = 1) being the independent variable, while controlling for 
#gender identity. With gender identity as the confounding variable, we will be able to see if 
#sex is still a significant predictor of the fundamental social motives. 

#Remove responses that answered ‘prefer not to answer’ or ‘other’to gender identity question
GenIdentitydata <- alldata
GenIdentitydata <- GenIdentitydata [GenIdentitydata$Dem_GenIdenty != 7, ]
GenIdentitydata <-  GenIdentitydata [GenIdentitydata$Dem_GenIdenty != 8, ]

#####All FSM besides Mate Retention (Gen and Breakup) and Kin Care (Child)####

#create dummy variables for gender identity options
female <- ifelse(GenIdentitydata$Dem_GenIdenty ==  2, 1, 0)
male <- ifelse(GenIdentitydata$Dem_GenIdenty == 4, 1, 0)
agender <- ifelse(GenIdentitydata$Dem_GenIdenty == 1, 1, 0)
genderqueerfluid <- ifelse(GenIdentitydata$Dem_GenIdenty == 3, 1, 0)
nonbinary <- ifelse(GenIdentitydata$Dem_GenIdenty == 5, 1, 0)
questioning <- ifelse(GenIdentitydata$Dem_GenIdenty == 6, 1, 0)

#create dummy variables for sex options
female_SEX <- ifelse(GenIdentitydata$Dem_SEX == 1, 1, 0)


#create data frame to use for regression (All FSM besides Mate Retention (Gen and Breakup) and 
#Kin Care (Child),as not everyone answered these questions, so they will be in separate analyses)

GenIdentity_df <- data.frame(MateSeeking = GenIdentitydata$FSM_MateSeek, 
                             SelfProtection = GenIdentitydata$FSM_SelfProtect,
                             DiseaseAvoidance = GenIdentitydata$FSM_DiseaseAvoid,
                             Affiliation_Group = GenIdentitydata$FSM_Affiliation_GROUP,
                             Affiliation_Exclude = GenIdentitydata$FSM_Affiliation_EXCLUDE,
                             Affiliation_Independent = GenIdentitydata$FSM_Affiliation_INDEP, 
                             Status = GenIdentitydata$FSM_Status,
                             KinCare_Family = GenIdentitydata$FSM_KinCare_FAMILY,
                             Female_sex = female_SEX,
                             Female = female,
                             Male = male,
                             Agender = agender,
                             GenderqueerGenderfluid = genderqueerfluid,
                             Nonbinary = nonbinary,
                             Questioning_Unsure = questioning)
GenIdentity_df

devtools::install_github("cardiomoon/ggiraphExtra")
require(moonBook)
require(ggplot2)
require(ggiraph)
require(ggiraphExtra)
require(plyr)


GenIdentitydata$RegSex <- GenIdentitydata$Dem_SEX
GenIdentitydata$RegSex <- as.factor(GenIdentitydata$RegSex)
levels(GenIdentitydata$RegSex)[levels(GenIdentitydata$RegSex)=="2"] <- "Female"
levels(GenIdentitydata$RegSex)[levels(GenIdentitydata$RegSex)=="1"] <- "Male"

GenIdentitydata$RegGenId <- GenIdentitydata$Dem_GenIdenty
GenIdentitydata$RegGenId <- as.factor(GenIdentitydata$RegGenId)
levels(GenIdentitydata$RegGenId)[levels(GenIdentitydata$RegGenId)=="5"] <- "Nonbinary"
levels(GenIdentitydata$RegGenId)[levels(GenIdentitydata$RegGenId)=="1"] <- "Agender"
levels(GenIdentitydata$RegGenId)[levels(GenIdentitydata$RegGenId)=="2"] <- "Female"
levels(GenIdentitydata$RegGenId)[levels(GenIdentitydata$RegGenId)=="3"] <- "Genderqueerfluid"
levels(GenIdentitydata$RegGenId)[levels(GenIdentitydata$RegGenId)=="4"] <- "Male"
levels(GenIdentitydata$RegGenId)[levels(GenIdentitydata$RegGenId)=="6"] <- "Questioning"

View(GenIdentitydata)
if(!require(car)){install.packages("car")}
if(!require(psych)){install.packages("psych")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(emmeans)){install.packages("emmeans")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(phia)){install.packages("phia")}


#linear regression Self Protection

SPGI <- data.frame(GenderIdentity = GenIdentitydata$RegGenId, Sex = GenIdentitydata$RegSex,SelfProtection = GenIdentitydata$FSM_SelfProtect )
SPGI
SPGI$Sex = factor(SPGI$Sex,
                  levels=unique(SPGI$Sex))
library(psych)
headTail(SPGI)
str(SPGI)
summary(SPGI)


SPmodel = lm(SelfProtection ~ Sex + GenderIdentity,
             data = SPGI)
SPmodeljustsex = lm(SelfProtection ~ Sex,
                    data = SPGI)
SPmodeljustgender = lm(SelfProtection ~ GenderIdentity,
                       data = SPGI)
summary(SPmodel)
summary(SPmodeljustsex)
summary(SPmodeljustgender)


#linear regression Disease Avoidance

DAGI <- data.frame(GenderIdentity = GenIdentitydata$RegGenId, Sex = GenIdentitydata$RegSex,DiseaseAvoidance = GenIdentitydata$FSM_DiseaseAvoid )
DAGI
DAGI$Sex = factor(DAGI$Sex,
                  levels=unique(DAGI$Sex))
library(psych)
headTail(DAGI)
str(DAGI)
summary(DAGI)


DAmodel = lm(DiseaseAvoidance ~ Sex + GenderIdentity,
             data = DAGI)
DAmodeljustsex = lm(DiseaseAvoidance ~ Sex,
                    data = DAGI)
DAmodeljustgender = lm(DiseaseAvoidance ~ GenderIdentity,
                       data = DAGI)
summary(DAmodel)
summary(DAmodeljustsex)
summary(DAmodeljustgender)

#linear regression Status

SGI <- data.frame(GenderIdentity = GenIdentitydata$RegGenId, Sex = GenIdentitydata$RegSex,Status = GenIdentitydata$FSM_Status )
SGI
SGI$Sex = factor(SGI$Sex,
                 levels=unique(SGI$Sex))
library(psych)
headTail(SGI)
str(SGI)
summary(SGI)


Smodel = lm(Status ~ Sex + GenderIdentity,
            data = SGI)
Smodeljustsex = lm(Status ~ Sex,
                   data = SGI)
Smodeljustgender = lm(Status ~ GenderIdentity,
                      data = SGI)
summary(Smodel)
summary(Smodeljustsex)
summary(Smodeljustgender)


#linear regression Affiliation (Group)
AGGI <- data.frame(GenderIdentity = GenIdentitydata$RegGenId, Sex = GenIdentitydata$RegSex,AffiliationGroup = GenIdentitydata$FSM_Affiliation_GROUP )
AGGI
AGGI$Sex = factor(AGGI$Sex,
                  levels=unique(AGGI$Sex))
library(psych)
headTail(AGGI)
str(AGGI)
summary(AGGI)


AGmodel = lm(AffiliationGroup ~ Sex + GenderIdentity,
             data = AGGI)
AGmodeljustsex = lm(AffiliationGroup ~ Sex,
                    data = AGGI)
AGmodeljustgender = lm(AffiliationGroup ~ GenderIdentity,
                       data = AGGI)
summary(AGmodel)
summary(AGmodeljustsex)
summary(AGmodeljustgender)

#linear regression Affiliation (Independence)
AIGI <- data.frame(GenderIdentity = GenIdentitydata$RegGenId, Sex = GenIdentitydata$RegSex, AffiliationIndependence = GenIdentitydata$FSM_Affiliation_INDEP )
AIGI
AIGI$Sex = factor(AIGI$Sex,
                  levels=unique(AIGI$Sex))
library(psych)
headTail(AIGI)
str(AIGI)
summary(AIGI)


AImodel = lm(AffiliationIndependence ~ Sex + GenderIdentity,
             data = AIGI)
AImodeljustsex = lm(AffiliationIndependence ~ Sex,
                    data = AIGI)
AImodeljustgender = lm(AffiliationIndependence ~ GenderIdentity,
                       data = AIGI)
summary(AImodel)
summary(AImodeljustsex)
summary(AImodeljustgender)

#linear regression Affiliation (Exclusion Concern)
AEGI <- data.frame(GenderIdentity = GenIdentitydata$RegGenId, Sex = GenIdentitydata$RegSex,AffiliationExclusion = GenIdentitydata$FSM_Affiliation_EXCLUDE )
AEGI
AEGI$Sex = factor(AEGI$Sex,
                  levels=unique(AEGI$Sex))
library(psych)
headTail(AEGI)
str(AEGI)
summary(AEGI)


AEmodel = lm(AffiliationExclusion ~ Sex + GenderIdentity,
             data = AEGI)
AEmodeljustsex = lm(AffiliationExclusion ~ Sex,
                    data = AEGI)
AEmodeljustgender = lm(AffiliationExclusion ~ GenderIdentity,
                       data = AEGI)
summary(AEmodel)
summary(AEmodeljustsex)
summary(AEmodeljustgender)


#linear regression Mate Seeking
MSGI <- data.frame(GenderIdentity = GenIdentitydata$RegGenId, Sex = GenIdentitydata$RegSex,MateSeeking = GenIdentitydata$FSM_MateSeek )
MSGI
MSGI$Sex = factor(MSGI$Sex,
                  levels=unique(MSGI$Sex))
library(psych)
headTail(MSGI)
str(MSGI)
summary(MSGI)


MSmodel = lm(MateSeeking ~ Sex + GenderIdentity,
             data = MSGI)
MSmodeljustsex = lm(MateSeeking ~ Sex,
                    data = MSGI)
MSmodeljustgender = lm(MateSeeking ~ GenderIdentity,
                       data = MSGI)
summary(MSmodel)
summary(MSmodeljustsex)
summary(MSmodeljustgender)


#linear regression Kin Care (Family)
KCFGI <- data.frame(GenderIdentity = GenIdentitydata$RegGenId, Sex = GenIdentitydata$RegSex,KinCareFamily = GenIdentitydata$FSM_KinCare_FAMILY )
KCFGI
KCFGI$Sex = factor(KCFGI$Sex,
                   levels=unique(KCFGI$Sex))
library(psych)
headTail(KCFGI)
str(KCFGI)
summary(KCFGI)


KCFmodel = lm(KinCareFamily ~ Sex + GenderIdentity,
              data = KCFGI)
KCFmodeljustsex = lm(KinCareFamily ~ Sex,
                     data = KCFGI)
KCFmodeljustgender = lm(KinCareFamily ~ GenderIdentity,
                        data = KCFGI)
summary(KCFmodel)
summary(KCFmodeljustsex)
summary(KCFmodeljustgender)
tab_model(KCFmodeljustgender)

#Tables
#"Self protection" "Disease Avoidance" "Status"
tab_model(SPmodel, DAmodel, Smodel, 
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Self Protection", "Disease Avoidance", "Status"),
          pred.labels = c("Sex","Gender Identity"),file = "GenIdSPDAS.html")
webshot("GenIdSPDAS.html", "GenIdSPDAS.png")

#"Affiliation (Exclusion Concern)" "Affiliation (Independence)" "Affiliation (Group)"
tab_model(AEmodel, AImodel, AGmodel, 
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Affiliation (Exclusion Concern)", "Affiliation (Independence)", "Affiliation (Group)"),
          pred.labels = c("Sex","Gender Identity"),file = "GenIdAEAIAG.html")
webshot("GenIdAEAIAG.html", "GenIdAEAIAG.png")
#"Kin Care (Family)" "MSmodel"
tab_model(MSmodel, KCFmodel,
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Mate Seeking", "Kin Care (Family)"),
          pred.labels = c("Sex","Gender Identity"),file = "GenIdMSKCF.html")
webshot("GenIdMSKCF.html", "GenIdMSKCF.png")

###

#Remove responses that are not in a relationship, and therefore did not answer the Mate Retention questions
GenIdentityMateRetdata <- GenIdentitydata
GenIdentityMateRetdata <- GenIdentityMateRetdata[GenIdentityMateRetdata$Relationship != "Exclude", ]
GenIdentityMateRetdata <- GenIdentityMateRetdata[GenIdentityMateRetdata$Relationship != "No", ]

#ANOVA Mate Retention (General)
MRGGI <- data.frame(GenderIdentity = GenIdentityMateRetdata$RegGenId, Sex = GenIdentityMateRetdata$RegSex,MateRetentionGeneral = GenIdentityMateRetdata$FSM_MateRet_GEN)
MRGGI
MRGGI$Sex = factor(MRGGI$Sex,
                   levels=unique(MRGGI$Sex))
library(psych)
headTail(MRGGI)
str(MRGGI)
summary(MRGGI)


MRGmodel = lm(MateRetentionGeneral ~ Sex + GenderIdentity,
              data = MRGGI)
MRGmodeljustsex = lm(MateRetentionGeneral ~ Sex,
                     data = MRGGI)
MRGmodeljustgender = lm(MateRetentionGeneral ~ GenderIdentity,
                        data = MRGGI)
summary(MRGmodel)
summary(MRGmodeljustsex)
summary(MRGmodeljustgender)


#ANOVA Mate Retention (Breakup Concern)
MRBGI <- data.frame(GenderIdentity = GenIdentityMateRetdata$RegGenId, Sex = GenIdentityMateRetdata$RegSex,MateRetentionBreakup = GenIdentityMateRetdata$FSM_MateRet_BREAKUP)
MRBGI
MRBGI$Sex = factor(MRBGI$Sex,
                   levels=unique(MRBGI$Sex))
library(psych)
headTail(MRBGI)
str(MRBGI)
summary(MRBGI)


MRBmodel = lm(MateRetentionBreakup ~ Sex + GenderIdentity,
              data = MRBGI)
MRBmodeljustsex = lm(MateRetentionBreakup ~ Sex,
                     data = MRBGI)
MRBmodeljustgender = lm(MateRetentionBreakup ~ GenderIdentity,
                        data = MRBGI)
summary(MRBmodel)
summary(MRBmodeljustsex)
summary(MRBmodeljustgender)


#"Mate Retention (General)" "Mate Retention (Breakup Concern)"
tab_model(MRGmodel, MRBmodel,
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Mate Retention (General)", "Mate Retention (Breakup Concern)"),
          pred.labels = c("Sex","Gender Identity"))

###

GenIdentityKCCdata <- GenIdentitydata
GenIdentityKCCdata <- GenIdentityKCCdata[GenIdentityKCCdata$Parent != "No",] 

#ANOVA Kin Care (Child)
KCCGI <- data.frame(GenderIdentity = GenIdentityKCCdata$RegGenId, Sex = GenIdentityKCCdata$RegSex,KinCareChild = GenIdentityKCCdata$FSM_KinCare_CHILD )
KCCGI
KCCGI$Sex = factor(KCCGI$Sex,levels=unique(KCCGI$Sex))

library(psych)
headTail(KCCGI)
str(KCCGI)
summary(KCCGI)

KCCmodel = lm(KinCareChild ~ Sex + GenderIdentity,data = KCCGI)
KCCmodeljustsex = lm(KinCareChild ~ Sex,data = KCCGI)
KCCmodeljustgender = lm(KinCareChild ~ GenderIdentity,data = KCCGI)
summary(KCCmodel)
summary(KCCmodeljustsex)
summary(KCCmodeljustgender)

#"Kin Care (Child))"
tab_model(KCCmodel,
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Kin Care (Child)"),
          pred.labels = c("Sex","Gender Identity"))





#linear regression
SIdata<- alldata
SIdata<- SIdata [SIdata$Dem_SexualOrientatio != 9, ]
SIdata<- SIdata [SIdata$Dem_SexualOrientatio != 10, ]
SPSID <- lm(FSM_SelfProtect ~  Dem_SexualOrientatio, data = SIdata )

#welchs t-test
#removing responses that answered 'prefer not to answer' or 'other'
SexualIdentitydata <- alldata
SexualIdentitydata <- SexualIdentitydata [SexualIdentitydata$Dem_SexualOrientatio != 9, ]
SexualIdentitydata <- SexualIdentitydata [SexualIdentitydata$Dem_SexualOrientatio != 10, ]

#turn sexual orientation into a variable with two levels (heterosexual vs. nonheterosexual)
SexualIdentitydata$SexualIdentity <- as.factor(SexualIdentitydata$Dem_SexualOrientatio)

levels(SexualIdentitydata$SexualIdentity)[levels(SexualIdentitydata$SexualIdentity)=="4"] <- "0"
levels(SexualIdentitydata$SexualIdentity)[levels(SexualIdentitydata$SexualIdentity)=="1"] <- "1"
levels(SexualIdentitydata$SexualIdentity)[levels(SexualIdentitydata$SexualIdentity)=="2"] <- "1"
levels(SexualIdentitydata$SexualIdentity)[levels(SexualIdentitydata$SexualIdentity)=="3"] <- "1"
levels(SexualIdentitydata$SexualIdentity)[levels(SexualIdentitydata$SexualIdentity)=="5"] <- "1"
levels(SexualIdentitydata$SexualIdentity)[levels(SexualIdentitydata$SexualIdentity)=="6"] <- "1"
levels(SexualIdentitydata$SexualIdentity)[levels(SexualIdentitydata$SexualIdentity)=="7"] <- "1"
levels(SexualIdentitydata$SexualIdentity)[levels(SexualIdentitydata$SexualIdentity)=="8"] <- "1"

View(SexualIdentitydata)
#t-tests
ttSelfPSexID<-t.test(FSM_SelfProtect ~ SexualIdentity, data=SexualIdentitydata,)
ttSelfPSexID #SIG# t = 2.5341, df = 44.329, p-value = 0.01488
# Calculate standard deviation for each group
sd_by_group <- aggregate(FSM_SelfProtect ~ SexualIdentity, data = SexualIdentitydata, FUN = sd)
print(sd_by_group)

ttDisAvSexID<-t.test(FSM_DiseaseAvoid ~ SexualIdentity, data=SexualIdentitydata,)
ttDisAvSexID #t = -0.25688, df = 27.382, p-value = 0.7992

ttStatusSexID<-t.test(FSM_Status ~ SexualIdentity, data=SexualIdentitydata,)
ttStatusSexID #t = -0.75381, df = 35.966, p-value = 0.4559

ttMateSeSexID<-t.test(FSM_MateSeek ~ SexualIdentity, data=SexualIdentitydata,)
ttMateSeSexID #t = -0.6974, df = 31.748, p-value = 0.4906

ttKinCFamSexID<-t.test(FSM_KinCare_FAMILY ~ SexualIdentity, data=SexualIdentitydata,)
ttKinCFamSexID #t = 0.0157, df = 31.94, p-value = 0.9876

ttAffEXSexID<-t.test(FSM_Affiliation_EXCLUDE ~ SexualIdentity, data=SexualIdentitydata,)
ttAffEXSexID #t = 1.832, df = 38.261, p-value = 0.07474

ttAffGRSexID<-t.test(FSM_Affiliation_GROUP ~ SexualIdentity, data=SexualIdentitydata,)
ttAffGRSexID #t = -1.0518, df = 26.593, p-value = 0.3024

ttAffINSexID<-t.test(FSM_Affiliation_INDEP ~ SexualIdentity, data=SexualIdentitydata,)
ttAffINSexID #t = -0.00806, df = 28.326, p-value = 0.9936

#CANNOT COMPLETE BECAUSE NOT ENOUGH PARTICIPANTS WERE PARENTS
#removing people who are not parents bc they didn't see the kin care child questions
SexIdentityKinCareChilddata <- SexualIdentitydata
SexIdentityKinCareChilddata <- SexIdentityKinCareChilddata [SexIdentityKinCareChilddata$Parent != "No", ]
#t-test
ttKinCCHILDSexID<-t.test(FSM_KinCare_CHILD ~ SexualIdentity, data=SexIdentityKinCareChilddata) 
ttKinCCHILDSexID

#removing people who aren't in a relationship because they didn't see the mate retention questions
SexIdentityMateRetdata <- SexualIdentitydata
SexIdentityMateRetdata <- SexIdentityMateRetdata [SexIdentityMateRetdata$Relationship != "Exclude", ]
SexIdentityMateRetdata <- SexIdentityMateRetdata [SexIdentityMateRetdata$Relationship != "No", ]
#t-test
ttMateRetGENSexID<-t.test(FSM_MateRet_GEN ~ SexualIdentity, data=SexIdentityMateRetdata) 
ttMateRetGENSexID #t = -0.38516, df = 15.972, p-value = 0.7052
#t-test
ttMateRetBUSexID<-t.test(FSM_MateRet_BREAKUP ~ SexualIdentity, data=SexIdentityMateRetdata) 
ttMateRetBUSexID #t = -0.25593, df = 15.583, p-value = 0.8014




######################Exploratory Analysis


#Self Protection
lr_CovidSP = lm(FSM_SelfProtect~FamDieCovid+FamCovid+Vaccine+IndvCovid,CovDisAvMateSeekdata)
summary(lr_CovidSP) 

#Status
lr_CovidSS = lm(FSM_Status~FamDieCovid+FamCovid+Vaccine+IndvCovid,CovDisAvMateSeekdata)
summary(lr_CovidSS) 

#Affiliation (Group)
lr_CovidAG = lm(FSM_Affiliation_GROUP~FamDieCovid+FamCovid+Vaccine+IndvCovid,CovDisAvMateSeekdata)
summary(lr_CovidAG) 

#Affiliation (Exclusion Concern)
lr_CovidAEC = lm(FSM_Affiliation_EXCLUDE~FamDieCovid+FamCovid+Vaccine+IndvCovid,CovDisAvMateSeekdata)
summary(lr_CovidAEC) 

#Affiliation (Independent)
lr_CovidAI = lm(FSM_Affiliation_INDEP~FamDieCovid+FamCovid+Vaccine+IndvCovid,CovDisAvMateSeekdata)
summary(lr_CovidAI) 

library(webshot)
#"Affiliation (Group)","Affiliation (Exclusion Concern)","Affiliation (Independent)"
tab_model(lr_CovidAG, lr_CovidAEC, lr_CovidAI, 
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Affiliation (Group)","Affiliation (Exclusion Concern)","Affiliation (Indpendent)"),
          pred.labels = c("Family Death from COVID","Family had COVID","Vaccine","Individual had COVID"),file = "AffPolCor.html")
webshot("AffPolCor.html", "AffPolCor.png")

#"Self Protection","Status"
tab_model(lr_CovidSP, lr_CovidSS,
          show.se=T, show.std = T, show.intercept = F,
          col.order= c("est","se","ci","std.est","p"),
          string.est = "B", string.std = "β", string.se = "SE",
          CSS = css_theme("cells"),
          dv.labels = c("Self Protection","Status"),
          pred.labels = c("Family Death from COVID","Family had COVID","Vaccine","Individual had COVID"),file = "SPSSPolCor.html")
webshot("SPSSPolCor.html", "SPSSPolCor.png")
