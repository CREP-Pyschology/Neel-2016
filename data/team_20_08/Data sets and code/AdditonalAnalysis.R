library(psych)
library(lsr)
library(car)
FSMData <- read.csv("FSMDataFull.csv")
Qkey <- read.csv("Qkey.csv")

##########Compare data against Neel et al.############
#Compare correlations
nData[,4:13]=lapply(nData[,4:13],as.character)
for(i in 1:11){for(j in 4:13){
  n=nchar(nData[i,j])
  if(substr(nData[i,j],n,n)=="*"){nData[i,j]=substr(nData[i,j],1,n-1)}
}}
nData[,4:13]=lapply(nData[,4:13],as.numeric)
a=which(names(FSMData)=="SelfProtection")
FSMDataCor=round(cor(FSMData[,a:(a+10)],use="pairwise.complete.obs"),2)
diff=nData[,4:13]-FSMDataCor
rownames(diff)=rownames(FSMDataCor)
colnames(diff)=colnames(FSMDataCor)[1:10]
#All numbers are Neel- us , so negative means ours is higher

##################################### demographic analysis ########################################
#####Self-Protection
##demographic analysis
SPdemo=lm(SelfProtection~gender+age+ChildhoodStability+ChildhoodResources+CurrentResources+kids+marital.status2+income+religious2+Liberal,FSMData[complete.cases(FSMData[,c("gender","age","ChildhoodStability","ChildhoodResources","CurrentResources","kids","marital.status2","income","religious2","Liberal")]),])
SPdemo2=step(SPdemo)
standardCoefs(SPdemo2)
summary(SPdemo2)
#results: gender, age, and kids were kept
##Comparison
cor.test(FSMData$SelfProtection, FSMData$BDW) #r=.28 *
cor.test(FSMData$SelfProtection, FSMData$PVDInfect) #r=.05 
cor.test(FSMData$SelfProtection, FSMData$PVDGerm) #r=.33 **
cor.test(FSMData$SelfProtection, FSMData$Neuroticism) #r=.26 **

#####	Disease Avoidance 
##demographic analysis
DAdemo=lm(DiseaseAvoidance~gender+age+ChildhoodStability+ChildhoodResources+CurrentResources+kids+marital.status2+income+religious2+Liberal,FSMData[complete.cases(FSMData[,c("gender","age","ChildhoodStability","ChildhoodResources","CurrentResources","kids","marital.status2","income","religious2","Liberal")]),])
DAdemo2=step(DAdemo)
standardCoefs(DAdemo2)
summary(DAdemo2)
#results: childhood stability and gender were kept

#####	Affiliation (Group)
##demographic analysis
AGdemo=lm(AffiliationGroup~gender+age+ChildhoodStability+ChildhoodResources+CurrentResources+kids+marital.status2+income+religious2+Liberal,FSMData[complete.cases(FSMData[,c("gender","age","ChildhoodStability","ChildhoodResources","CurrentResources","kids","marital.status2","income","religious2","Liberal")]),])
AGdemo2=step(AGdemo)
standardCoefs(AGdemo2)
summary(AGdemo2)
#results: childhood stability, current resources, and kids were kept

#####	Affiliation (Exclusion Concern) 
##demographic analysis
AECdemo=lm(AffiliationExclusion~gender+age+ChildhoodStability+ChildhoodResources+CurrentResources+kids+marital.status2+income+religious2+Liberal,FSMData[complete.cases(FSMData[,c("gender","age","ChildhoodStability","ChildhoodResources","CurrentResources","kids","marital.status2","income","religious2","Liberal")]),])
AECdemo2=step(AECdemo)
standardCoefs(AECdemo2)
summary(AECdemo2)
#results: childhood stability, childhood resources, current resources, kids, and age were kept

#####	Affiliation (Independence) 
##demographic analysis
AIdemo=lm(AffiliationIndependence~gender+age+ChildhoodStability+ChildhoodResources+CurrentResources+kids+marital.status2+income+religious2+Liberal,FSMData[complete.cases(FSMData[,c("gender","age","ChildhoodStability","ChildhoodResources","CurrentResources","kids","marital.status2","income","religious2","Liberal")]),])
AIdemo2=step(AIdemo)
standardCoefs(AIdemo2)
summary(AIdemo2)
#results: childhood stability and kids were kept

#####	Status 
##demographic analysis
Sdemo=lm(Status~gender+age+ChildhoodStability+ChildhoodResources+CurrentResources+kids+marital.status2+income+religious2+Liberal,FSMData[complete.cases(FSMData[,c("gender","age","ChildhoodStability","ChildhoodResources","CurrentResources","kids","marital.status2","income","religious2","Liberal")]),])
Sdemo2=step(Sdemo)
standardCoefs(Sdemo2)
summary(Sdemo2)
#results: current resources, age, gender, and kids were kept

#####	Mate Seeking
##demographic anaylsis
MSdemo=lm(MateSeeking~gender+age+ChildhoodStability+ChildhoodResources+CurrentResources+kids+marital.status2+income+religious2+Liberal,FSMData[complete.cases(FSMData[,c("gender","age","ChildhoodStability","ChildhoodResources","CurrentResources","kids","marital.status2","income","religious2","Liberal")]),])
MSdemo2=step(MSdemo)
standardCoefs(MSdemo2)
summary(MSdemo2)
#results: gender, age, and marital.status2 were kept

#####	Mate Retention (General)
##demographic analysis
MRGdemo=lm(MateRetentionGeneral~gender+age+ChildhoodStability+ChildhoodResources+CurrentResources+kids+marital.status2+income+religious2+Liberal,FSMData[complete.cases(FSMData[,c("gender","age","ChildhoodStability","ChildhoodResources","CurrentResources","kids","marital.status2","income","religious2","Liberal")]),])
MRGdemo2=step(MRGdemo)
standardCoefs(MRGdemo2)
summary(MRGdemo2)
#results: current resources, childhood resources, age, and martial.status2 were kept

#####	Mate Retention (Breakup Concern) 
##demographic analysis
MRBdemo=lm(MateRetentionBreakup~gender+age+ChildhoodStability+ChildhoodResources+CurrentResources+kids+marital.status2+income+religious2+Liberal,FSMData[complete.cases(FSMData[,c("gender","age","ChildhoodStability","ChildhoodResources","CurrentResources","kids","marital.status2","income","religious2","Liberal")]),])
MRBdemo2=step(MRBdemo)
standardCoefs(MRBdemo2)
summary(MRBdemo2)
#results: marital.status2, age, and current resources were kept

#####	Kin Care (Family) 
##demographic analysis
KCFdemo=lm(KinCareFamily~gender+age+ChildhoodStability+ChildhoodResources+CurrentResources+kids+marital.status2+income+religious2+Liberal,FSMData[complete.cases(FSMData[,c("gender","age","ChildhoodStability","ChildhoodResources","CurrentResources","kids","marital.status2","income","religious2","Liberal")]),])
KCFdemo2=step(KCFdemo)
standardCoefs(KCFdemo2)
summary(KCFdemo2)
#results: current resources, childhood stability, and kids were kept

#####	Kin Care (Child) 
##demographic analysis
KCCdemo=lm(KinCareChild~gender+age+ChildhoodStability+ChildhoodResources+CurrentResources+marital.status2+income+religious2+Liberal,FSMData[complete.cases(FSMData[,c("gender","age","ChildhoodStability","ChildhoodResources","CurrentResources","marital.status2","income","religious2","Liberal")]),])
KCCdemo2=step(KCCdemo)
standardCoefs(KCCdemo2)
summary(KCCdemo2)
#results: gender was kept

#####################################Life Events Models#######################################

##emergency room for own illness (#1)
#Disease Avoidance - agreeableness was kept
emergencyIllnessDA=glm(life.survey_1 ~ DiseaseAvoidance+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
emergencyIllnessDA2=step(emergencyIllnessDA)
exp(cbind(OR = coef(emergencyIllnessDA2), confint(emergencyIllnessDA2)))
summary(emergencyIllnessDA2)
#Self Protection - agreeableness was kept
emergencyIllnessSP=glm(life.survey_1 ~ SelfProtection+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
emergencyIllnessSP2=step(emergencyIllnessSP)
exp(cbind(OR = coef(emergencyIllnessSP2), confint(emergencyIllnessSP2)))
summary(emergencyIllnessSP2)

##emergency room for own injury (#5)
#Self Protection - extraversion was kept
emergencyInjurySP=glm(life.survey_5 ~ SelfProtection+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
emergencyInjurySP2=step(emergencyInjurySP)
exp(cbind(OR = coef(emergencyInjurySP2), confint(emergencyInjurySP2)))
summary(emergencyInjurySP2)
#DiseaseAvoidance - agreeableness was kept
emergencyInjuryDA=glm(life.survey_5 ~ DiseaseAvoidance+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
emergencyInjuryDA2=step(emergencyInjuryDA)
exp(cbind(OR = coef(emergencyInjuryDA2), confint(emergencyInjuryDA2)))
summary(emergencyInjuryDA2)

##taken child to emergency room (#6)
#Kin Care Child - neuroticism and kin care child were kept
childEmergency=glm(life.survey_6 ~ KinCareChild+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
childEmergency2=step(childEmergency)
exp(cbind(OR = coef(childEmergency2), confint(childEmergency2)))
summary(childEmergency2)
#Kin Care Family - extraversion and conscientiousness were kept
childEmergencyF=glm(life.survey_6 ~ KinCareFamily+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
childEmergencyF2=step(childEmergencyF)
exp(cbind(OR = coef(childEmergencyF2), confint(childEmergencyF2)))
summary(childEmergencyF2)

##been in car accident (#7)
#Self Protection - nothing was kept
carAccidentSP=glm(life.survey_7 ~ SelfProtection+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
carAccidentSP2=step(carAccidentSP)
exp(cbind(OR = coef(carAccidentSP2), confint(carAccidentSP2)))
summary(carAccidentSP2)
#DiseaseAvoidance - disease avoidance was kept
carAccidentDA=glm(life.survey_7 ~ DiseaseAvoidance+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
carAccidentDA2=step(carAccidentDA)
exp(cbind(OR = coef(carAccidentDA2), confint(carAccidentDA2)))
summary(carAccidentDA2)
#Kin Care Family - nothing was kept
carAccidentKCF=glm(life.survey_7 ~ KinCareFamily+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
carAccidentKCF2=step(carAccidentKCF)


##been in physical fight (#8)
#Self Protection - neuroticism and agreeableness were kept
physicalFight=glm(life.survey_8 ~ SelfProtection+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
physicalFight2=step(physicalFight)
exp(cbind(OR = coef(physicalFight2), confint(physicalFight2)))
summary(physicalFight2)


##used a home security system (#10)
#Self Protection - openess and self protection were kept
homeSecurity=glm(life.survey_10 ~ SelfProtection+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
homeSecurity2=step(homeSecurity)
exp(cbind(OR = coef(homeSecurity2), confint(homeSecurity2)))
summary(homeSecurity2)

##volunteered your time for an organization (#15)
#Affiliation Group - affiliation group was kept
volunteerTime=glm(life.survey_15 ~ AffiliationGroup+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
volunteerTime2=step(volunteerTime)
exp(cbind(OR = coef(volunteerTime2), confint(volunteerTime2)))
summary(volunteerTime2)

##had a friend close to you die (#16)
#Self Protection - agreeableness was kept
friendDieSP=glm(life.survey_15 ~ SelfProtection+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
friendDieSP2=step(friendDieSP)
exp(cbind(OR = coef(friendDieSP2), confint(friendDieSP2)))
summary(friendDieSP2)
#Affilation Group - extraversion was kept
friendDieAG=glm(life.survey_16 ~ AffiliationGroup+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
friendDieAG2=step(friendDieAG)
exp(cbind(OR = coef(friendDieAG2), confint(friendDieAG2)))
summary(friendDieAG2)


##been laid off from work (#22)
#Affiliation Exclusion - affilation exclusion was kept
laidOffAE=glm(life.survey_22 ~ AffiliationExclusion+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
laidOffAE2=step(laidOffAE)
exp(cbind(OR = coef(laidOffAE2), confint(laidOffAE2)))
summary(laidOffAE2)
#Status - agreeableness, neuroticism, and status were kept
laidOffS=glm(life.survey_22 ~ Status+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
laidOffS2=step(laidOffS)
exp(cbind(OR = coef(laidOffS2), confint(laidOffS2)))
summary(laidOffS2)


##collected unemployment (#24)
#Affiliation Exclusion - nothing was kept
unemploymentAE=glm(life.survey_24 ~ AffiliationExclusion+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
unemploymentAE2=step(unemploymentAE)
exp(cbind(OR = coef(unemployment2), confint(emergencyIllnessDA2)))
summary(emergencyIllnessDA2)
#Status - nothing was kept
unemploymentS=glm(life.survey_24 ~ Status+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
unemploymentS2=step(unemploymentS)

##purchased a brand new car (#25)
#Status - status was kept
newCarS=glm(life.survey_25 ~ Status+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
newCarS2=step(newCarS)
exp(cbind(OR = coef(newCarS2), confint(newCarS2)))
summary(newCarS2)
#AffiliationIndependence - affiliation independence was kept
newCarAI=glm(life.survey_25 ~ AffiliationIndependence+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
newCarAI2=step(newCarAI)
exp(cbind(OR = coef(newCarAI2), confint(newCarAI2)))
summary(newCarAI2)


##taken the subway or light rail (#30)
#Disease Avoidance - none were kept
subwayDA=glm(life.survey_30 ~ DiseaseAvoidance+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
subwayDA2=step(subwayDA)
#Status - none were kept
subwayS=glm(life.survey_30 ~ Status+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
subwayS2=step(subwayS)


##taken a train (#32)
#Disease Avoidance - agreeableness was kept
trainDA=glm(life.survey_32 ~ DiseaseAvoidance+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
trainDA2=step(trainDA)
exp(cbind(OR = coef(trainDA2), confint(trainDA2)))
summary(trainDA2)
#Status - none were kept
trainS=glm(life.survey_32 ~ Status+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
trainS2=step(trainS)

##spent more than $100 on dinner with a date (#33)
#Status - neuroticism and status were kept
dinnerDateS=glm(life.survey_33 ~ Status+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
dinnerDateS2=step(dinnerDateS)
exp(cbind(OR = coef(dinnerDateS2), confint(dinnerDateS2)))
summary(dinnerDateS2)
#Mate Seeking - mate seeking and extraversion were kept
dinnerDateMS=glm(life.survey_33 ~ MateSeeking+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
dinnerDateMS2=step(dinnerDateMS)
exp(cbind(OR = coef(dinnerDateMS2), confint(dinnerDateMS2)))
summary(dinnerDateMS2)
#Mate Retention - none were kept
dinnerDateMR=glm(life.survey_33 ~ MateRetentionGeneral+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("MateRetentionGeneral","Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
dinnerDateMR2=step(dinnerDateMR)
exp(cbind(OR = coef(dinnerDateMR2), confint(dinnerDateMR2)))
summary(dinnerDateMR2)


##punched or forcefully shoved someone (#35)
#Self Protection - openness, neuroticism, and agreeableness
punched=glm(life.survey_35 ~ SelfProtection+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
punched2=step(punched)
exp(cbind(OR = coef(punched2), confint(punched2)))
summary(punched2)


##been on a diet (#40)
#Mate Seeking - neuroticism, mate seeking, and openness were kept
dietMS=glm(life.survey_40 ~ MateSeeking+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
dietMS2=step(dietMS)
exp(cbind(OR = coef(dietMS2), confint(dietMS2)))
summary(dietMS2)
#Affiliation Exclusion - openness and affiliation exclusion were kept
dietAE=glm(life.survey_40 ~ AffiliationExclusion+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
dietAE2=step(dietAE)
exp(cbind(OR = coef(dietAE2), confint(dietAE2)))
summary(dietAE2)


##played music, sang, or performed for others (#46)
#Affiliation Group - affiliation group, neuroticism, and openness were kept
performedAE=glm(life.survey_46 ~ AffiliationExclusion+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
performedAE2=step(performedAE)
exp(cbind(OR = coef(performedAE2), confint(performedAE2)))
summary(performedAE2)
#Status - agreeableness and openness were kept
performedS=glm(life.survey_46 ~ Status+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
performedS2=step(performedS)
exp(cbind(OR = coef(performedS2), confint(performedS2)))
summary(performedS2)


##gone out dancing (#47)
#Mate Seeking - extraversion was kept
dancingMS=glm(life.survey_47 ~ MateSeeking+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
dancingMS2=step(dancingMS)
exp(cbind(OR = coef(dancingMS2), confint(dancingMS2)))
summary(dancingMS2)


##gone a full day without eating (#48)
#Mate Seeking - agreeableness and openness were kept
noEatingMS=glm(life.survey_48 ~ MateSeeking+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
noEatingMS2=step(noEatingMS)
exp(cbind(OR = coef(noEatingMS2), confint(noEatingMS2)))
summary(noEatingMS2)
#Affiliation Exclusion - agreeableness and openness were kept
noEatingAE=glm(life.survey_48 ~ AffiliationExclusion+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
noEatingAE2=step(noEatingAE)
exp(cbind(OR = coef(noEatingAE2), confint(noEatingAE2)))
summary(noEatingAE2)


##purchased a home (#53)
#Kin Care Family - conscientiousness and kin care family were kept
purchaseHomeKCF=glm(life.survey_53 ~ KinCareFamily+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
purchaseHomeKCF2=step(purchaseHomeKCF)
exp(cbind(OR = coef(purchaseHomeKCF2), confint(purchaseHomeKCF2)))
summary(purchaseHomeKCF2)
#Kin Care Child - conscientiousness and openness were kept
purchaseHomeKCC=glm(life.survey_53 ~ KinCareChild+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("KinCareChild","Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
purchaseHomeKCC2=step(purchaseHomeKCC)
exp(cbind(OR = coef(purchaseHomeKCC2), confint(purchaseHomeKCC2)))
summary(purchaseHomeKCC2)
#Status - status and conscientiousness were kept
purchaseHomeS=glm(life.survey_53 ~ Status+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
purchaseHomeS2=step(purchaseHomeS)
exp(cbind(OR = coef(purchaseHomeS2), confint(purchaseHomeS2)))
summary(purchaseHomeS2)


##become a parent of a child (#56)
#Self Protection - self protection and extraversion were kept
parentSP=glm(life.survey_56 ~ SelfProtection+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
parentSP2=step(parentSP)
exp(cbind(OR = coef(parentSP2), confint(parentSP2)))
summary(parentSP2)
#KinCareFamily - neuroticism, extraversion, and kin care family were kept
parentKCF=glm(life.survey_56 ~ KinCareFamily+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
parentKCF2=step(parentKCF)
exp(cbind(OR = coef(parentKCF2), confint(parentKCF2)))
summary(parentKCF2)
#KinCareChild - ERROR!! Doesn't make sense to test, all responders to KinCareChild should say yes
#parentKCC=glm(life.survey_56 ~ KinCareChild+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("KinCareChild", "Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
#parentKCC2=step(parentKCC)

##taken a relative to the hospital (#58)
#Disease Avoidance - extraversion was kept
relativeHospital=glm(life.survey_58 ~ DiseaseAvoidance+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
relativeHospital2=step(relativeHospital)
exp(cbind(OR = coef(relativeHospital2), confint(relativeHospital2)))
summary(relativeHospital2)
#Affiliation Group
relativeHospitalAG=glm(life.survey_58 ~ AffiliationGroup+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
relativeHospitalAG2=step(relativeHospitalAG)
exp(cbind(OR = coef(relativeHospitalAG2), confint(relativeHospitalAG2)))
summary(relativeHospitalAG2)
#Kin Care Family - kin care family and extraversion were kept
relativeHospitalKCF=glm(life.survey_58 ~ KinCareFamily+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
relativeHospitalKCF2=step(relativeHospitalKCF)
exp(cbind(OR = coef(relativeHospitalKCF2), confint(relativeHospitalKCF2)))
summary(relativeHospitalKCF2)


##bought or traded stock in a market (#63)
#Status - neuroticism and status were kept
stock=glm(life.survey_63 ~ Status+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
stock2=step(stock)
exp(cbind(OR = coef(stock2), confint(stock2)))
summary(stock2)


##borrowed money from a family member (#64)
#Status - agreeableness, extraversion, and status were kept
borrowFamilyS=glm(life.survey_64 ~ Status+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
borrowFamilyS2=step(borrowFamilyS)
exp(cbind(OR = coef(borrowFamilyS2), confint(borrowFamilyS2)))
summary(borrowFamilyS2)
#Kin Care Family - extraversion and kin care family were kept
borrowFamilyKCF=glm(life.survey_64 ~ KinCareFamily+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
borrowFamilyKCF2=step(borrowFamilyKCF)
exp(cbind(OR = coef(borrowFamilyKCF2), confint(borrowFamilyKCF2)))
summary(borrowFamilyKCF2)


##played a non-team sport (#65)
#Affiliation Group - openness was kept
nonTeamSportAG=glm(life.survey_65 ~ AffiliationGroup+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
nonTeamSportAG2=step(nonTeamSportAG)
exp(cbind(OR = coef(nonTeamSportAG2), confint(nonTeamSportAG2)))
summary(nonTeamSportAG2)
#Affiliation Independence - openness was kept
nonTeamSportAI=glm(life.survey_65 ~ AffiliationIndependence+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
nonTeamSportAI2=step(nonTeamSportAI)
exp(cbind(OR = coef(nonTeamSportAI2), confint(nonTeamSportAI2)))
summary(nonTeamSportAI2)
#Affiliation Exclusion concern - openness was kept
nonTeamSportAE=glm(life.survey_65 ~ AffiliationExclusion+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
nonTeamSportAE2=step(nonTeamSportAE)
exp(cbind(OR = coef(nonTeamSportAE2), confint(nonTeamSportAE2)))
summary(nonTeamSportAE2)


##smoked cigarettes (#69)
#Disease Avoidance - none were kept
cigarettesDA=glm(life.survey_69 ~ DiseaseAvoidance+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
cigarettesDA2=step(cigarettesDA)
exp(cbind(OR = coef(cigarettesDA2), confint(cigarettesDA2)))
summary(cigarettesDA2)
#Affiliation Group - none were kept
cigarettesAG=glm(life.survey_69 ~ AffiliationGroup+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
cigarettesAG2=step(cigarettesAG)
exp(cbind(OR = coef(cigarettesAG2), confint(cigarettesAG2)))
summary(emergencyIllnessDA2)
#Mate Seeking - none were kept
cigarettesMS=glm(life.survey_69 ~ MateSeeking+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
cigarettesMS2=step(cigarettesMS)


##had a job where other people worked for you (#70)
#Status - conscientiousness and status were kept
bossS=glm(life.survey_70 ~ Status+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
bossS2=step(bossS)
exp(cbind(OR = coef(bossS2), confint(bossS2)))
summary(emergencyIllnessDA2)
#Mate Seeking - conscientiousness and extraversion were kept
bossMS=glm(life.survey_70 ~ MateSeeking+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
bossMS2=step(bossMS)
exp(cbind(OR = coef(bossMS2), confint(bossMS2)))
summary(bossMS2)


##gotten a flu shot (#75)
#Disease Avoidance - disease avoidance was kept
fluShot=glm(life.survey_75 ~ DiseaseAvoidance+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
fluShot2=step(fluShot)
exp(cbind(OR = coef(fluShot2), confint(fluShot2)))
summary(fluShot2)


##broken a bone (#79)
#Disease Avoidance - conscientiousness was kept
brokenBoneDA=glm(life.survey_79 ~ DiseaseAvoidance+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
brokenBoneDA2=step(brokenBoneDA)
exp(cbind(OR = coef(brokenBoneDA2), confint(brokenBoneDA2)))
summary(brokenBoneDA2)
#Mate Seeking - conscientiousness was kept
brokenBoneMS=glm(life.survey_79 ~ MateSeeking+Extraversion+Agreeableness+Neuroticism+Conscientiousness+Openness, data = FSMData[complete.cases(FSMData[,c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")]),], family = "binomial")
brokenBoneMS2=step(brokenBoneMS)
exp(cbind(OR = coef(brokenBoneMS2), confint(brokenBoneMS2)))
summary(brokenBoneMS2)
# 
# #correlate motives with eachother
# a=which(names(FSMData)=="SelfProtection")
# round(cor(FSMData[,a:(a+10)],use="pairwise.complete.obs"),2)
# cor.test(FSMData$SelfProtection,FSMData$MateSeeking)
# #make a graph with line of best fit:
# plot(FSMData$SelfProtection,FSMData$MateSeeking)
# model=lm(MateSeeking~SelfProtection+AffiliationGroup,FSMData) #order is important, note that it's opposite of what is in the plot function!
# abline(model[1],model[2])
# 
# #compute motive average (for acquisence bias)
# FSMData$MotiveAve=rowMeans(FSMData[,a:(a+10)],na.rm=T)
# 
# 
# #Correlate motives with Big5
# cor(FSMData[,c("SelfProtection","Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness")],use="pairwise.complete.obs")
# cor.test(FSMData$SelfProtection,FSMData$Neuroticism)
# 
# #Compare with extra scales
# cor.test(FSMData$SelfProtection,FSMData$BDW)
# 
# #Look at some life events 
# Qkey$life.survey_4 #check what the question was
# independentSamplesTTest(DiseaseAvoidance~life.survey_4,FSMData)
# 
# #combine frequency of sex with and without a condom:
# FSMData$SexualActivity=FSMData$past.year.2_6+FSMData$past.year.2_7
# FSMData$PercentCondom=FSMData$past.year.2_7/(FSMData$past.year.2_6+FSMData$past.year.2_7)
# cor.test(FSMData$DiseaseAvoidance,FSMData$PercentCondom)
# 
# #event frequency (numeric)
# cor.test(FSMData$SelfProtection,FSMData$past.year.survey_29)
# 
# #event frequency (ordinal)
# cor.test(FSMData$SelfProtection,FSMData$past.year.2_18,method="spearman")
# 
# #Covid-19?
# cor.test(FSMData$DiseaseAvoidance,FSMData$covid)
# 
# #Multiple Regression
# model=lm(scale(covid)~scale(DiseaseAvoidance)*scale(PVDInfect)+scale(Liberal),FSMData)
# summary(model)
# standardCoefs(model)
# etaSquared(model)
# 
# 
# model=lm(government~covid+Liberal+age,FSMData)
# 
# #logit regression
# mylogit <- glm(life.survey_78 ~ SelfProtection+Neuroticism+Conscientiousness, data = FSMData, family = "binomial")
# 
# #Summed score for angry things
# FSMData$ProtectEvent=FSMData$past.year.survey_8+FSMData$past.year.survey_9+FSMData$past.year.survey_10
# cor.test(FSMData$ProtectEvent,FSMData$SelfProtection)
# 
# #For looking at mate seeking, we need to account for relationship status first!
# 
# #how do religion and politics predict motives?
# 
# #having kids predicts stronger kid care family. Mixed up responses, or third variables?
# model=lm(KinCareFamily~kids+Agreeableness+Extraversion+marital.status,FSMData)
# summary(model)
# standardCoefs(model)
# etaSquared(model)
# 
# #religion
# FSMData[FSMData$religious %in% c("Evangelical Christian","Jewish","Catholic"),"religious"]="Protestant Christian"
# FSMData$religious=factor(FSMData$religious)
# oneway.test(KinCareFamily~religious,FSMData)
# plot(STMO~religious,FSMData)
# oneway.test(STMO~religious,FSMData)




#confirmatory factor analysis?

#relation to life history variables

#demographic variables


#factor analysis of FSM + big5
a=which(names(fsm)=="SelfProtection")
p=fa.parallel(FSMData[,a:(a+15)])
fa5=fa(FSMData[,a:(a+15)],nfactors=5,rotate="promax")
print.psych(fa5,sort=TRUE)
fa11=fa(FSMData[,a:(a+15)],nfactors=11,rotate="promax")
print.psych(fa11,sort=TRUE)

#FA with first half of scales 
p=fa.parallel(FSMData[,a:(a+21)])
fa6=fa(FSMData[,a:(a+21)],nfactors=6,rotate="promax")
print.psych(fa6,sort=TRUE)

#FA with second half of scales 
a=which(names(fsm)=="SelfProtection")
p=fa.parallel(FSMData[,c(a:(a+15),(a+22):(a+25))])
fa4=fa(FSMData[,c(a:(a+15),(a+22):(a+25))],nfactors=4,rotate="promax")
print.psych(fa4,sort=TRUE)

