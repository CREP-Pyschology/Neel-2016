#Combining Neels original data with our two samples:

Neel=read.csv("NeelOriginalData.csv")
Neel=Neel[,c("gender", "kids", "marital.status", "religious", "eAmericanIndian", "eAsian", "eBlack", "eHispanic", "eWhite", "ePacificIslander", "age", "ChildhoodResources", "CurrentResources", "ChildhoodStability", "Liberal", "SelfProtection", "DiseaseAvoidance", "AffiliationGroup", "AffiliationExclusion", "AffiliationIndependence", "Status", "MateSeeking", "MateRetentionGeneral", "MateRetentionBreakup", "KinCareFamily", "KinCareChild")]
Neel$sample="Neel"

Replication=read.csv("FSMDataFull.csv")
Replication=Replication[,c("gender", "kids", "marital.status", "religious", "eAmericanIndian", "eAsian", "eBlack", "eHispanic", "eWhite", "ePacificIslander", "age", "ChildhoodResources", "CurrentResources", "ChildhoodStability", "Liberal", "SelfProtection", "DiseaseAvoidance", "AffiliationGroup", "AffiliationExclusion", "AffiliationIndependence", "Status", "MateSeeking", "MateRetentionGeneral", "MateRetentionBreakup", "KinCareFamily", "KinCareChild")]
Replication$sample="Replication"
allData=rbind(Neel,Replication)

#recode marital status
allData$marital.status2=as.character(allData$marital.status)
allData[allData$marital.status2 %in% c("Dating Several","Dating One","Single"),"marital.status2"]="Single"
allData[allData$marital.status2 %in% c("Committed Relationship","Married"),"marital.status2"]="Committed"
allData[!is.na(allData$marital.status2) & allData$marital.status2=="Other","marital.status2"]=NA
allData$marital.status2=as.factor(allData$marital.status2)

#recode gender
allData[!is.na(allData$gender) & allData$gender=="Other","gender"]=NA

#recode religion
allData$religious2=as.character(allData$religious)
allData[allData$religious2 %in% c("Catholic", "Evangelical Christian","Jewish","Protestant Christian","Muslim","Hindu"),"religious2"]="Theist"
allData[allData$religious2 %in% c("Other"),"religious2"]="Other"
allData$religious2=as.factor(allData$religious2)

write.csv(allData,"CombinedDataCREPE.csv",row.names = F)

#################   Analysis   ##########################
library(lsr)
library(psych)
allData=read.csv("CombinedDataCREPE.csv", stringsAsFactors = T)

allData$sample=relevel(allData$sample,"Neel")
allData$LiberalZ=scale(allData$Liberal)

#compare demographics
describeBy(allData,allData$sample)

#comparing size of motives.  positive beta means higher for our sample
#Our sample has higher self protection and disease avoidance, lower mate seeking, affiliation exclusion, and status

#SelfProtection (no liberal interaction)
SP=step(lm(SelfProtection~sample+gender+age+ChildhoodStability+CurrentResources+kids+eWhite+eBlack+eAmericanIndian+eAsian+eHispanic+religious2+Liberal,allData))#droped marital.status2 + ChildhoodResources
standardCoefs(SP) #sample beta=.12
summary(SP)
confint(SP)

DA2=step(lm(DiseaseAvoidance~sample*LiberalZ+gender+ChildhoodStability+CurrentResources+kids+marital.status2+eWhite+eBlack+eAmericanIndian+eAsian+eHispanic,allData))
cor.test(allData[allData$sample=="Neel","Liberal"],allData[allData$sample=="Neel","DiseaseAvoidance"])
cor.test(allData[allData$sample=="Replication","Liberal"],allData[allData$sample=="Replication","DiseaseAvoidance"])
standardCoefs(DA2)#sample beta=.27, lib*sample beta=.08
summary(DA2)
confint(DA2)

AG=step(lm(AffiliationGroup~sample+ChildhoodStability+CurrentResources+kids+religious2+Liberal,allData))#dropped marital.status2,ethnicity,gender,age,+ChildhoodResources
standardCoefs(AG)#sample beta=.05
summary(AG)
confint(AG)

AE=step(lm(AffiliationExclusion~sample+gender+age+kids+marital.status2,allData)) #dropped liberal, childResources, stability, religious2,CurrentResources,ethnicity
standardCoefs(AE)#sample beta=-.15 (no liberal interaction)
summary(AE)
confint(AE)

AI=step(lm(AffiliationIndependence~sample+gender+CurrentResources+kids+marital.status2,allData)) #Drop Eth,age,Liberal,Stable, ChildResources
standardCoefs(AI)#sample beta=.10, no liberal interaction
summary(AI)
confint(AI)

S=step(lm(Status~sample*LiberalZ+gender+age+ChildhoodResources+CurrentResources+kids+eWhite+eBlack+eAmericanIndian+eAsian+eHispanic+religious2,allData))#drop marital, liberal,stable
standardCoefs(S)#sample beta=-.10, lib*sample beta=-.09
summary(S)
confint(S)
cor.test(allData[allData$sample=="Neel","Liberal"],allData[allData$sample=="Neel","Status"])
cor.test(allData[allData$sample=="Replication","Liberal"],allData[allData$sample=="Replication","Status"])

MS=step(lm(MateSeeking~sample+gender+age+ChildhoodStability+kids+marital.status2+eWhite+eBlack+eAmericanIndian+eAsian+eHispanic+religious2,allData))#drop current/childhood resources
standardCoefs(MS)#sample beta=-.13, no interaction wiht liberal
summary(MS)
confint(MS)

KCF=step(lm(KinCareFamily~sample+gender+ChildhoodStability+CurrentResources+kids+marital.status2+eWhite+eBlack+eAmericanIndian+eAsian+eHispanic+religious2,allData))#drop child resources,liberal
standardCoefs(KCF)#sample beta=.05, p=.04
summary(KCF)
confint(KCF)

KCC=step(lm(KinCareChild~sample+gender+age+ChildhoodStability+ChildhoodResources+CurrentResources+eWhite+eBlack+eAmericanIndian+eAsian+eHispanic+religious2+Liberal,allData))#drop marital
standardCoefs(KCC)#sample beta=.07, p=.06
summary(KCC)
confint(KCC)

MRB=step(lm(MateRetentionBreakup~sample+gender+age+ChildhoodStability+CurrentResources+kids+marital.status2+eWhite+eBlack+eAmericanIndian+eAsian+eHispanic,allData))#drop religious,child resources, liberal
standardCoefs(MRB)#sample beta=-.07
summary(MRB)
confint(MRB)

MRG=step(lm(MateRetentionGeneral~sample+gender+age+ChildhoodStability+kids+marital.status2+eWhite+eBlack+eAmericanIndian+eAsian+eHispanic+religious2,allData))# drop CurrentResources, liberal, ChildhoodResources
standardCoefs(MRG)#sample beta=.06, p=.04
summary(MRG)
confint(MRG)

