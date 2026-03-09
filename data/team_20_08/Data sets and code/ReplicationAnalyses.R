library(psych)
library(lsr)
library(car)
#read the data (you can just click the "import Dataset" button instead)
FSMData <- read.csv("FSMDataFull.csv")
Qkey <- read.csv("Qkey.csv")

################## Analyzing our data #####################
# types of analyses:
#     -descriptive statistics and correlations
#     -predict life events (dichotmous/frequency): Neel Table S3
#     -look for demographic predictors of motives: Neel Table 4

#find which life events have enough data (more than 40 for each):
a=which(names(FSMData)=="life.survey_1")
summary(FSMData[,a:(a+80)])

#find which frequencies have enough variability
#describe(FSMData[,(a+81):(a+152)])

##########################################Replicating Tables############################################
################ Descriptive statistics
a=which(names(FSMData)=="SelfProtection")
describe(FSMData[,a:(a+10)])
library(Hmisc)
################ FMI correlations
a=which(names(FSMData)=="SelfProtection")
FMIcorData <- FSMData[,a:(a+10)]
FMIcor<-round(cor(big5FMIdata,use="pairwise.complete.obs"),2)
FMIcorSig <- rcorr(as.matrix(FMIcordata))

################ FMI correlation with Big 5
a=which(names(FSMData)=="SelfProtection")
big5FMIdata <- FSMData[,a:(a+15)] 
big5FMI<-round(cor(big5FMIdata,use="pairwise.complete.obs"),2)
big5FMISig <- rcorr(as.matrix(big5FMIdata))
big5FMISig

################ FMI correlation with other scales
FMIscaledata <- FSMData[,c("SelfProtection","DiseaseAvoidance","AffiliationGroup","AffiliationExclusion","AffiliationIndependence","Status","MateSeeking","MateRetentionGeneral","MateRetentionBreakup","KinCareFamily","KinCareChild","STMO","LTMO","PVDInfect","PVDGerm","Dominance","Prestige", "ECRAnxiety" , "ECRAvoidance","NeedtoBelong","BDW","PVD")] 
scaleCor<-round(cor(FMIscaledata,use="pairwise.complete.obs"),2)
scaleCorSig <- rcorr(as.matrix(FMIscaledata))
scaleCorSig

####################################Life history predictors###################################
######table 4 replicaton
#Predictors: Age, Sex, Relationship status, Parent status, Childhood stability, Childhood resources, Current resources
library(sjPlot)
library(sjmisc)
library(sjlabelled)

#self-protection
SPlifeHist = lm(SelfProtection~age+gender+marital.status2+kids+ChildhoodStability+ChildhoodResources+CurrentResources,FSMData)
summary(SPlifeHist) 
standardCoefs(SPlifeHist)

#Disease avoidance
DAlifeHist = lm(DiseaseAvoidance~age+gender+marital.status2+kids+ChildhoodStability+ChildhoodResources+CurrentResources,FSMData) 
summary(DAlifeHist)
standardCoefs(DAlifeHist)

#Affiliation (Group)
AGlifeHist = lm(AffiliationGroup~age+gender+marital.status2+kids+ChildhoodStability+ChildhoodResources+CurrentResources,FSMData) 
summary(AGlifeHist)
standardCoefs(AGlifeHist)

#Affiliation (Exclusion concern)
AElifeHist = lm(AffiliationExclusion~age+gender+marital.status2+kids+ChildhoodStability+ChildhoodResources+CurrentResources,FSMData) 
summary(AElifeHist)
standardCoefs(AElifeHist)

#Affiliation (Independence)
AIlifeHist = lm(AffiliationIndependence~age+gender+marital.status2+kids+ChildhoodStability+ChildhoodResources+CurrentResources,FSMData) 
summary(AIlifeHist)
standardCoefs(AIlifeHist)

#Status
SlifeHist = lm(Status~age+gender+marital.status2+kids+ChildhoodStability+ChildhoodResources+CurrentResources,FSMData) 
summary(SlifeHist)
standardCoefs(SlifeHist)

#Mate seeking
MSlifeHist = lm(MateSeeking~age+gender+marital.status2+kids+ChildhoodStability+ChildhoodResources+CurrentResources,FSMData) 
summary(MSlifeHist)
standardCoefs(MSlifeHist)

#Mate Retention General
MRGlifeHist = lm(MateRetentionGeneral~age+gender+marital.status2+kids+ChildhoodStability+ChildhoodResources+CurrentResources,FSMData) 
summary(MRGlifeHist)
standardCoefs(MRGlifeHist)

#Mate Retention Breakup Concern
MRBlifeHist = lm(MateRetentionBreakup~age+gender+marital.status2+kids+ChildhoodStability+ChildhoodResources+CurrentResources,FSMData) 
summary(MRBlifeHist)
standardCoefs(MRBlifeHist)

#Kin Care (Family)
KCFlifeHist = lm(KinCareFamily~age+gender+marital.status2+kids+ChildhoodStability+ChildhoodResources+CurrentResources,FSMData) 
summary(KCFlifeHist)
standardCoefs(KCFlifeHist)

#Kin Care (Child)
KCClifeHist = lm(KinCareChild~gender+age+ChildhoodStability+ChildhoodResources+CurrentResources+marital.status2,FSMData)
summary(KCClifeHist)
standardCoefs(KCClifeHist)

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

####################################Life Events Odds Ratios###################################
######table S1 replication

###Self protection
#self defense class
SelfdefenseModel= glm(life.survey_78~SelfProtection,FSMData,family="binomial")
summary(SelfdefenseModel)
exp(coef(SelfdefenseModel))
#mace
SPmaceModel= glm(life.survey_12~SelfProtection,FSMData,family="binomial")
summary(SPmaceModel)
exp(coef(SPmaceModel))
#mace freq
SPmace2Freq= lm(past.year.2_18~SelfProtection,FSMData[FSMData$past.year.2_18!=0,]) #n =11 beta = .11 p = .72
summary(SPmace2Freq) #not sig
standardCoefs(SPmace2Freq)
#home security
SPhomeSecModel= glm(life.survey_10~SelfProtection,FSMData,family="binomial")
summary(SPhomeSecModel)
exp(coef(SPhomeSecModel))
#weapon public
SPweaponModel= glm(life.survey_11~SelfProtection,FSMData,family="binomial")
summary(SPweaponModel) #not significant
exp(coef(SPweaponModel))
#frequency of weapon in public
summary(FSMData$past.year.2_19!=0) #n = 12, beta = .08, p = .80 
SPgunFreq= lm(past.year.2_19~SelfProtection,FSMData[FSMData$past.year.2_19!=0,])
summary(SPgunFreq) #not sig
standardCoefs(SPgunFreq)
#screamed at someone
SPscreamModel= glm(life.survey_34~SelfProtection,FSMData,family="binomial")
summary(SPscreamModel) #not significant
exp(coef(SPscreamModel))
#shoved someone 
SPshoveModel= glm(life.survey_35~SelfProtection,FSMData,family="binomial")
summary(SPshoveModel) #not significant
exp(coef(SPshoveModel))
#freq of shoving
summary(FSMData$past.year.survey_29!=0)
SPshoveFreq= lm(past.year.survey_29~SelfProtection,FSMData[FSMData$past.year.survey_29!=0,])
summary(SPshoveFreq)
standardCoefs(SPshoveFreq) #beta = .13, n = 1  

###Disease avoidance
#avoided shaking hands with someone who seemed sick
DAshakeModel= glm(life.survey_4~DiseaseAvoidance,FSMData,family="binomial")
summary(DAshakeModel) 
exp(coef(DAshakeModel))
#freq of avoiding hand shake
DAshake2Freq= lm(past.year.survey_4~DiseaseAvoidance,FSMData[FSMData$past.year.survey_4!=0,]) #n= 71
summary(DAshake2Freq) #not sig
standardCoefs(DAshake2Freq)
#smoked cigarettes
DAsmokeModel= glm(life.survey_69~DiseaseAvoidance,FSMData,family="binomial")
summary(DAsmokeModel) #not significant
exp(coef(DAsmokeModel))

###Affiliation group
#playing a team sport
AGteamsportModel= glm(life.survey_14~AffiliationGroup,FSMData,family="binomial")
summary(AGteamsportModel)
exp(coef(AGteamsportModel))
#attending religious services
AGreligiousModel= glm(life.survey_54~AffiliationGroup,FSMData,family="binomial")
summary(AGreligiousModel) #not significant
exp(coef(AGreligiousModel))
#volunteering
AGvolunteer= glm(life.survey_15~AffiliationGroup,FSMData,family="binomial")
summary(AGvolunteer) 
exp(coef(AGvolunteer))
#smoked ciagrettes
AGsmokeModel= glm(life.survey_69~AffiliationGroup,FSMData,family="binomial")
summary(AGsmokeModel) #not significant
exp(coef(AGsmokeModel))

###Affiliation Exclusion Concern
#facebook
AEfacebookModel= glm(life.survey_17~AffiliationExclusion,FSMData,family="binomial")
summary(AEfacebookModel) 
exp(coef(AEfacebookModel))
#twitter
AEtwitterModelModel= glm(life.survey_74~AffiliationExclusion,FSMData,family="binomial")
summary(AEtwitterModelModel) 
exp(coef(AEtwitterModelModel))
#social media frequency 
AEsocialMediaFreq= lm(past.year.2_1~AffiliationExclusion,FSMData[FSMData$past.year.2_1!=0,]) #n=132
summary(AEsocialMediaFreq)
standardCoefs(AEsocialMediaFreq)

###Status
#job where other people worked for them
SjobModel= glm(life.survey_70~Status,FSMData,family="binomial")
summary(SjobModel) 
exp(coef(SjobModel))
#promotion
SpromotionModel= glm(life.survey_21~Status,FSMData,family="binomial")
summary(SpromotionModel) 
exp(coef(SpromotionModel))
#preformed for others
SpreformModel= glm(life.survey_46~Status,FSMData,family="binomial")
summary(SpreformModel) 
exp(coef(SpreformModel))
#art
SartModel= glm(life.survey_42~Status,FSMData,family="binomial")
summary(SartModel) #not sig
exp(coef(SartModel))

###Mate seeking
#choosen to end relationship
MSdumpModel= glm(life.survey_36~MateSeeking,FSMData,family="binomial")
summary(MSdumpModel) 
exp(coef(MSdumpModel))
#been broken up with
MSbreakupModel= glm(life.survey_37~MateSeeking,FSMData,family="binomial")
summary(MSbreakupModel) #not sig 
exp(coef(MSbreakupModel))
#asked someone out
MSdateModel= glm(life.survey_44~MateSeeking,FSMData,family="binomial")
summary(MSdateModel) #not sig
exp(coef(MSdateModel))
#been asked out
MSaskedoutModel= glm(life.survey_43~MateSeeking,FSMData,family="binomial")
summary(MSaskedoutModel) #not sig
exp(coef(MSaskedoutModel))
#gone dancing
MSdanceModel= glm(life.survey_47~MateSeeking,FSMData,family="binomial")
summary(MSdanceModel) #not sig
exp(coef(MSdanceModel))
#freq of dancing
summary(FSMData$past.year.survey_37!=0) #n = 18 beta = -.31 p = .21
MSdanceFreq= lm(past.year.survey_37~MateSeeking,FSMData[FSMData$past.year.survey_37!=0,])
summary(MSdanceFreq) #not sig
standardCoefs(MSdanceFreq)
#sex with condom
MScondomModel= glm(life.survey_61~MateSeeking,FSMData,family="binomial")
summary(MScondomModel) #not sig
exp(coef(MScondomModel))
#freqency of going to concert
summary(FSMData$past.year.survey_35!=0) #n = 42, beta = .07, p = .65 
MSconcertFreq= lm(past.year.survey_35~MateSeeking,FSMData[FSMData$past.year.survey_35!=0,]) 
summary(MSconcertFreq) #not sig
standardCoefs(MSconcertFreq)
#promotion and new job controling for motives and big 5?
#promotion
MSpromoModel= glm(life.survey_21~MateSeeking,FSMData,family="binomial")
summary(MSpromoModel) #not sig
exp(coef(MSpromoModel))
#controling for motives and big 5
MSpromoControlModel= glm(life.survey_21~MateSeeking+SelfProtection+DiseaseAvoidance+AffiliationGroup+AffiliationExclusion+AffiliationIndependence+Status+MateSeeking+MateRetentionGeneral+MateRetentionBreakup+KinCareFamily+KinCareChild+Extraversion+Agreeableness+Conscientiousness+Neuroticism+Openness,FSMData,family="binomial") #warnings: did not converge
MSpromoControlModel= step(MSpromoControlModel)# warnings
summary(MSpromoControlModel) #not sig
exp(coef(MSpromoControlModel))
#new job 
MSnewjobModel= glm(life.survey_23~MateSeeking,FSMData,family="binomial")
summary(MSnewjobModel) #not sig
exp(coef(MSnewjobModel)) #I decided not to try to conrol for the other motives bc it was not significant
#falling out with friend and moved within a city controling for age?
#falling out with friend
MSfalloutModel= glm(life.survey_13~MateSeeking,FSMData,family="binomial") #controling for age made no difference
summary(MSfalloutModel) #not sig
exp(coef(MSfalloutModel))
#moved within a city
MSmovedModel= glm(life.survey_18~MateSeeking,FSMData,family="binomial") #controling for age made no difference
summary(MSmovedModel) #not sig
exp(coef(MSmovedModel))
#freq of going a day without eating
summary(FSMData$past.year.survey_38!=0) #n = 43, beta = .08, p = .61 
MSnoeatFreq= lm(past.year.survey_38~MateSeeking,FSMData[FSMData$past.year.survey_38!=0,]) 
summary(MSnoeatFreq) #not sig
standardCoefs(MSnoeatFreq)
#non holiday gift for people in relationship
MSgiftModel= glm(life.survey_52~MateSeeking,FSMData[FSMData$marital.status2=="Committed",],family="binomial") #not working
summary(MSgiftModel) 
exp(coef(MSgiftModel))
#smoked cigarettes
MScigModel= glm(life.survey_69~MateSeeking,FSMData,family="binomial")
summary(MScigModel) #not sig
exp(coef(MScigModel))
#broken a bone
MSboneModel= glm(life.survey_79~MateSeeking,FSMData,family="binomial")
summary(MSboneModel) #not sig
exp(coef(MSboneModel))
#long term mating orientation
cor.test(~LTMO+MateSeeking,FSMData[FSMData$marital.status2=="Committed",]) #in committed relationship
cor.test(~LTMO+MateSeeking,FSMData[FSMData$marital.status2=="Single",]) #single
#short term mating orientation
cor.test(~STMO+MateSeeking,FSMData[FSMData$marital.status2=="Committed",]) #in committed relationship
cor.test(~STMO+MateSeeking,FSMData[FSMData$marital.status2=="Single",]) #single

###Mate retention general
describe(FSMData$MateRetentionGeneral) #n = 91
#holiday gift
MRGholidayModel= glm(life.survey_51~MateRetentionGeneral,FSMData,family="binomial")
summary(MRGholidayModel) 
exp(coef(MRGholidayModel))
#non holiday gift
MRGgiftModel= glm(life.survey_52~MateRetentionGeneral,FSMData,family="binomial")
summary(MRGgiftModel) 
exp(coef(MRGgiftModel))
#unfaithful to partner
MRGcheatedModel= glm(life.survey_71~MateRetentionGeneral,FSMData,family="binomial")
summary(MRGcheatedModel) 
exp(coef(MRGcheatedModel))

###Mate retention breakup concern
describe(FSMData$MateRetentionBreakup) #n = 91
#relatioship counselor
MRBcounselModel= glm(life.survey_50~MateRetentionBreakup,FSMData,family="binomial")
summary(MRBcounselModel) 
exp(coef(MRBcounselModel))
#unfaithful to partner
MRBcheatedModel= glm(life.survey_71~MateRetentionBreakup,FSMData,family="binomial")
summary(MRBcheatedModel) 
exp(coef(MRBcheatedModel))

###kin care family
#cared for younger relative
KFcaredModel= glm(life.survey_59~KinCareFamily,FSMData,family="binomial")
summary(KFcaredModel) 
exp(coef(KFcaredModel))
#skydiving
KFskydiveModel= glm(life.survey_80~KinCareFamily,FSMData,family="binomial")
summary(KFskydiveModel) 
exp(coef(KFskydiveModel))
#arrested
KFarrestedModel= glm(life.survey_73~KinCareFamily,FSMData,family="binomial")
summary(KFarrestedModel) 
exp(coef(KFarrestedModel))
#moved in same city
KFmovinCModel= glm(life.survey_18~KinCareFamily,FSMData,family="binomial")
summary(KFmovinCModel) 
exp(coef(KFmovinCModel))
#moved to diff city
KFmovCityModel= glm(life.survey_19~KinCareFamily,FSMData,family="binomial")
summary(KFmovCityModel) 
exp(coef(KFmovCityModel))
#moved to diff country
KFmovCountryModel= glm(life.survey_20~KinCareFamily,FSMData,family="binomial")
summary(KFmovCountryModel) 
exp(coef(KFmovCountryModel))
#kept gun at home
KFgunHomeModel= glm(life.survey_81~KinCareFamily,FSMData,family="binomial")
summary(KFgunHomeModel) 
exp(coef(KFgunHomeModel))
#cooked meal at home
KFcookModel= glm(life.survey_66~KinCareFamily,FSMData,family="binomial")
summary(KFcookModel) 
exp(coef(KFcookModel))
#asked someone out
KFaskoutModel= glm(life.survey_44~KinCareFamily,FSMData,family="binomial")
summary(KFaskoutModel) 
exp(coef(KFaskoutModel))
#ended relationship
KFdumpedModel= glm(life.survey_36~KinCareFamily,FSMData,family="binomial")
summary(KFdumpedModel) 
exp(coef(KFdumpedModel))

#kin care child 
describe(FSMData$KinCareChild) #n = 56
cor.test(FSMData$KinCareChild,FSMData$Agreeableness)
cor.test(FSMData$KinCareChild,FSMData$Prestige)
cor.test(FSMData$KinCareChild,FSMData$Dominance)
cor.test(FSMData$KinCareChild,FSMData$STMO)



