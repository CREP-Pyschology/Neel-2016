library(psych)
library(car)
library(lsr)
fsm=read.csv("rawData.csv")
fsm=fsm[,-c(1:3,6:10)] # gets rid of unwanted columns (edit as needed)
Qkey=fsm[1,] #saves the textual description of questions for reference
write.table(Qkey,file="Qkey.csv",sep=",",row.names=F)
fsm=fsm[-c(1,2),] #gets rid of first two non-data rows
fsm=type.convert(fsm) #converts columns to appropriate type

#exclude participants who finished faster than 1/2 mean completion time
nrow(fsm[fsm$Duration..in.seconds.<.5*mean(fsm$Duration..in.seconds.),])
fsm=fsm[fsm$Duration..in.seconds.>=.5*mean(fsm$Duration..in.seconds.),]

#FMI scales
#	Self-Protection 1-6, R 3
fsm$SelfProtection=(fsm$FMI_1+fsm$FMI_2+8-fsm$FMI_3+fsm$FMI_4+fsm$FMI_5+fsm$FMI_6)/6
#	Disease Avoidance 7-12, R 10,11,12
fsm$DiseaseAvoidance=(fsm$FMI_7+fsm$FMI_8+fsm$FMI_9+8-fsm$FMI_10+8-fsm$FMI_11+8-fsm$FMI_12)/6
#	Affiliation (Group) 13-18, R 16
fsm$AffiliationGroup=(fsm$FMI_13+fsm$FMI_14+fsm$FMI_15+8-fsm$FMI_16+fsm$FMI_17+fsm$FMI_18)/6
#	Affiliation (Exclusion Concern) 19-24
fsm$AffiliationExclusion=(fsm$FMI_19+fsm$FMI_20+fsm$FMI_21+fsm$FMI_22+fsm$FMI_23+fsm$FMI_24)/6
#	Affiliation (Independence) 25-30
fsm$AffiliationIndependence=(fsm$FMI_25+fsm$FMI_26+fsm$FMI_27+fsm$FMI_28+fsm$FMI_29+fsm$FMI_30)/6
#	Status 31-36, R 36
fsm$Status=(fsm$FMI_31+fsm$FMI_32+fsm$FMI_33+fsm$FMI_34+fsm$FMI_35+8-fsm$FMI_36)/6
#	Mate Seeking 37-42, R 39,40,41
fsm$MateSeeking=(fsm$FMI_37+fsm$FMI_38+8-fsm$FMI_39+8-fsm$FMI_40+8-fsm$FMI_41)/6
#	Mate Retention (General) 43-48, R 45,46,47,48
fsm$MateRetentionGeneral=(fsm$FMI_43+fsm$FMI_44+8-fsm$FMI_45+8-fsm$FMI_46+8-fsm$FMI_47+8-fsm$FMI_48)/6
#	Mate Retention (Breakup Concern) 49-54
fsm$MateRetentionBreakup=(fsm$FMI_49+fsm$FMI_50+fsm$FMI_51+fsm$FMI_52+fsm$FMI_53+fsm$FMI_54)/6
#	Kin Care (Family) 55-60, R 56,57,58
fsm$KinCareFamily=(fsm$FMI_55+8-fsm$FMI_56+8-fsm$FMI_57+8-fsm$FMI_58+fsm$FMI_59+fsm$FMI_60)/6
#	Kin Care (Child) 61-66, R 63, 65
fsm$KinCareChild=(fsm$FMI_61+fsm$FMI_62+8-fsm$FMI_63+fsm$FMI_64+8-fsm$FMI_65+fsm$FMI_66)/6
#internal reliability checks for FMI
# a=which(names(fsm)=="FMI_1")
# for(i in 1:11){
#   print(alpha(fsm[,(i*6+a-6):(i*6+a-1)],check.keys = T))
# }
#Factor analysis for FMI
# p=fa.parallel(fsm[,a:(a+65)])
# plot(p$fa.values,col="blue",type="o",xlim=c(1,12))
# lines(p$fa.sim,lty="dashed",col="red",type="l")
# fa.FMI=fa(fsm[,a:(a+65)],nfactors=8,rotate="promax")
# print.psych(fa.FMI,sort=TRUE)

#code the big 5 after finding out which ones are reverse coded and then average
fsm$Extraversion=(fsm$Big.5_1+6-fsm$Big.5_6+fsm$Big.5_11+fsm$Big.5_16+6-fsm$Big.5_21+fsm$Big.5_26+6-fsm$Big.5_31+fsm$Big.5_36)/8
fsm$Agreeableness=(6-fsm$Big.5_2+fsm$Big.5_7+6-fsm$Big.5_12+fsm$Big.5_17+fsm$Big.5_22+6-fsm$Big.5_27+fsm$Big.5_32+6-fsm$Big.5_37+fsm$Big.5_42)/9
fsm$Conscientiousness=(fsm$Big.5_3+6-fsm$Big.5_8+fsm$Big.5_13+6-fsm$Big.5_18+6-fsm$Big.5_23+fsm$Big.5_28+fsm$Big.5_33+fsm$Big.5_38+6-fsm$Big.5_43)/9
fsm$Neuroticism=(fsm$Big.5_4+6-fsm$Big.5_9+fsm$Big.5_14+fsm$Big.5_19+6-fsm$Big.5_24+fsm$Big.5_29+6-fsm$Big.5_34+fsm$Big.5_39)/8
fsm$Openness=(fsm$Big.5_5+fsm$Big.5_10+fsm$Big.5_15+fsm$Big.5_20+fsm$Big.5_25+fsm$Big.5_30+6-fsm$Big.5_35+fsm$Big.5_40+6-fsm$Big.5_41+fsm$Big.5_44)/10
#Factor analysis for Big 5
# a=which(names(fsm)=="Big.5_1")
# p=fa.parallel(fsm[,a:(a+43)])
# plot(p$fa.values,col="blue",type="o",xlim=c(1,8))
# lines(p$fa.sim,lty="dashed",col="red",type="l")
# fa.Big5=fa(fsm[,a:(a+43)],nfactors=5,rotate="promax")
# print.psych(fa.Big5,sort=TRUE)
#internal consistency:
a=which(names(fsm)=="Big.5_1")
#Ext (r 6, 21, 31)
alpha(fsm[,a-1+c(1,6,11,16,21,26,31,36)],check.keys = T)
#Ag (r 2, 12, 27, 37)
alpha(fsm[,a+c(1,6,11,16,21,26,31,36,41)],check.keys = T)
#con (r 8, 18, 23, 43)
alpha(fsm[,a+1+c(1,6,11,16,21,26,31,36,41)],check.keys = T)
#nuer (r 9, 24, 34)
alpha(fsm[,a+2+c(1,6,11,16,21,26,31,36)],check.keys = T)
#open (r 35, 41)
alpha(fsm[,a+3+c(1,6,11,16,21,26,31,36,37,40)],check.keys = T)

#code SOI
fsm$STMO=(fsm$soi_1+fsm$soi_2+fsm$soi_3+fsm$soi_4+fsm$soi_5+fsm$soi_6+6-fsm$soi_7+fsm$soi_8+fsm$soi_9+6-fsm$soi_10)/10
a=which(names(fsm)=="soi_1")
alpha(fsm[,a:(a+9)],check.keys = T)
fsm$LTMO=(fsm$soi_11+fsm$soi_12+fsm$soi_13+6-fsm$soi_14+6-fsm$soi_15+fsm$soi_16+fsm$soi_17)/7
alpha(fsm[,(a+10):(a+16)],check.keys = T)

#code perceived vulnerability to disease
fsm$PVDInfect=(fsm$disease_2+8-fsm$disease_5+fsm$disease_6+fsm$disease_8+fsm$disease_10+8-fsm$disease_12+8-fsm$disease_14)/7
fsm$PVDGerm=(fsm$disease_1+8-fsm$disease_3+fsm$disease_4+fsm$disease_7+fsm$disease_9+8-fsm$disease_11+8-fsm$disease_13+fsm$disease_15)/8
a=which(names(fsm)=="disease_1")-1
alpha(fsm[,a+c(2,5,6,8,10,12,14)],check.keys = T)
alpha(fsm[,a+c(1,3,4,7,9,11,13,15)],check.keys = T)

#code dominance-prestige
#Dominance: 3,5,7,9,10R,11,12R,16, Prestige: 1,2R,4,6R,8,13,14,15,17R
fsm$Dominance=(fsm$dominance_3+fsm$dominance_5+fsm$dominance_7+fsm$dominance_9+8-fsm$dominance_10+fsm$dominance_11+8-fsm$dominance_12+fsm$dominance_16)/8
fsm$Prestige=(fsm$dominance_1+8-fsm$dominance_2+fsm$dominance_4+8-fsm$dominance_6+fsm$dominance_8+fsm$dominance_13+fsm$dominance_14+fsm$dominance_15+8-fsm$dominance_17)/9
a=which(names(fsm)=="dominance_1")-1
alpha(fsm[,a+c(3,5,7,9,10,11,12,16)],check.keys = T)
alpha(fsm[,a+c(1,2,4,6,8,13,14,15,17)],check.keys = T)

#code ECR-R
fsm$ECRAnxiety=(fsm$ECR.R_1+fsm$ECR.R_2+fsm$ECR.R_3+fsm$ECR.R_4+fsm$ECR.R_5+fsm$ECR.R_6+fsm$ECR.R_7+fsm$ECR.R_8+8-fsm$ECR.R_9+fsm$ECR.R_10+8-fsm$ECR.R_11+fsm$ECR.R_12+fsm$ECR.R_13+fsm$ECR.R_14+fsm$ECR.R_15+fsm$ECR.R_17+fsm$ECR.R_18)/18
fsm$ECRAvoidance=(fsm$ECR.R_19+8-fsm$ECR.R_20+fsm$ECR.R_21+8-fsm$ECR.R_22+fsm$ECR.R_23+fsm$ECR.R_24+fsm$ECR.R_25+8-fsm$ECR.R_26+8-fsm$ECR.R_27+8-fsm$ECR.R_28+8-fsm$ECR.R_29+8-fsm$ECR.R_30+8-fsm$ECR.R_31+fsm$ECR.R_32+8-fsm$ECR.R_33+8-fsm$ECR.R_34+8-fsm$ECR.R_35+8-fsm$ECR.R_36)/18
a=which(names(fsm)=="ECR.R_1")
alpha(fsm[,a:(a+17)],check.keys = T)
alpha(fsm[,(a+18):(a+35)],check.keys = T)

#code need to belong
fsm$NeedtoBelong=(6-fsm$belong_1+fsm$belong_2+6-fsm$belong_3+fsm$belong_4+fsm$belong_5+fsm$belong_6+6-fsm$belong_7+fsm$belong_8+fsm$belong_9+fsm$belong_10)/10
a=which(names(fsm)=="belong_1")
alpha(fsm[,a:(a+9)],check.keys = T)

#code BDW
fsm$BDW=(fsm$BDW_1+8-fsm$BDW_2+fsm$BDW_3+8-fsm$BDW_4+8-fsm$BDW_5+fsm$BDW_6+8-fsm$BDW_7+fsm$BDW_8+8-fsm$BDW_9+fsm$BDW_10+fsm$BDW_11+8-fsm$BDW_12)/12
a=which(names(fsm)=="BDW_1")
alpha(fsm[,a:(a+11)],check.keys = T)

#Childhood resources (first four from family history)
fsm$ChildhoodResources=(fsm$family.history_1+fsm$family.history_2+fsm$family.history_3+8-fsm$family.history_4)/4
a=which(names(fsm)=="family.history_1")
alpha(fsm[,a:(a+3)],check.keys = T)

#Current resources (last four from family history)
fsm$CurrentResources=(fsm$family.history_5+fsm$family.history_6+fsm$family.history_7+fsm$family.history_8)/4
alpha(fsm[,(a+4):(a+7)],check.keys = T)

#Childhood stability: 3 Q's at end of demographics, reverse code so high=stable
fsm$ChildhoodStability=(8-fsm$stable+8-fsm$predictable+8-fsm$hard.life)/3
a=which(names(fsm)=="stable")
alpha(fsm[,a:(a+2)],check.keys = T)

#Liberal (high) vs. Conservatism (low):
fsm$Liberal=(fsm$political.questions_1+fsm$political.questions_2+fsm$political.questions_3)/3
alpha(fsm[,c("political.questions_1","political.questions_2","political.questions_3")],check.keys = T)

#######recode categorical variables#############
library(car)

#fix life events (numeric ones)
a=which(names(fsm)=="past.year.survey_1")
fsm[,a:(a+50)]=fsm[,a:(a+50)]-1
for(a in a:(a+50)){fsm[is.na(fsm[,a]),a]=0}

#fix life events (ordinal ones)
a=which(names(fsm)=="past.year.2_1")
fsm[,a:(a+19)]=fsm[,a:(a+19)]-1
for(a in a:(a+19)){fsm[is.na(fsm[,a]),a]=0}

#fix life events (categorical ones)
a=which(names(fsm)=="past.year.have_1")
fsm[,a:(a+7)]=apply(fsm[,a:(a+7)],2, function(x) {x <- recode(x, "1='Yes'; 2='Maybe'; 3='No'");x})
for(a in a:(a+7)){fsm[is.na(fsm[,a]),a]="No"}

fsm$kids=as.factor(recode(fsm$kids, "1='Yes'; 2='No'"))
fsm$marital.status=as.factor(recode(fsm$martial.status,"1='Married'; 2='Committed Relationship'; 3='Dating One'; 4='Dating Several'; 5='Single'; 6='Other'"))
fsm$martial.status=NULL
a=which(names(fsm)=="life.survey_1")
fsm[,a:(a+80)]=apply(fsm[,a:(a+80)],2, function(x) {x <- recode(x, "1='Yes'; 2='No'");x})
fsm[,a:(a+80)]=lapply(fsm[,a:(a+80)],as.factor)
fsm$gender=as.factor(recode(fsm$gender,"1='Male'; 2='Female'; 3='Other'"))
fsm$eAmericanIndian=grepl(1,fsm[,"ethnicity"])
fsm$eAsian=grepl(2,fsm[,"ethnicity"])
fsm$eBlack=grepl(3,fsm[,"ethnicity"])
fsm$eHispanic=grepl(4,fsm[,"ethnicity"])
fsm$eWhite=grepl(5,fsm[,"ethnicity"])
fsm$ePacificIslander=grepl(6,fsm[,"ethnicity"])
fsm$eOther=grepl(6,fsm[,"ethnicity"])
fsm$education=as.factor(recode(fsm$education,"1='No high school'; 2='Some high school'; 3='High scool completed or GED'; 4='Some college'; 5='Associates degree'; 6='Bachelors degree'; 7='Some graduate or professional training'; 8='Graduate or professional degree'"))
fsm$wFullTime=grepl(1,fsm[,"occupational.status"])
fsm$wPartTime=grepl(2,fsm[,"occupational.status"])
fsm$wUnemployedLooking=grepl(3,fsm[,"occupational.status"])
fsm$wUnemployedNotLooking=grepl(4,fsm[,"occupational.status"])
fsm$wStudent=grepl(5,fsm[,"occupational.status"])
fsm$wOther=grepl(6,fsm[,"occupational.status"])
fsm$socioeconomic=as.factor(recode(fsm$socioeconomic, "1='Working'; 2='Lower Middle'; 3='Middle Middle'; 4='Upper Middle'; 5='Upper'"))
fsm$income=as.factor(recode(fsm$income, "1='Under 20,000'; 2='20,000-39,999'; 3='30,000-39,999'; 4='40,000-49,999'; 5='50,000-59,999'; 6='60,000-69,999'; 7='70,000-79,999'; 8='80,000-89,999'; 9='90,000-99,999'; 10='100,000-149,999'; 11='150,000-199,999'; 12='Decline to answer'"))
fsm$political.views=as.factor(recode(fsm$political.views, "1='Republican'; 2='Democrat'; 3='Independent'; 4='Libertarian'; 5='Green Party'; 6='Tea Party'; 7='Other or Unaffiliated'; 8='Decline to Respond'"))
fsm$religious=as.factor(recode(fsm$religious, "1='Protestant Christian'; 2='Catholic'; 3='Evangelical Christian'; 4='Jewish'; 5='Muslim'; 6='Hindu'; 7='Atheist'; 8='Other'"))
#the below is probably better as numeric (but ordinal)
#fsm$relationship.time=as.factor(recode(fsm$relationship.time, "1='Not currently in a relationship'; 2='0-3 months'; 3='3-6 months'; 4='6-12 months'; 5='1-2 years'; 6='2-5 years'; 7='5-10 years'; 8='10-20 years'; 9='More than 20 years'"))
fsm$height=fsm$height...feet*12+fsm$height...inches
fsm$work=as.factor(recode(fsm$work, "3='Yes'; 4='Sometimes'; 5='No'"))
fsm$essential=as.factor(recode(fsm$essential, "1='Yes'; 2='Maybe'; 3='No'"))
fsm$home=as.factor(recode(fsm$home, "1='Yes'; 2='No'"))
fsm$community=as.factor(recode(fsm$community, "1='Yes'; 2='No'"))

########recoding variables:
#recode martial status
fsm$marital.status2=as.character(fsm$marital.status)
fsm[fsm$marital.status2 %in% c("Dating Several","Dating One","Single"),"marital.status2"]="Single"
fsm[fsm$marital.status2 %in% c("Committed Relationship","Married"),"marital.status2"]="Committed"
fsm[!is.na(fsm$marital.status2) & fsm$marital.status2=="Other","marital.status2"]=NA
fsm$marital.status2=as.factor(fsm$marital.status2)

#recode gender
fsm[fsm$gender=="Other","gender"]=NA

#recode income
fsm[!is.na(fsm$income) & fsm$income=="Decline to answer","income"]=NA
fsm$income=as.numeric(factor(fsm$income, order = TRUE,levels = c("Under 20,000","20,000-39,999", "30,000-39,999","40,000-49,999","50,000-59,999","60,000-69,999", "70,000-79,999", "80,000-89,999","90,000-99,999","100,000-149,999","150,000-199,999")))

#recode religion
fsm$religious2=as.character(fsm$religious)
fsm[fsm$religious2 %in% c("Catholic", "Evangelical Christian","Jewish","Protestant Christian","Muslim","Hindu"),"religious2"]="Theist"
fsm[fsm$religious2 %in% c("Atheist"),"relgious2"]="Atheist"
#fsm[fsm$religious2 %in% c("Other"),"relgious2"]="Other"
fsm$religious2=as.factor(fsm$religious2)

#recode religion
allData$religious2=as.character(allData$religious)
allData[allData$religious2 %in% c("Catholic", "Evangelical Christian","Jewish","Protestant Christian","Muslim","Hindu"),"religious2"]="Theist"
allData[allData$religious2 %in% c("Other"),"religious2"]="Other"
allData$religious2=as.factor(allData$religious2)

#create single PVD scale
fsm$PVD=(fsm$disease_2+8-fsm$disease_5+fsm$disease_6+fsm$disease_8+fsm$disease_10+8-fsm$disease_12+8-fsm$disease_14+fsm$disease_1+8-fsm$disease_3+fsm$disease_4+fsm$disease_7+fsm$disease_9+8-fsm$disease_11+8-fsm$disease_13+fsm$disease_15)/15

#Recode yes/no to 1/0
first=which(names(fsm)=="life.survey_1")
fsm[,first:(first+80)]=apply(fsm[,first:(first+80)], 2, function(x) {x <- recode(x,"'Yes'=1; 'No'=0"); x})

write.table(fsm,file="FSMDataFull.csv",sep=",",row.names=F)

#getting rid of individual questions that make up a scale:
smallData=fsm[,-c(1:2,4:which(names(fsm)=="FMI_66"),which(names(fsm)=="Big.5_1"):which(names(fsm)=="BDW_12"),which(names(fsm)=="family.history_1"):which(names(fsm)=="political.questions_3"),which(names(fsm)=="height...feet"),which(names(fsm)=="height...inches"),which(names(fsm)=="ethnicity_7_TEXT"),which(names(fsm)=="occupational.status"),which(names(fsm)=="ethnicity"),which(names(fsm)=="MTurkCode"))]

write.table(smallData,file="FSMDataShort.csv",sep=",",row.names=F)
