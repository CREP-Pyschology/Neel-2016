



##16 had a close friend to you die
LifeEventsdata$led16 <- as.factor(LifeEventsdata$Life_Data_16)
levels(LifeEventsdata$led16)[levels(LifeEventsdata$led16)=="2"] <-"0"
SPled16Model <- glm(led16~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled16Model)
exp(coef(SPled16Model))
waldtest(SPled16Model)
odds.ratio(SPled16Model, level=0.95)
#frequency
LifeEventsdata$led16freq <- LifeEventsdata$Life_Data_13_Freq
SPled16freq= lm(led16freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led16freq!=0,]) 
summary(SPled16freq)
standardCoefs(SPled16freq)

#Model 2
SPled16Model2 <- glm(led16~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SPled16Model2)
exp(coef(SPled16Model2))
waldtest(SPled16Model2)
odds.ratio(SPled16Model2, level=0.95)
#frequency
led16freqdata <- LifeEventsdata
led16freqdata$led16freqNA <- is.na(led16freqdata$led16freq)
led16freqdata <- led16freqdata[led16freqdata$led16freqNA != TRUE, ]
SPled16freq2 = lm(led16freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata) 
summary(SPled16freq2)
standardCoefs(SPled16freq2)
confint.lm(SPled16freq2)
--------------------


##27 (n=90) purchased a new phone
LifeEventsdata$led27 <- as.factor(LifeEventsdata$Life_Data_27)
levels(LifeEventsdata$led27)[levels(LifeEventsdata$led27)=="2"] <-"0"
SPled27Model <- glm(led27~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled27Model)
exp(coef(SPled27Model))
waldtest(SPled27Model)
odds.ratio(SPled27Model, level=0.95)
#frequency
LifeEventsdata$led27freq <- LifeEventsdata$Life_Data_23_Freq
SPled27freq= lm(led27freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led27freq!=0,]) 
summary(SPled27freq)
standardCoefs(SPled27freq)

#Model 2
SPled27Model2 <- glm(led27~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SPled27Model2)
exp(coef(SPled27Model2))
waldtest(SPled27Model2)
odds.ratio(SPled27Model2, level=0.95)
#frequency
led27freqdata <- LifeEventsdata
led27freqdata$led27freqNA <- is.na(led27freqdata$led27freq)
led27freqdata <- led27freqdata[led27freqdata$led27freqNA != TRUE, ]
SPled27freq2 = lm(led27freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led27freqdata) 
summary(SPled27freq2)
standardCoefs(SPled27freq2)
confint.lm(SPled27freq2)

-----------



##40 (n=60) been on a diet
LifeEventsdata$led40 <- as.factor(LifeEventsdata$Life_Data_40)
levels(LifeEventsdata$led40)[levels(LifeEventsdata$led40)=="2"] <-"0"
SPled40Model <- glm(led40~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled40Model)
exp(coef(SPled40Model))
waldtest(SPled40Model)
odds.ratio(SPled40Model, level=0.95)
#frequency
LifeEventsdata$led40freq <- LifeEventsdata$Life_Data_1_Yr_Y_N
SPled40freq= lm(led40freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led40freq!=0,]) 
summary(SPled40freq)
standardCoefs(SPled40freq)

#Model 2
SPled40Model2 <- glm(led40~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SPled40Model2)
exp(coef(SPled40Model2))
waldtest(SPled40Model2)
odds.ratio(SPled40Model2, level=0.95)
#frequency
led40freqdata <- LifeEventsdata
led40freqdata$led40freqNA <- is.na(led40freqdata$led40freq)
led40freqdata <- led40freqdata[led40freqdata$led40freqNA != TRUE, ]
SPled40freq2 = lm(led40freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led40freqdata) 
summary(SPled40freq2)
standardCoefs(SPled40freq2)
confint.lm(SPled40freq2)
--------


##41 (n=78) lost 10 or more pounds
LifeEventsdata$led41 <- as.factor(LifeEventsdata$Life_Data_41)
levels(LifeEventsdata$led41)[levels(LifeEventsdata$led41)=="2"] <-"0"
SPled41Model <- glm(led41~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled41Model)
exp(coef(SPled41Model))
waldtest(SPled41Model)
odds.ratio(SPled41Model, level=0.95)
#frequency
LifeEventsdata$led41freq <- LifeEventsdata$Life_Data_2_Yr_Y_N
SPled41freq= lm(led41freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led41freq!=0,]) 
summary(SPled41freq)
standardCoefs(SPled41freq)

#Model 2
SPled41Model2 <- glm(led41~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SPled41Model2)
exp(coef(SPled41Model2))
waldtest(SPled41Model2)
odds.ratio(SPled41Model2, level=0.95)
#frequency
led41freqdata <- LifeEventsdata
led41freqdata$led41freqNA <- is.na(led41freqdata$led41freq)
led41freqdata <- led41freqdata[led41freqdata$led41freqNA != TRUE, ]
SPled41freq2 = lm(led41freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led41freqdata) 
summary(SPled41freq2)
standardCoefs(SPled41freq2)
confint.lm(SPled41freq2)
--------


##62 (n=94) put money in a savings account
LifeEventsdata$led62 <- as.factor(LifeEventsdata$Life_Data_62)
levels(LifeEventsdata$led62)[levels(LifeEventsdata$led62)=="2"] <-"0"
SPled62Model <- glm(led62~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled62Model)
exp(coef(SPled62Model))
waldtest(SPled62Model)
odds.ratio(SPled62Model, level=0.95)
#frequency
LifeEventsdata$led62freq <- LifeEventsdata$Life_Data_10_WkFreq
SPled62freq= lm(led62freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led62freq!=0,]) 
summary(SPled62freq)
standardCoefs(SPled62freq)

#Model 2
SPled62Model2 <- glm(led62~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SPled62Model2)
exp(coef(SPled62Model2))
waldtest(SPled62Model2)
odds.ratio(SPled62Model2, level=0.95)
#frequency
led62freqdata <- LifeEventsdata
led62freqdata$led62freqNA <- is.na(led62freqdata$led62freq)
led62freqdata <- led62freqdata[led62freqdata$led62freqNA != TRUE, ]
SPled62freq2 = lm(led62freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led62freqdata) 
summary(SPled62freq2)
standardCoefs(SPled62freq2)
confint.lm(SPled62freq2)
--------



##65 (n=49) played a non-team sport
LifeEventsdata$led65 <- as.factor(LifeEventsdata$Life_Data_65)
levels(LifeEventsdata$led65)[levels(LifeEventsdata$led65)=="2"] <-"0"
SPled65Model <- glm(led65~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled65Model)
exp(coef(SPled65Model))
waldtest(SPled65Model)
odds.ratio(SPled65Model, level=0.95)
#frequency
LifeEventsdata$led65freq <- LifeEventsdata$Life_Data_13_WkFreq
SPled65freq= lm(led65freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led65freq!=0,]) 
summary(SPled65freq)
standardCoefs(SPled65freq)
confint.lm(SPled65freq)

#Model 2
SPled65Model2 <- glm(led65~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SPled65Model2)
exp(coef(SPled65Model2))
waldtest(SPled65Model2)
odds.ratio(SPled65Model2, level=0.95)
#frequency
led65freqdata <- LifeEventsdata
led65freqdata$led65freqNA <- is.na(led65freqdata$led65freq)
led65freqdata <- led65freqdata[led65freqdata$led65freqNA != TRUE, ]
SPled65freq2 = lm(led65freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led65freqdata) 
summary(SPled65freq2)
standardCoefs(SPled65freq2)
confint.lm(SPled65freq2)
----------
##66 (n=105) cooked a meal at home
LifeEventsdata$led66 <- as.factor(LifeEventsdata$Life_Data_66)
levels(LifeEventsdata$led66)[levels(LifeEventsdata$led66)=="2"] <-"0"
SPled66Model <- glm(led66~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled66Model)
exp(coef(SPled66Model))
waldtest(SPled66Model)
odds.ratio(SPled66Model, level=0.95)
#frequency
LifeEventsdata$led66freq <- LifeEventsdata$Life_Data_11_WkFreq
SPled66freq= lm(led66freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led66freq!=0,]) 
summary(SPled66freq)
standardCoefs(SPled66freq)

#Model 2
SPled66Model2 <- glm(led66~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Sex10+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,LifeEventsdata,family="binomial")
summary(SPled66Model2)
exp(coef(SPled66Model2))
waldtest(SPled66Model2)
odds.ratio(SPled66Model2, level=0.95)
#frequency
led66freqdata <- LifeEventsdata
led66freqdata$led66freqNA <- is.na(led66freqdata$led66freq)
led66freqdata <- led66freqdata[led66freqdata$led66freqNA != TRUE, ]
SPled66freq2 = lm(led66freq~FSM_SelfProtect+FSM_DiseaseAvoid+FSM_Affiliation_GROUP+FSM_Affiliation_EXCLUDE+FSM_Affiliation_INDEP+FSM_Status+FSM_MateSeek+Big5_EXTVRT+Big5_AGREE+Big5_CONC+Big5_NEUR+Big5_OPEN+Dem_AGE+Relationship10+CldhdStablty+Resources_CHILDHOOD+Resources_CURRENT,led66freqdata) 
summary(SPled66freq2)
standardCoefs(SPled66freq2)
confint.lm(SPled66freq2)
------------



##70 had a job where other people worked for you
LifeEventsdata$led70 <- as.factor(LifeEventsdata$Life_Data_70)
levels(LifeEventsdata$led70)[levels(LifeEventsdata$led70)=="2"] <-"0"
SPled70Model <- glm(led70~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled70Model)
exp(coef(SPled70Model))
waldtest(SPled70Model)
odds.ratio(SPled70Model, level=0.95)
#frequency
LifeEventsdata$led70freq <- LifeEventsdata$Life_Data_5_Yr_Y_N
SPled70freq= lm(led70freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led70freq!=0,]) 
summary(SPled70freq)
standardCoefs(SPled70freq)


##71 been unfaithful to a romantic/sexual partner
LifeEventsdata$led71 <- as.factor(LifeEventsdata$Life_Data_71)
levels(LifeEventsdata$led71)[levels(LifeEventsdata$led71)=="2"] <-"0"
SPled71Model <- glm(led71~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled71Model)
exp(coef(SPled71Model))
waldtest(SPled71Model)
odds.ratio(SPled71Model, level=0.95)
#frequency
LifeEventsdata$led71freq <- LifeEventsdata$Life_Data_48_Freq
SPled71freq= lm(led71freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led71freq!=0,]) 
summary(SPled71freq)
standardCoefs(SPled71freq)



##72 (n=74) taken a cab 
LifeEventsdata$led72 <- as.factor(LifeEventsdata$Life_Data_72)
levels(LifeEventsdata$led72)[levels(LifeEventsdata$led72)=="2"] <-"0"
SPled72Model <- glm(led72~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled72Model)
exp(coef(SPled72Model))
waldtest(SPled72Model)
odds.ratio(SPled72Model, level=0.95)
#frequency
LifeEventsdata$led72freq <- LifeEventsdata$Life_Data_15_WkFreq
SPled72freq= lm(led72freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led72freq!=0,]) 
summary(SPled72freq)
standardCoefs(SPled72freq)


##73 been arrested
LifeEventsdata$led73 <- as.factor(LifeEventsdata$Life_Data_73)
levels(LifeEventsdata$led73)[levels(LifeEventsdata$led73)=="2"] <-"0"
SPled73Model <- glm(led73~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled73Model)
exp(coef(SPled73Model))
waldtest(SPled73Model)
odds.ratio(SPled73Model, level=0.95)
#frequency
LifeEventsdata$led73freq <- LifeEventsdata$Life_Data_49_Freq
SPled73freq= lm(led73freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led73freq!=0,]) 
summary(SPled73freq)
standardCoefs(SPled73freq)


##75 (n=84) used twitter 
LifeEventsdata$led75 <- as.factor(LifeEventsdata$Life_Data_75)
levels(LifeEventsdata$led75)[levels(LifeEventsdata$led75)=="2"] <-"0"
SPled75Model <- glm(led75~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled75Model)
exp(coef(SPled75Model))
waldtest(SPled75Model)
odds.ratio(SPled75Model, level=0.95)
#frequency
LifeEventsdata$led75freq <- LifeEventsdata$Life_Data_17_WkFreq
SPled75freq= lm(led75freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led75freq!=0,]) 
summary(SPled75freq)
standardCoefs(SPled75freq)


##76 (n=98) gotten a flu shot
LifeEventsdata$led76 <- as.factor(LifeEventsdata$Life_Data_76)
levels(LifeEventsdata$led76)[levels(LifeEventsdata$led76)=="2"] <-"0"
SPled76Model <- glm(led76~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled76Model)
exp(coef(SPled76Model))
waldtest(SPled76Model)
odds.ratio(SPled76Model, level=0.95)
#frequency
LifeEventsdata$led76freq <- LifeEventsdata$Life_Data_6_Yr_Y_N
SPled76freq= lm(led76freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led76freq!=0,]) 
summary(SPled76freq)
standardCoefs(SPled76freq)


##77 gotten a patent on an idea or invention
LifeEventsdata$led77 <- as.factor(LifeEventsdata$Life_Data_77)
levels(LifeEventsdata$led77)[levels(LifeEventsdata$led77)=="2"] <-"0"
SPled77Model <- glm(led77~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled77Model)
exp(coef(SPled77Model))
waldtest(SPled77Model)
odds.ratio(SPled77Model, level=0.95)
#frequency
LifeEventsdata$led77freq <- LifeEventsdata$Life_Data_50_Freq
SPled77freq= lm(led77freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led77freq!=0,]) 
summary(SPled77freq)
standardCoefs(SPled77freq)


##78 (n=56) commented on an online blog, forum, or news article
LifeEventsdata$led78 <- as.factor(LifeEventsdata$Life_Data_78)
levels(LifeEventsdata$led78)[levels(LifeEventsdata$led78)=="2"] <-"0"
SPled78Model <- glm(led78~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled78Model)
exp(coef(SPled78Model))
waldtest(SPled78Model)
odds.ratio(SPled78Model, level=0.95)
#frequency
LifeEventsdata$led78freq <- LifeEventsdata$Life_Data_20_WkFreq
SPled78freq= lm(led78freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led78freq!=0,]) 
summary(SPled78freq)
standardCoefs(SPled78freq)


##79 taken a self-defense class
LifeEventsdata$led79 <- as.factor(LifeEventsdata$Life_Data_79)
levels(LifeEventsdata$led79)[levels(LifeEventsdata$led79)=="2"] <-"0"
SPled79Model <- glm(led79~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled79Model)
exp(coef(SPled79Model))
waldtest(SPled79Model)
odds.ratio(SPled79Model, level=0.95)
#frequency
LifeEventsdata$led79freq <- LifeEventsdata$Life_Data_7_Yr_Y_N
SPled79freq= lm(led79freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led79freq!=0,]) 
summary(SPled79freq)
standardCoefs(SPled79freq)


##80 (n=56) commented on an online blog, forum, or news article
LifeEventsdata$led80 <- as.factor(LifeEventsdata$Life_Data_80)
levels(LifeEventsdata$led80)[levels(LifeEventsdata$led80)=="2"] <-"0"
SPled80Model <- glm(led80~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled80Model)
exp(coef(SPled80Model))
waldtest(SPled80Model)
odds.ratio(SPled80Model, level=0.95)
#frequency
LifeEventsdata$led80freq <- LifeEventsdata$Life_Data_51_Freq
SPled80freq= lm(led80freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led80freq!=0,]) 
summary(SPled80freq)
standardCoefs(SPled80freq)

##81 been skydiving
LifeEventsdata$led81 <- as.factor(LifeEventsdata$Life_Data_81)
levels(LifeEventsdata$led81)[levels(LifeEventsdata$led81)=="2"] <-"0"
SPled81Model <- glm(led81~FSM_SelfProtect,LifeEventsdata,family="binomial")
summary(SPled81Model)
exp(coef(SPled81Model))
waldtest(SPled81Model)
odds.ratio(SPled81Model, level=0.95)
#frequency
LifeEventsdata$led81freq <- LifeEventsdata$Life_Data_52_Freq
SPled81freq= lm(led81freq~FSM_SelfProtect,LifeEventsdata[LifeEventsdata$led81freq!=0,]) 
summary(SPled81freq)
standardCoefs(SPled81freq)


##82 kept a gun in your home
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