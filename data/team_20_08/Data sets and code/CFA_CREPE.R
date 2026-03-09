#Confirmatory Factor Analyses of FSM data
library(lavaan)

######################## Dataset Preparation ########################
#Loading data set
cfaData=read.csv("FSMDataFull.csv")
a=which(names(cfaData)=="FMI_1")
cfaData=cfaData[,a:(a+65)]

#recoding reverse items
cfaData$FMI_3=8-cfaData$FMI_3
cfaData$FMI_10=8-cfaData$FMI_10
cfaData$FMI_11=8-cfaData$FMI_11
cfaData$FMI_12=8-cfaData$FMI_12
cfaData$FMI_16=8-cfaData$FMI_16
cfaData$FMI_36=8-cfaData$FMI_36
cfaData$FMI_39=8-cfaData$FMI_39
cfaData$FMI_40=8-cfaData$FMI_40
cfaData$FMI_41=8-cfaData$FMI_41
cfaData$FMI_45=8-cfaData$FMI_45
cfaData$FMI_46=8-cfaData$FMI_46
cfaData$FMI_47=8-cfaData$FMI_47
cfaData$FMI_48=8-cfaData$FMI_48
cfaData$FMI_56=8-cfaData$FMI_56
cfaData$FMI_57=8-cfaData$FMI_57
cfaData$FMI_58=8-cfaData$FMI_58
cfaData$FMI_63=8-cfaData$FMI_63
cfaData$FMI_65=8-cfaData$FMI_65

######################## Model 1 ######################## 
#includes all 11 motives

model <- ' SelfProtection =~ FMI_1+FMI_2+FMI_3+FMI_4+FMI_5+FMI_6 
           DiseaseAvoidance =~ FMI_7+FMI_8+FMI_9+FMI_10+FMI_11+FMI_12
           AffiliationGroup =~ FMI_13+FMI_14+FMI_15+FMI_16+FMI_17+FMI_18
           AffiliationExclusion =~ FMI_19+FMI_20+FMI_21+FMI_22+FMI_23+FMI_24
           AffiliationIndependence =~ FMI_25+FMI_26+FMI_27+FMI_28+FMI_29+FMI_30 
           Status =~ FMI_31+FMI_32+FMI_33+FMI_34+FMI_35+FMI_36
           MateSeeking =~ FMI_37+FMI_38+FMI_39+FMI_40+FMI_41
           MateRetentionGeneral =~ FMI_43+FMI_44+FMI_45+FMI_46+FMI_47+FMI_48
           MateRetentionBreakup =~ FMI_49+FMI_50+FMI_51+FMI_52+FMI_53+FMI_54 
           KinCareFamily =~ FMI_55+FMI_56+FMI_57+FMI_58+FMI_59+FMI_60
           KinCareChild =~ FMI_61+FMI_62+FMI_63+FMI_64+FMI_65+FMI_66 '

fit <- cfa(model, data=cfaData) #not working sample size too small

summary(fit, fit.measures=TRUE, standardized=TRUE)

############################ Model 1 ############################
#Without demographic specific sub-scales 
#(KinCareChild, MateRetentionGeneral, MateRetentionBreakup)

model2 <- ' SelfProtection =~ FMI_1+FMI_2+FMI_3+FMI_4+FMI_5+FMI_6 
           DiseaseAvoidance =~ FMI_7+FMI_8+FMI_9+FMI_10+FMI_11+FMI_12
           AffiliationGroup =~ FMI_13+FMI_14+FMI_15+FMI_16+FMI_17+FMI_18
           AffiliationExclusion =~ FMI_19+FMI_20+FMI_21+FMI_22+FMI_23+FMI_24
           AffiliationIndependence =~ FMI_25+FMI_26+FMI_27+FMI_28+FMI_29+FMI_30 
           Status =~ FMI_31+FMI_32+FMI_33+FMI_34+FMI_35+FMI_36
           MateSeeking =~ FMI_37+FMI_38+FMI_39+FMI_40+FMI_41
           KinCareFamily =~ FMI_55+FMI_56+FMI_57+FMI_58+FMI_59+FMI_60 '

fit2 <- cfa(model2, data=cfaData) #this works because we kept the full sample size

summary(fit2, fit.measures=TRUE, standardized=TRUE) #Does not meet the bench marks
#χ²(1006)=1734, p<.001
#RMSEA = .071, TLI = .839, CFI = .85, SRMR = .083 


########################## Neel Benchmarks ##########################
# (RMSEA) < .06 indicates good fit, < .10 indicates acceptable fit  #
# non-normed fit index (NNFI)/(TLI) > .95                           #
# confirmatory fit index (CFI) > .95                                #
# standardized root-mean-residual (SRMR) < .08                      #
#####################################################################