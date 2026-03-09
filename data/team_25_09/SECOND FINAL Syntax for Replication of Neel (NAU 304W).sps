* Encoding: UTF-8.


*Change Gender, Ethnicity, Political Affiliation, Religious Affiliation,  Scale of Measurement to Nominal,  Basically anything that shouldn't have a mean calculated needs to be Nominal
    Change Age from String to Numeric, and Change Scale from Nominal to Scale
    



*Reverse score SP3, DA4-6, AG4R

DATASET ACTIVATE DataSet1.
RECODE Q3_45 Q3_52 Q3_53 Q3_54  Q3_24 (1=7) (2=6) (3=5) (4=4) (5=3) (6=4) (7=1) INTO Q3_45R Q3_52R Q3_53R Q3_24R Q3_54R.
EXECUTE.

*Reverse Score Status

RECODE Q3_14 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q3_14R.
EXECUTE.

*Reverse Score Mate Seeking
  
RECODE Q3_3 Q3_4 Q3_5 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q3_3R Q3_4R Q3_5R.
EXECUTE.

*Reverse Score Mate Retention (General)
 
RECODE Q3_57 Q3_65 Q3_66 Q3_67 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q3_57R Q3_65R Q3_66R 
    Q3_67R.
EXECUTE.

*Recoding Childhood Stability

RECODE QID55 QID56 QID57 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO QID55R QID56R QID57R.
EXECUTE.

*FSMI

*Creating Self-Protection scale

COMPUTE SP_FSM=MEAN(Q3_43,Q3_44,Q3_45R,Q3_46,Q3_47,Q3_48).
EXECUTE.


*Creating Disease Avoidance Scale

COMPUTE DA_FSM=MEAN(Q3_49,Q3_50,Q3_51,Q3_52R,Q3_53R,Q3_54R).
EXECUTE.

*Creating Affiliation (Group) Scale 

COMPUTE AG_FSM=MEAN(Q3_21,Q3_22,Q3_23,Q3_24R,Q3_27,Q3_15).
EXECUTE.

*Creating Affiliation (Exclusion Concern) Scale
    
COMPUTE AEC_FSM=MEAN(Q3_16, Q3_25,Q3_26,Q3_28,Q3_29,Q3_32).
EXECUTE.  

*Creating Affiliation (Independence) Scale

COMPUTE AI_FSM=MEAN(Q3_18,Q3_19,Q3_20,Q3_33,Q3_34,Q3_35).
EXECUTE.

*Creating Status Scale
  
COMPUTE Status_FSM=MEAN(Q3_18, Q3_14R, Q3_19,Q3_20,Q3_33,Q3_34,Q3_35).
EXECUTE.

*Creating Mate Seeking Scale

COMPUTE MateSeeking_FSM=MEAN(Q3_1, Q3_2, Q3_3R, Q3_4R, Q3_5R, Q3_6).
EXECUTE.

*Creating Mate Retention (General) Scale

COMPUTE MRGeneral_FSM=MEAN(Q3_55,Q3_56, Q3_57R, Q3_65R, Q3_66R, Q3_67R).
EXECUTE.

*Creating Mate Retention (Breakup Concern) Scale

COMPUTE MRBreakupConcern_FSM=MEAN(Q3_58, Q3_59, Q3_60, Q3_62, Q3_63, Q3_64).
EXECUTE.


*Creating Childhood Stability Scale
    
COMPUTE Childhood_Stability=MEAN(QID55R,QID56R, QID57R).
EXECUTE.

*Creating Current Resources Scale

COMPUTE Current_Resources=MEAN(QID54_5,QID54_6,QID54_7,QID54_8).
EXECUTE.

*Childhood Resources Stuff, was having issues so recoding and scale is all together

RECODE QID54_4 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO QID54_4R.
EXECUTE.

COMPUTE Childhood_Resources=MEAN(QID54_1,QID54_2, QID54_3, QID54_4R).
EXECUTE.

*Contrast Codes
 
*Parental Status, -1 is if have children

IF  (Q1=1) Parental_Status=-1.
IF  (Q1=2) Parental_Status=1.
EXECUTE.

*Gender, 1 is Female

IF  (QID36=1) Sex=-1.
IF  (QID36=2) Sex=1.
EXECUTE.

*Relationship Status
    
IF  (Q2=1) Relationship_Status=1.
IF  (Q2=2) Relationship_Status=1.
IF  (Q2=3) Relationship_Status=1.
IF  (Q2=4) Relationship_Status=1.
IF  (Q2=5) Relationship_Status=-1.

*Age
 
COMPUTE Age=QID41.
EXECUTE.



*Regression Analyses
    


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT AG_FSM
  /METHOD=ENTER Age Sex Relationship_Status Childhood_Stability Childhood_Resources Current_Resources.
  
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT AEC_FSM
  /METHOD=ENTER Age Sex Relationship_Status Childhood_Stability Childhood_Resources Current_Resources.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT AI_FSM
  /METHOD=ENTER Age Sex Relationship_Status Childhood_Stability Childhood_Resources Current_Resources.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT MateSeeking_FSM
  /METHOD=ENTER Age Sex Relationship_Status Childhood_Stability Childhood_Resources Current_Resources.




REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT MRGeneral_FSM
  /METHOD=ENTER Age Sex Childhood_Stability Childhood_Resources Current_Resources.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT MRBreakupConcern_FSM
  /METHOD=ENTER Age Sex Childhood_Stability Childhood_Resources Current_Resources.



























DATASET ACTIVATE DataSet1.
DESCRIPTIVES VARIABLES=Relationship_Status Parental_Status MateSeeking_FSM MRGeneral_FSM 
    MRBreakupConcern_FSM
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.

CORRELATIONS
  /VARIABLES=Relationship_Status MRBreakupConcern_FSM MRGeneral_FSM MateSeeking_FSM Parental_Status
  /PRINT=TWOTAIL NOSIG FULL
  /MISSING=PAIRWISE.


*Just selects people in a relationship

USE ALL.
COMPUTE filter_$=(Relationship_Status = 1).
VARIABLE LABELS filter_$ 'Relationship_Status = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.


  
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT MRGeneral_FSM
  /METHOD=ENTER Age Sex Relationship_Status Childhood_Stability Childhood_Resources Current_Resources.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT MRBreakupConcern_FSM
  /METHOD=ENTER Age Sex Relationship_Status Childhood_Stability Childhood_Resources Current_Resources.



EXECUTE.


*Removed six participants for incomplete responses
*Removed two responses from 134.114.101.122 for duplicate
*Removed one response from 134.114.101.143 for duplicate
*Removed one response from 134.114.101.50 for duplicate and extreme duration
*Removed one response from 47.215.150.137 for duplicate
*Removed 11 responses for indicating parental status with no children, suspicious durations, etc

DESCRIPTIVES VARIABLES=Age Sex
  /STATISTICS=MEAN STDDEV MIN MAX KURTOSIS SKEWNESS.
