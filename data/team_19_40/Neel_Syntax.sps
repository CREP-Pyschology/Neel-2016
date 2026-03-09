* Encoding: UTF-8.
* reversing all the items that have to be reversed

*Fundamental social motives Q40

DATASET ACTIVATE DataSet1.
RECODE Q40Idonotworryaboutkeepingmyselfsafefromothers (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q40_45_R.

RECODE Q40Idonotworryverymuchaboutgettinggermsfromothers (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q40_52_R.
RECODE Q40Whensomeonenearmeissickitdoesnâ€™tbothermeverymuch (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q40_53_R.
RECODE Q40Idonâ€™tmindbeingaroundpeoplewhoaresick (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q40_54_R.

RECODE Q40Workinginagroupisusuallymoretroublethanitâ€™sworth (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q40_24_R.

RECODE Q40Idonotworryverymuchaboutlosingstatus (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q40_14_R.

RECODE Q40Iamnotinterestedinmeetingpeopletoflirtwithordate (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q40_3_R.
RECODE Q40Startinganewromanticsexualrelationshipisnotahighpriorityforme (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q40_4_R.
RECODE Q40Irarelythinkaboutfindingaromanticorsexualpartner (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q40_5_R.

RECODE Q53Idonotspendmuchtimeandenergydoingthingstokeepmypartnerinveste (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q53_57_R.
RECODE Q53ItwouldnotbethatbigadealtomeifmypartnerandIbrokeup (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q53_66_R.
RECODE Q53Ifotherswereromanticallyinterestedinmypartneritwouldnotbother (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q53_67_R.
RECODE Q53Ifmypartnerweretohaveromanticorsexualrelationshipswithotherst (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q53_68_R.

RECODE Q40Havingclosetiestomyfamilyisnotveryimportanttome (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q40_37_R.
RECODE Q40Iamnotveryinterestedinhelpingmyfamilymembers (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q40_38_R.
RECODE Q40Iwouldrathernotspendtimewithfamilymembers (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q40_39_R.

RECODE Q56Takingcareofmychildrenisnotahighpriorityformerightnow (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q56_71_R.
RECODE Q56Irarelythinkaboutprotectingmychildren (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO Q40_73_R.

EXECUTE.

COMPUTE Self_Protection=mean(Q40Ithinkalotabouthowtostaysafefromdangerouspeople,Q40Iammotivatedtokeepmyselfsafefromothers,Q40_45_R,
Q40Iworryaboutdangerouspeople,Q40Ithinkabouthowtoprotectmyselffromdangerouspeople,Q40Iammotivatedtoprotectmyselffromdangerousothers).
COMPUTE Disease_Avoid=mean(Q40Iavoidplacesandpeoplethatmightcarrydiseases,Q40Iavoidpeoplewhomighthaveacontagiousillness,
Q40Iworryaboutcatchingcoldsandflufromtoomuchcontactwithotherpeop,Q40_52_R,Q40_53_R,Q40_54_R).
COMPUTE Affiliation_group=mean(Q40Gettingalongwiththepeoplearoundmeisahighpriority, Q40Beingpartofagroupisimportanttome, Q40Ienjoyworkingwithagrouptoaccomplishagoal, 
Q40Ilikebeingpartofateam, Q40_24_R, Q40WhenIâ€™minagroupIdothingstohelpthegroupstaytogether).
COMPUTE Affiliation_Exclusion=mean(Q40Iwouldbeextremelyhurtifafriendexcludedme, Q40Itwouldbeabigdealtomeifagroupexcludedme, Q40ItbothersmewhengroupsofpeopleIknowdothingswithoutme,
 Q40Iworryaboutbeingrejected, Q40IoftenwonderwhetherIambeingexcluded, Q40Ioftenthinkaboutwhetherotherpeopleacceptme).
COMPUTE Affiliation_Independence=mean(Q40Iwouldprefertospendtimealonethantobesurroundedbyotherpeople, Q40IliketobealoneevenifImightlosesomefriendsbecauseofit, 
Q40Beingapartfrommyfriendsforlongperiodsoftimedoesnotbotherme, Q40Idontmindbeingbymyselfforlongperiodsoftime, Q40Havingtimealoneisextremelyimportanttome, Q40Iliketobebymyself).
COMPUTE Status=mean(Q40Itâ€™simportanttomethatotherpeoplelookuptome, Q40Iwanttobeinapositionofleadership, Q40Itsimportanttomethatothersrespectmyrankorposition, 
Q40IdothingstoensurethatIdonâ€™tlosethestatusIhave, Q40Idonotlikebeingatthebottomofahierarchy, Q40_14_R).
COMPUTE Mateseeking=mean(Q40Ispendalotoftimethinkingaboutwaystomeetpossibledatingpartners, Q40Iaminterestedinfindinganewromanticsexualpartner, 
Q40_3_R, Q40_4_R, Q40_5_R, Q40Iwouldliketofindanewromanticsexualpartnersoon).
COMPUTE MateRetention_Gen = mean(Q53Itisimportanttomethatmypartnerissexuallyloyaltome, Q53Itisimportanttomethatmypartnerisemotionallyloyaltome,
 Q53_57_R, Q53_66_R, Q53_67_R, Q53_68_R).
COMPUTE MateRetention_Breakup = mean(Q53Idonotspendmuchtimeandenergydoingthingstokeepmypartnerinveste, Q53Ioftenthinkaboutwhethermypartnerwillleaveme,
 Q53Iworryaboutothersstealingmyromanticsexualpartner, Q53Igetjealousifotherspayattentiontomyromanticsexualpartner, Q53Iworrythatotherpeopleareinterestedinmyromanticsexualpartner,
 Q53IamworriedthatmypartnerandImightbreakup).
COMPUTE Kin_Family=mean(Q40Caringforfamilymembersisimportanttome, Q40_37_R, Q40_38_R, Q40_39_R, Q40Beingclosetomyfamilymembersisextremelyimportanttome,
 Q40Itisextremelyimportanttometohavegoodrelationshipswithmyfamily).
COMPUTE Kin_Children=mean(Q56Ihelptakecareofmychildren, Q56Iliketospendtimewithmychildren, Q56_71_R, Q56IoftenthinkabouthowIcouldstopbadthingsfromhappeningtomychildr, 
Q40_73_R, Q56Providingformychildrenisimportanttome).
EXECUTE.

*Daily experiences
*NOT SURE IF I WILL DO THIS ONE, HAVE TO CODE WITH TWO RATERS TO SEE WHICH GROUPS PEOPLE MENTION

*SOI recoding.

RECODE Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_F (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q_7R.
RECODE Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_J (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q_10R.
RECODE Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_K (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q_11R.
RECODE Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_L (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q_12R.
RECODE Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_I (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q_13R.
RECODE Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_O (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q_16R.
RECODE Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_Q (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q_18R.
EXECUTE.

COMPUTE SOI=mean(Q_7R,Q_10R,Q_11R,Q_12R,Q_13R,Q_16R,Q_18R, Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_A,Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_B,
Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_C,Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_D, Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_E,
Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_G,Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_H, Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_M,
Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_N, Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_P, Pleaseratehowthefollowingstatementsapplytoyoufromstronglydisag_R).
EXECUTE.

COMPUTE PerceivedVulnToDisease=
    mean(Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgust,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_A,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_B,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_C,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_D,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_E,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_F,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_G,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_H,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_I,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_J,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_K,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_L,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_M,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_N,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_O,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_P,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_Q,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_R,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_S,
    Thefollowingitemsdescribeavarietyofconcepts.Pleaseratehowdisgu_T).
EXECUTE.

*Dominanec and Prestige

RECODE Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_A (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q99_2_R.
RECODE Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_E (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q99_6_R.
RECODE Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_I (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q99_10_R.
RECODE Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_K (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q99_12_R.
RECODE Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_P (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q99_17_R.
EXECUTE.

COMPUTE Dominance=
    mean(Q99_10_R, Q99_12_R,Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_B,
Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_D,
Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_F,
Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_H,
Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_J,
Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_O).


COMPUTE Prestige=
    mean(Q99_2_R, Q99_6_R,Pleaseindicatetheextenttowhicheachstatementaccuratelydescribesyo,
Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_C,
Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_G,
Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_L,
Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_M,
Pleaseindicatetheextenttowhicheachstatementaccuratelydescribes_N,
Q99_17_R).

*BIg 5

RECODE Iamsomeonewho...6.Isreserved (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_6R.
RECODE Iamsomeonewho...21.Tendstobequiet (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_21R.
RECODE Iamsomeonewho...31.Issometimesshyinhibited (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_31R.
RECODE Iamsomeonewho...2.Tendstofindfaultwithothers (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_2R.
RECODE Iamsomeonewho...12.Startsquarrelswithothers (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_12R.
RECODE Iamsomeonewho...27.Canbecoldandaloof (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_27R.
RECODE Iamsomeonewho...37.Issometimesrudetoothers (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_37R.
RECODE Iamsomeonewho...8.Canbesomewhatcareless (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_8R.
RECODE Iamsomeonewho...18.Tendstobedisorganized (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_18R.
RECODE Iamsomeonewho...23.Tendstobelazy (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_23R.
RECODE Iamsomeonewho...28.Perseveresuntilthetaskisfinished (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_28R.
RECODE Iamsomeonewho...43.Iseasilydistracted (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_43R.
RECODE Iamsomeonewho...35.Prefersworkthatisroutine (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_35R.
RECODE Iamsomeonewho...41.Hasfewartisticinterests (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_41R.
RECODE Iamsomeonewho...9.Isrelaxedhandlesstresswell (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_9R.
RECODE Iamsomeonewho...24.Isemotionallystable (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_24R.
RECODE Iamsomeonewho...34.Remainscalmintensesituations (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q60_34R.
EXECUTE.

COMPUTE Extraversion=
    mean(Iamsomeonewho...1.Istalkative, Q60_6R,
Iamsomeonewho...11.Isfullisenergy,
Iamsomeonewho...16.Generatesalotofenthusiasm, Q60_21R,
Iamsomeonewho...26.Hasanassertivepersonality, Q60_31R,
Iamsomeonewho...36.Isoutgoingsociable).

COMPUTE Agreeableness=
    mean(Q60_2R,
Iamsomeonewho...7.Ishelpfulandunselfishwithothers, Q60_12R,
Iamsomeonewho...17.Hasaforgivingnature, Iamsomeonewho...22.Isgenerallytrusting,Q60_27R,
Iamsomeonewho...32.Isconsiderateandkindtoalmosteveryone,Q60_37R,
Iamsomeonewho...42.Likestocooperatewithothers).

COMPUTE Conscientiousness=
    mean(Iamsomeonewho...3.Doesathoroughjob, Iamsomeonewho...13.Isareliableworker, Iamsomeonewho...33.Doesthingsefficiently,
Iamsomeonewho...38.Makesplansandfollowsthroughwiththem,Q60_8R,Q60_18R,Q60_23R, Q60_28R, Q60_43R).

COMPUTE Openness=
    mean(Iamsomeonewho...5.Isoriginalcomesupwithnewideas,Iamsomeonewho...10.Iscuriousaboutmanydifferentthings,
Iamsomeonewho...15.Isingeniousadeepthinker,Iamsomeonewho...20.Hasanactiveimagination,Iamsomeonewho...25.Isinventive,
Iamsomeonewho...30.Valuesartisticaestheticexperience,Iamsomeonewho...40.Likestoreflectplaywithideas,Iamsomeonewho...44.Issophisticatedinartmusicorliterature,
Q60_35R,Q60_41R).

COMPUTE Neuroticism=
    mean(Iamsomeonewho...4.Isdepressedblue,Iamsomeonewho...14.Canbetense,Iamsomeonewho...19.Worriesalot,
Iamsomeonewho...29.Canbemoody,Iamsomeonewho...39.Getsnervouseasily,Q60_9R,Q60_24R,Q60_34R).
EXECUTE.

*Avoidance and anxiety subscales 

RECODE Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_H (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q100_9_R.
RECODE Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_J (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q100_11_R.
RECODE Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_U (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q100_22_R.
RECODE Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_Y (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q100_26_R.
RECODE Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_Z (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q100_27_R.
RECODE Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelat_AA (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q100_28_R.
RECODE Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelat_AB (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q100_29_R.
RECODE Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelat_AC (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q100_30_R.
RECODE Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelat_AD (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q100_31_R.
RECODE Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelat_AF (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q100_33_R.
RECODE Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelat_AG (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q100_34_R.
RECODE Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelat_AH (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q100_35_R.
RECODE Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelat_AI (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q100_36_R.

EXECUTE.

COMPUTE Anxiety=mean(Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelation,Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_A,
Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_B,Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_C,
Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_D,Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_E,
Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_F,Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_G,
Q100_9_R, Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_I,
Q100_11_R,Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_K,
Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_L, Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_M,
Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_N, Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_O,
Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_P, Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_Q).

COMPUTE Avoidance=mean(Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_R, Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_S,
Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_T, Q100_22_R,
Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_V,Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_W,
Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelati_X, Q100_26_R,
Q100_27_R, Q100_28_R, Q100_29_R, Q100_30_R, Q100_31_R, 
Thestatementsbelowconcernhowyoufeelinemotionallyintimaterelat_AE,
Q100_33_R,
Q100_34_R,
Q100_35_R,
Q100_36_R).
EXECUTE.

*need to belong

RECODE Foreachofthestatementsbelowindicatethedegreetowhichyouagreeordis (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q101_1R.
RECODE Foreachofthestatementsbelowindicatethedegreetowhichyouagreeord_B (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q101_3R.
RECODE Foreachofthestatementsbelowindicatethedegreetowhichyouagreeord_F (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q101_7R.
EXECUTE.

COMPUTE NeedToBelong=mean(Q101_1R,
Foreachofthestatementsbelowindicatethedegreetowhichyouagreeord_A,
Q101_3R,
Foreachofthestatementsbelowindicatethedegreetowhichyouagreeord_C,
Foreachofthestatementsbelowindicatethedegreetowhichyouagreeord_D,
Foreachofthestatementsbelowindicatethedegreetowhichyouagreeord_E,
Q101_7R,
Foreachofthestatementsbelowindicatethedegreetowhichyouagreeord_G,
Foreachofthestatementsbelowindicatethedegreetowhichyouagreeord_H,Foreachofthestatementsbelowindicatethedegreetowhichyouagreeord_I
).
EXECUTE.

*BDW

RECODE Foreachofthefollowingstatementsratetheextenttowhichyouagreeord_P (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q102_2_R.
RECODE Foreachofthefollowingstatementsratetheextenttowhichyouagreeord_R (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q102_4_R.
RECODE Foreachofthefollowingstatementsratetheextenttowhichyouagreeord_S (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q102_5_R.
RECODE Foreachofthefollowingstatementsratetheextenttowhichyouagreeord_U (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q102_7_R.
RECODE Foreachofthefollowingstatementsratetheextenttowhichyouagreeord_W (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q102_9_R.
RECODE Foreachofthefollowingstatementsratetheextenttowhichyouagreeord_Z (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q102_12_R.
EXECUTE.

COMPUTE BDW=mean(Foreachofthefollowingstatementsratetheextenttowhichyouagreeord_O,
Foreachofthefollowingstatementsratetheextenttowhichyouagreeord_Q,
Foreachofthefollowingstatementsratetheextenttowhichyouagreeord_T,
Foreachofthefollowingstatementsratetheextenttowhichyouagreeord_V,
Foreachofthefollowingstatementsratetheextenttowhichyouagreeord_X,
Foreachofthefollowingstatementsratetheextenttowhichyouagreeord_Y,
Q102_2_R,
Q102_4_R,
Q102_5_R,
Q102_7_R,
Q102_9_R,
Q102_12_R
).
EXECUTE.

*contrast codes

IF  (Doyouhavechildren=1) NonParent_Parent=-1.
IF  (Doyouhavechildren=2) NonParent_Parent=1.
EXECUTE.

IF  (GenderImostlyidentifyas=1) M_F=-1.
IF  (GenderImostlyidentifyas=2) M_F=1.
EXECUTE.

IF  (WhatisyourcurrentrelationshipstatusSelectedChoice=1) Single_Rel=1.
IF  (WhatisyourcurrentrelationshipstatusSelectedChoice=2) Single_Rel=1.
IF  (WhatisyourcurrentrelationshipstatusSelectedChoice=3) Single_Rel=1.
IF  (WhatisyourcurrentrelationshipstatusSelectedChoice=4) Single_Rel=1.
IF  (WhatisyourcurrentrelationshipstatusSelectedChoice=5) Single_Rel=-1.
EXECUTE.

*Making life history variables

*Childhood stability

RECODE Comparedtotheaveragepersonhowstablewasyourhomelifewhenyouweregro (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q129_R.
RECODE Comparedtotheaveragepersonhowpredictablewasyourhomelifewhenyouwe (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q130_R.
RECODE Comparedtotheaveragepersonhowhardwasyourhomelifewhenyouweregrowi (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q131_R.
EXECUTE.

COMPUTE ChildhoodStability=mean(
Q129_R,
Q130_R,
Q131_R).
EXECUTE.


*childhood resources

RECODE Pleaserateyouragreementwiththefollowingstatementsonascalefrom1_C (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO Q16_4_R.
EXECUTE.

COMPUTE ChildhoodResources=mean(
Pleaserateyouragreementwiththefollowingstatementsonascalefrom1st,
Pleaserateyouragreementwiththefollowingstatementsonascalefrom1_A,
Pleaserateyouragreementwiththefollowingstatementsonascalefrom1_B,
Q16_4_R).
EXECUTE.

*Current resources

COMPUTE CurrentResources=mean(Pleaserateyouragreementwiththefollowingstatementsonascalefrom1_E,
Pleaserateyouragreementwiththefollowingstatementsonascalefrom1_F).
EXECUTE.

*BIS BAS Calculation

RECODE Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_A (1=4) (2=3) (3=2) (4=1) INTO Q138_2_R.
RECODE Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_U (1=4) (2=3) (3=2) (4=1) INTO Q138_22_R.
EXECUTE.

COMPUTE BAS_Drive=mean(Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_B,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_H,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_K,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_T
).
COMPUTE BAS_FunSeeking=mean(Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_D,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_I,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_S,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_N
).
COMPUTE BAS_RewardResponse=mean(Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_C,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_F,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_M,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_Q,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_V
).
COMPUTE BIS=mean(Q138_2_R,Q138_22_R,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_G,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_L,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_O,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_R,
Respondtoeachitemasifitweretheonlyitem.Thatisdontworryaboutbei_W
).
EXECUTE.


*ANALYSES
*Be aware it looks like SOI should be broken into two subcomponents but I dont currently have the scoring to do this

CORRELATIONS
  /VARIABLES= Self_Protection Disease_Avoid Affiliation_group Affiliation_Exclusion 
    Affiliation_Independence Status Mateseeking MateRetention_Gen MateRetention_Breakup Kin_Family 
    Kin_Children BDW PerceivedVulnToDisease NeedToBelong Dominance Prestige SOI Avoidance Anxiety
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=Self_Protection Disease_Avoid Affiliation_group Affiliation_Exclusion 
    Affiliation_Independence Status Mateseeking MateRetention_Gen MateRetention_Breakup Kin_Family 
    Kin_Children Extraversion Agreeableness Conscientiousness Neuroticism Openness
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

*Regressions


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Self_Protection
  /METHOD=ENTER Whatisyourageinyears NonParent_Parent M_F Single_Rel CurrentResources ChildhoodResources ChildhoodStability.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Disease_Avoid
  /METHOD=ENTER Whatisyourageinyears NonParent_Parent M_F Single_Rel CurrentResources ChildhoodResources ChildhoodStability.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Affiliation_group
  /METHOD=ENTER Whatisyourageinyears NonParent_Parent M_F Single_Rel CurrentResources ChildhoodResources ChildhoodStability.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Affiliation_Exclusion
  /METHOD=ENTER Whatisyourageinyears NonParent_Parent M_F Single_Rel CurrentResources ChildhoodResources ChildhoodStability.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Affiliation_Independence
  /METHOD=ENTER Whatisyourageinyears NonParent_Parent M_F Single_Rel CurrentResources ChildhoodResources ChildhoodStability.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Status
  /METHOD=ENTER Whatisyourageinyears NonParent_Parent M_F Single_Rel CurrentResources ChildhoodResources ChildhoodStability.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Mateseeking
  /METHOD=ENTER Whatisyourageinyears NonParent_Parent M_F Single_Rel CurrentResources ChildhoodResources ChildhoodStability.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT MateRetention_Gen
  /METHOD=ENTER Whatisyourageinyears NonParent_Parent M_F Single_Rel CurrentResources ChildhoodResources ChildhoodStability.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT MateRetention_Breakup
  /METHOD=ENTER Whatisyourageinyears NonParent_Parent M_F Single_Rel CurrentResources ChildhoodResources ChildhoodStability.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Kin_Family
  /METHOD=ENTER Whatisyourageinyears NonParent_Parent M_F Single_Rel CurrentResources ChildhoodResources ChildhoodStability.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Kin_Children
  /METHOD=ENTER Whatisyourageinyears NonParent_Parent M_F Single_Rel CurrentResources ChildhoodResources ChildhoodStability.
