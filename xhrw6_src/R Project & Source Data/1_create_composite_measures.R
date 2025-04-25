# Create Composite Measures----------------------------------------------------------------------------------------------------------------------
# In this section, we'll calculate composite scores for our primary measures (e.g., EDNH, Critical Reflection, Discrimination, etc.).

# ~ Low-Status Demographic characteristics (additive measure and binary indicator)---- 
Measures <- Measures %>%
        mutate(LowStatus_Gender = case_when( (Trans == 1 | Gender == 3) ~ 1, TRUE ~ 0)) %>%
        mutate(LowStatus_Race = case_when(Race != 2 ~ 1, TRUE ~ 0)) %>%
        mutate(LowStatus_Ethn = case_when(Ethnicity == 1 ~ 1, TRUE ~ 0)) %>%
        mutate(LowStatus_Disab = case_when(Disability == 1 ~ 1, TRUE ~ 0)) %>%
        mutate(LowStatus_Sexual = case_when(Sexual %in% (2:4) ~ 1, TRUE ~ 0)) %>%
        mutate(LowStatus_SES = case_when( (Income_Current == 1 & HH_Size > 1 & Ed_Mom < 3 & Ed_Dad < 3) | 
                                                  (Agegroup > 2 & Ed_You < 3 & Ed_Mom < 3 & Ed_Dad < 3) |
                                                  (Income_Current == 1 & HH_Size > 2) ~ 1, TRUE ~ 0)
        ) %>%
        mutate(LowStatus = structure(LowStatus_Gender + LowStatus_Race + LowStatus_Ethn + LowStatus_Disab +
                                             LowStatus_Sexual + LowStatus_SES, label = "Low-Status Demographic Characteristics")) %>%
        mutate(Normative = structure(case_when(LowStatus == 0 ~ 1, LowStatus > 0 ~ 0), label = "Normative Sample Binary Indicator")) %>%
        arrange(desc(LowStatus))

sjmisc::frq(Measures$LowStatus, out = "v")
ggplot(Measures, aes(LowStatus)) + geom_histogram(binwidth=1)
sjmisc::descr(Measures, Measures$LowStatus, out = "v")  

# The following is a nifty way to visualize the overlapping sets (since a Venn diagram with six sets is basically useless)
Input <- Measures %>% select(id, LowStatus_Gender,LowStatus_Race,LowStatus_Ethn,LowStatus_Disab,LowStatus_Sexual,LowStatus_SES) %>%
              rename(Gender= LowStatus_Gender, Race = LowStatus_Race, Ethnicity = LowStatus_Ethn, Disability = LowStatus_Disab,
                     Sexuality = LowStatus_Sexual, SES = LowStatus_SES)
Input <- as.data.frame(lapply(Input[2:7], as.integer))
upset(Input, nsets = 6, number.angles = 30, point.size = 3.5, line.size = 2, order.by = c("freq","degree"), 
      mainbar.y.label = "Intersections", sets.x.label = "Set Size", 
      text.scale = c(1.5, 1.3, 1.5, 1, 2, 2))


# Let's take a look at how/whether COVID is interacting with the income of who I've designated as "low SES"
CrossTable(as_factor(Measures$COVID_Income_1), as_factor(Measures$LowStatus_SES), format=c("SPSS"))
CrossTable(as_factor(Measures$COVID_Income_2), as_factor(Measures$LowStatus_SES), format=c("SPSS"))

# ~ EDNh ----
# NOTE: For the EDNh measure, I'm referring to SPSS syntax in a document entitled "Narvaez EDN-History Measure 2020 with syntax.docx".
# It includes the calculation of several subscores, separate negative and positive home climate scores, and an overall composite score.
# NOTE, however, that the document refers to several different versions of the composite score (e.g., some with breastfeeding, some without, etc.)
# After discussion, I decided to go with version #5 (includes emotional abuse, breastfed at all, and organized play).
# 
# Note that all NEGATIVE items in this measure must be reverse-coded.

Measures <- Measures %>%
        mutate(EDNh_Exp_4_R = 6 - EDNh_Exp_4,      # Reverse-code item #4 (corporal punishment)
               EDNh_Exp_5_R = 6 - EDNh_Exp_5) %>%  # Reverse-code item #5 (emotional abuse)
        
        # Create new sub-scales/re-codes
        mutate(EDHhBfed = structure(case_when(EDNh_Breastfed1 == 1 ~ 1, EDNh_Breastfed1 == 2 ~0, EDNh_Breastfed1 == 3 ~ 0), label = "Breastfed at All"),
               EDNhSEmb = structure(rowMeans(select(., EDNh_Exp_1:EDNh_Exp_2), na.rm=TRUE), label = "Social Embeddedness"),
               EDNhTch = structure(rowMeans(select(., EDNh_Exp_3, EDNh_Exp_4_R), na.rm=TRUE), label = "Touch"),
               EDNhFreePlay = structure(rowMeans(select(., EDNh_Exp_7:EDNh_Exp_8), na.rm=TRUE), label = "Free Play"),
               EDNhResp  = structure(rowMeans(select(., EDNh_Exp_9:EDNh_Exp_11), na.rm=TRUE), label = "Responsive Social Environment")) %>% 
        
        # For home (emotional) climate items, re-code values of "99" to missing.
        mutate_at(vars(contains("EDNh_Emotion")), ~ifelse(. == 99, NA, .)) %>%  # (Not sure exactly how this code works, but it does)
        
        # Emotional Home Climate sub-scales 
        # NOTE that we will only reverse-score the negative climate sub-scale in the actual composite calculation
        mutate(EDNPosHC = structure(rowMeans(select(., EDNh_Emotion_7:EDNh_Emotion_10), na.rm=TRUE), label = "Positive Home Climate"),
               EDNNegHC = structure(rowMeans(select(., EDNh_Emotion_1:EDNh_Emotion_6), na.rm=TRUE), label = "Negative Home Climate"),
               EDNNegHC_R = structure(6-(rowMeans(select(., EDNh_Emotion_1:EDNh_Emotion_6), na.rm=TRUE)), label = "Negative Home Climate - Reversed")) %>%
        
        # Finally, calculate the composite score (we're using "EDNhComp5" from the coding document.)
        mutate(EDNhComp = structure(rowSums(select(., EDHhBfed,EDNhSEmb,EDNhTch,EDNhFreePlay,EDNhResp,EDNPosHC,EDNNegHC_R,EDNh_Exp_5_R,EDNh_Exp_6), 
                                            na.rm=TRUE), label = "EDNh Composite"))

sjmisc::descr(Measures, Measures$EDNhComp, out = "v")
ggplot(Measures, aes(EDNhComp)) + geom_histogram(binwidth=1)
psych::alpha(Measures %>% select(EDHhBfed,EDNhSEmb,EDNhTch,EDNhFreePlay,EDNhResp,EDNPosHC,EDNNegHC_R,EDNh_Exp_5_R,EDNh_Exp_6) )


# ~ ACEs ----
# NOTE: Based on looking around, I THINK all one needs to do is add the 10 items up (after recoding 2=0). I found some stuff on Box related to "ACES"
# and the FHHQ, but that didn't seem relevant to this particular measure. But you should check with Darcia to confirm correct coding.

Measures <- Measures %>%
        mutate_at(vars(contains("ACEs")), ~ifelse(. == 2, 0, .)) %>%  # (Not sure exactly how this code works, but it does)
        mutate(ACEs_Sum = structure(rowSums(select(., ACEs_1:ACEs_10), na.rm=TRUE), label = "ACEs Composite (Sum)"))

sjmisc::descr(Measures, Measures$ACEs_Sum, out = "v")
ggplot(Measures, aes(ACEs_Sum)) + geom_histogram(binwidth=1)
psych::alpha(Measures %>% select(contains("ACEs")) %>% select(-ACEs_Sum) )


# ~ Basic Needs - Childhood ----
# For the basic needs scales, the original scale development included two sub-scales, known as "Basic Needs Effectance" (11 items)
# and "Basic Needs Discouragement" (8 items). Darcia has said that prior work has shown that these two sub-scales tend to behave slightly
# differently in regressions and that it is best to consider them separately. However, I also want to examine the effects of individual
# needs within the scale (belonging, purpose, etc.), and because of many of the those would be single items if considered separately 
# (e.g., autonomy effectance), I think I will reverse-score the discouragement items and create mean scores when considering
# each individual need. (You'll notice I had originally calculated effectance and discouragement for individual needs.)
Measures <- Measures %>%
        
        mutate(BNSSh_Eff_Mean = structure(rowMeans(select(., BNSSh_1:BNSSh_11), na.rm=TRUE), label = "Basic Needs History Effectance Composite"),
               BNSSh_Dis_Mean = structure(rowMeans(select(., BNSSh_12:BNSSh_19), na.rm=TRUE), label = "Basic Needs History Discouoragement Composite")) %>%
        
        # mutate(BNSSh_Belonging_Eff = structure(rowMeans(select(., BNSSh_1:BNSSh_2), na.rm=TRUE), label = "Basic Needs History Effectance - Belonging"),
        #        BNSSh_Control_Eff = structure(rowMeans(select(., BNSSh_3:BNSSh_4), na.rm=TRUE), label = "Basic Needs History Effectance - Control"),
        #        BNSSh_Competence_Eff = structure(BNSSh_5, label = "Basic Needs History Effectance - Competence"),
        #        BNSSh_Autonomy_Eff = structure(BNSSh_6, label = "Basic Needs History Effectance - Autonomy"),
        #        BNSSh_Trust_Eff = structure(rowMeans(select(., BNSSh_7:BNSSh_8), na.rm=TRUE), label = "Basic Needs History Effectance - Trust"),
        #        BNSSh_Purpose_Eff = structure(BNSSh_9, label = "Basic Needs History Effectance - Purpose"),
        #        BNSSh_Enhancement_Eff = structure(BNSSh_10, label = "Basic Needs History Effectance - Self-enhancement"),
        #        BNSSh_Satisf_Eff = structure(BNSSh_11, label = "Basic Needs History Effectance - Satisfaction"),
        #        
        #        BNSSh_Belonging_Disc = structure(BNSSh_12, label = "Basic Needs History Discouragement - Belonging"),
#        BNSSh_Control_Disc = structure(BNSSh_13, label = "Basic Needs History Discouragement - Control"),
#        BNSSh_Competence_Disc = structure(BNSSh_14, label = "Basic Needs History Discouragement - Competence"),
#        BNSSh_Autonomy_Disc = structure(BNSSh_15, label = "Basic Needs History Discouragement - Autonomy"),
#        BNSSh_Trust_Disc = structure(BNSSh_16, label = "Basic Needs History Discouragement - Trust"),
#        BNSSh_Purpose_Disc = structure(BNSSh_17, label = "Basic Needs History Discouragement - Purpose"),
#        BNSSh_Enhancement_Disc = structure(BNSSh_18, label = "Basic Needs History Discouragement - Self-enhancement"),
#        BNSSh_Satisf_Disc = structure(BNSSh_19, label = "Basic Needs History Discouragement - Satisfaction"))  %>%

mutate(BNSSh_12_R = 6 - BNSSh_12,
       BNSSh_13_R = 6 - BNSSh_13,
       BNSSh_14_R = 6 - BNSSh_14,
       BNSSh_15_R = 6 - BNSSh_15,
       BNSSh_16_R = 6 - BNSSh_16,
       BNSSh_17_R = 6 - BNSSh_17,
       BNSSh_18_R = 6 - BNSSh_18,
       BNSSh_19_R = 6 - BNSSh_19) %>%
        
        mutate(BNSSh_Belonging = structure(rowMeans(select(., BNSSh_1,BNSSh_2,BNSSh_12_R), na.rm=TRUE), label = "Basic Needs History - Belonging"),
               BNSSh_Control = structure(rowMeans(select(., BNSSh_3,BNSSh_4,BNSSh_13_R), na.rm=TRUE), label = "Basic Needs History - Control"),
               BNSSh_Competence = structure(rowMeans(select(., BNSSh_5,BNSSh_14_R), na.rm=TRUE), label = "Basic Needs History - Competence"),
               BNSSh_Autonomy = structure(rowMeans(select(., BNSSh_6,BNSSh_15_R), na.rm=TRUE), label = "Basic Needs History - Autonomy"),
               BNSSh_Trust = structure(rowMeans(select(., BNSSh_7,BNSSh_8,BNSSh_16_R), na.rm=TRUE), label = "Basic Needs History - Trust"),
               BNSSh_Purpose = structure(rowMeans(select(., BNSSh_9,BNSSh_17_R), na.rm=TRUE), label = "Basic Needs History - Purpose"),
               BNSSh_Enhancement = structure(rowMeans(select(., BNSSh_10,BNSSh_18_R), na.rm=TRUE), label = "Basic Needs History - Self-enhancement"),
               BNSSh_Satisf = structure(rowMeans(select(., BNSSh_11,BNSSh_19_R), na.rm=TRUE), label = "Basic Needs History - Satisfaction"))

sjmisc::descr(Measures, Measures$BNSSh_Eff_Mean, out = "v")
ggplot(Measures, aes(BNSSh_Eff_Mean)) + geom_histogram(binwidth=1)
psych::alpha(Measures %>% select(BNSSh_1:BNSSh_11))

sjmisc::descr(Measures, Measures$BNSSh_Dis_Mean, out = "v")
ggplot(Measures, aes(BNSSh_Dis_Mean)) + geom_histogram(binwidth=1)
psych::alpha(Measures %>% select(BNSSh_12:BNSSh_19))


# ~ Perceived Discrimination (two mean composite scores)----

# Major Discrimination
Measures <- Measures %>%
        # Re-code original values to 0,1, and then calculate a sum
        mutate_at(vars(contains("Discrim_Major")), function(x) case_when(x == 1 ~ 1, x == 2 ~ 0)) %>%
        mutate(Discrim_Major_Sum = structure(rowSums(select(., Discrim_Major_1:Discrim_Major_7), na.rm=TRUE), 
                                             label = "Major Discrimination Sum"))


sjmisc::frq(Measures$Discrim_Major_Sum, out = "v")
ggplot(Measures, aes(Discrim_Major_Sum)) + geom_histogram(binwidth=1)
sjmisc::descr(Measures, Measures$Discrim_Major_Sum, out = "v")    

psych::alpha(Measures %>% select(contains("Discrim_Major")) %>% select(-Discrim_Major_Sum) )

# Let's take a closer look at the two cases who reported all 7 types of major discrimination; are they serious cases? 
# (UPDATE: They seem to be).
Test  <- Measures %>%
        filter(Discrim_Major_Sum == 7)
rm(Test)

# Everyday Discrimination
Measures <- Measures %>%
        mutate(Discrim_Everyday_Mean = structure(rowMeans(select(., Discrim_Everyday_1:Discrim_Everyday_9), na.rm=TRUE), 
                                                 label = "Everyday Discrimination Composite"))


sjmisc::descr(Measures, Measures$Discrim_Everyday_Mean, out = "v")
ggplot(Measures, aes(Discrim_Everyday_Mean)) + geom_histogram(binwidth=1)
psych::alpha(Measures %>% select(contains("Discrim_Everyday")) %>% select(-Discrim_Everyday_Mean) )


# ~ Multicultural Experience ----
# NOTE: For this measure, I am using as a reference the following:
#             Narvaez, D., Endicott, L., & Hill, P. (2009). Guide for using the Multicultural Experiences Questionnaire (MEQ): 
#             For college students and adults. South Bend, IN: Moral Psychology Laboratory, University of Notre Dame. 
#             
#       That document matches the questions in this measure, and it includes scoring instructions (p. 3). Note, however, that
#       the item numbers get a little funky, because the reference has some sub-items written with the same item #, but with a, b, etc.
#       Those items aren't included in the official scoring. However, those items are distinct question numbers in Qualtrics, which means
#       that the question number ordering gets thrown off. So, this code needs to be written carefully...Instead of renumbering everything,
#       I'm going to match the scoring document to MY question numbers.
Measures <- Measures %>%
        # Create new sub-scales
        mutate(MEXQ_Exp = structure(rowSums(select(., MEXQ1_1, MEXQ3_1, MEXQ4_1, MEXQ5_1, MEXQ8_1, MEXQ9_1, MEXQ10_1, MEXQ11_1, MEXQ11_2), 
                                            na.rm=TRUE), label = "Multicultural Experience"),  
               MEXQ_Desire = structure(rowSums(select(., MEXQ2_1, MEXQ7_1, MEXQ8_2, MEXQ8_3, MEXQ8_4, MEXQ8_5), 
                                               na.rm=TRUE), label = "Multicultural Desire"),
               # Calculate composite
               MEQ_Total = structure(MEXQ_Exp + MEXQ_Desire, label = "MEQ Composite (Sum)"))

sjmisc::descr(Measures, Measures$MEQ_Total, out = "v")
ggplot(Measures, aes(MEQ_Total)) + geom_histogram(binwidth=1)
psych::alpha(Measures %>% select(contains("MEXQ")) %>% select(-MEXQ_Exp, -MEXQ_Desire) )

psych::alpha(Measures %>% select(MEXQ1_1, MEXQ3_1, MEXQ4_1, MEXQ5_1, MEXQ8_1, MEXQ9_1, MEXQ10_1, MEXQ11_1, MEXQ11_2))

# ~ Basic Needs - Adult----
# (SEE NOTES ABOVE FOR THE CHILDHOOD VERSION OF THIS SCALE, BNSSh)
Measures <- Measures %>%
        
        mutate(BNSS_Eff_Mean = structure(rowMeans(select(., BNSS_1:BNSS_11), na.rm=TRUE), label = "Basic Needs Effectance Composite"),
               BNSS_Dis_Mean = structure(rowMeans(select(., BNSS_12:BNSS_19), na.rm=TRUE), label = "Basic Needs Discouoragement Composite")) %>%
        
        # mutate(BNSS_Belonging_Eff = structure(rowMeans(select(., BNSS_1:BNSS_2), na.rm=TRUE), label = "Basic Needs Effectance - Belonging"),
        #        BNSS_Control_Eff = structure(rowMeans(select(., BNSS_3:BNSS_4), na.rm=TRUE), label = "Basic Needs Effectance - Control"),
        #        BNSS_Competence_Eff = structure(BNSS_5, label = "Basic Needs Effectance - Competence"),
        #        BNSS_Autonomy_Eff = structure(BNSS_6, label = "Basic Needs Effectance - Autonomy"),
        #        BNSS_Trust_Eff = structure(rowMeans(select(., BNSS_7:BNSS_8), na.rm=TRUE), label = "Basic Needs Effectance - Trust"),
        #        BNSS_Purpose_Eff = structure(BNSS_9, label = "Basic Needs Effectance - Purpose"),
        #        BNSS_Enhancement_Eff = structure(BNSS_10, label = "Basic Needs Effectance - Self-enhancement"),
        #        BNSS_Satisf_Eff = structure(BNSS_11, label = "Basic Needs Effectance - Satisfaction"),
        #        
        #        BNSS_Belonging_Disc = structure(BNSS_12, label = "Basic Needs Discouragement - Belonging"),
#        BNSS_Control_Disc = structure(BNSS_13, label = "Basic Needs Discouragement - Control"),
#        BNSS_Competence_Disc = structure(BNSS_14, label = "Basic Needs Discouragement - Competence"),
#        BNSS_Autonomy_Disc = structure(BNSS_15, label = "Basic Needs Discouragement - Autonomy"),
#        BNSS_Trust_Disc = structure(BNSS_16, label = "Basic Needs Discouragement - Trust"),
#        BNSS_Purpose_Disc = structure(BNSS_17, label = "Basic Needs Discouragement - Purpose"),
#        BNSS_Enhancement_Disc = structure(BNSS_18, label = "Basic Needs Discouragement - Self-enhancement"),
#        BNSS_Satisf_Disc = structure(BNSS_19, label = "Basic Needs Discouragement - Satisfaction"))  %>%

mutate(BNSS_12r = 6 - BNSS_12,
       BNSS_13r = 6 - BNSS_13,
       BNSS_14r = 6 - BNSS_14,
       BNSS_15r = 6 - BNSS_15,
       BNSS_16r = 6 - BNSS_16,
       BNSS_17r = 6 - BNSS_17,
       BNSS_18r = 6 - BNSS_18,
       BNSS_19r = 6 - BNSS_19) %>%
        
        mutate(BNSS_Belonging = structure(rowMeans(select(., BNSS_1,BNSS_2,BNSS_12r), na.rm=TRUE), label = "Basic Needs - Belonging"),
               BNSS_Control = structure(rowMeans(select(., BNSS_3,BNSS_4,BNSS_13r), na.rm=TRUE), label = "Basic Needs - Control"),
               BNSS_Competence = structure(rowMeans(select(., BNSS_5,BNSS_14r), na.rm=TRUE), label = "Basic Needs - Competence"),
               BNSS_Autonomy = structure(rowMeans(select(., BNSS_6,BNSS_15r), na.rm=TRUE), label = "Basic Needs - Autonomy"),
               BNSS_Trust = structure(rowMeans(select(., BNSS_7,BNSS_8,BNSS_16r), na.rm=TRUE), label = "Basic Needs - Trust"),
               BNSS_Purpose = structure(rowMeans(select(., BNSS_9,BNSS_17r), na.rm=TRUE), label = "Basic Needs - Purpose"),
               BNSS_Enhancement = structure(rowMeans(select(., BNSS_10,BNSS_18r), na.rm=TRUE), label = "Basic Needs - Self-enhancement"),
               BNSS_Satisf = structure(rowMeans(select(., BNSS_11,BNSS_19r), na.rm=TRUE), label = "Basic Needs - Satisfaction"))

sjmisc::descr(Measures, Measures$BNSS_Eff_Mean, out = "v")
ggplot(Measures, aes(BNSS_Eff_Mean)) + geom_histogram(binwidth=1)
psych::alpha(Measures %>% select(BNSS_1:BNSS_11))

sjmisc::descr(Measures, Measures$BNSS_Dis_Mean, out = "v")
ggplot(Measures, aes(BNSS_Dis_Mean)) + geom_histogram(binwidth=1)
psych::alpha(Measures %>% select(BNSS_12:BNSS_19))


# ~ Critical Reflection ----
# Reverse-code necessary items so that all items have higher scores indicating higher critical reflection.
# NOTE: That because there are a lot of items needing reverse-coding, I'm opting to do it the LESS clean way
#       (by replacing existing values). At some point, you may want to come back and revise this. 
# (See: Shin, R. Q., Ezeofor, I., Smith, L. C., Welch, J. C., & Goodrich, K. M. (2016). The development and validation 
#       of the Contemporary Critical Consciousness Measure. Journal of Counseling Psychology, 63(2), 210.)
#       
# NOTE: We'll also first create a system-justification-only composite, because I'd like to do some follow-up analyses just on 
# those items.
Measures <- Measures %>%
        
        mutate(System_Justif_Mean = structure(rowMeans(select(., Crit_Reflection2_1:Crit_Reflection2_3), 
                                                       na.rm=TRUE), label = "System Justification Composite"))  %>%
        
        mutate(Crit_Reflection1_1 = 8 - Crit_Reflection1_1,
               Crit_Reflection1_2 = 8 - Crit_Reflection1_2,
               Crit_Reflection1_3 = 8 - Crit_Reflection1_3,
               Crit_Reflection1_4 = 8 - Crit_Reflection1_4,
               Crit_Reflection1_5 = 8 - Crit_Reflection1_5,
               Crit_Reflection1_6 = 8 - Crit_Reflection1_6,
               Crit_Reflection1_7 = 8 - Crit_Reflection1_7,
               Crit_Reflection1_8 = 8 - Crit_Reflection1_8,
               Crit_Reflection1_13 = 8 - Crit_Reflection1_13, 
               Crit_Reflection2_1 = 8 - Crit_Reflection2_1,
               Crit_Reflection2_2 = 8 - Crit_Reflection2_2,
               Crit_Reflection2_3 = 8 - Crit_Reflection2_3)


#Also, Since I'm combining two different measures here, let's first make sure all those items hang together:
psych::alpha(Measures %>% select(contains("Crit_Reflection1")))
psych::alpha(Measures %>% select(contains("Crit_Reflection2")))
psych::alpha(Measures %>% select(contains("Crit_Reflection")))


# Since they do, we can go ahead and create the composite. However, we'll create two versions
# (one with the System Justification items and one without them).
Measures <- Measures %>%
        mutate(Crit_Reflection_Sub = structure(rowMeans(select(., Crit_Reflection1_1:Crit_Reflection1_19), 
                                                         na.rm=TRUE), label = "Critical Reflection Sub-Composite (No System Justification ItemS"),
               Crit_Reflection_Mean = structure(rowMeans(select(., Crit_Reflection1_1:Crit_Reflection1_19,Crit_Reflection2_1:Crit_Reflection2_3), 
                                                         na.rm=TRUE), label = "Critical Reflection Composite"))


sjmisc::descr(Measures, Measures$Crit_Reflection_Mean, out = "v")
ggplot(Measures, aes(Crit_Reflection_Mean)) + geom_histogram(binwidth=1)
ggplot(Measures, aes(System_Justif_Mean)) + geom_histogram(binwidth=1)

rcorr(as.matrix(Measures[,c("Crit_Reflection_Mean","System_Justif_Mean")]), type="pearson")

# The system justification mean has some positive skew. Just for the heck of it, let's explore
# some transformations to see if they help:
ggplot(Measures, aes(x=1, y=System_Justif_Mean)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1) + 
  stat_summary(fun=mean, geom="point", shape=23, size=2)
symbox(~ System_Justif_Mean, data=Measures)

ggplot(Measures, aes(x=1, y=log(System_Justif_Mean))) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1) + 
  stat_summary(fun=mean, geom="point", shape=23, size=2)

ggplot(Measures, aes(log(System_Justif_Mean))) + geom_histogram(binwidth=.5)

# ~ Sociopolitical Efficacy ----
# Note that the items in this measure were changed between pilot and production. The measure is now a combination of four
# "internal political efficacy" items referred to in Morrell (2003) and two additional items that I created (with Darcia's help).
# None of these items require reverse-coding, and higher scores indicate higher efficacy.

Measures <- Measures %>%
        mutate(Efficacy_Mean = structure(rowMeans(select(., Efficacy_1:Efficacy_6), na.rm=TRUE), label = "Political Efficacy Composite"))

psych::alpha(Measures %>% select(contains("Efficacy")) %>% select(-Efficacy_Mean))

sjmisc::descr(Measures, Measures$Efficacy_Mean, out = "v")
ggplot(Measures, aes(Efficacy_Mean)) + geom_histogram(binwidth=1)


# ~ Critical Action ----
# NOTE: This measure was adapted from Diemer, M. A., Rapa, L. J., Park, C. J., & Perry, J. C. (2017). 
#                                    Development and validation of the Critical Consciousness Scale. Youth & Society, 49(4), 461-483. 
# It is the ""Critical Action: Sociopolitical Participation" sub-scale from that publication. 
# Note, though, another slight difference from the pilot study. The word "justice" or "social justice" was added on many of the items.

Measures <- Measures %>%
        mutate(Crit_Action_Mean = structure(rowMeans(select(., Crit_Action_1:Crit_Action_9), na.rm=TRUE), label = "Critical Action Composite"))

sjmisc::descr(Measures, Measures$Crit_Action_Mean, out = "v")
ggplot(Measures, aes(Crit_Action_Mean)) + geom_histogram(binwidth=1)
psych::alpha(Measures %>% select(contains("Crit_Action")) %>% select(-Crit_Action_Mean) )

# While we're here, let's look at the distribution of BLM Activism
ggplot(Measures, aes(BLM_Activism)) + geom_histogram(binwidth=1)

# Note: Both activism measures have positive skew in the distribution (unsurprising)


# ~ Dialecticism and Zero-Sum Thinking ----
Measures <- Measures %>%
        mutate(Dialectical_1 = 8 - Dialectical_1,      
               Dialectical_3 = 8 - Dialectical_3,
               Dialectical_5 = 8 - Dialectical_5) %>%
        mutate(Dialectical_Mean = structure(rowMeans(select(., Dialectical_1:Dialectical_6), na.rm=TRUE), label = "Dialecticism Composite")) %>%
        mutate(Zero_Sum_Mean = structure(rowMeans(select(., Zero_Sum_1:Zero_Sum_4), na.rm=TRUE), label = "Zero-Sum Thinking Composite"))

psych::alpha(Measures %>% select(contains("Dialectical")) %>% select(-Dialectical_Mean) )
rcorr(as.matrix(Measures[,c("Dialectical_1","Dialectical_2")]), type="pearson")

psych::alpha(Measures %>% select(contains("Zero_Sum")) %>% select(-Zero_Sum_Mean) )


# ~ Violence and Militarism Attitudes ---- 
# NOTE: For the two violence attitudes questions, they are single-item questions related to whether violence is viewed as 
# effective and morally justified. The militarism scale was taken from the "Militaristic Attitudes" questionnaire in:
#   Nelson, L. L., & Milburn, T. W. (1999). Relationships between problem-solving competencies and militaristic attitudes: 
#   Implications for peace education. Peace and Conflict, 5(2), 149-168.
Measures <- Measures %>%
        # Reverse-code necessary items
        mutate(Militarism_5 = 8 - Militarism_5,
               Militarism_6 = 8 - Militarism_6,
               Militarism_7 = 8 - Militarism_7,
               Militarism_8 = 8 - Militarism_8,
               Militarism_9 = 8 - Militarism_9,
               Militarism_10 = 8 - Militarism_10) %>%
        
        mutate(Militarism_Mean = structure(rowMeans(select(., Militarism_1:Militarism_11), na.rm=TRUE), label = "Militarism Composite"))


sjmisc::descr(Measures, Measures$Violence_Just, out = "v")
ggplot(Measures, aes(Violence_Just)) + geom_histogram(binwidth=1)

sjmisc::descr(Measures, Measures$Violence_Effective, out = "v")
ggplot(Measures, aes(Violence_Effective)) + geom_histogram(binwidth=1)

sjmisc::descr(Measures, Measures$Militarism_Mean, out = "v")
ggplot(Measures, aes(Militarism_Mean)) + geom_histogram(binwidth=1)
psych::alpha(Measures %>% select(contains("Militarism")) %>% select(-Militarism_Mean) )

# Let's see how the alpha looks if we add our two single-item measures:
rcorr(as.matrix(Measures %>% select(contains("Militarism"), Violence_Just, Violence_Effective, -Militarism_Mean)), type="pearson")

# Interestingly, those two items on violence are basically uncorrelated with the militarism measure! (Though they are highly correlated
# with each other.) Not sure why that's the case...

# ~ Social Desirability ---- 
Measures <- Measures %>%
        mutate(Militarism_Mean = structure(rowMeans(select(., Militarism_1:Militarism_11), na.rm=TRUE), label = "Militarism Composite"))  


# Before moving on, let's also make sure that all demographic variables we want to treat as categorical are factored
# so that we don't accidentally treat them as continuous (note that I'm leaving income and education variables as continuous)
Measures$Gender <- as_factor(Measures$Gender)
Measures$Trans <- as_factor(Measures$Trans)
Measures$Ethnicity <- as_factor(Measures$Ethnicity)
Measures$Race <- as_factor(Measures$Race)
Measures$Disability <- as_factor(Measures$Disability)
Measures$Sexual <- as_factor(Measures$Sexual)



# Descriptives of Composites-----------------------------------------------------------------------------------------------------------------        
# Here we are going to generate and export a descriptive statistics summary of our measures. I'm going to experiment with a couple of
# different ways of doing it. First, I'll use a combination of the gt package with the "describe" function in the psych package. Then,
# as an alternative, I'm going to try the "gtsummary" package and tbl_summary function, which promises to be a more integrated
# (and flexible) approach.

# colnames(Measures)

# Approach #1
Descriptives2 <- Measures %>%
  select(Age,Gender,Adj_Income,Parent_Edu,LowStatus,Political,ACEs_Sum,EDNhComp,Discrim_Major_Sum,Discrim_Everyday_Mean,MEXQ_Exp,
         Crit_Reflection_Mean,Efficacy_Mean,Crit_Action_Mean,BLM_Activism, Militarism_Mean) %>%
  psych::describe() %>%
  as.data.frame() %>%
  rownames_to_column(var="Measure") %>%
  select(-vars) #%>%
  #gt() %>%
  #apa_style() %>%
  #gtsave("Descriptives1.html", path = here("Output"))


# # Approach #2 (summarytools)
# Test <- 
# Measures %>%
#   select(LowStatus,EDNhComp,ACEs_Sum,Political,MEXQ_Exp,Discrim_Major_Sum,Discrim_Everyday_Mean,
#          BNSS_Eff_Mean,BNSSh_Dis_Mean,Crit_Reflection_Mean,Efficacy_Mean,Crit_Action_Mean,BLM_Activism, Militarism_Mean) %>%
#   summarytools::descr(stats = c("mean", "sd"), transpose = TRUE)
# 
# 
# # Approach #3 (skimr)
# Test2 <- 
#   Measures %>%
#   select(LowStatus,EDNhComp,ACEs_Sum,Political,MEXQ_Exp,Discrim_Major_Sum,Discrim_Everyday_Mean,
#          BNSS_Eff_Mean,BNSSh_Dis_Mean,Crit_Reflection_Mean,Efficacy_Mean,Crit_Action_Mean,BLM_Activism, Militarism_Mean) %>%
#   skimr::skim()
# 
# rm(Test)
# rm(Test2)
