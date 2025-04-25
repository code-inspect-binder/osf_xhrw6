# SEM Analysis Section ---------------------------------------------------------------------------------------------------------------------
# The following section sets up a series of path models. Note that this is a highly revised version of the SEM section, which
# originally tried to incorporate a complex latent variable model to simultaneously test all hypotheses. I decided to break it down into 
# smaller, simpler models, and to focused on observed-variable-only models using the composite meausres instead. I'm not sure if that's 
# the "best" approach or not, but I think it at least makes for a simpler approach. The code below is thus a mixture of what I originally
# wrote and the revised models.

# Let's check differences between the two groups:
t.test(Measures$Age~Measures$Normative)
t.test(Measures$Adj_Income~Measures$Normative)
t.test(Measures$Parent_Edu~Measures$Normative)
t.test(Measures$Political~Measures$Normative)
t.test(Measures$Crit_Reflection_Mean~Measures$Normative)
t.test(Measures$Efficacy_Mean~Measures$Normative)
t.test(Measures$Crit_Action_Mean~Measures$Normative)
t.test(Measures$BLM_Activism~Measures$Normative)
t.test(Measures$Militarism_Mean~Measures$Normative)
t.test(Measures$EDNhComp~Measures$Normative)
t.test(Measures$ACEs_Sum~Measures$Normative)
t.test(Measures$MEQ_Total~Measures$Normative)
t.test(Measures$Discrim_Major_Sum~Measures$Normative)
t.test(Measures$Discrim_Everyday_Mean~Measures$Normative)
t.test(Measures$BNSS_Mean~Measures$Normative)
t.test(Measures$BNSSh_Mean~Measures$Normative)
t.test(Measures$Zero_Sum_Mean~Measures$Normative)


sjmisc::frq(Measures$Race, out = "v")

Measures %>% 
        group_by(Race) %>%
        summarise(BLM_Mean = mean(BLM_Activism))

data <- Measures %>% 
        filter(Race %in% c("Black or African-American","White or Caucasian") & !is.na(BLM_Activism)) %>%
        mutate(Black = ifelse(Race == "Black or African-American", 1,0))

# To assess BLM Activism by race, keep in mind that the BLM measure is count data (with a lot of zeros)
# A t-test therefore is not appropriate. We will use "zero-inflated Poisson regression"
ggplot(Measures, aes(BLM_Activism)) + geom_histogram()
model.zi = zeroinfl(BLM_Activism ~ Black,
                    data = data,
                    dist = "poisson")
summary(model.zi)

Descriptives3 <- Measures %>%
  select(Normative,Age,Adj_Income,Parent_Edu,Political,ACEs_Sum,EDNhComp,Discrim_Major_Sum,Discrim_Everyday_Mean,MEXQ_Exp,
         Crit_Reflection_Mean,Efficacy_Mean,Crit_Action_Mean,BLM_Activism, Militarism_Mean) %>%
  psych::describeBy(group="Normative") %>%
  as.data.frame() %>%
  rownames_to_column(var="Measure") %>%
  select(-vars)

DescrNonNorm<- as.data.frame(Descriptives3[1]) %>%
  rownames_to_column(var="Measure")
DescrNorm<- as.data.frame(Descriptives3[2]) %>%
  rownames_to_column(var="Measure")

# So, yes, not hugely surprising, we see that the normative group is more politically conservative, less critically-minded, and a bit more
# militaristic than the non-normative group. The normative sample tends to be slightly higher in Efficacy. No group differences in Critical Action, though.
# Interestingly, no group differences in multicultural experience. Childhood experiences and needs satisfaction are different (in expected directions), and
# the non-normative group tends to have higher levels of zero-sum thinking. I'm not sure.

SEM <- Measures %>%
        select(id,EDHhBfed,EDNhSEmb,EDNhTch,EDNhFreePlay,EDNhResp,EDNPosHC,EDNNegHC,EDNh_Exp_5,EDNh_Exp_6,
               ACEs_1,ACEs_2,ACEs_3,ACEs_4,ACEs_5,ACEs_6,ACEs_7,ACEs_8,ACEs_9,ACEs_10,
               BNSSh_1,BNSSh_2,BNSSh_3,BNSSh_4,BNSSh_5,BNSSh_6,BNSSh_7,BNSSh_8,BNSSh_9,
               BNSSh_10,BNSSh_11,BNSSh_12,BNSSh_13,BNSSh_14,BNSSh_15,BNSSh_16,BNSSh_17,BNSSh_18,BNSSh_19,
               Discrim_Major_1,Discrim_Major_2,Discrim_Major_3,Discrim_Major_4,Discrim_Major_5,Discrim_Major_6,Discrim_Major_7, 
               Discrim_Everyday_1,Discrim_Everyday_2,Discrim_Everyday_3,Discrim_Everyday_4,
               Discrim_Everyday_5,Discrim_Everyday_6,Discrim_Everyday_7,Discrim_Everyday_8,Discrim_Everyday_9,
               BNSS_1,BNSS_2,BNSS_3,BNSS_4,BNSS_5,BNSS_6,BNSS_7,BNSS_8,BNSS_9,BNSS_10,BNSS_11,BNSS_12,BNSS_13,BNSS_14,BNSS_15,BNSS_16,BNSS_17,
               BNSS_18,BNSS_19,
               MEXQ1_1, MEXQ3_1, MEXQ4_1, MEXQ5_1, MEXQ8_1, MEXQ9_1, MEXQ10_1, MEXQ11_1, MEXQ11_2,
               MEXQ2_1, MEXQ7_1, MEXQ8_2, MEXQ8_3, MEXQ8_4, MEXQ8_5,
               Crit_Reflection1_1,Crit_Reflection1_2,Crit_Reflection1_3,Crit_Reflection1_4,Crit_Reflection1_5,
               Crit_Reflection1_6,Crit_Reflection1_7,Crit_Reflection1_8,Crit_Reflection1_9,Crit_Reflection1_10,
               Crit_Reflection1_11,Crit_Reflection1_12,Crit_Reflection1_13,Crit_Reflection1_14,Crit_Reflection1_15,
               Crit_Reflection1_16,Crit_Reflection1_17,Crit_Reflection1_18,Crit_Reflection1_19,Crit_Reflection2_1,
               Crit_Reflection2_2,Crit_Reflection2_3,
               Efficacy_1,Efficacy_2,Efficacy_3,Efficacy_4,Efficacy_5,Efficacy_6,
               Crit_Action_1,Crit_Action_2,Crit_Action_3,Crit_Action_4,Crit_Action_5,Crit_Action_6,Crit_Action_7,Crit_Action_8,Crit_Action_9,
               Militarism_1,Militarism_2,Militarism_3,Militarism_4,Militarism_5,Militarism_6,Militarism_7,Militarism_8,Militarism_9,Militarism_10,Militarism_11)


# Let's examine the pattern of missingess
md.pattern(SEM)
Missing <- as.data.frame(md.pattern(SEM))
Missing <- tibble::rownames_to_column(Missing, "Participants")

# Hard to see with all the variables, but the gist is that only 342 of 360 cases have complete data, so 5% of cases would be deleted
# with listwise deletion. These 18 cases contain a total of 59 item-level missing values, the majority of which are on the negative emotional 
# climate sub-scale of EDNh. (That's very likely due to a number of "Don't Recall" responses on that sub-scale). 
# The rest is scattered across a bunch of other variables. There's only two participants that have more than 10 missing values.
sjmisc::frq(SEM$EDNNegHC, out = "v")

# If we instead use only composite scores, the situation is obviously much improved (only 2 missing values in the DVs)
md.pattern(Measures %>% select(Parent_Edu,Adj_Income,Crit_Reflection_Mean,Efficacy_Mean,Crit_Action_Mean,BLM_Activism,Militarism_Mean))


# ~ Assessing CC dimensions------------------------------------------------------------------------------------------------------------------
# The first thing we want to do is to see whether the three (and maybe four) dimensions of CC that we're working with--
# Critical Reflection, Sociopolitical Efficacy, Critical Action, (and maybe Militarism), empirically "hang together" as
# a single latent construct. We will test this by running a CFA that uses the composite scores from those measures.

# First, we'll test the three-dimension model:
# Let's look at correlations first.
rcorr(as.matrix(Measures[,c("Crit_Reflection_Sub","System_Justif_Mean","Crit_Reflection_Mean",
                            "Efficacy_Mean","Crit_Action_Mean","BLM_Activism","Militarism_Mean")],use="complete.obs"))
psych::alpha(Measures %>% select(Crit_Reflection_Mean,Efficacy_Mean,Crit_Action_Mean))

# Let's also check multivariate normality on these measures
cfa1 <- Measures[which(complete.cases(Measures[,c('Crit_Reflection_Mean', 'Efficacy_Mean', 'Crit_Action_Mean')])),
                 c('Crit_Reflection_Mean', 'Efficacy_Mean', 'Crit_Action_Mean')]

cfa1$mahal <- mahalanobis(cfa1, colMeans(cfa1), cov(cfa1))
cfa1$p <- pchisq(cfa1$mahal, df=2, lower.tail=FALSE)

mult.norm(cfa1, s = cov(cfa1), chicrit = 0.01)

# There are roughly 40 cases that have a high enough Mahalanobis distance where p < .05, but only 14 cases where p < .01.
# However, tests for both skewness and kurtosis failed, so we should considered using robust ML, which is robust to
# non-normality.

CC_CFA1 <- 'CC =~ Crit_Reflection_Mean + Efficacy_Mean + Crit_Action_Mean'

fit_CC1 <- cfa(CC_CFA1, data=Measures, missing="fiml", se="robust")
summary(fit_CC1, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)

# This resulted in a negative error variance (for Critical Action). Generally, this model is not working well.
# Let's try to fit it one more time with a multiple groups CFA (to see if it's a better fit for, say, the 
# "non-normative" sample).
fit_CC1_Groups <- sem(CC_CFA1, data=Measures, missing="fiml", group = "Normative", se="robust")
summary(fit_CC1_Groups, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

fit_CC1_Groups <- sem(CC_CFA1, data=Measures %>% filter(Normative == 0), missing="fiml", se="robust")

# Nope. Same situation. Based on correlations and the CFA attempt, it's not looking like the dimensions hang together well.
# However, before we move to other specifications (or just move on completely), let's first try a couple of different options:
# 
# 1) We'll first try to fix the standardized loading to 1 (seeing error variance to zero) and see if it helps. 
#    Although perhaps not an ideal solution, the Wald z-test for the variance was non-sig, so we can probably justify it.
# 2) We can also try setting all factor loadings to be equal to see if that helps

CC_CFA1a <- 'CC =~ Crit_Reflection_Mean + Efficacy_Mean + Crit_Action_Mean
            # fix variance of C ricial Action to zero
            Crit_Action_Mean ~~ 0*Crit_Action_Mean'

fit_CC1a <- cfa(CC_CFA1a, data=Measures, missing="fiml", se="robust")
summary(fit_CC1a, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)

# So, this resulted in a decent model fit, but at the cost of basically making it a latent variable almost 
# entirely driven by explaining variance in Critical Action. The others are along for the ride, but don't 
# have especially high factor loadings. Let's compare this to a model that makes all loadings equal.
CC_CFA1b <- 'CC =~ a*Crit_Reflection_Mean + a*Efficacy_Mean + a*Crit_Action_Mean'

fit_CC1b <- cfa(CC_CFA1b, data=Measures, missing="fiml", se="robust")
summary(fit_CC1b, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)

anova(fit_CC1a,fit_CC1b)

# So, a model that forces all indicators to have equal factor loadings was a sig. worse fit to the data.
# For these three indicators, then, the only way we can get a good fit is if we have a factor over-represented
# by Critical Action. If we use that analytically as an outcome, we'd be losing information.

# Let's try replacing Critical Action with BLM Activism
CC_CFA2 <- 'CC =~ Crit_Reflection_Mean + Efficacy_Mean + BLM_Activism
            # fix variance of BLM Activism to zero
            BLM_Activism ~~ 0*BLM_Activism'

fit_CC2 <- cfa(CC_CFA2, data=Measures, missing="fiml", se="robust")
summary(fit_CC2, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)


# We initially had the same issue with a negative error variance on BLM Activism. Fixing it to zero
# basically did the same thing as before. Except, interestingly, now Critical Reflection has a fairly
# decent factor loading, while Efficacy really doesn't. Bottom line: these three just aren't fitting 
# together nicely.

CC_CFA3 <- 'CC =~ Crit_Reflection_Mean + Efficacy_Mean + Crit_Action_Mean + BLM_Activism'

fit_CC3 <- cfa(CC_CFA3, data=Measures, missing="fiml", se="robust")
summary(fit_CC3, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)

# Just for the heck of it, let's see if we can get some kind of fit if we also add militarism.
CC_CFA4 <- 'CC =~ Crit_Reflection_Mean + Efficacy_Mean + Crit_Action_Mean + Militarism_Mean'

fit_CC4 <- cfa(CC_CFA4, data=Measures, missing="fiml", se="robust")
summary(fit_CC4, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)

CC_CFA4a <- 'CC =~ Crit_Reflection_Mean + Efficacy_Mean + Crit_Action_Mean + Militarism_Mean
            # fix variance of Critical Reflection to zero
            Crit_Reflection_Mean ~~ 0*Crit_Reflection_Mean'

fit_CC4a <- cfa(CC_CFA4a, data=Measures, missing="fiml", se="robust")
summary(fit_CC4a, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)


CC_CFA5 <- 'CC =~ Crit_Reflection_Mean + Efficacy_Mean + BLM_Activism + Militarism_Mean'

fit_CC5 <- cfa(CC_CFA5, data=Measures, missing="fiml", se="robust")
summary(fit_CC5, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)


CC_CFA6 <- 'CC =~ Crit_Reflection_Mean + BLM_Activism + Militarism_Mean'

fit_CC6 <- cfa(CC_CFA6, data=Measures, missing="fiml", se="robust")
summary(fit_CC6, standardized = TRUE, fit.measures = TRUE, rsquare=TRUE)


# Plotting diagrams
view(get_edges(fit_CC6))
view(get_nodes(fit_CC6))

graph_sem(fit_CC6)

prepare_graph(model = fit_CC6, 
              # layout=read_excel(here("Source Data","CFA_Layout.xlsx"), sheet = "Layout1", col_names = FALSE),
              edges = get_edges(fit_CC6, label = "est_sig_std"),
              nodes = get_nodes(fit_CC6) %>% 
                mutate(label = str_replace(label,"BLM_Activism","BLM Activism"),
                       label = str_replace(label,"Crit_Reflection_Mean","Crit. Reflection"),
                       label = str_replace(label,"Militarism_Mean","Militarism"),),
              variance_diameter = 0.3,
              rect_width = .85) %>%
  # hide_nonsig_edges() %>%
  plot() 


# This last model above was the only one that I could fit that seemed to work ok.
# It's a saturated model and se we can't assess model fit, but all factor loadings
# are relatively high. People high on this factor would be critically reflective,
# low endorsement of militarism, and active with respect to BLM.


# ~ Demographic Background & Injustice--------------------------------------------------------------------------------------------------------        
# Do CC outcome variables follow different paths of developmental influence depending on demographic "status" markers and 
# experiences of injustice in adulthood? For this first path analysis, we're going to focus on the potential "mediating" effects
# of perceived discrimination and omit MEQ. This makes sense because the focus here in on our "Low Status" variable, which
# has basically zero correlation with MEQ. This provides a handy rationale for breaking up these path models so they don't 
# get too complicated.  

# First, we'll fit a saturated model with LowStatus as the sole exogenous predictor (later on, we'll refit this model
# adding a bunch of additional demographics to make sure that basic conclusions hold)
LowStatus_Model1 <- '# Regressions
               Crit_Reflection_Mean ~ LowStatus + Discrim_Everyday_Mean + Discrim_Major_Sum
               Efficacy_Mean ~ LowStatus + Discrim_Everyday_Mean + Discrim_Major_Sum
               Crit_Action_Mean ~  LowStatus + Discrim_Everyday_Mean + Discrim_Major_Sum
               BLM_Activism ~  LowStatus + Discrim_Everyday_Mean + Discrim_Major_Sum
               Militarism_Mean ~  LowStatus + Discrim_Everyday_Mean + Discrim_Major_Sum
               
               Discrim_Major_Sum ~ LowStatus
               Discrim_Everyday_Mean ~ LowStatus
 
               # Correlations and Correlated Errors
               # (This particular one makes sense)        
               Discrim_Major_Sum ~~  Discrim_Everyday_Mean' 


LowStatus_fit_Path1 <- sem(LowStatus_Model1, data=Measures, missing="fiml", se="robust")
summary(LowStatus_fit_Path1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
lavResiduals(LowStatus_fit_Path1, type = "cor", se = TRUE)
modindices(LowStatus_fit_Path1, minimum.value = 4)

# Trimmed model
LowStatus_Model2 <- '# Regressions
               Crit_Reflection_Mean ~ LowStatus + Discrim_Everyday_Mean + Discrim_Major_Sum
               Efficacy_Mean ~ LowStatus + Discrim_Everyday_Mean + Discrim_Major_Sum
               Crit_Action_Mean ~  LowStatus + Discrim_Everyday_Mean
               BLM_Activism ~  LowStatus
               Militarism_Mean ~  LowStatus + Discrim_Everyday_Mean
               
               Discrim_Major_Sum ~ LowStatus
               Discrim_Everyday_Mean ~ LowStatus
 
               # Correlations and Correlated Errors
               # (This particular one makes sense)        
               Discrim_Major_Sum ~~  Discrim_Everyday_Mean' 


LowStatus_fit_Path2 <- sem(LowStatus_Model2, data=Measures, missing="fiml", se="robust")
summary(LowStatus_fit_Path2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
lavResiduals(LowStatus_fit_Path1, type = "cor", se = TRUE)
modindices(LowStatus_fit_Path1, minimum.value = 4)
standardizedsolution(LowStatus_fit_Path2)

# Plotting diagrams
view(get_edges(LowStatus_fit_Path2))
view(get_nodes(LowStatus_fit_Path2))

prepare_graph(model = LowStatus_fit_Path2, 
              layout=read_excel(here("Source Data","Layout_LowStatus_fit_Path2.xlsx"), sheet = "Layout1", col_names = FALSE),
              edges = get_edges(LowStatus_fit_Path2, label = "est_sig_std") %>%
                mutate(label_location = .5),
              nodes = get_nodes(LowStatus_fit_Path2) %>% 
                mutate(label = str_replace(label,"BLM_Activism","BLM Activism"),
                       label = str_replace(label,"Crit_Reflection_Mean","Crit. Reflection"),
                       label = str_replace(label,"Militarism_Mean","Militarism"),
                       label = str_replace(label,"Efficacy_Mean","Efficacy"),
                       label = str_replace(label,"Crit_Action_Mean","Crit. Action"),
                       label = str_replace(label,"Discrim_Major_Sum","Major Discrim."),
                       label = str_replace(label,"Discrim_Everyday_Mean","D-to-D Discrim."),
                       label = str_replace(label,"LowStatus","Low Status Index")),
              variance_diameter = 0.3,
              rect_width = 1,
              rect_height = 1) %>%
  hide_var() %>%
  hide_cov() %>%
  linetype_nonsig(linetype = 2) %>%
  if_edges(grepl("Crit_Reflection_Mean.ON.Discrim_Everyday_Mean",label_results), {label_location = .15} ) %>%
  if_edges(grepl("Efficacy_Mean.ON.Discrim_Everyday_Mean",label_results), {label_location = .35} ) %>%
  if_edges(grepl("Crit_Action_Mean.ON.Discrim_Everyday_Mean",label_results), {label_location = .3} ) %>%
  plot() 



# Now we'll fit the same models adding only Age and Gender
LowStatus_Model1 <- '# Regressions
               Crit_Reflection_Mean ~ LowStatus + Age + GenderBinary + Discrim_Everyday_Mean + Discrim_Major_Sum
               Efficacy_Mean ~ LowStatus + Age + GenderBinary + Discrim_Everyday_Mean + Discrim_Major_Sum
               Crit_Action_Mean ~  LowStatus + Age + GenderBinary + Discrim_Everyday_Mean + Discrim_Major_Sum
               BLM_Activism ~  LowStatus + Age + GenderBinary + Discrim_Everyday_Mean + Discrim_Major_Sum
               Militarism_Mean ~  LowStatus + Age + GenderBinary + Discrim_Everyday_Mean + Discrim_Major_Sum
               
               Discrim_Major_Sum ~ LowStatus + Age + GenderBinary
               Discrim_Everyday_Mean ~ LowStatus + Age + GenderBinary
 
               # Correlations and Correlated Errors
               LowStatus ~~ Age
               LowStatus ~~ GenderBinary
               Age ~~ GenderBinary
               # (This particular one makes sense)        
               Discrim_Major_Sum ~~  Discrim_Everyday_Mean' 


LowStatus_fit_Path1 <- sem(LowStatus_Model1, data=Measures, missing="fiml", se="robust")
summary(LowStatus_fit_Path1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
lavResiduals(LowStatus_fit_Path1, type = "cor", se = TRUE)
modindices(LowStatus_fit_Path1, minimum.value = 4)

# Trimmed model
LowStatus_Model2 <- '# Regressions
               Crit_Reflection_Mean ~ LowStatus + GenderBinary + Discrim_Everyday_Mean + Discrim_Major_Sum
               Efficacy_Mean ~ LowStatus + Discrim_Everyday_Mean + Discrim_Major_Sum
               Crit_Action_Mean ~  Discrim_Everyday_Mean
               BLM_Activism ~  LowStatus + Age + GenderBinary
               Militarism_Mean ~  LowStatus + Discrim_Everyday_Mean
               
               Discrim_Major_Sum ~ LowStatus + Age
               Discrim_Everyday_Mean ~ LowStatus + Age
 
               # Correlations and Correlated Errors
               LowStatus ~~ Age
               LowStatus ~~ GenderBinary
               Age ~~ GenderBinary
               # (This particular one makes sense)        
               Discrim_Major_Sum ~~  Discrim_Everyday_Mean' 


LowStatus_fit_Path2 <- sem(LowStatus_Model2, data=Measures, missing="fiml", se="robust")
summary(LowStatus_fit_Path2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
lavResiduals(LowStatus_fit_Path1, type = "cor", se = TRUE)
modindices(LowStatus_fit_Path1, minimum.value = 4)



# ~~~ CC Factor ---------------------------------------
LowStatusCC_Model1 <- '
               # Latent Factors
               CC =~ Crit_Reflection_Mean + BLM_Activism + Militarism_Mean
               
               # Regressions
               CC ~ LowStatus + Discrim_Everyday_Mean + Discrim_Major_Sum
               
               Discrim_Major_Sum ~ LowStatus
               Discrim_Everyday_Mean ~ LowStatus
 
               # Correlations and Correlated Errors
               # (This particular one makes sense)        
               Discrim_Major_Sum ~~  Discrim_Everyday_Mean' 


LowStatusCC_fit_Path1 <- sem(LowStatusCC_Model1, data=Measures, missing="fiml", se="robust")
summary(LowStatusCC_fit_Path1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Reduced
LowStatusCC_Model2 <- '
               # Latent Factors
               CC =~ Crit_Reflection_Mean + BLM_Activism + Militarism_Mean
               
               # Regressions
               CC ~ LowStatus
               
               Discrim_Major_Sum ~ LowStatus
               Discrim_Everyday_Mean ~ LowStatus
 
               # Correlations and Correlated Errors
               # (This particular one makes sense)        
               Discrim_Major_Sum ~~  Discrim_Everyday_Mean' 


LowStatusCC_fit_Path2 <- sem(LowStatusCC_Model2, data=Measures, missing="fiml", se="robust")
summary(LowStatusCC_fit_Path2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)



# ~~~ Including SES variables ---------------------------------------
LowStatus_Model1 <- '# Regressions
               Crit_Reflection_Mean ~ Age + GenderBinary + LowStatus + Adj_Income + Parent_Edu + Discrim_Everyday_Mean + Discrim_Major_Sum
               Efficacy_Mean ~ Age + GenderBinary + LowStatus + Adj_Income + Parent_Edu + Discrim_Everyday_Mean + Discrim_Major_Sum
               Crit_Action_Mean ~  Age + GenderBinary + LowStatus + Adj_Income + Parent_Edu + Discrim_Everyday_Mean + Discrim_Major_Sum
               BLM_Activism ~  Age + GenderBinary + LowStatus + Adj_Income + Parent_Edu + Discrim_Everyday_Mean + Discrim_Major_Sum
               Militarism_Mean ~  Age + GenderBinary + LowStatus + Adj_Income + Parent_Edu + Discrim_Everyday_Mean + Discrim_Major_Sum
               
               Discrim_Major_Sum ~ Age + GenderBinary + LowStatus + Adj_Income + Parent_Edu
               Discrim_Everyday_Mean ~ Age + GenderBinary + LowStatus + Adj_Income + Parent_Edu
 
               # Correlations and Correlated Errors
               LowStatus ~~ Parent_Edu
               LowStatus ~~ Adj_Income
               LowStatus ~~ Age
               LowStatus ~~ GenderBinary
               Parent_Edu ~~ Adj_Income
               Parent_Edu ~~ Age
               Parent_Edu ~~ GenderBinary
               Age ~~ Adj_Income
               GenderBinary ~~ Adj_Income
               Age ~~ GenderBinary
               
               # (This particular one makes sense)        
               Discrim_Major_Sum ~~  Discrim_Everyday_Mean' 


LowStatus_fit_Path1 <- sem(LowStatus_Model1, data=Measures, missing="fiml", se="robust")
summary(LowStatus_fit_Path1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
lavResiduals(LowStatus_fit_Path1, type = "cor", se = TRUE)
modindices(LowStatus_fit_Path1, minimum.value = 4)

# Trimmed model
LowStatus_Model2 <- '# Regressions
               Crit_Reflection_Mean ~ Age + GenderBinary + LowStatus + Discrim_Everyday_Mean
               Efficacy_Mean ~ Age + Adj_Income + Parent_Edu + Discrim_Major_Sum
               Crit_Action_Mean ~  Parent_Edu + Discrim_Everyday_Mean
               BLM_Activism ~  Age + GenderBinary + LowStatus + Parent_Edu
               Militarism_Mean ~  LowStatus + Discrim_Everyday_Mean
               
               Discrim_Major_Sum ~ Age + LowStatus
               Discrim_Everyday_Mean ~ Age + LowStatus
 
               # Correlations and Correlated Errors
               LowStatus ~~ Parent_Edu
               LowStatus ~~ Adj_Income
               LowStatus ~~ Age
               LowStatus ~~ GenderBinary
               Parent_Edu ~~ Adj_Income
               Parent_Edu ~~ Age
               Parent_Edu ~~ GenderBinary
               Age ~~ Adj_Income
               GenderBinary ~~ Adj_Income
               Age ~~ GenderBinary
               
               # (This particular one makes sense)        
               Discrim_Major_Sum ~~  Discrim_Everyday_Mean' 


LowStatus_fit_Path2 <- sem(LowStatus_Model2, data=Measures, missing="fiml", se = "robust")
summary(LowStatus_fit_Path2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
standardizedsolution(LowStatus_fit_Path2)
lavResiduals(LowStatus_fit_Path2, type = "cor", se = TRUE)
modindices(LowStatus_fit_Path2, minimum.value = 4)

# Plotting diagrams
view(get_edges(LowStatus_fit_Path2))
view(get_nodes(LowStatus_fit_Path2))

prepare_graph(model = LowStatus_fit_Path2, 
              layout=read_excel(here("Source Data","Layout_LowStatus_fit_Path2.xlsx"), sheet = "Layout3", col_names = FALSE),
              edges = get_edges(LowStatus_fit_Path2, label = "est_sig_std") %>% mutate(label_location = .5), 
              variance_diameter = 0.3,
              rect_width = .85) %>%
  # if_edit(grepl("7", est_sig_std), {color = "red"}) %>%
  #if_edit(grepl("Crit_Action_Mean.ON.Parent_Edu",label_results), {label_location = .3} ) %>%
  #if_edit(grepl("BLM_Activism.ON.LowStatus",label_results), {label_location = .6} ) %>%
  #hide_nonsig_edges() %>%
  #if_edit(grepl("Militarism_Mean.ON.Discrim_Everyday_Mean",label_results), {show = TRUE} ) %>%
  #if_edit(grepl("Militarism_Mean.ON.Discrim_Everyday_Mean",label_results), {linetype = 2} ) %>%
  plot() 
  
semTable(LowStatus_fit_Path2, paramSets=c("slopes"), type="html", file=here("Output","LowStatus Covariate SEM"))


# ~~~ Indirect Effects -----------------------------------------------------------------------------------------------
# Now, we will add the following tests of indirect effects:
# 1. Low Status to Everyday discrimination to Critical Action
# NOTE: This is a potentially misleading indirect effect, since the multiple groups analysis shows that each part 
#       is being driven from a different group of participants! (In order to straighten it out, you might want to see if you
#       can get indirect effects working in the multiple groups analysis)

LowStatus_Model2b <- '# Regressions
                Crit_Reflection_Mean ~ LowStatus + Discrim_Major_Sum + Discrim_Everyday_Mean
                Efficacy_Mean ~ LowStatus + Discrim_Everyday_Mean + Discrim_Major_Sum
                Crit_Action_Mean ~  c*LowStatus + b*Discrim_Everyday_Mean
                BLM_Activism ~  LowStatus
                Militarism_Mean ~  LowStatus + Discrim_Everyday_Mean
                
                Discrim_Major_Sum ~ LowStatus
                Discrim_Everyday_Mean ~ a*LowStatus
                
                # Correlations and Correlated Errors
                # (This particular one makes sense)        
                Discrim_Major_Sum ~~  Discrim_Everyday_Mean
                
                # Indirect Effect
                ab := a*b
                
                # Total Effect
                total := c + (a*b)' 

LowStatus_fit_Path2b <- sem(LowStatus_Model2b, data=Measures, missing="fiml", se="bootstrap")
summary(LowStatus_fit_Path2b, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
lavResiduals(LowStatus_fit_Path2b, type = "cor", se = TRUE)
modindices(LowStatus_fit_Path2b, minimum.value = 4)


# 2. Low Status to Everyday discrimination to Militarism
LowStatus_Model2c <- '# Regressions
                Crit_Reflection_Mean ~ LowStatus + Discrim_Major_Sum + Discrim_Everyday_Mean
                Efficacy_Mean ~ LowStatus + Discrim_Everyday_Mean + Discrim_Major_Sum
                Crit_Action_Mean ~  c*LowStatus + Discrim_Everyday_Mean
                BLM_Activism ~  LowStatus
                Militarism_Mean ~  LowStatus + b*Discrim_Everyday_Mean
                
                Discrim_Major_Sum ~ LowStatus
                Discrim_Everyday_Mean ~ a*LowStatus
                
                # Correlations and Correlated Errors
                # (This particular one makes sense)        
                Discrim_Major_Sum ~~  Discrim_Everyday_Mean
                
                # Indirect Effect
                ab := a*b
                
                # Total Effect
                total := c + (a*b)' 

LowStatus_fit_Path2c <- sem(LowStatus_Model2c, data=Measures, missing="fiml", se="bootstrap")
summary(LowStatus_fit_Path2c, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# 3. Low Status to Major discrimination to Efficacy
LowStatus_Model2d <- '# Regressions
                Crit_Reflection_Mean ~ LowStatus + Discrim_Major_Sum + Discrim_Everyday_Mean
                Efficacy_Mean ~ LowStatus + Discrim_Everyday_Mean + b*Discrim_Major_Sum
                Crit_Action_Mean ~  c*LowStatus + Discrim_Everyday_Mean
                BLM_Activism ~  LowStatus
                Militarism_Mean ~  LowStatus + Discrim_Everyday_Mean
                
                Discrim_Major_Sum ~ a*LowStatus
                Discrim_Everyday_Mean ~ LowStatus
                
                # Correlations and Correlated Errors
                # (This particular one makes sense)        
                Discrim_Major_Sum ~~  Discrim_Everyday_Mean
                
                # Indirect Effect
                ab := a*b
                
                # Total Effect
                total := c + (a*b)' 

LowStatus_fit_Path2d <- sem(LowStatus_Model2d, data=Measures, missing="fiml", se="bootstrap")
summary(LowStatus_fit_Path2d, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# ~~~ Multiple Groups -----------------------------------------------------------------------------------------------
# Now, we're going to go back to the fully saturated model and look at the multiple groups version. This time,
# the "LowStatus" variable will be dropped, and we're only looking at discrimination and outcomes
LowStatsus_Groups_Model1 <- '# Regressions
               Crit_Reflection_Mean ~ Discrim_Major_Sum + Discrim_Everyday_Mean
               Efficacy_Mean ~ Discrim_Major_Sum + Discrim_Everyday_Mean
               Crit_Action_Mean ~ Discrim_Major_Sum + Discrim_Everyday_Mean
               BLM_Activism ~  Discrim_Major_Sum + Discrim_Everyday_Mean
               Militarism_Mean ~ Discrim_Major_Sum + Discrim_Everyday_Mean

               # Correlations and Correlated Errors
               # (This particular one makes sense)        
               Discrim_Major_Sum ~~  Discrim_Everyday_Mean' 

LowStatus_fit_Groups1 <- sem(LowStatsus_Groups_Model1, data=Measures, missing="fiml", group="Normative",se = "robust")   
summary(LowStatus_fit_Groups1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
lavResiduals(LowStatus_fit_Groups1, type = "cor", se = TRUE)
modindices(LowStatus_fit_Groups1, minimum.value = 4)

# Reduced Model
LowStatsus_Groups_Model2 <- '# Regressions
               Crit_Reflection_Mean ~ Discrim_Everyday_Mean
               Efficacy_Mean ~ Discrim_Major_Sum
               Crit_Action_Mean ~ Discrim_Major_Sum + Discrim_Everyday_Mean
               BLM_Activism ~  Discrim_Everyday_Mean
               Militarism_Mean ~ Discrim_Everyday_Mean

               # Correlations and Correlated Errors
               # (This particular one makes sense)        
               Discrim_Major_Sum ~~  Discrim_Everyday_Mean' 

LowStatus_fit_Groups2 <- sem(LowStatsus_Groups_Model2, data=Measures, missing="fiml", group="Normative",se = "robust")   
summary(LowStatus_fit_Groups2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
lavResiduals(LowStatus_fit_Groups2, type = "cor", se = TRUE)
modindices(LowStatus_fit_Groups2, minimum.value = 4)
standardizedsolution(LowStatus_fit_Groups2)


# For testing the paths, let's first compare the unconstrained model to one that constrains all paths to be equal. 
# If the fit is significantly worse, then it means there is SOME interaction.
LowStatus_fit_Groups2b <- sem(LowStatsus_Groups_Model2, data=Measures, missing="fiml", group = "Normative", group.equal = c("regressions"))
summary(LowStatus_fit_Groups2b, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
anova(LowStatus_fit_Groups2,LowStatus_fit_Groups2b)


# Plotting diagrams
view(get_edges(LowStatus_fit_Groups2))
view(get_nodes(LowStatus_fit_Groups2))

prepare_graph(model = LowStatus_fit_Groups2, 
              layout=read_excel(here("Source Data","Layout_LowStatus_fit_Path2.xlsx"), sheet = "Layout4", col_names = FALSE),
              edges = get_edges(LowStatus_fit_Groups2, label = "est_sig_std") %>%
                mutate(label_location = .5),
              nodes = get_nodes(LowStatus_fit_Groups2) %>%
                mutate(label = str_replace(label,"BLM_Activism","BLM Activism"),
                       label = str_replace(label,"Crit_Reflection_Mean","Crit. Reflection"),
                       label = str_replace(label,"Militarism_Mean","Militarism"),
                       label = str_replace(label,"Efficacy_Mean","Efficacy"),
                       label = str_replace(label,"Crit_Action_Mean","Crit. Action"),
                       label = str_replace(label,"Discrim_Major_Sum","Major Discrim."),
                       label = str_replace(label,"Discrim_Everyday_Mean","D-to-D Discrim."),
                       label = str_replace(label,"LowStatus","Low Status Index")),
              variance_diameter = 0.3,
              rect_width = 1,
              rect_height = 1) %>%
  hide_var() %>%
  #hide_cov() %>%
  hide_nonsig() %>%
  #linetype_nonsig(linetype = 2) %>%
  #if_edges(grepl("Crit_Reflection_Mean.ON.Discrim_Everyday_Mean",label_results), {label_location = .15} ) %>%
  #if_edges(grepl("Efficacy_Mean.ON.Discrim_Everyday_Mean",label_results), {label_location = .35} ) %>%
  #if_edges(grepl("Crit_Action_Mean.ON.Discrim_Everyday_Mean",label_results), {label_location = .3} ) %>%
  plot() 

# ~ Early Life & Multicultural Experience ------------------------------------------------------------------------------------------------------        
# Path model looking at ACES, EDNh, MEQ and outcomes 

# Like our first path model, we will start out without the other demographic predictors
# Saturated Model
EDN_Path_Model1 <- '# Regressions
               Crit_Reflection_Mean ~ ACEs_Sum + EDNhComp + MEXQ_Exp
               Efficacy_Mean ~ ACEs_Sum + EDNhComp + MEXQ_Exp
               Crit_Action_Mean ~ ACEs_Sum + EDNhComp + MEXQ_Exp
               BLM_Activism ~  ACEs_Sum + EDNhComp + MEXQ_Exp
               Militarism_Mean ~ ACEs_Sum + EDNhComp + MEXQ_Exp
               
               MEXQ_Exp ~ ACEs_Sum + EDNhComp

               # Correlations
               ACEs_Sum ~~ EDNhComp' 


EDN_fit_Path1 <- sem(EDN_Path_Model1, data=Measures, missing="fiml", se="robust")
summary(EDN_fit_Path1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
lavResiduals(EDN_fit_Path1, type = "cor", se = TRUE)
modindices(EDN_fit_Path1, minimum.value = 4)

# Trimmed Model
EDN_Path_Model2 <- '# Regressions
               Crit_Reflection_Mean ~ ACEs_Sum + MEXQ_Exp
               Efficacy_Mean ~ ACEs_Sum + EDNhComp + MEXQ_Exp
               Crit_Action_Mean ~ ACEs_Sum + MEXQ_Exp
               BLM_Activism ~  ACEs_Sum + MEXQ_Exp
               Militarism_Mean ~ ACEs_Sum + EDNhComp + MEXQ_Exp
               
               MEXQ_Exp ~ ACEs_Sum + EDNhComp

               # Correlations
               ACEs_Sum ~~ EDNhComp' 


EDN_fit_Path2 <- sem(EDN_Path_Model2, data=Measures, missing="fiml",se="robust")
summary(EDN_fit_Path2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
lavResiduals(EDN_fit_Path2, type = "cor", se = TRUE)
modindices(EDN_fit_Path2, minimum.value = 4)
standardizedsolution(EDN_fit_Path2)


# Plotting diagrams
view(get_edges(EDN_fit_Path2))
view(get_nodes(EDN_fit_Path2))

prepare_graph(model = EDN_fit_Path2, 
              layout=read_excel(here("Source Data","Layout_EDNh_fit_Path2.xlsx"), sheet = "Layout1", col_names = FALSE),
              edges = get_edges(EDN_fit_Path2, label = "est_sig_std") %>%
                mutate(label_location = .5),
              nodes = get_nodes(EDN_fit_Path2) %>% 
                mutate(label = str_replace(label,"BLM_Activism","BLM Activism"),
                       label = str_replace(label,"Crit_Reflection_Mean","Crit. Reflection"),
                       label = str_replace(label,"Militarism_Mean","Militarism"),
                       label = str_replace(label,"Efficacy_Mean","Efficacy"),
                       label = str_replace(label,"Crit_Action_Mean","Crit. Action"),
                       label = str_replace(label,"ACEs_Sum","ACEs"),
                       label = str_replace(label,"EDNhComp","EDNh"),
                       label = str_replace(label,"MEXQ_Exp","MEQ")),
              variance_diameter = 0.3,
              rect_width = 1,
              rect_height = 1,
              angle = 0) %>%
  hide_var() %>%
  hide_cov() %>%
  linetype_nonsig(linetype = 2) %>%
  if_nodes(grepl("Means.BLM_Activism",label_results), {linetype = 1} ) %>%
  if_edges(grepl("Militarism_Mean.ON.EDNhComp",label_results), {label_location = .3} ) %>%
  if_edges(grepl("Efficacy_Mean.ON.ACEs_Sum",label_results), {label_location = .3} ) %>%
  if_edges(grepl("BLM_Activism.ON.MEXQ_Exp",label_results), {label_location = .3} ) %>%
  if_edges(grepl("Militarism_Mean.ON.MEXQ_Exp",label_results), {label_location = .4} ) %>%
  plot() 



# ~~~ CC Factor ---------------------------------------
EDN_Path_CC_Model1 <- '
               # Latent Factors
               CC =~ Crit_Reflection_Mean + BLM_Activism + Militarism_Mean
               
               # Regressions
               CC ~ ACEs_Sum + EDNhComp + MEXQ_Exp
               
               MEXQ_Exp ~ ACEs_Sum + EDNhComp
 
               # Correlations and Correlated Errors' 


EDN_Path_CC_fit1 <- sem(EDN_Path_CC_Model1, data=Measures, missing="fiml", se="robust")
summary(EDN_Path_CC_fit1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Reduced
EDN_Path_CC_Model2 <- '
               # Latent Factors
               CC =~ Crit_Reflection_Mean + BLM_Activism + Militarism_Mean
               
               # Regressions
               CC ~ MEXQ_Exp
               
               MEXQ_Exp ~ ACEs_Sum + EDNhComp
 
               # Correlations and Correlated Errors' 


EDN_Path_CC_fit2 <- sem(EDN_Path_CC_Model2, data=Measures, missing="fiml", se="robust")
summary(EDN_Path_CC_fit2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)



# ~~~ Including SES variables ---------------------------------------
# Now with the added demographics
# Saturated Model
EDN_Path_Model1 <- '# Regressions
               Crit_Reflection_Mean ~ Age + GenderBinary + ACEs_Sum + EDNhComp + Adj_Income + Parent_Edu + MEXQ_Exp
               Efficacy_Mean ~ Age + GenderBinary + ACEs_Sum + EDNhComp + Adj_Income + Parent_Edu + MEXQ_Exp
               Crit_Action_Mean ~ Age + GenderBinary + ACEs_Sum + EDNhComp + Adj_Income + Parent_Edu + MEXQ_Exp
               BLM_Activism ~  Age + GenderBinary + ACEs_Sum + EDNhComp + Adj_Income + Parent_Edu + MEXQ_Exp
               Militarism_Mean ~  Age + GenderBinary + ACEs_Sum + EDNhComp + Adj_Income + Parent_Edu + MEXQ_Exp
               
               MEXQ_Exp ~ Age + GenderBinary + ACEs_Sum + EDNhComp + Adj_Income + Parent_Edu

               # Correlations
               ACEs_Sum ~~ EDNhComp
               ACEs_Sum ~~ Adj_Income
               ACEs_Sum ~~ Parent_Edu
               ACEs_Sum ~~ Age
               ACEs_Sum ~~ GenderBinary
               EDNhComp ~~ Adj_Income
               EDNhComp ~~ Parent_Edu
               EDNhComp ~~ Age
               EDNhComp ~~ GenderBinary
               Adj_Income ~~ Parent_Edu
               Adj_Income ~~ Age
               Adj_Income ~~ GenderBinary
               Age ~~ GenderBinary' 


EDN_fit_Path1 <- sem(EDN_Path_Model1, data=Measures, missing="fiml",se="robust")
summary(EDN_fit_Path1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
lavResiduals(EDN_fit_Path1, type = "cor", se = TRUE)
modindices(EDN_fit_Path1, minimum.value = 4)

# Trimmed Model
EDN_Path_Model2 <- '# Regressions
               Crit_Reflection_Mean ~ Age + GenderBinary + Adj_Income + Parent_Edu + EDNhComp + MEXQ_Exp
               Efficacy_Mean ~ Age + ACEs_Sum + EDNhComp + MEXQ_Exp
               Crit_Action_Mean ~  ACEs_Sum + Parent_Edu + MEXQ_Exp
               BLM_Activism ~  Age + GenderBinary + ACEs_Sum + Parent_Edu + MEXQ_Exp
               Militarism_Mean ~  Age + ACEs_Sum + EDNhComp + MEXQ_Exp
               
               MEXQ_Exp ~ ACEs_Sum + EDNhComp + Adj_Income + Parent_Edu

               # Correlations
               ACEs_Sum ~~ EDNhComp
               ACEs_Sum ~~ Adj_Income
               ACEs_Sum ~~ Parent_Edu
               ACEs_Sum ~~ Age
               ACEs_Sum ~~ GenderBinary
               EDNhComp ~~ Adj_Income
               EDNhComp ~~ Parent_Edu
               EDNhComp ~~ Age
               EDNhComp ~~ GenderBinary
               Adj_Income ~~ Parent_Edu
               Adj_Income ~~ Age
               Adj_Income ~~ GenderBinary
               Age ~~ GenderBinary' 


EDN_fit_Path2 <- sem(EDN_Path_Model2, data=Measures, missing="fiml",se="robust")
EDN_fit_Path2 <- sem(EDN_Path_Model2, data=Measures, missing="fiml",se="robust", group="Normative")
summary(EDN_fit_Path2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
lavResiduals(EDN_fit_Path2, type = "cor", se = TRUE)
modindices(EDN_fit_Path2, minimum.value = 4)


semTable(EDN_fit_Path2, paramSets=c("slopes"), type="html", file=here("Output","EDN Covariate SEM"))

# ~~~ Indirect Effects -----------------------------------------------------------------------------------------------
# Now, we will add the following tests of indirect effects:
# 1. EDNh to MEQ to Critical Reflection
Path_Model2b <- '# Regressions
               Crit_Reflection_Mean ~ c*EDNhComp + b*MEXQ_Exp
               Efficacy_Mean ~ EDNhComp + MEXQ_Exp
               Crit_Action_Mean ~  ACEs_Sum + MEXQ_Exp
               BLM_Activism ~  ACEs_Sum + MEXQ_Exp
               Militarism_Mean ~  MEXQ_Exp
               
               MEXQ_Exp ~ ACEs_Sum + a*EDNhComp
               
               # Correlations
               ACEs_Sum ~~ EDNhComp
               
                # Indirect Effect
                ab := a*b
                
                # Total Effect
                total := c + (a*b)' 

fit_Path2b <- sem(Path_Model2b, data=Measures, missing="fiml",se = "bootstrap")
summary(fit_Path2b, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# 2. EDNh to MEQ to Efficacy
Path_Model2c <- '# Regressions
               Crit_Reflection_Mean ~ EDNhComp + MEXQ_Exp
               Efficacy_Mean ~ c*EDNhComp + b*MEXQ_Exp
               Crit_Action_Mean ~  ACEs_Sum + MEXQ_Exp
               BLM_Activism ~  ACEs_Sum + MEXQ_Exp
               Militarism_Mean ~  MEXQ_Exp
               
               MEXQ_Exp ~ ACEs_Sum + a*EDNhComp
               
               # Correlations
               ACEs_Sum ~~ EDNhComp

                # Indirect Effect
                ab := a*b
                
                # Total Effect
                total := c + (a*b)' 

fit_Path2c <- sem(Path_Model2c, data=Measures, missing="fiml", se = "bootstrap")
summary(fit_Path2c, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)


# 3. EDNh to MEQ to Action
Path_Model2d <- '# Regressions
               Crit_Reflection_Mean ~ EDNhComp + MEXQ_Exp
               Efficacy_Mean ~ EDNhComp + MEXQ_Exp
               Crit_Action_Mean ~  ACEs_Sum + c*EDNhComp + b*MEXQ_Exp
               BLM_Activism ~  ACEs_Sum + MEXQ_Exp
               Militarism_Mean ~  MEXQ_Exp
               
               MEXQ_Exp ~ ACEs_Sum + a*EDNhComp
               
               # Correlations
               ACEs_Sum ~~ EDNhComp

                # Indirect Effect
                ab := a*b
                
                # Total Effect
                total := c + (a*b)' 

fit_Path2d <- sem(Path_Model2d, data=Measures, missing="fiml", se = "bootstrap")
summary(fit_Path2d, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)


# 4. EDNh to MEQ to Militarism
Path_Model2e <- '# Regressions
               Crit_Reflection_Mean ~ EDNhComp + MEXQ_Exp
               Efficacy_Mean ~ EDNhComp + MEXQ_Exp
               Crit_Action_Mean ~  ACEs_Sum + MEXQ_Exp
               BLM_Activism ~  ACEs_Sum + MEXQ_Exp
               Militarism_Mean ~  c*EDNhComp + b*MEXQ_Exp
               
               MEXQ_Exp ~ ACEs_Sum + a*EDNhComp
               
               # Correlations
               ACEs_Sum ~~ EDNhComp

                # Indirect Effect
                ab := a*b
                
                # Total Effect
                total := c + (a*b)' 

fit_Path2e <- sem(Path_Model2e, data=Measures, missing="fiml", se = "bootstrap")
summary(fit_Path2e, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# 5. EDNh to MEQ to BLM Activism
Path_Model2f <- '# Regressions
               Crit_Reflection_Mean ~ EDNhComp + MEXQ_Exp
               Efficacy_Mean ~ EDNhComp + MEXQ_Exp
               Crit_Action_Mean ~  ACEs_Sum + MEXQ_Exp
               BLM_Activism ~  c*EDNhComp + ACEs_Sum + b*MEXQ_Exp
               Militarism_Mean ~  EDNhComp + MEXQ_Exp
               
               MEXQ_Exp ~ ACEs_Sum + a*EDNhComp
               
               # Correlations
               ACEs_Sum ~~ EDNhComp

                # Indirect Effect
                ab := a*b
                
                # Total Effect
                total := c + (a*b)' 

fit_Path2f <- sem(Path_Model2f, data=Measures, missing="fiml", se = "bootstrap")
summary(fit_Path2f, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# ~~~ Multiple Groups -----------------------------------------------------------------------------------------------
# Now, we're going to go back to the fully saturated model and look at the multiple groups version.
EDN_fit_Groups1 <- sem(EDN_Path_Model1, data=Measures, missing="fiml", group="Normative")   
summary(EDN_fit_Groups1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
lavResiduals(EDN_fit_Groups1, type = "cor", se = TRUE)
modindices(EDN_fit_Groups1, minimum.value = 4)


EDN_Groups_Model2 <- '# Regressions
               Crit_Reflection_Mean ~ EDNhComp + MEXQ_Exp
               Efficacy_Mean ~ ACEs_Sum + EDNhComp + MEXQ_Exp
               Crit_Action_Mean ~  ACEs_Sum + MEXQ_Exp
               BLM_Activism ~  ACEs_Sum + MEXQ_Exp
               Militarism_Mean ~  ACEs_Sum + EDNhComp + MEXQ_Exp
               
               MEXQ_Exp ~ ACEs_Sum + EDNhComp

               # Correlations
               ACEs_Sum ~~ EDNhComp' 

EDN_fit_Groups2 <- sem(EDN_Groups_Model2, data=Measures, missing="fiml", group="Normative",se="robust")   
summary(EDN_fit_Groups2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
lavResiduals(EDN_fit_Groups2, type = "cor", se = TRUE)
modindices(EDN_fit_Groups2, minimum.value = 4)



# For testing the paths, let's first compare the unconstrained model to one that constrains all paths to be equal. 
# If the fit is significantly worse, then it means there is SOME interaction.
EDN_fit_Groups2b <- sem(EDN_Groups_Model2, data=Measures, missing="fiml", group = "Normative", se="robust", group.equal = c("regressions"))
summary(EDN_fit_Groups2b, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
anova(EDN_fit_Groups2,EDN_fit_Groups2b)

#Test fully saturated models
EDN_fit_Groups1b <- sem(EDN_Path_Model1, data=Measures, missing="fiml", group = "Normative", se="robust", group.equal = c("regressions"))
anova(EDN_fit_Groups1,EDN_fit_Groups1b)
