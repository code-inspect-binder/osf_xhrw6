# Bivariate Correlations & Scatterplots--------------------------------------------------------------------------------------------------------------
# In this section, we'll re-summarize descriptive summaries of the measures, and we'll look at bivariate correlations, scatterplots, etc.

# First, we'll use a function I found online to create a nice format for correlation matrices,  
source(here("Functions", "flat_cor_mat.R"))


# ~ Create and export correlation table ---- 
rMeasures <- rcorr(as.matrix(Measures[,c("Age","GenderBinary","Adj_Income","Parent_Edu","LowStatus","Political",
                                         "ACEs_Sum","EDNhComp","MEXQ_Exp",
                                         "Discrim_Major_Sum","Discrim_Everyday_Mean",
                                         "Crit_Reflection_Mean","Efficacy_Mean",
                                         "Crit_Action_Mean","BLM_Activism","Militarism_Mean")],
                             use="complete.obs"))


my_cor_matrix <- flat_cor_mat(rMeasures$n, rMeasures$r, rMeasures$P)

cor <- Measures %>%
        select(Age,GenderBinary,Adj_Income,Parent_Edu,LowStatus,Political,ACEs_Sum,EDNhComp,Discrim_Major_Sum,Discrim_Everyday_Mean,MEXQ_Exp,
               Crit_Reflection_Mean,Efficacy_Mean,Crit_Action_Mean,BLM_Activism, Militarism_Mean)

#apaTables::apa.cor.table(cor,
 #                        landscape = TRUE, 
 #                        table.number = 2, 
 #                        filename = here("Output", "CorrMatrix.rtf"))

sink(here("Output", "corrmatrix.html"))
corstars(cor, method = "pearson", removeTriangle = "upper",result = "html")
sink()

# corrplot::corrplot(rMeasures, method = "circle", type="upper")


# Scatterplots ------------------------------------------------------------------------------------------------------------------------
# 
# First, it's worth noting that all three CC domains (reflection, efficacy, and action) are positively related (which is consistent with theory and 
# good to see here). Second, Multicultural Experience is the only measure that seems strongly related, on average, to all three domains of CC. All
# other predictors seem to have more complex relationships. We'll start off summarizing those relationships:

# # Multicultural Experience/Desire and CC:
# ggplot(Measures, aes(MEQ_Total, Crit_Reflection_Mean)) +
#         geom_point() +
#         geom_smooth(method = "lm") + stat_regline_equation(output.type = "text", aes(label =  paste(..eq.label.., ..rr.label.., sep = "     ")))
# 
# ggplot(Measures, aes(MEQ_Total, Efficacy_Mean)) +
#         geom_point() +
#         geom_smooth(method = "lm") + stat_regline_equation(output.type = "text", aes(label =  paste(..eq.label.., ..rr.label.., sep = "     ")))
# 
# ggplot(Measures, aes(MEQ_Total, Crit_Action_Mean)) +
#         geom_point() +
#         geom_smooth(method = "lm") + stat_regline_equation(output.type = "text", aes(label =  paste(..eq.label.., ..rr.label.., sep = "     ")))
# 
# 
