################################################################################################################################-
## CREATED: 12/14/2020
## VERSION: 1.0
## LAST UPDATED:
## 
## AUTHOR: Joel Devonshire, The University of Notre Dame
## 
## DESCRIPTION: Function I found online that creates a "long" version of a correlation matrix (e.g., for exporting to Excel
##              and sorting, filtering, formatting, etc.). The function takes as input a correlation matrix from the rcorr
##              package (see commented example beneath the function code below).
##  
##   
## RELEASE NOTES: 
## 
##                
## KNOWN ISSUES:
## 
###############################################################################################################################- 

regression_diagnostics <- function(model){

        # Residual Plots
        residualPlots(model, main="Residual Plots")
        marginalModelPlots(model, main="Marginal Plots")        
        
        # Normality of Residuals
        sresid <- rstudent(model)
        hist(sresid, freq=FALSE,
             main="Distribution of Studentized Residuals")
        xfit<-seq(min(sresid),max(sresid),length=40)
        yfit<-dnorm(xfit)
        lines(xfit, yfit)
        
        # Assessing Outliers (and Normality)
        outlierTest(model)  # Bonferonni p-value for most extreme obs
        qqPlot(model, main="QQ Plot") # QQ plot for studentized resid
        leveragePlots(model) # leverage plots
        
        # Influential Observations
        # Cook's D plot
        # identify D values > 4/(n-k-1)
        cutoff <- 4/((nrow(data)-length(model$coefficients)-2))
        plot(model, which=4, cook.levels=cutoff)
        # Influence Plot
        influencePlot(model, main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
        
        # Evaluating Collinearity
        vif(model) # variance inflation factors
        sqrt(vif(model)) > 2 # problem?
}