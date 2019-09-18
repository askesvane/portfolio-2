Exercise 4
==========

Include individual differences in your model of language development (in children). Identify the best model.

Before fitting models, we discussed which variables could have an impact and whether they are fixed or random variables. From this discussion we agreed that ADOS/severity, verbal IQ, nonverbal IQ and socialization could have an impact on child MLU as fixed variables.

In addition to the interaction between time (visit) and diagnosis, the MLU of the children is also correlated with the interaction between time and verbal IQ for the first visit and the interaction between time and ADOS score at first visit. Using AIC and BIC as a criterium, we compared models of increasing complexity and found that our new model is significantly better at describing the data (AIC changed from 572.46 to 504.06 and BIC changed from 603.37 to 550.43). Furthermore, the R-squared value increased from R^2=0.35 to R^2=0.65. Whereas the old model only explained 35% of the variance with the fixed effects, the new model explains 65%. Thereby, this model significantly better explains the data when not taking overfitting and lack of interpretability into account.

Pseudo code: CHI\_MLU ~ VISIT*Diagnosis + VISIT*verbalIQ1 + VISIT \* severity1 + (1 + VISIT | SUBJ)

Code
====

``` r
# loading packages 
pacman::p_load(lmerTest, MuMIn,tidyverse, lme4)

# loading data
data <- read.csv("clean_data.csv")

# relevel diagnosis
data$Diagnosis <- relevel(data$Diagnosis, ref = "TD")

# Our model with interaction between visit and diagnosis from exercise 2.
model1_interaction <- lmer(CHI_MLU ~ VISIT * Diagnosis + (1 + VISIT|SUBJ), data=data)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    ## control$checkConv, : Model failed to converge with max|grad| = 0.00252571
    ## (tol = 0.002, component 1)

``` r
# Summary
summary(model1_interaction)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: CHI_MLU ~ VISIT * Diagnosis + (1 + VISIT | SUBJ)
    ##    Data: data
    ## 
    ## REML criterion at convergence: 572.5
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.47375 -0.52331 -0.08425  0.43358  2.73473 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  SUBJ     (Intercept) 0.30947  0.5563        
    ##           VISIT       0.01192  0.1092   -0.17
    ##  Residual             0.16062  0.4008        
    ## Number of obs: 352, groups:  SUBJ, 61
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)         1.08744    0.11896 57.44649   9.141 8.47e-13 ***
    ## VISIT               0.35387    0.02611 60.54417  13.551  < 2e-16 ***
    ## DiagnosisASD        0.21732    0.17246 57.34452   1.260    0.213    
    ## VISIT:DiagnosisASD -0.25344    0.03774 59.95599  -6.716 7.65e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) VISIT  DgnASD
    ## VISIT       -0.447              
    ## DiagnossASD -0.690  0.308       
    ## VISIT:DgASD  0.309 -0.692 -0.445
    ## convergence code: 0
    ## Model failed to converge with max|grad| = 0.00252571 (tol = 0.002, component 1)

``` r
# R squared of the model from exercise 2
r.squaredGLMM(model1_interaction)
```

    ## Warning: 'r.squaredGLMM' now calculates a revised statistic. See the help
    ## page.

    ##            R2m       R2c
    ## [1,] 0.3509609 0.8182941

``` r
# Our improved complex model with more fixed effects
CHI_MLU_best <-  lmer(CHI_MLU ~ VISIT*Diagnosis + VISIT*verbalIQ1 + VISIT*severity1 + (1 + VISIT|SUBJ), data=data)
# Summary
summary(CHI_MLU_best)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## CHI_MLU ~ VISIT * Diagnosis + VISIT * verbalIQ1 + VISIT * severity1 +  
    ##     (1 + VISIT | SUBJ)
    ##    Data: data
    ## 
    ## REML criterion at convergence: 531
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.28712 -0.60397 -0.06939  0.43857  2.95634 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr 
    ##  SUBJ     (Intercept) 0.102720 0.32050       
    ##           VISIT       0.007197 0.08484  -0.33
    ##  Residual             0.161237 0.40154       
    ## Number of obs: 352, groups:  SUBJ, 61
    ## 
    ## Fixed effects:
    ##                     Estimate Std. Error        df t value Pr(>|t|)    
    ## (Intercept)        -0.571593   0.259415 55.520631  -2.203 0.031733 *  
    ## VISIT               0.446395   0.067624 55.781241   6.601 1.60e-08 ***
    ## DiagnosisASD       -0.193575   0.345000 55.275256  -0.561 0.577005    
    ## verbalIQ1           0.080304   0.011570 56.262411   6.941 4.24e-09 ***
    ## severity1           0.048003   0.025081 55.088985   1.914 0.060826 .  
    ## VISIT:DiagnosisASD  0.097244   0.090513 56.378585   1.074 0.287232    
    ## VISIT:verbalIQ1    -0.003516   0.003016 56.408942  -1.166 0.248509    
    ## VISIT:severity1    -0.027002   0.006573 55.992436  -4.108 0.000131 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) VISIT  DgnASD vrbIQ1 svrty1 VISIT:D VISIT:I
    ## VISIT       -0.657                                            
    ## DiagnossASD  0.307 -0.202                                     
    ## verbalIQ1   -0.938  0.616 -0.352                              
    ## severity1   -0.499  0.329 -0.925  0.464                       
    ## VISIT:DgASD -0.201  0.309 -0.658  0.233  0.608                
    ## VISIT:vrIQ1  0.617 -0.937  0.234 -0.658 -0.307 -0.358         
    ## VISIT:svrt1  0.327 -0.500  0.609 -0.305 -0.657 -0.926   0.467

``` r
# R squared of the new improved model
r.squaredGLMM(CHI_MLU_best)
```

    ##            R2m       R2c
    ## [1,] 0.6501324 0.8173844

``` r
# Comparison of the old and the new model
anova(model1_interaction, CHI_MLU_best)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: data
    ## Models:
    ## model1_interaction: CHI_MLU ~ VISIT * Diagnosis + (1 + VISIT | SUBJ)
    ## CHI_MLU_best: CHI_MLU ~ VISIT * Diagnosis + VISIT * verbalIQ1 + VISIT * severity1 + 
    ## CHI_MLU_best:     (1 + VISIT | SUBJ)
    ##                    Df    AIC    BIC  logLik deviance  Chisq Chi Df
    ## model1_interaction  8 572.46 603.37 -278.23   556.46              
    ## CHI_MLU_best       12 504.06 550.43 -240.03   480.06 76.399      4
    ##                    Pr(>Chisq)    
    ## model1_interaction               
    ## CHI_MLU_best        1.008e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
