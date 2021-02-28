Causal Inference with R: Matching
================
Mobin A Piracha
2015-07-23T21:13:14-05:00

The following is a data anlaysis project on Matching using R. This
project is a apart of Coursera’s “A Crash Course on Causality” taught by
The University of Pennsylvania. Matching is a technique used in Causal
Inference in which we infer causal effects from observational data by
matching treated and control subjects with roughly similar covariates in
order to estimate causal effects. There are different types of matching
such as greedy nearest neighbor, optimal matching. We will also conduct
propensity score matching whereby we estimate the probability of
treatment for subjects and match each treated and control subject based
on their probability of treatment. We can match based on greedy nearest
neighbor or any other matching method based on covariates after we have
estimated the propensity scores.

# Impact Evaluation of National Supported Work Demonstration

For this project, I will use Lalonda (1986) which is a labor training
program, on post inervention income levels. The goal of this project is
to estimate causal effect of this training program on income. First we
will install packages “tableone”, “Matching” and “MatchIt”. Once you are
done installing load the packages.

Now we will load the data Lalonde which is a part of the MatchIt
package.

``` r
data("lalonde")
View(lalonde)
```

The data has 614 subjects and 10 variables. When we view the data we see
variables age, educ, black, hispan, married, nodegree, re74 (real
earnings in 1974). re75 (real earnings in 1975), re78 (real earnings in
1978). treat. The outcome variable is re78 which is the post
intervention income. According to the data, the potential confounding
variables are age, educ, black, hispan, married, nodegree, re74 and
re75.

Now, the following is the code for computing standardized differences
for all confounding variables. We will create a dataset with just our
required variables and then create a shortlist.

Find the standardized differences for all of the confounding variables
(pre-matching). What is the standardized difference for married (to
nearest hundredth)? Now we shall create a table to look at the
standardize mean differences of covariates before matching.

``` r
table1<-CreateTableOne(vars = xvars, strata = "treatment", data = mydata, test = FALSE)
print(table1, smd=TRUE)
##                        Stratified by treatment
##                         0                 1                 SMD   
##   n                         429               185                 
##   age (mean (SD))         28.03 (10.79)     25.82 (7.16)     0.242
##   education (mean (SD))   10.24 (2.86)      10.35 (2.01)     0.045
##   black (mean (SD))        0.20 (0.40)       0.84 (0.36)     1.668
##   white (mean (SD))        0.66 (0.48)       0.10 (0.30)     1.406
##   hispanic (mean (SD))     0.00 (0.00)       0.00 (0.00)    <0.001
##   married (mean (SD))      0.51 (0.50)       0.19 (0.39)     0.719
##   nodegree (mean (SD))     0.60 (0.49)       0.71 (0.46)     0.235
##   re74 (mean (SD))      5619.24 (6788.75) 2095.57 (4886.62)  0.596
##   re75 (mean (SD))      2466.48 (3292.00) 1532.06 (3219.25)  0.287
```

The table shows the standardized mean differences of all co-variates
pre-matching. We find that the standardized mean difference for married
is 0.719. What is the raw (unadjusted) mean of real earnings in 1978 for
treated subjects minus the mean of real earnings in 1978 for untreated
subjects?

    ## [1] -635.0262

We find that the raw undjusted mean of real earnings for treated
subjects minus control subjects for 1978 is -635. Now we will conduct
greedy matching on Mahalobis distance. For this analysis we will use the
Match package. We will calculate the standardized mean differences after
matching and conduct an outcome analysis through a t-test.

    ##                        Stratified by treatment
    ##                         0                 1                 SMD   
    ##   n                         185               185                 
    ##   age (mean (SD))         24.75 (9.52)      25.82 (7.16)     0.126
    ##   education (mean (SD))   10.08 (2.45)      10.35 (2.01)     0.118
    ##   black (mean (SD))        0.47 (0.50)       0.84 (0.36)     0.852
    ##   white (mean (SD))        0.29 (0.45)       0.10 (0.30)     0.494
    ##   hispanic (mean (SD))     0.00 (0.00)       0.00 (0.00)    <0.001
    ##   married (mean (SD))      0.25 (0.44)       0.19 (0.39)     0.156
    ##   nodegree (mean (SD))     0.69 (0.47)       0.71 (0.46)     0.047
    ##   re74 (mean (SD))      3338.74 (5336.17) 2095.57 (4886.62)  0.243
    ##   re75 (mean (SD))      2106.64 (3323.71) 1532.06 (3219.25)  0.176
    ## 
    ##  One Sample t-test
    ## 
    ## data:  difference
    ## t = 0.79477, df = 184, p-value = 0.4278
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  -844.9986 1985.0355
    ## sample estimates:
    ## mean of x 
    ##  570.0185
    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  y_trt and y_con
    ## V = 8283, p-value = 0.552
    ## alternative hypothesis: true location shift is not equal to 0

We matched using the greedy nearest neighbor matching using the
mahalanobis distance. We found that our standardized mean differences
were greater than 0,1 for age, black, and re74. For the variable black
it was especially greater at 0.943. We found that our t-test was
insignificant at the 95% level with a high p-value= 0.2646. A wilcox
test is also not significant with a p-value=0.2185. Now we will conduct
proposensity score matching using a logistic regression model. We will
once again conduct greedy matching. We will use a caliper of 0.2.

    ## 
    ## Call:
    ## glm(formula = treatment ~ age + education + black + hispanic + 
    ##     married + nodegree + re74 + re75, family = binomial(), data = mydata)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7774  -0.5044  -0.3120   0.7529   2.6484  
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -4.266e+00  9.909e-01  -4.306 1.67e-05 ***
    ## age          1.386e-02  1.336e-02   1.037  0.29953    
    ## education    1.475e-01  6.501e-02   2.269  0.02326 *  
    ## black        2.786e+00  2.423e-01  11.499  < 2e-16 ***
    ## hispanic            NA         NA      NA       NA    
    ## married     -8.678e-01  2.892e-01  -3.001  0.00269 ** 
    ## nodegree     7.193e-01  3.376e-01   2.130  0.03313 *  
    ## re74        -7.309e-05  2.869e-05  -2.547  0.01085 *  
    ## re75         6.033e-05  4.661e-05   1.294  0.19556    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 751.49  on 613  degrees of freedom
    ## Residual deviance: 492.83  on 606  degrees of freedom
    ## AIC: 508.83
    ## 
    ## Number of Fisher Scoring iterations: 5
    ##                        Stratified by treatment
    ##                         0                 1                 SMD   
    ##   n                         108               108                 
    ##   age (mean (SD))         25.84 (11.00)     25.45 (6.85)     0.042
    ##   education (mean (SD))   10.50 (2.56)      10.22 (2.29)     0.114
    ##   black (mean (SD))        0.71 (0.45)       0.73 (0.45)     0.041
    ##   white (mean (SD))        0.26 (0.44)       0.17 (0.37)     0.227
    ##   hispanic (mean (SD))     0.00 (0.00)       0.00 (0.00)    <0.001
    ##   married (mean (SD))      0.20 (0.40)       0.21 (0.41)     0.023
    ##   nodegree (mean (SD))     0.66 (0.48)       0.65 (0.48)     0.019
    ##   re74 (mean (SD))      2845.64 (4823.63) 1819.82 (5106.86)  0.207
    ##   re75 (mean (SD))      1849.14 (2754.92)  898.84 (2292.53)  0.375
    ## 
    ##  One Sample t-test
    ## 
    ## data:  difference_1
    ## t = 0.63065, df = 107, p-value = 0.5296
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  -1151.052  2225.108
    ## sample estimates:
    ## mean of x 
    ##  537.0276

After fitting the model using logistic regression model and doing greedy
propensity score matching. We matched 111 pairs with all standardized
differences were lower than the 0.1 threshold except for re78. We also
find that our mean difference was 1246.806. However, with a
p-value=0.1441, our results were insignificant.
