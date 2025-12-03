################################################################################
#               Practical 4: Sample size and penalised regression              #
################################################################################

# Instructions 

# In this practical we will consider the application of the proposed 
# sample size calculation based by Riley et al, and also gain experience of 
# using the lasso for developing a prediction model 

#####################################  
# SAMPLE SIZE FOR MODEL DEVELOPMENT #
#####################################

# key refs: 
# - Riley RD, Ensor J, Snell KIE et al. Calculating the sample size required
# for developing a clinical prediction model. BMJ 2020;368:m441. 
# - Riley RD, Snell KI, Ensor J et al. Minimum sample size for developing a 
# multivariable prediction model: Part II - binary and time-to-event outcomes. 
# Stat Med 2019;38(7):1276-96. 
# - Riley RD, Snell KIE, Ensor J et al. Minimum sample size for developing a 
# multivariable prediction model: Part I - Continuous outcomes. 
# Stat Med 2019;38(7):1262-75. 
  
# Load the pmsampsize library and look at the help file to familarise yourself with the 
# format and options available
library(pmsampsize)
library(survival)
library(boot)
library(rms)
library(flexsurv)
library(mfp)
library(glmnet)
#If you still need to install survcomp, see "R install file.R"
library(survcomp)
setwd("ecg_scores/")




approximate_R2 <- function(auc, prev, n = 1000000){
  
  # define mu as a function of the C statistic
  
  mu   <- sqrt(2) * qnorm(auc)
  
  # simulate large sample linear prediction based on two normals
  
  # for non‐eventsN(0, 1), events and N(mu, 1)
  
  LP <- c(rnorm(prev*n,  mean=0, sd=1), rnorm((1-prev)*n, mean=mu, sd=1))
  
  y <- c(rep(0, prev*n), rep(1, (1-prev)*n))
  
  # Fit a logistic regression with LP as covariate;
  
  # this is essentially a calibration model, and the intercept and
  
  # slope estimate will ensure the outcome proportion is accounted
  
  # for, without changing C statistic
  
  fit <- lrm(y~LP)
  print("end")
  
  max_R2 <- function(prev){
    1 - (prev^prev*(1-prev)^(1-prev))^2
    #1 - (prevˆprev*(1-prev)ˆ(1-prev))ˆ2
    
  }
  
  return(list(R2.nagelkerke = as.numeric(fit$stats['R2']),
              
              R2.coxsnell = as.numeric(fit$stats['R2']) * max_R2(prev)))
  
}

set.seed(1234)

approximate_R2(auc = 0.73, prev = 0.06, n=1000000)

# Load the saved R workspace
load(file = "ecg_scores/rs_model/fp_models.RData")

##########
# Part 1 #
##########

# Let's start with a continuous outcome example

# For this, we need to specify a conservative value for the model's R-squared, 
# the desired shrinkage factor (0.9), the anticipated mean outcome value,
# the standard deviation of outcome values & the number of candidate predictors.

# Let's consider we want to predict the FEV1 6 months after an asthma diagnosis
# and that a previous model has an adjusted R-squared of 0.2,
# that FEV1 values in the population have a mean of 1.9 and SD of 0.6,
# and that we plan to consider 25 candidate predictor parameters
# The sample size code is then: 

pmsampsize(type = "c", rsquared = 0.35, parameters = 34, intercept = 0, sd = 4.855042)

# How many participants are required to meet the minimum sample size criteria and 
# what is the SPP (subjects per predictor parameter)?
  
# Which of the criteria is driving this minimum sample size?

##########
# Part 2 #
##########

# Now consider a binary outcome

# For this, we need to specify a conservative estimate for Cox-Snell R-squared,
# alongside the outcome event proportion (prevalence), the number of candidate 
# predictors, and the target shrinkage factor (0.9).

# Let's say we want to predict the risk of adverse outcome by 6 months in 
# patients with a traumatic brain injury.
# Previous studies suggest the outcome event proportion is 0.174, & a previous
# model suggests an adjusted Cox-Snell R-squared of 0.288. This corresponds to 
# a Nagelkerke R-squared of 0.288*max(Cox-Snell R-squared) = 0.288/0.603 = 0.48 
# (or 48%), and so is quite high. We are planning to update this model and 
# potentially include up to 24 predictor parameters.
  
# Then the sample size code is:
  
pmsampsize(type = "b", rsquared = 0.35, parameters = 34, prevalence = 0.065)

# How many participants are required to meet the minimum sample size criteria and 
# what is the EPP (events per predictor parameter)?

# Which of the criteria is driving this minimum sample size?

# Why is the EPP quite small in this example?
  
# What happens to the required EPP if the previous model had rather 
# provided an adjusted Cox-Snell R-squared of 0.1?
  
# Note that the Cox-Snell R-squared may not always be reported for an existing
# prediction model of a binary outcome. In that situation, it is possible 
# to use the reported C-statistic (AUROC) instead, as explained in this 
# paper 

# - Riley RD, Van Calster B, Collins G. A note on estimating the Cox-Snell R(2) 
# from a reported C statistic (AUROC) to inform sample size calculations for 
# developing a prediction model with a binary outcome. Stat Med 2021;40:859-64.

# Essentially we use C to calculate R-squared behind the scenes for you. 
# For example, in the previous example if the adjusted C-statistic of the  
# existing model was reported to be 0.90, then we could use the following code
  
pmsampsize(type = "b", cstatistic = 0.90, parameters = 24, prevalence = 0.174)

# As for  R-squared, the smaller C is, the larger the required sample size.

pmsampsize(type = "b", cstatistic = 0.70, parameters = 24, prevalence = 0.174)

##########
# Part 3 #
##########

# Now consider a survival outcome
  
# For this, we need to specify a conservative value for the model's R-squared, 
# the desired shrinkage factor (0.9), the anticipated mean follow-up time, 
# the outcome event rate in the population, and the time-point of interest. 
# We need to use the same time-unit when we specify the rate, mean follow-up, 
# and the time-point of interest.

# Assume we want to predict the risk of a recurrence of a venous thromboembolism
# by 2 years after cessation of treatment for the first occurrence.
# A previous Cox model suggests a Cox-Snell R-squared adjusted of 0.051. 
# Further, a previous cohort study had a mean follow-up of 2.07 years, and
# an overall event rate of 0.065 (that is 0.065 events per person-year).  
# We want to update and extend the existing model and will include potentially 
# 25 predictor parameters. 


pmsampsize(type = "s", rsquared = 0.051, parameters = 25, rate = 0.0065,
           timepoint = 10, meanfup = 10.09)

# How many participants are required to meet the minimum sample size criteria and 
# what is the EPP (events per predictor parameter)?

# Which of the criteria is driving this minimum sample size?

# Why is the sample size required so large in this example?

# What happens to the required EPP if the previous model had rather reported 
# an adjusted Cox-Snell R-squared of 0.25? 

ev_sum <- sum(Subjects_full$macce_10y)
n <- nrow(Subjects_full)

ev_rate = ev_sum / (n * 10)
print(ev_rate)

pmsampsize(type = "s", rsquared = 0.04, parameters = 34, rate = ev_rate,
           timepoint = 10.0, meanfup = 12.0)

pmsampsize(type = "s", rsquared = 0.04, parameters = 40, rate = ev_rate,
           timepoint = 10.0, meanfup = 12.0)

##############################################
# APPLICATION OF LASSO FOR MODEL DEVELOPMENT #
##############################################

##########
# Part 4 #
##########

# Now we will consider applying shrinkage and variable selection during the 
# model estimation by using the lasso. This requires the use of the glmnet package

# load libraries
library(glmnet)

# BINARY OUTCOME EXAMPLE

# (we will only include binary outcomes in this special practical - but the 
# lasso can be applied to survival models adapting Cox regression using glmnet
# and the family = Cox option)

# Set working directory
# Example given below, replace with the path to your working folder
#setwd("birm_rp/")

# Load dataset - including mfp variables
#rest_hc <- read.csv("/well/doherty/projects/scratch/ecg_hc/rest_hc.csv")
rest_hc <- read.csv("ecg_scores/macce_res/features/rest_mid_hcf.csv")
rest_hc <- na.omit(rest_hc)
rest_hc <- merge(rest_hc, Subjects_full[, c("eid", "age",'Sex','BodyMassIndex.Bmi.','diab_t1', 'diab_t2','hyp','bp','TownsendDeprIndexRecruit','hdl_ratio','Corticosteroid', 'Migraine','ra', 'lupus', 'sme','af','sbp5','EthnicBackground','SmokeStatus_never','SmokeStatus_former','SmokeStatus_current','macce_10y','macce_obs_time','ukbb_q')], by = "eid")


#calculate qrisk 3 interaction terms 
rest_hc['dage'] <- rest_hc$age /10
rest_hc['dbmi'] <- rest_hc$BodyMassIndex.Bmi. /10

rest_hc['age_1'] <- ifelse(rest_hc$Sex == 1, rest_hc$adge^-1, rest_hc$dage^-2)
rest_hc['age_2'] <- ifelse(rest_hc$Sex == 1, rest_hc$adge^3, rest_hc$dage)

rest_hc['bmi_1'] <- rest_hc$dbmi^-2
rest_hc['bmi_2'] <- rest_hc$dbmi^-2 * log(rest_hc$dbmi)


rest_hc['age_1_hyp'] <-  rest_hc$age_1  *rest_hc$hyp
rest_hc['age_1_diab'] <-  rest_hc$age_1  *rest_hc$diab_t1 + rest_hc$age_1  *rest_hc$diab_t2
rest_hc['age_1_bmi'] <-  rest_hc$age_1  *rest_hc$bmi_1 + rest_hc$age_1  *rest_hc$bmi_2
rest_hc['age_1_dep'] <-  rest_hc$age_1  *rest_hc$TownsendDeprIndexRecruit
rest_hc['age_1_bp'] <-  rest_hc$age_1  *rest_hc$bp

rest_hc['age_2_hyp'] <-  rest_hc$age_2  *rest_hc$hyp
rest_hc['age_2_diab'] <-  rest_hc$age_2  *rest_hc$diab_t1 + rest_hc$age_2  *rest_hc$diab_t2
rest_hc['age_2_bmi'] <-  rest_hc$age_2  *rest_hc$bmi_1 + rest_hc$age2  *rest_hc$bmi_2
rest_hc['age_2_dep'] <-  rest_hc$age_2  *rest_hc$TownsendDeprIndexRecruit
rest_hc['age_2_bp'] <-  rest_hc$age_2  *rest_hc$bp
attach(rest_hc)


# We will begin by fitting a model which includes the following predictors:
# HTN, SEX, IAGE__1, HYP, HRT, PMI, ISTE__1
# Remember that predictors IAGE__1 and ISTE__1, represent transformations of AGE 
# and STE, respectively (which is IAGE_1 = age^3 and ISTE_1 = STE)

# Fit standard logistic regression model selected from practical 2
mod_logistic <- glm(macce_10y ~ PR.interval_0 + QRS.interval_0 + QTc.interval_0 + RRInterval_0 + HRV_RMSSD_0 + entropy_0 + Ireg_0 + reg_0 + dense_0 + ant_0 + pace_0 + afe_0 + ate_0 , family = binomial)

mod_logistic

# recall: this model does not allow any variable selection & is not adjusted for 
# optimism - in the previous practical 3 we used a uniform shrinkage to adjust 
# for optimism post-estimation, which shrinks all predictors by the same value

# Lasso allows us to adjust for optimism when we actually fit the model and has 
# the additional benefit of shrinking each predictor differently (according to 
# their variance) and thus allows variable selection (if some predictors are 
# shrink to zero).

# So let us fit a logistic regression with a lasso penalty (alpha = 1), and use 
# cross validation (cv) to select the estimation of lambda (the penalty term)  
# using the default stopping options. 
# First we need to define the matrix of predictor values
pickle <- import("pickle")
#fold_path <- "/gpfs3ecg_scores/folds/"
fold_path <- "ecg_scores/macce_res/full_set/folds/"
train_fold <- read.csv(paste0(fold_path,"train_0_fold.csv"))$X0
val_fold <- read.csv(paste0(fold_path,"val_0_fold.csv"))$X0
test_fold <- read.csv(paste0(fold_path,"test_0_fold.csv"))$X0

train_df <- rest_hc[rest_hc$eid %in% train_fold, ]
test_df <- rest_hc[rest_hc$eid %in% test_fold, ]
attach(train_df)
x <- as.matrix(data.frame(PR.interval_0 , QRS.interval_0 , QTc.interval_0 , RRInterval_0 , HRV_RMSSD_0 , entropy_0 , Ireg_0 , reg_0 , dense_0 , ant_0 , pace_0 , afe_0 , ate_0 , age , Sex , BodyMassIndex.Bmi. , diab_t1 , diab_t2 , hyp , bp , TownsendDeprIndexRecruit , hdl_ratio , Corticosteroid , Migraine , ra , lupus , sme , af , sbp5 , EthnicBackground ,   SmokeStatus_current,SmokeStatus_former,SmokeStatus_never ))

y <- Surv(train_df$macce_obs_time, train_df$macce_10y)

cv.lambda <-cv.glmnet(x, y=y, alpha = 0.5, family = "cox") # fit <- glmnet(x, y, family = "cox", alpha = 0.5)
# we can plot the cross validation estimate of lambda
png("/gpfs3ecg_scores/rs_model/EN_plot.png", type="cairo")  
plot(cv.lambda)
dev.off()
# the CV estimate of lambda is 0.000383

# fit the lasso with this lambda
lasso_fit<-glmnet(x, y=y, alpha = 0.5,keep = c("SmokeStatus_never","SmokeStatus_former","SmokeStatus_current"), family = "cox", 
                  lambda = cv.lambda$lambda.min,)

# display model estimates 
coef(lasso_fit)

# Which predictors has the lasso selected for inclusion?

# compare these lasso results to the original logistic regression estimates 
#mod_logistic
#cox_norm <-coxph(Surv(rest_hc$t, rest_hc$y)~PR.interval_0 + QRS.interval_0 + QTc.interval_0 + RRInterval_0 + HRV_RMSSD_0 + entropy_0 + Ireg_0 + reg_0 + dense_0 + ant_0 + pace_0 + afe_0 + ate_0 + age + Sex + BodyMassIndex.Bmi. + diab_t1 + diab_t2 + hyp + bp + TownsendDeprIndexRecruit + hdl_ratio + Corticosteroid + Migraine + ra + lupus + sme + af + sbp5 + EthnicBackground + SmokeStatus_never + SmokeStatus_former + SmokeStatus_current)
#elatic net selected vars
cox_norm <-coxph(Surv(train_df$macce_obs_time, train_df$macce_10y)~ PR.interval_0  + QRS.interval_0 + QTc.interval_0 + RRInterval_0 + HRV_RMSSD_0 + entropy_0 + Ireg_0 + reg_0 + afe_0 +  age + Sex + BodyMassIndex.Bmi. + diab_t1 + diab_t2 + hyp + bp + TownsendDeprIndexRecruit + hdl_ratio + Corticosteroid + Migraine + ra + lupus + sme + af + sbp5 + EthnicBackground +  SmokeStatus_current +  SmokeStatus_former + SmokeStatus_never)
cox_norm
library(mfp)
library(reticulate)
# Define the variables and interactions to keep
keep_vars <- c("age_1", "age_2", "Sex", "bmi_1", "bmi_2", "diab_t1", "diab_t2", "hyp", "bp", "TownsendDeprIndexRecruit", "hdl_ratio", "Corticosteroid", "Migraine", "ra", "lupus", "sme", "af", "sbp5", "EthnicBackground", "SmokeStatus_never", "SmokeStatus_former", "SmokeStatus_current", "age_1*hyp", "age_1*diab_t1", "age_1*diab_t2", "age_1*bmi_1", "age_1*bmi_2", "age_1*TownsendDeprIndexRecruit", "age_1*bp", "age_2*hyp", "age_2*diab_t1", "age_2*diab_t2", "age_2*bmi_1", "age_2*bmi_2", "age_2*TownsendDeprIndexRecruit", "age_2*bp")
# fp(dense_0) + fp(ant_0) + fp(pace_0)fp(ate_0)
mfp_mod <- mfp(Surv(train_df$t,train_df$y)~fp(PR.interval_0) + fp(QRS.interval_0) + fp(QTc.interval_0) + fp(RRInterval_0) + fp(HRV_RMSSD_0) + fp(entropy_0) + fp(Ireg_0) + fp(reg_0) + fp(dense_0) + fp(ant_0) + fp(pace_0) + fp(afe_0) + fp(ate_0) + fp(age) + Sex + fp(BodyMassIndex.Bmi.) + diab_t1 + diab_t2 + hyp + bp + fp(TownsendDeprIndexRecruit) + hdl_ratio + Corticosteroid + Migraine + ra + lupus + sme + af + sbp5 + EthnicBackground + SmokeStatus_never + SmokeStatus_former + SmokeStatus_current,family="cox",verbose=TRUE,select=0.1,,data=train_df)

mfp_elatic <- mfp(Surv(train_df$t,train_df$y)~fp(PR.interval_0) + fp(QRS.interval_0) + fp(QTc.interval_0) + fp(RRInterval_0) + fp(HRV_RMSSD_0) + fp(entropy_0) + fp(Ireg_0) + fp(reg_0) + fp(age) + Sex + fp(BodyMassIndex.Bmi.) + diab_t1 + diab_t2 + hyp + bp + fp(TownsendDeprIndexRecruit) + hdl_ratio + Corticosteroid + Migraine + ra + lupus + sme + af + sbp5 + EthnicBackground + SmokeStatus_never + SmokeStatus_former + SmokeStatus_current,family="cox",verbose=TRUE,select=1,,data=train_df)

mfp_mod_all <- mfp(Surv(train_df$t,train_df$y)~fp(PR.interval_0) + fp(QRS.interval_0) + fp(QTc.interval_0) + fp(RRInterval_0) + fp(HRV_RMSSD_0) + fp(entropy_0) + fp(Ireg_0) + fp(reg_0) + fp(dense_0) + fp(ant_0) + fp(pace_0) + fp(afe_0) + fp(ate_0) + fp(age) + Sex + fp(BodyMassIndex.Bmi.) + diab_t1 + diab_t2 + hyp + bp + fp(TownsendDeprIndexRecruit) + hdl_ratio + Corticosteroid + Migraine + ra + lupus + sme + af + sbp5 + EthnicBackground + SmokeStatus_never + SmokeStatus_former + SmokeStatus_current,family="cox",verbose=TRUE,select=1,,data=rest_hc)

qvars_fp_mod <- mfp(Surv(train_df$macce_obs_time,train_df$macce_10y)~fp(PR.interval_0) + fp(QRS.interval_0) + fp(QTc.interval_0) + fp(RRInterval_0) + fp(HRV_RMSSD_0) + fp(entropy_0) + fp(Ireg_0) + fp(reg_0) + fp(pace_0) + fp(afe_0) + fp(age) + Sex + fp(BodyMassIndex.Bmi.) + diab_t1 + diab_t2 + hyp + bp + TownsendDeprIndexRecruit + hdl_ratio + Corticosteroid + Migraine + ra + lupus + sme + af + sbp5 + EthnicBackground + SmokeStatus_never + SmokeStatus_former + SmokeStatus_current,family="cox",verbose=TRUE,select=1,data=train_df)
qvars_fp_full <- mfp(Surv(train_df$macce_obs_time,train_df$macce_10y)~fp(PR.interval_0) + fp(QRS.interval_0) + fp(QTc.interval_0) + fp(RRInterval_0) + fp(HRV_RMSSD_0) + fp(entropy_0) + fp(Ireg_0) + fp(reg_0) +  fp(afe_0) + fp(dense_0) + fp(ant_0) + fp(pace_0)+fp(ate_0) + fp(age) + Sex + fp(BodyMassIndex.Bmi.) + diab_t1 + diab_t2 + hyp + bp + TownsendDeprIndexRecruit + hdl_ratio + Corticosteroid + Migraine + ra + lupus + sme + af + sbp5 + EthnicBackground + SmokeStatus_never + SmokeStatus_former + SmokeStatus_current,family="cox",verbose=TRUE,select=1,data=train_df)


qrisk_fp_mod <- mfp(Surv(train_df$t,train_df$y)~fp(PR.interval_0) + fp(QRS.interval_0) + fp(QTc.interval_0) + fp(RRInterval_0) + fp(HRV_RMSSD_0) + fp(entropy_0) + fp(Ireg_0) + fp(reg_0) + fp(dense_0) + fp(ant_0) + fp(pace_0) + fp(afe_0) + fp(ate_0) + age_1 +age_2 + Sex + bmi_1 + bmi_2 + diab_t1 + diab_t2 + hyp + bp + TownsendDeprIndexRecruit + hdl_ratio + Corticosteroid + Migraine + ra + lupus + sme + af + sbp5 + EthnicBackground + SmokeStatus_never + SmokeStatus_former + SmokeStatus_current + age_1*hyp + age_1*diab_t1 + age_1*diab_t2 + age_1*bmi_1 + age_1*bmi_2 + age_1*TownsendDeprIndexRecruit+age_1*bp+age_2*hyp + age_2*diab_t1 + age_2*diab_t2 + age_2*bmi_1 + age_2*bmi_2 + age_2*TownsendDeprIndexRecruit + age_2*bp,family="cox",verbose=TRUE,select=0.1,data=train_df,keep =keep_vars)
qrisk_fp_laso <- mfp(Surv(train_df$t,train_df$y)~ fp(PR.interval_0) + fp(QRS.interval_0) + fp(QTc.interval_0) + fp(RRInterval_0) + fp(HRV_RMSSD_0) + fp(entropy_0) + fp(pace_0) + fp(reg_0) + fp(dense_0) +fp(afe_0) + age_1 +age_2 + Sex + bmi_1 + bmi_2 + diab_t1 + diab_t2 + hyp + bp + TownsendDeprIndexRecruit + hdl_ratio + Corticosteroid + Migraine + ra + lupus + sme + af + sbp5 + EthnicBackground + SmokeStatus_never + SmokeStatus_former + SmokeStatus_current + age_1*hyp + age_1*diab_t1 + age_1*diab_t2 + age_1*bmi_1 + age_1*bmi_2 + age_1*TownsendDeprIndexRecruit+age_1*bp+age_2*hyp + age_2*diab_t1 + age_2*diab_t2 + age_2*bmi_1 + age_2*bmi_2 + age_2*TownsendDeprIndexRecruit + age_2*bp,family="cox",verbose=TRUE,select=1,data=train_df)

qrisk_fp_full <- mfp(Surv(train_df$t,train_df$y)~fp(PR.interval_0) + fp(QRS.interval_0) + fp(QTc.interval_0) + fp(RRInterval_0) + fp(HRV_RMSSD_0) + fp(entropy_0) + fp(Ireg_0) + fp(reg_0) +  fp(afe_0) + fp(dense_0) + fp(ant_0) + fp(pace_0)+fp(ate_0)+age_1 +age_2 + Sex + bmi_1 + bmi_2 + diab_t1 + diab_t2 + hyp + bp + TownsendDeprIndexRecruit + hdl_ratio + Corticosteroid + Migraine + ra + lupus + sme + af + sbp5 + EthnicBackground + SmokeStatus_never + SmokeStatus_former + SmokeStatus_current + age_1*hyp + age_1*diab_t1 + age_1*diab_t2 + age_1*bmi_1 + age_1*bmi_2 + age_1*TownsendDeprIndexRecruit+age_1*bp+age_2*hyp + age_2*diab_t1 + age_2*diab_t2 + age_2*bmi_1 + age_2*bmi_2 + age_2*TownsendDeprIndexRecruit + age_2*bp,family="cox",verbose=TRUE,select=1,data=train_df,keep =keep_vars)
qrisk_fp_el <- mfp(Surv(train_df$t,train_df$y)~fp(PR.interval_0) + fp(QRS.interval_0) + fp(QTc.interval_0) + fp(RRInterval_0) + fp(HRV_RMSSD_0) + fp(entropy_0) + fp(Ireg_0) + fp(reg_0) +  fp(afe_0) + age_1 +age_2 + Sex + bmi_1 + bmi_2 + diab_t1 + diab_t2 + hyp + bp + TownsendDeprIndexRecruit + hdl_ratio + Corticosteroid + Migraine + ra + lupus + sme + af + sbp5 + EthnicBackground + SmokeStatus_never + SmokeStatus_former + SmokeStatus_current + age_1*hyp + age_1*diab_t1 + age_1*diab_t2 + age_1*bmi_1 + age_1*bmi_2 + age_1*TownsendDeprIndexRecruit+age_1*bp+age_2*hyp + age_2*diab_t1 + age_2*diab_t2 + age_2*bmi_1 + age_2*bmi_2 + age_2*TownsendDeprIndexRecruit + age_2*bp,family="cox",verbose=TRUE,select=1,data=train_df,keep =keep_vars)

save.image(file = "ecg_scores/fp_models.RData")


# Now calculate the shrunken models baseline survival prob at 5 years by setting the shrunken lp as a offset and predicting the baseline survival
library(dynpred)
cox_n <-(qvars_fp_full$formula)
pred_LP <- predict(cox_n,type = 'lp')
test_lp <- predict(cox_n, newdata = test_df,type = 'lp')
test_risk = predict(cox_n, newdata = test_df,type = 'risk')
fp_mod <- coxph(Surv(train_df$macce_obs_time,train_df$macce_10y)~offset(pred_LP))
y10_Cox_shrunk <- summary(survfit(fp_mod),time=10)$surv
y10_Cox_shrunk
test_df['fp_pred'] <- 100 * (1 - y10_Cox_shrunk^exp(test_lp))

surv_test <- Surv(test_df$macce_obs_time, test_df$macce_10y)

concordance_index <- rcorr.cens(test_risk, surv_test)
concordance_index
summary(coxph(Surv(test_df$macce_obs_time,test_df$macce_10y)~offset(test_lp)))$concordance[6]


Y <- test_df[,'macce_10y']
P <- test_df[,'fp_pred']/100 # q_surv$ukbb_q / 100 #1 - ecg_surv$surv_10  surv_prob_q

loess.calibrate <- loess(Y ~ P)

# Estimate loess-based smoothed calibration curve

P.calibrate <- predict (loess.calibrate, newdata = P)

# This is the point on the loess calibration curve corresponding to a given predicted probability.

ICI <- mean (abs(P.calibrate - P))
ICI


P <- test_df[,'ukbb_q']/100 # q_surv$ukbb_q / 100 #1 - ecg_surv$surv_10  surv_prob_q

loess.calibrate <- loess(Y ~ P)

# Estimate loess-based smoothed calibration curve

P.calibrate <- predict (loess.calibrate, newdata = P)

# This is the point on the loess calibration curve corresponding to a given predicted probability.

ICI <- mean (abs(P.calibrate - P))
ICI


# Broadly how much shrinkage has taken place when using the lasso?

# Many researchers are excited by lasso as it allows them to consider 
# hundreds or even thousands of predictors - however, this is dangerous when 
# the sample size is small as it leads to instability and large shrinkage, even 
# for important predictors 

# Let us now create 50 noise continuous variables - i.e. ones with no prognostic 
# value at all (unbeknown to the researcher) - that are to be added to the 
# set of candidate predictors; call these variables v1 to v50 */

# define matrix for 50 variables
mymat <- matrix(nrow=2188, ncol=50)
# replace entries with random values from normal(0,1)
for(i in 1:dim(mymat)[1]) {
  for(j in 1:dim(mymat)[2]) {
    # set a unique seed for each draw
    set.seed = (i*100)+ j
    mymat[i,j] = rnorm(1,0,1)
  }
}
    
# now create a data frame from this matrix and then merge with original data
noise <- data.frame(mymat)
newdata <- cbind(Practical_GustoW_dev_mfp, noise)

# the 50 noise variables are denoted by X1 to X50

# now fit a logistic regression with the 7 predictors + 50 noise variables
mod_logistic2 <- glm(DAY30~HTN+SEX+IAGE__1+HYP+HRT+PMI+ISTE__1
                    +X1+X2+X3+X4+X5+X6+X7+X8+X9+X10
                    +X11+X12+X13+X14+X15+X16+X17+X18+X19+X20 
                    +X21+X22+X23+X24+X25+X26+X27+X28+X29+X30 
                    +X31+X32+X33+X34+X35+X36+X37+X38+X39+X40 
                    +X41+X42+X43+X44+X45+X46+X47+X48+X49+X50 
                    ,family="binomial",x=TRUE,y=TRUE, data=newdata)
mod_logistic2

# heuristic shrinkage 
# Obtain chi2
null_model <- glm(DAY30~1,family="binomial")
chisq <- anova(null_model,mod_logistic2,test="Chisq")
chi2 <- chisq$Deviance[2]
chi2
df_log <- chisq$Df[2]
df_log
vanH <- (chi2 - df_log)/chi2
vanH
# this shows considerable overfitting as expected 
# (indeed we are breaking our sample size guidance here!)

# let us apply lasso and see what variables are selected
attach(newdata)
z <- as.matrix(data.frame(HTN, SEX, IAGE__1, HYP, HRT, PMI, ISTE__1,
                          X1, X2, X3, X4, X5, X6, X7, X8, X9, X10
                          , X11, X12, X13, X14, X15, X16, X17, X18, X19, X20 
                          , X21, X22, X23, X24, X25, X26, X27, X28, X29, X30 
                          , X31, X32, X33, X34, X35, X36, X37, X38, X39, X40 
                          , X41, X42, X43, X44, X45, X46, X47, X48, X49, X50 ))
 

cv.lambda2 <-cv.glmnet(z, y=DAY30, alpha = 1, family = "binomial")
 # we can plot the cross validation estimate of lambda
 plot(cv.lambda2)
 cv.lambda2
 # the CV estimate of lambda is 0.004298
 
 # fit the lasso with this lambda
 lasso_fit2<-glmnet(z, y=DAY30, alpha = 1, family = "binomial", 
                   lambda = cv.lambda2$lambda.min)
 
# display model estimates 
 lasso_fit2
coef(lasso_fit2)

# How many predictors are selected by the lasso?  In particular, consider 
# how many of the original 7 predictors are retained and how many noise 
# variables were selected.

# Compare the logistic and lasso estimates - how much shrinkage was applied by the lasso now?
mod_logistic2

### LEARNING POINT ###
# Here is an example where lasso selects many noise variables 
# and considerably shrinks the prognostic effect of important predictors. 
# This leads to much instability and the model is unlikely to perform well in 
# new data. 

# In comparison, in the previous example without the noise predictors, there 
# was only a small amount of shrinkage necessary and the lasso results are 
# more stable and more likely to perform well in new data. This emphasises the 
# importance of adhering to sample size recommendations to minimise overfitting