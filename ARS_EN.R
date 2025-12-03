
print("hello")
df <- imputed_datasets[[1]]
attach(df)
score2 <- "Age + SystolBloodPressur + Cholesterol + HdlCholesterol + Diabetes + Smoking_binary + Age * Smoking_binary + Age * Diabetes + Age * SystolBloodPressur + Age * Cholesterol + Age *  HdlCholesterol"

score2_formula <- as.formula(paste("Surv(macce_obs_time, macce.incident) ~", score2))

score2_ars_m <- as.formula(paste("Surv(macce_obs_time, macce.incident) ~", 'ARS_1 +  ',score2,' + ARS_1 * Age'))
score2_ars_f <- as.formula(paste("Surv(macce_obs_time, macce.incident) ~", 'ARS_1 + ARS_2 + ',score2,' + ARS_1 * Age + ARS_2 * Age'))

sex_cond = df$Sex == 1

q_cols <- c('Age','Cholesterol','HdlCholesterol','Diabetes', 'Smoking_binary', 'Age:Smoking_binary' , 'Age:Diabetes' , 'Age:SystolBloodPressur' , 'Age:Cholesterol' , 'Age:HdlCholesterol')
ars_cols_m <- c('ARS_1',q_cols,'ARS_1:Age')
ars_cols_f <- c('ARS_1','ARS_2',q_cols,'ARS_1:Age','ARS_2:Age')

y_m <- Surv(df[sex_cond,]$macce_obs_time, df[sex_cond,]$macce.incident)
y_f <- Surv(df[!sex_cond,]$macce_obs_time, df[!sex_cond,]$macce.incident)

q_rf_m <-   model.matrix(as.formula(score2_formula), data = df[sex_cond,])
ars_rf_m <- model.matrix(as.formula(score2_ars_m), data = df[sex_cond,])

q_rf_f <-   model.matrix(as.formula(score2_formula), data = df[!sex_cond,])
ars_rf_f <- model.matrix(as.formula(score2_ars_f), data = df[!sex_cond,])



print("cv net")
cv.lambda_q_m <-cv.glmnet(q_rf_m, y=y_m, alpha = 0.5, family = "cox",data = df[sex_cond,])

cv.lambda_ars_m <-cv.glmnet(ars_rf_m, y=y_m, alpha = 0.5, family = "cox",data = df[sex_cond,])

cv.lambda_q_f <-cv.glmnet(q_rf_f, y=y_f, alpha = 0.5, family = "cox",data = df[!sex_cond,])

cv.lambda_ars_f <-cv.glmnet(ars_rf_f, y=y_f, alpha = 0.5, family = "cox",data = df[!sex_cond,])

ars_cox <- coxph(formula = score2_ars_m,data = df[sex_cond,])

my_lp = 0
for(col in ars_cols_m){
  print(col)
  my_lp  = my_lp + test_ars_m[,col] * coef(en_ars_m)[col,]
}
print("fit")
# fit the lasso with this lambda
en_q_m <- glmnet(q_rf_m, y=y_m, alpha = 0.5,keep = c("SmokeStatus_never","SmokeStatus_former","SmokeStatus_current"), family = "cox", 
                  lambda = cv.lambda_q_m$lambda.min,)

en_ars_m <- glmnet(ars_rf_m, y=y_m, alpha = 0.5,keep = c("SmokeStatus_never","SmokeStatus_former","SmokeStatus_current"), family = "cox",
                   lambda = 1)

en_q_f <- glmnet(q_rf_f, y=y_f, alpha = 0.5,keep = c("SmokeStatus_never","SmokeStatus_former","SmokeStatus_current"), family = "cox", 
                 lambda = cv.lambda_q_f$lambda.min,)

en_ars_f <- glmnet(ars_rf_f, y=y_f, alpha = 0.5,keep = c("SmokeStatus_never","SmokeStatus_former","SmokeStatus_current"), family = "cox", 
                   lambda = cv.lambda_ars_f$lambda.min,)

print("lasso vars")
# display model estimates 
lasso_coef <- coef(en_ars_m)
lasso_vars <-  names(lasso_coef[lasso_coef[,'s0']!=0,])


en_q_lp_m <- predict(en_q_m,newx = model.matrix(as.formula(score2_formula), data = df_val[val_cond,]),type = 'link')
rs_em_q <- get_scores(df_val[val_cond,],en_q_lp_m,lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = baseline_surv[[1]])

en_q_lp_f <- predict(en_q_f,newx = model.matrix(as.formula(score2_formula), data = df_val[!val_cond,]),type = 'link')
rs_em_q_f <- get_scores(df_val[!val_cond,],en_q_lp_f,lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = baseline_surv[[2]])


en_ars_lp_m <- predict(en_ars_m,newx = model.matrix(as.formula(score2_ars_m), data = df_val[val_cond,]),type = 'link')
rs_em_ars <- get_scores(df_val[val_cond,],en_ars_lp_m,lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = baseline_surv[[3]])

en_ars_lp_f <- predict(en_ars_f,newx = model.matrix(as.formula(score2_ars_f), data = df_val[!val_cond,]),type = 'link')
rs_em_ars_f <- get_scores(df_val[val_cond,],en_ars_lp_f,lab = 'macce.incident',time = 6,th = 0.5,baseline_surv = baseline_surv[[4]])

df_val['q_en'] <- 0
df_val['ars_en'] <- 0

df_val[val_cond,'q_en'] <- rs_em_q
df_val[val_cond,'ars_en'] <- rs_em_ars

df_val[!val_cond,'q_en'] <- rs_em_q_f
df_val[!val_cond,'ars_en'] <- rs_em_ars_f

th = 0.05
calc_nb(df_val[val_cond,],df_val[val_cond,'qrisk3'],lab = 'macce.incident',th = th)
calc_nb(df_val[val_cond,],df_val[val_cond,'q_en'],lab = 'macce.incident',th = th)

calc_nb(df_val[val_cond,],df_val[val_cond,'ars'],lab = 'macce.incident',th = th)
calc_nb(df_val[val_cond,],df_val[val_cond,'ars_en'],lab = 'macce.incident',th = th)

calc_nb(df_val[!val_cond,],df_val[!val_cond,'qrisk3'],lab = 'macce.incident',th = th)
calc_nb(df_val[!val_cond,],df_val[!val_cond,'q_en'],lab = 'macce.incident',th = th)

calc_nb(df_val[!val_cond,],df_val[!val_cond,'ars'],lab = 'macce.incident',th = th)
calc_nb(df_val[!val_cond,],df_val[!val_cond,'ars_en'],lab = 'macce.incident',th = th)

