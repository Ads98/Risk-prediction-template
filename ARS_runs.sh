#!/bin/bash  

echo "------------------------------------------------" 
echo "Run on host: "`hostname` 
echo "Operating system: "`uname -s` 
echo "Username: "`whoami` 
echo "Started at: "`date` 
echo "------------------------------------------------" 
 
echo "Hello, world!"


echo SLURM_ARRAY_TASK_MIN=${SLURM_ARRAY_TASK_MIN}, SLURM_ARRAY_TASK_MAX=${SLURM_ARRAY_TASK_MAX}, SLURM_ARRAY_TASK_STEP=${SLURM_ARRAY_TASK_STEP} 


# Loop over task ids in this step 
#while [ "${SLURM_ARRAY_TASK_ID}" -le "${this_step_last}" ] 
#do 
#echo `date`: starting work on SLURM_ARRAY_TASK_ID=`printenv SLURM_ARRAY_TASK_ID` 
         
     ########################################################################################## 
        # 
        #   Do your per-task processing here 
        # 
        ##  For example, run an R script that uses the task id directly: 
        ##  Rscript /path/to/my/rscript.R ${SLURM_ARRAY_TASK_ID} 
        ##  rv=$? 
        # 
        ########################################################################################## 

module swap foss/2018b foss/2021a

sbatch -p short --mem-per-cpu=64G ARS_pred.sh qrisk /ukb_models/cut_off/cv/stacked/all_nri/9y/center/all/ null f pa ukb "StepsDayMedAdjusted,ModerVigorActivity,LightActivity,SedentaryActivity,AccOverallAvg" t
#sbatch -p short --mem-per-cpu=64G ARS_pred.sh qrisk /ukb_models/cut_off/cv/stacked/all_nri/9y/center/all/ null f hc ukb "StepsDayMedAdjusted,SleepActivity" t
sbatch -p short --mem-per-cpu=64G ARS_pred.sh qrisk /ukb_models/cut_off/cv/stacked/all_nri/9y/center/all/ null f ars ukb "ActivityRiskScore" t

sbatch -p short --mem-per-cpu=64G ARS_pred.sh qrisk /ukb_models/cut_off/cv/stacked/all_nri/9y/center/all/ null t ars ukb "ActivityRiskScore" t
#sbatch -p short --mem-per-cpu=64G ARS_pred.sh qrisk /ukb_models/cut_off/cv/stacked/all_nri/9y/center/all/ null t pa ukb "StepsDayMedAdjusted,ModerVigorActivity,LightActivity,SedentaryActivity,AccOverallAvg" t
#sbatch -p short --mem-per-cpu=64G ARS_pred.sh qrisk /ukb_models/cut_off/cv/stacked/all_nri/9y/center/all/ null t hc ukb "StepsDayMedAdjusted,SleepActivity" t


#score2
sbatch -p short --mem-per-cpu=64G ARS_pred.sh score2 /ukb_models/cut_off/cv/stacked/all_nri/9y/center/all/ null f ars ukb "ActivityRiskScore" t
sbatch -p short --mem-per-cpu=64G ARS_pred.sh score2 /ukb_models/cut_off/cv/stacked/all_nri/9y/center/all/ null t ars ukb "ActivityRiskScore" t

sbatch -p short --mem-per-cpu=64G ARS_pred.sh score2 /ukb_models/cut_off/cv/stacked/all_nri/9y/center/all/ null f pa ukb "StepsDayMedAdjusted,ModerVigorActivity,LightActivity,SedentaryActivity,AccOverallAvg" t
#sbatch -p short --mem-per-cpu=64G ARS_pred.sh score2 /ukb_models/cut_off/cv/stacked/all_nri/9y/center/all/ null t pa ukb "StepsDayMedAdjusted,ModerVigorActivity,LightActivity,SedentaryActivity,AccOverallAvg" t

#sbatch -p short --mem-per-cpu=64G ARS_pred.sh qrisk /ukb_models/cut_off/cv/stacked/all_nri/9y/center/all/ null f hc ukb "StepsDayMedAdjusted,SleepActivity" t
#sbatch -p short --mem-per-cpu=64G ARS_pred.sh qrisk /ukb_models/cut_off/cv/stacked/all_nri/9y/center/all/ null t hc ukb "StepsDayMedAdjusted,SleepActivity" t





#########GP labs########################

#QRISK3
#sbatch -p short --mem-per-cpu=64G ARS_pred.sh qrisk /gp_models/cut_off/cv/stacked/all_nri/9y/center/all/ null f pa  gp  "StepsDayMedAdjusted,ModerVigorActivity,LightActivity,SedentaryActivity,AccOverallAvg" t

#sbatch -p short --mem-per-cpu=64G ARS_pred.sh qrisk /gp_models/cut_off/cv/stacked/all_nri/9y/center/all/ null f ars gp "ActivityRiskScore" t

#sbatch -p short --mem-per-cpu=64G ARS_pred.sh qrisk /gp_models/cut_off/cv/stacked/all_nri/9y/center/all/ null t ars gp "ActivityRiskScore" t

#sbatch -p short --mem-per-cpu=64G ARS_pred.sh qrisk /gp_models/cut_off/cv/stacked/all_nri/9y/center/all/ null t pa  gp "StepsDayMedAdjusted,ModerVigorActivity,LightActivity,SedentaryActivity,AccOverallAvg" t

#sbatch -p short --mem-per-cpu=64G ARS_pred.sh qrisk /gp_models/cut_off/cv/stacked/all_nri/9y/center/all/ null f hc ukb "StepsDayMedAdjusted,SleepActivity" t

#sbatch -p short --mem-per-cpu=64G ARS_pred.sh qrisk /gp_models/cut_off/cv/stacked/all_nri/9y/center/all/ null t hc ukb "StepsDayMedAdjusted,SleepActivity" t

                                                                              
                                                                                       
#score2                                                                         
 #sbatch -p short --mem-per-cpu=64G ARS_pred.sh score2 /gp_models/cut_off/cv/stacked/all_nri/center/all/ null f ars gp "ActivityRiskScore" t 
# 
 #sbatch -p short --mem-per-cpu=64G ARS_pred.sh score2 /gp_models/cut_off/cv/stacked/all_nri/center/all/ null t ars gp "ActivityRiskScore" t
# 
 #sbatch -p short --mem-per-cpu=64G ARS_pred.sh score2 /gp_models/cut_off/cv/stacked/all_nri/center/all/ null f pa gp "StepsDayMedAdjusted,ModerVigorActivity,LightActivity,SedentaryActivity,AccOverallAvg" t
# 
# #sbatch -p short --mem-per-cpu=64G ARS_pred.sh score2 /gp_models/cut_off/cv/stacked/all_nri/center/all/ null t pa gp "StepsDayMedAdjusted,ModerVigorActivity,LightActivity,SedentaryActivity,AccOverallAvg" t
# 
# sbatch -p short --mem-per-cpu=64G ARS_pred.sh score2 /gp_models/cut_off/cv/stacked/all_nri/center/ null f hc ukb "StepsDayMedAdjusted,SleepActivity" t
# #sbatch -p short --mem-per-cpu=64G ARS_pred.sh score2 /gp_models/cut_off/cv/stacked/all_nri/center/ null t hc ukb "StepsDayMedAdjusted,SleepActivity" t
# 


#Rscript risk_scores/array_job/ARS_array_stacked.R ${SLURM_ARRAY_TASK_ID} ${model} ${store} ${split} ${fit_poly} ${model_type} ${data_source} "${terms[@]}" 
# Increment SGE_TASK_ID 
#                export SLURM_ARRAY_TASK_ID=$(( SLURM_ARRAY_TASK_ID + 1 )) 
#            done 


echo `date`: task complete 
exit $rv 


#Rscript risk_scoress/array_job/ARS_array_bs.R
