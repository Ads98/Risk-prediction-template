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

        
        
# Extract command line arguments
model="$1"
store="$2"
split="$3"
fit_poly="$4"
model_type="$5"                                                                      
data_source="$6"                                                                      
terms="$7" 
stack="$8" 

# Print the variables
echo "model: $model"
echo "store: $store"
echo "split: $split"
echo "fit_poly: $fit_poly"
echo "model_type: $model_type"
echo "data_source: $data_source"
echo "terms: $terms"

module load R/4.1.0-foss-2021a

Rscript risk_scores/array_job/9_year_censoring/centered/ARS_main.R  ${model} ${store} ${split} ${fit_poly} ${model_type} ${data_source} "${terms[@]}" ${stack}



sbatch --array 1-1000 risk_scores/array_job/9_year_censoring/centered/nri_scripts/array_arg.sh ${model} ${store} ${split} ${fit_poly} ${model_type} ${data_source} "${terms[@]}"

#Rscript risk_scores/array_job/ARS_array_stacked.R ${SLURM_ARRAY_TASK_ID} ${model} ${store} ${split} ${fit_poly} ${model_type} ${data_source} "${terms[@]}" 
# Increment SGE_TASK_ID 
#                export SLURM_ARRAY_TASK_ID=$(( SLURM_ARRAY_TASK_ID + 1 )) 
#            done 


echo `date`: task complete 
exit $rv 


#Rscript risk_scoress/array_job/ARS_array_bs.R
