#!/bin/bash  

echo "------------------------------------------------" 
echo "Run on host: "`hostname` 
echo "Operating system: "`uname -s` 
echo "Username: "`whoami` 
echo "Started at: "`date` 
echo "------------------------------------------------" 
 
echo "Hello, world!"


echo SLURM_ARRAY_TASK_MIN=${SLURM_ARRAY_TASK_MIN}, SLURM_ARRAY_TASK_MAX=${SLURM_ARRAY_TASK_MAX}, SLURM_ARRAY_TASK_STEP=${SLURM_ARRAY_TASK_STEP} 

model="score2"

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
module load R/4.1.0-foss-2021a
# Check if the number of arguments is not equal to 5
echo "Received arguments:"
for arg in "$@"; do
        echo "$arg"
    done

if [ "$#" -ne 7 ]; then #6 command line arguments + 1 slurm argument
        echo "Usage: $0 arg1 arg2 arg3 arg4 arg5"
        echo  "$#"
            exit 1
        fi 

model=$1
store=$2
split=$3
fit_poly=$4
model_type=$5
dat=$6
fp_terms=$7

echo 'ID'
echo ${SLURM_ARRAY_TASK_ID}
echo 'model'
echo ${model}
echo 'store'
echo ${store}
echo 'split'
echo ${split}
echo 'fit poly'
echo ${fit_poly}
echo 'type'
echo ${model_type}
echo 'dat source'
echo ${dat}
echo 'terms'
echo ${fp_terms}





Rscript ARS_array_stacked.R ${SLURM_ARRAY_TASK_ID} ${model} ${store} ${split} ${fit_poly} ${model_type} ${dat} ${fp_terms}
echo `date`: task complete 
exit $rv 


