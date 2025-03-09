#!/bin/bash
#BSUB -q long
#BSUB -n 12
#BSUB -R "rusage[mem=204800, ngpus_excl_p=1]"
#BSUB -M 204800
#BSUB -gpu "num=1:mode=exclusive_process"
#BSUB -o /export/home/dor/dlucko/Downloads/job_output_%J.log
#BSUB -e /export/home/dor/dlucko/Downloads/job_error_%J.log

module load R

export LD_LIBRARY_PATH=/export/home/dor/dlucko/local/lib:$LD_LIBRARY_PATH
export R_LIBS_USER=/export/home/dor/dlucko/Rlibs

Rscript /export/home/dor/dlucko/Downloads/analysis.R
