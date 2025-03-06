#!/bin/bash
#BSUB -q long                     # Submit to the "long" queue
#BSUB -n 12                       # Request 12 CPU cores
#BSUB -R "rusage[mem=204800]"     # Request 204800 MB (approx. 200 GB) of memory per core
#BSUB -M 204800                   # Set the memory limit to 204800 MB per process
#BSUB -o /export/home/dor/dlucko/Downloads/job_output_%J.log  # Standard output file
#BSUB -e /export/home/dor/dlucko/Downloads/job_error_%J.log   # Standard error file

# Load the R module if required by your environment
module load R

# Set LD_LIBRARY_PATH if you have a local V8 installation (adjust path as needed)
export LD_LIBRARY_PATH=/export/home/dor/dlucko/local/lib:$LD_LIBRARY_PATH
export R_LIBS_USER=/export/home/dor/dlucko/Rlibs

# Run the R script using Rscript
Rscript /export/home/dor/dlucko/Downloads/analysis.R
