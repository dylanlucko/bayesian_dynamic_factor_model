#### This Analysis Uses a LSF Cluster for Computations ####

To run the simulations, you must follow these steps:

1. On the compute cluster of your choosing (modify code if not LSF), ensure the 'analysis.R' and 'submit_job.sh' 
   files are located in the same folder.
1.1. You can either copy the submit_job.sh file directly, or copy its content using the 
     'bash_running_on_cluster_compute.txt' file. 
2. Also, ensure the panel dataset is downloaded on to the compute cluster, this file is meant to work with a ZIP folder. 
   I keep it in 'Downloads' for ease. 
3. The analysis script will unzip the panel data folder, ensure no NAs are present, then run the MCMC simulations. 

# Running the job in BASH

1. Follow the steps above to ensure the file structure is correct. 
2. Open the Terminal and set your CD as the folder where the 'analysis.R' and 'submit_job.sh' live. In this case, 
   I choose the Downloads folder for ease.
	cd /export/home/dor/dlucko/Downloads
3. Make each of the files executable:
	chmod +x analysis.R
	chmod +x submit_job.sh
4. Submit the Job:
	bsub < submit_job.sh


# Compute Cluster Requirements built into .sh file

1. -R ==> 200GB RAM per core
2. -M ==> Set the memory limit to 204800 MB per process 
3. -n ==> 12 CPU cores
4. -gpu ==> 0 GPU cores
5. -q ==> Set the queue to 'long' ==> max 7 days of run time


# Optional Settings

1. -u ==> set email
2. -B ==> Email when job begins
3. -N ==> Email when job ends
4. -o ==> Add an output file 
5. -e ==> Add an error file
6. module load R ==> if R is not loaded on your compute cluster


# Final Notes

1. To stop a job that you began, use the command 'bkill <jobid> (Bash Kill)
2. To check the status of all running jobs, use the command bjobs (Bash Jobs)