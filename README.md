# deluge_synthesis
Project synthesizing deluges at CPER

**Data processing workflow and files (CONTINOUS DATA):**
1. **"VAR_merge.Rmd"** files input raw data from each study and exports **"VAR_harmonized.csv"** files (combined all studies together) -- these files are different for each response variable so only need to run whatever response is being updated
2. **"Treatment_Standardizing_Filtering.Rmd"** inputs **"VAR_harmonized.csv"** files and exports **"VAR_filtered.csv"** files (removing studies, years, or treatments that we do not want and making sure all treatment/variable names and labels are correct) -- these files all include all studies/variables that we are interested in and need to be updated any time VAR_merge is updated
3. **"cont_processing.Rmd"** collapses full plot-level data from **"VAR_filtered.csv"** files into daily averages (at the treatment instead of plot level -- **"continuous_long_dailyavs.csv"**) and calculates relative 20% thresholds, response lengths (days > threshold) and response AUC's (magnitude and length together) exported as **"continuous_response_calcs.csv"** 
4. **"cont_data_analysis.Rmd"** runs linear models on **"continuous_response_calcs.csv"** and model comparisons

*Note: several other files/code were used in intermediate steps before this workflow -- Continuous_Exploration_GCC-VWC-SR.Rmd has lot of additional threshold options, ESA_continuous_analysis.Rmd was primarily used for data processing/plot in ESA 2206 talk (and used to write "cont_processing.Rmd")