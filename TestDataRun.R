# Test script for running Franken on published test bone marrow data from Bendall et al. 2011

# Define path where your data is stored
yourdatafolder = "~/Downloads/11295719"

DataDir = yourdatafolder

setwd(DataDir)

# Name and create directory to save results
fseed 					= 123
resultsLabel            = paste0('Franken_s',fseed)
resultsDir              = file.path(DataDir, resultsLabel)
dir.create(resultsDir)

# Find all .fcs files in this directory 
fileNames               = list.files(path = DataDir, pattern = "\\.fcs$", all.files = FALSE,
                        full.names = FALSE, recursive = FALSE,
                        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# Read in fcs input files:
readin_files            = ReadInput(fileNames,  scale = FALSE)

marker_ids              = 5:36 # Levine_32dim

frk_ls            		= Franken(readin_files, resultsDir, marker_ids, fseed = fseed)
read_aff_SOM 			= frk_ls$read_aff_SOM
consensus_output 		= frk_ls$metaclus
markerOutputs 			= frk_ls$markerOutputs

