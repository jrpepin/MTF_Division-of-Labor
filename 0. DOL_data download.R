#####################################################################################
# Set-up the Directories
mainDir <- "C:/Users/Joanna/Dropbox/Data/" # This should be your master data folder 
subDir  <- "@Monitoring the Future/icpsr_data" # This will be the name of the folder where we download the MTF data
dataDir <- file.path(mainDir, subDir)

## This will create a sub-directory folder in the master data directory if doesn't exist
if (!dir.exists(dataDir)){
  dir.create(dataDir)
} else {
  print("Data directory already exists!")
}

setwd(file.path(mainDir, subDir)) # Set the working-directory to the sub-folder where we will download the data

## Load libraries
library(icpsrdata)
library(tidyverse)

#####################################################################################
# Download the data
setwd(file.path(mainDir, subDir)) # Set the working-directory to the sub-foler where we will download the data

# https://cran.r-project.org/web/packages/icpsrdata/vignettes/icpsrdata-vignette.html
if (!require(remotes)) install.packages("remotes")
remotes::install_github("fsolt/icpsrdata")

icpsr_download(file_id = c( 7927,  7928,  7929,  7930,
                            7900,  9013,  9045,  8387,  8388, 
                            8546,  8701,  9079,  9259,  9397, 
                            9745,  9871,  6133,  6367,  6517,
                            6716,  2268,  2477,  2751,  2939,
                            3184,  3425,  3753,  4019,  4264,
                            4536, 20022, 22480, 25382, 28401,
                           30985, 34409, 34861, 35218, 36263))
########################################################################################################
# Delete the extra files downloaded to clear up space on your computer.

## Clean up folders -- WARNING -- The code below will delete files on your hard-drive. USE WITH CAUTION.
### Remove the # to run the following code.

### Make sure working directory is the dataDir
getwd()

### Delete unused SAS files
# list(list.files(pattern = "\\.sas$", recursive = TRUE)) # Make sure list only includes file in the sub-directory
# do.call(file.remove, list(list.files(pattern = "\\.sas$", recursive = TRUE))) # Delete the files

# list(list.files(pattern = "\\.xpt$", recursive = TRUE)) # Make sure list only includes file in the sub-directory
# do.call(file.remove, list(list.files(pattern = "\\.xpt$", recursive = TRUE))) # Delete the files

### Delete unused SPSS files
# list(list.files(pattern = "\\.por$", recursive = TRUE)) # Make sure list only includes file in the sub-directory
# do.call(file.remove, list(list.files(pattern = "\\.por$", recursive = TRUE))) # Delete the files

# list(list.files(pattern = "\\.sps$", recursive = TRUE)) # Make sure list only includes file in the sub-directory
# do.call(file.remove, list(list.files(pattern = "\\.sps$", recursive = TRUE))) # Delete the files

#####################################################################################
# Importing the (Stata) data files

form2dta <- list.files(pattern = ".-0002-Data.dta$|.-0003-Data.dta$", recursive = TRUE) # create a list of each Form 2 data file -- different folders based on the year

mtf <- ldply(form2dta, read_dta) # Convert the list of data frames into one data frame

## Keep only Form 2 variables
mtf <- select(mtf, V1, V5, V13, ARCHIVE_WT, starts_with("V2"))

## Keep only Form 2 survey respondents
mtf <- subset(mtf, !is.na(V2151))

## Save the dataframe for easy open in the future
### Note: This data is NOT harmonized. Make frequent and judicious referral to the codebooks.
save(mtf, file="mtf_form2.Rda")