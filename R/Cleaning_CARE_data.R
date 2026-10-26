#one time only: install the remotes R package
install.packages('remotes')

#each time you log onto R: load in the remotes R package
library(remotes)

#install pipeline from Chris's github
install_github("cllstrauss/tottagpipe")

#load pipeline
library(tottagpipe)

#go through files that contain CARE data and use the list_folders
#function to identify files that can be processed and cleaned with
#tottagpipe

#one time only: install function to read in excel files
install.packages('readxl')

#load readxl
library(readxl)

#you may need to make slight updates to the path file
folders <- list_folders('X:/Daily_2/ABC/CARE/TotTag Files')

#load in spreadsheet that contains study dates for each deployment
device_dates <- read_excel(
  'X:/Daily_2/ABC/tottag R code/Current Cleaning Process/Device Date Use Generator/Device_Date_Tracking_CARE.xlsx')


#run the pipeline

#update was is in the brackets after folders with the folders you are assigned to clean
#update filepaths to BOX to match your own computer

data_pipeline(folders[], '12m',
              false_movement = 5, zero_ranging = 1, lag_diff_0 = 1,
              clean_data = T, device_dates = device_dates,
              #update path files to BOX
              'C:/Users/chris/Box/SEA Lab/Tottag Data/processed raw data CARE',
              'C:/Users/chris/Box/SEA Lab/Tottag Data/processed clean data CARE')
