#####################################################################################
# Set-up the environment

## Set-up the Directories
mainDir <- "C:/Users/joanna/Dropbox/Data/" # This should be your master data folder 
subDir  <- "@Monitoring the Future/icpsr_data" # This will be the name of the folder where you saved the MTF data
dataDir <- file.path(mainDir, subDir)

repoDir <- "C:/Users/joanna/Dropbox/Repositories/MTF_Division-of-Labor" # This should be your master project folder (Project GitRepository)
outDir <- file.path(repoDir, "data") # This will be the name of the folder where data output goes
figDir <- file.path(repoDir, "figures") # This will be the name of the folder where figures are saved

## This will create a data sub-directory folder in the master project directory if doesn't exist
if (!dir.exists(outDir)){
  dir.create(outDir)
} else {
  print("output directory already exists!")
}

## This will create a figures sub-directory folder in the master project directory if doesn't exist
if (!dir.exists(figDir)){
  dir.create(figDir)
} else {
  print("figure directory already exists!")
}

setwd(repoDir) # This will set the working directory to the master project folder

## Open the data
load(paste0(dataDir, "/mtf_form2.Rda"))

## Load the libraries
library(tidyverse)

#####################################################################################
# Set-up the data

## Select Variables
data <- select(mtf, V5, ARCHIVE_WT, V1, V13,                           # Survey variables
               V2248, V2249, V2250, V2251, V2252, V2253, V2239,        # Project specific
               V2151, V2150, V2155, V2156,  V2164, V2165, V2169)       # Demographic

## Rename Variables
data <- dplyr::rename(data,      
                      wt7611 = V5,      wt1214   = ARCHIVE_WT,  raceeth  = V2151,
                      hfw0   = V2248,   hfwh     = V2249,       hfwf     = V2250,
                      hhwh   = V2251,   hhwf     = V2252,       h0wf     = V2253,
                      year   = V1,      region   = V13,         gender   = V2150,
                      father = V2155,   mother   = V2156,       momed    = V2164,   
                      momemp   = V2165, religion = V2169,       getmar   = V2239)

# Create crosswalk of survey year and ICPSR Study ID 
studyid    <- c(7927,  7928,  7929,  7930,
                7900,  9013,  9045,  8387,  8388,
                8546,  8701,  9079,  9259,  9397,
                9745,  9871,  6133,  6367,  6517,
                6716,  2268,  2477,  2751,  2939,
                3184,  3425,  3753,  4019,  4264,
                4536, 20022, 22480, 25382, 28401,
                30985, 34409, 34861, 35218, 36263)

surveyyear <- c(1976, 1977, 1978, 1979,
                1980, 1981, 1982, 1983, 1984,
                1985, 1986, 1987, 1988, 1989,
                1990, 1991, 1992, 1993, 1994,
                1995, 1996, 1997, 1998, 1999,
                2000, 2001, 2002, 2003, 2004,
                2005, 2006, 2007, 2008, 2009,
                2010, 2011, 2012, 2013, 2014)

Xwalk <- data.frame(surveyyear, studyid)

#####################################################################################
# data Wrangling: data Transformation

## Year
data$year[data$year == "76"] <- "1976"
data$year[is.na(data$year)]  <- "1978" # 30 people in 1978 have a missing year variable

data$year <- as.integer(data$year)

## Weights
table(data$year, !is.na(data$wt7611))
table(data$year, !is.na(data$wt1214))

data <- data %>%
  mutate(
    weight = case_when(
      year  <= 2011 ~ wt7611,
      year  >= 2012 ~ wt1214))

## Division of Labor arrangements
# "hfw0", "hfwh", "hfwf", "hhwh",  "hhwf", "h0wf"

data <- data %>%
  mutate(
      hfw0 = case_when(
        (year != 1993 & year <=1998) & hfw0 == 1 ~ "NOT AT ALL ACCEPTABLE",
        (year == 1993 | year >=1999) & hfw0 == 2 ~ "NOT AT ALL ACCEPTABLE",
        (year != 1993 & year <=1998) & hfw0 == 2 ~ "SOMEWHAT ACCEPTABLE",
        (year == 1993 | year >=1999) & hfw0 == 3 ~ "SOMEWHAT ACCEPTABLE",
        (year != 1993 & year <=1998) & hfw0 == 3 ~ "ACCEPTABLE",
        (year == 1993 | year >=1999) & hfw0 == 4 ~ "ACCEPTABLE",
        (year != 1993 & year <=1998) & hfw0 == 4 ~ "DESIRABLE",
        (year == 1993 | year >=1999) & hfw0 == 5 ~ "DESIRABLE",
        TRUE      ~  NA_character_ ),
      hfwh = case_when(
        (year != 1993 & year <=1998) & hfwh == 1 ~ "NOT AT ALL ACCEPTABLE",
        (year == 1993 | year >=1999) & hfwh == 2 ~ "NOT AT ALL ACCEPTABLE",
        (year != 1993 & year <=1998) & hfwh == 2 ~ "SOMEWHAT ACCEPTABLE",
        (year == 1993 | year >=1999) & hfwh == 3 ~ "SOMEWHAT ACCEPTABLE",
        (year != 1993 & year <=1998) & hfwh == 3 ~ "ACCEPTABLE",
        (year == 1993 | year >=1999) & hfwh == 4 ~ "ACCEPTABLE",
        (year != 1993 & year <=1998) & hfwh == 4 ~ "DESIRABLE",
        (year == 1993 | year >=1999) & hfwh == 5 ~ "DESIRABLE",
        TRUE      ~  NA_character_ ),        
      hfwf = case_when(
        (year != 1993 & year <=1998) & hfwf == 1 ~ "NOT AT ALL ACCEPTABLE",
        (year == 1993 | year >=1999) & hfwf == 2 ~ "NOT AT ALL ACCEPTABLE",
        (year != 1993 & year <=1998) & hfwf == 2 ~ "SOMEWHAT ACCEPTABLE",
        (year == 1993 | year >=1999) & hfwf == 3 ~ "SOMEWHAT ACCEPTABLE",
        (year != 1993 & year <=1998) & hfwf == 3 ~ "ACCEPTABLE",
        (year == 1993 | year >=1999) & hfwf == 4 ~ "ACCEPTABLE",
        (year != 1993 & year <=1998) & hfwf == 4 ~ "DESIRABLE",
        (year == 1993 | year >=1999) & hfwf == 5 ~ "DESIRABLE",
        TRUE      ~  NA_character_ ), 
      hhwh = case_when(
        (year != 1993 & year <=1998) & hhwh == 1 ~ "NOT AT ALL ACCEPTABLE",
        (year == 1993 | year >=1999) & hhwh == 2 ~ "NOT AT ALL ACCEPTABLE",
        (year != 1993 & year <=1998) & hhwh == 2 ~ "SOMEWHAT ACCEPTABLE",
        (year == 1993 | year >=1999) & hhwh == 3 ~ "SOMEWHAT ACCEPTABLE",
        (year != 1993 & year <=1998) & hhwh == 3 ~ "ACCEPTABLE",
        (year == 1993 | year >=1999) & hhwh == 4 ~ "ACCEPTABLE",
        (year != 1993 & year <=1998) & hhwh == 4 ~ "DESIRABLE",
        (year == 1993 | year >=1999) & hhwh == 5 ~ "DESIRABLE",
        TRUE      ~  NA_character_ ), 
      hhwf = case_when(
        (year != 1993 & year <=1998) & hhwf == 1 ~ "NOT AT ALL ACCEPTABLE",
        (year == 1993 | year >=1999) & hhwf == 2 ~ "NOT AT ALL ACCEPTABLE",
        (year != 1993 & year <=1998) & hhwf == 2 ~ "SOMEWHAT ACCEPTABLE",
        (year == 1993 | year >=1999) & hhwf == 3 ~ "SOMEWHAT ACCEPTABLE",
        (year != 1993 & year <=1998) & hhwf == 3 ~ "ACCEPTABLE",
        (year == 1993 | year >=1999) & hhwf == 4 ~ "ACCEPTABLE",
        (year != 1993 & year <=1998) & hhwf == 4 ~ "DESIRABLE",
        (year == 1993 | year >=1999) & hhwf == 5 ~ "DESIRABLE",
        TRUE      ~  NA_character_ ),   
      h0wf = case_when(
        (year != 1993 & year <=1998) & h0wf == 1 ~ "NOT AT ALL ACCEPTABLE",
        (year == 1993 | year >=1999) & h0wf == 2 ~ "NOT AT ALL ACCEPTABLE",
        (year != 1993 & year <=1998) & h0wf == 2 ~ "SOMEWHAT ACCEPTABLE",
        (year == 1993 | year >=1999) & h0wf == 3 ~ "SOMEWHAT ACCEPTABLE",
        (year != 1993 & year <=1998) & h0wf == 3 ~ "ACCEPTABLE",
        (year == 1993 | year >=1999) & h0wf == 4 ~ "ACCEPTABLE",
        (year != 1993 & year <=1998) & h0wf == 4 ~ "DESIRABLE",
        (year == 1993 | year >=1999) & h0wf == 5 ~ "DESIRABLE",
        TRUE      ~  NA_character_ ))

data$hfw0 <- ordered(data$hfw0, levels = c("DESIRABLE", "ACCEPTABLE", "SOMEWHAT ACCEPTABLE",  "NOT AT ALL ACCEPTABLE"))
data$hfwh <- ordered(data$hfwh, levels = c("DESIRABLE", "ACCEPTABLE", "SOMEWHAT ACCEPTABLE",  "NOT AT ALL ACCEPTABLE"))
data$hfwf <- ordered(data$hfwf, levels = c("DESIRABLE", "ACCEPTABLE", "SOMEWHAT ACCEPTABLE",  "NOT AT ALL ACCEPTABLE"))
data$hhwh <- ordered(data$hhwh, levels = c("DESIRABLE", "ACCEPTABLE", "SOMEWHAT ACCEPTABLE",  "NOT AT ALL ACCEPTABLE"))
data$hhwf <- ordered(data$hhwf, levels = c("DESIRABLE", "ACCEPTABLE", "SOMEWHAT ACCEPTABLE",  "NOT AT ALL ACCEPTABLE"))
data$h0wf <- ordered(data$h0wf, levels = c("DESIRABLE", "ACCEPTABLE", "SOMEWHAT ACCEPTABLE",  "NOT AT ALL ACCEPTABLE"))

## Race
data <- data %>%
  mutate(
    race = case_when(
      (year != 1993 & year <=1998)                   & raceeth == 0   ~ "White",
      (year == 1993 | (year >=1999 & year <= 2004))  & raceeth == 2   ~ "White",
       year >= 2005                                  & raceeth == 3   ~ "White",
      (year != 1993 & year <=1998)                   & raceeth == 1   ~ "Black",
      (year == 1993 | (year >=1999 & year <= 2004))  & raceeth == 3   ~ "Black",
      year >= 2005                                   & raceeth == 2   ~ "Black",
      TRUE                                                             ~  NA_character_ ))

## Gender
data <- data %>%
  mutate(
    gender = case_when(
      (year != 1993 & year <=1998) & gender == 1 ~ "Men",
      (year == 1993 | year >=1999) & gender == 2 ~ "Men",
      (year != 1993 & year <=1998) & gender == 2 ~ "Women",
      (year == 1993 | year >=1999) & gender == 3 ~ "Women",
      TRUE                                       ~ NA_character_ ))

## Create racesex
data <- data %>%
  mutate(
    racesex = case_when(
      race == "White" & gender == "Men"   ~ "White men",
      race == "White" & gender == "Women" ~ "White women",
      race == "Black" & gender == "Men"   ~ "Black men",
      race == "Black" & gender == "Women" ~ "Black women",
      TRUE                                ~  NA_character_))

data$racesex <- as_factor(data$racesex)
data$racesex <- factor(data$racesex, levels = c("White men", "White women", "Black men", "Black women"), ordered = FALSE) 

## Mothers' Education
data <- data %>%
  mutate(
    momed = case_when(
      (year != 1993 & year <=1998) & momed == 1 ~ "COMPLETED GRADE SCHOOL OR LESS",
      (year == 1993 | year >=1999) & momed == 2 ~ "COMPLETED GRADE SCHOOL OR LESS",     
      (year != 1993 & year <=1998) & momed == 2 ~ "SOME HIGH SCHOOL",
      (year == 1993 | year >=1999) & momed == 3 ~ "SOME HIGH SCHOOL",     
      (year != 1993 & year <=1998) & momed == 3 ~ "COMPLETED HIGH SCHOOL",
      (year == 1993 | year >=1999) & momed == 4 ~ "COMPLETED HIGH SCHOOL",     
      (year != 1993 & year <=1998) & momed == 4 ~ "SOME COLLEGE",
      (year == 1993 | year >=1999) & momed == 5 ~ "SOME COLLEGE",     
      (year != 1993 & year <=1998) & momed == 5 ~ "COMPLETED COLLEGE",
      (year == 1993 | year >=1999) & momed == 6 ~ "COMPLETED COLLEGE",      
      (year != 1993 & year <=1998) & momed == 6 ~ "GRADUATE OR PROFESSIONAL SCHOOL AFTER COLLEGE",
      (year == 1993 | year >=1999) & momed == 7 ~ "GRADUATE OR PROFESSIONAL SCHOOL AFTER COLLEGE",  
      (year != 1993 & year <=1998) & momed == 7 ~ "DON'T KNOW, OR DOES NOT APPLY",      # These don't match but make missing so doesn't matter
      (year == 1993 | year >=1999) & momed == 8 ~ "DON'T KNOW, OR DOES NOT APPLY",      # These don't match but make missing so doesn't matter
      TRUE       ~  NA_character_ ))

data$momed[data$momed == "DON'T KNOW, OR DOES NOT APPLY"]  <- NA
data$momed[data$momed == "COMPLETED GRADE SCHOOL OR LESS"] <- "LESS THAN HIGH SCHOOL"
data$momed[data$momed == "SOME HIGH SCHOOL"]               <- "LESS THAN HIGH SCHOOL"

data$momed <- factor(data$momed, levels = c("LESS THAN HIGH SCHOOL", "COMPLETED HIGH SCHOOL", "SOME COLLEGE", 
                                            "COMPLETED COLLEGE", "GRADUATE OR PROFESSIONAL SCHOOL AFTER COLLEGE"), ordered = FALSE) 

## Mothers' Employment
data <- data %>%
  mutate(
    momemp = case_when(
      (year != 1993 & year <=1998) & momemp == 1 ~ "NO, NOT EMPLOYED",
      (year == 1993 | year >=1999) & momemp == 2 ~ "NO, NOT EMPLOYED",      
      (year != 1993 & year <=1998) & momemp == 2 ~ "YES, SOME OF THE TIME WHEN I WAS GROWING UP",
      (year == 1993 | year >=1999) & momemp == 3 ~ "YES, SOME OF THE TIME WHEN I WAS GROWING UP",      
      (year != 1993 & year <=1998) & momemp == 3 ~ "YES, MOST OF THE TIME",
      (year == 1993 | year >=1999) & momemp == 4 ~ "YES, MOST OF THE TIME",      
      (year != 1993 & year <=1998) & momemp == 4 ~ "YES, ALL OR NEARLY ALL OF THE TIME",
      (year == 1993 | year >=1999) & momemp == 5 ~ "YES, ALL OR NEARLY ALL OF THE TIME",      
      TRUE        ~  NA_character_ ))

data$momemp <- factor(data$momemp, levels = c("NO, NOT EMPLOYED", "YES, SOME OF THE TIME WHEN I WAS GROWING UP", 
                                              "YES, MOST OF THE TIME", "YES, ALL OR NEARLY ALL OF THE TIME"), ordered = FALSE) 

## Family Structure
data <- data %>%
  mutate(
    mother = case_when(
      (year != 1993 & year <=1998) & mother == 0 ~ "NO",
      (year == 1993 | year >=1999) & mother == 2 ~ "NO",      
      (year != 1993 & year <=1998) & mother == 1 ~ "YES",
      (year == 1993 | year >=1999) & mother == 3 ~ "YES",
      TRUE        ~ NA_character_ ),
    father = case_when(
      (year != 1993 & year <=1998) & father == 0 ~ "NO",
      (year == 1993 | year >=1999) & father == 2 ~ "NO",      
      (year != 1993 & year <=1998) & father == 1 ~ "YES",
      (year == 1993 | year >=1999) & father == 3 ~ "YES",
      TRUE        ~ NA_character_ ))

data$famstru <-paste(data$father, data$mother)

data <- data %>%
  mutate(
    famstru = case_when(
      (famstru == "YES YES")   ~ "Both Mother & Father",
      (famstru == "NO YES")    ~ "Mother Only",
      (famstru == "YES NO")    ~ "Father Only",
      (famstru == "NO NO")     ~ "Neither Mother/Father",
      TRUE                     ~  NA_character_
    ))
data$famstru <- as_factor(data$famstru)

data$famstru <- factor(data$famstru, levels = c("Both Mother & Father", "Mother Only", 
                                                "Father Only", "Neither Mother/Father"), ordered = FALSE) 

## Religiosity
data <- data %>%
  mutate(
    religion = case_when(
      (year != 1993 & year <=1998) & religion == 1 ~ "NEVER",
      (year == 1993 | year >=1999) & religion == 2 ~ "NEVER",      
      (year != 1993 & year <=1998) & religion == 2 ~ "RARELY",
      (year == 1993 | year >=1999) & religion == 3 ~ "RARELY",      
      (year != 1993 & year <=1998) & religion == 3 ~ "ONCE OR TWICE A MONTH",
      (year == 1993 | year >=1999) & religion == 4 ~ "ONCE OR TWICE A MONTH",      
      (year != 1993 & year <=1998) & religion == 4 ~ "ABOUT ONCE A WEEK OR MORE",
      (year == 1993 | year >=1999) & religion == 5 ~ "ABOUT ONCE A WEEK OR MORE",      
      TRUE        ~  NA_character_ ))

data$religion <- factor(data$religion, levels = c("NEVER", "RARELY", 
                                                  "ONCE OR TWICE A MONTH", "ABOUT ONCE A WEEK OR MORE"), ordered = FALSE) 

## Region
data <- data %>%
  mutate(
    region = case_when(
      region   == 1 | region == "NE" | region == "NE:(1)" | region == "NORTHEAST"     | region == "NORTHEAST:(1)"      ~ "Northeast",
      region   == 2 | region == "NC" | region == "NC:(2)" | region == "NORTH CENTRAL" | region == "NORTH CENTRL:(2)"   ~ "North Central",
      region   == 3 | region == "S"  | region == "S:(3)"  | region == "SOUTH"         | region == "SOUTH:(3)"          ~ "South",
      region   == 4 | region == "W"  | region == "W:(4)"  | region == "WEST"          | region == "WEST:(4)"           ~ "West",
      TRUE                                                                                                             ~  NA_character_ ))

data$region <- factor(data$region, levels = c("Northeast", "North Central", "South", "West"), ordered = FALSE) 

## Get Married
## For sensitivity test, assign missing level
data <- data %>%
  mutate(
    getmar = case_when(
      (year != 1993 & year <=1998) & getmar == 1 ~ "NOT GETTING MARRIED",
      (year == 1993 | year >=1999) & getmar == 2 ~ "NOT GETTING MARRIED",
      (year != 1993 & year <=1998) & getmar == 2 ~ "I HAVE NO IDEA",
      (year == 1993 | year >=1999) & getmar == 3 ~ "I HAVE NO IDEA",      
      (year != 1993 & year <=1998) & getmar == 3 ~ "GETTING MARRIED",
      (year == 1993 | year >=1999) & getmar == 4 ~ "GETTING MARRIED",      
      (year != 1993 & year <=1998) & getmar == 8 ~ "AM ALREADY MARRIED", #This was incorrectly listed as missing in Stata .do files
      (year == 1993 | year >=1999) & getmar == 5 ~ "AM ALREADY MARRIED",      
      TRUE        ~  "MISSING"))

data$getmar <- factor(data$getmar, levels = c("GETTING MARRIED", "NOT GETTING MARRIED", "I HAVE NO IDEA", 
                                              "AM ALREADY MARRIED", "MISSING"), ordered = FALSE) 

#####################################################################################
# Select Sample
## Rename Variables
data <- select(data, weight, year, hfw0, hfwh, hfwf, hhwh, hhwf, h0wf, racesex, 
                  race, gender, momed, momemp, famstru, religion, region, getmar)

# Missing data  
colSums(is.na(data))

## Create database to analyze missing data later
miss <- data

## Given the large total sample size, we used listwise deletion to address missing cases.

data <- na.omit(data)