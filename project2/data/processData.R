# load libraries ----
library(magrittr)

# get NHIS from 2015-2017
years <- c(2016:2017) # enlist years

# load functions ----
source("data/helpers/downloadZips.R")
source("data/helpers/getMaster.R")
source("data/helpers/binnarize.R")

# download, unpack and consolidate data ----
dataList <- lapply(c(2016:2017), function(x){
  # download files
  downloadZips(x)
  
  # read csv files and consolidate it into a master file
  master <- getMaster()
  unlink(paste0(tempdir(), "/nhis"), recursive = TRUE) # delete all files from temp folder...
  return(master)
})

# bind
masterRaw <- rbindlist(dataList)

# DELETE ROWS FOR WHICH THERE IS "NA" FOR THE FOLLOWING COLUMNS: -----
masterRaw  <- masterRaw %>%
   tidyr::drop_na(REGION, SEX, AGE_P, RACERPI2,
          DEV_BORN, P_DEV_BORN,
          FM_KIDS, INCGRP5) %>%
          dplyr::select(-HHX, -FMX, -SRVY_YR)

saveRDS(masterRaw, "data/masterRaw.RDS")
# masterRaw <- readRDS("data/masterRaw.RDS")

# Imputation ----
# repalce values in the value to be inputted by NA
masterRaw[BWTGRM_P > 5485, BWTGRM_P := NA]

# create ID, this will be used to merge the masterRaw data and the imputed data
masterRaw <- dplyr::mutate(masterRaw, ID = seq_len(nrow(masterRaw))) 

# select vars of interest
impMaster <- dplyr::select(masterRaw, ID, REGION, AGE_P, RACERPI2, BWTGRM_P, DEV_BORN)

impMaster <- binnarize(impMaster, c("REGION", "RACERPI2"))

# start imputation
set.seed(3152019)
imp <- mice::mice(impMaster, m = 50, method = "norm.nob",maxit = 50)

# do diagnostics, see if imputted values changed the correlations ----
fit1 <- with(impMaster, 
            lm(BWTGRM_P ~ AGE_P +  DEV_BORN + REGION_2 + REGION_3 + REGION_4 + RACERPI2_2 + RACERPI2_3 + RACERPI2_4 + RACERPI2_5 + RACERPI2_6))

fit2 <- with(imp, lm(BWTGRM_P ~ AGE_P + DEV_BORN + REGION_2 + REGION_3 + REGION_4 + RACERPI2_2 + RACERPI2_3 + RACERPI2_4 + RACERPI2_5 + RACERPI2_6))

saveRDS(list(fit1$coefficients,
             Reduce(`+`, lapply(seq_along(fit2$analyses), function(x) return(fit2$analyses[[x]]$coefficients)))/length(fit2$analyses)), 
"project_analysis/results/imputation_comps.RDS")


imputted <- mice::complete(imp)

# merge with previous data ----
masterRaw <- masterRaw %>% 
  dplyr::select(-REGION, -RACERPI2, -BWTGRM_P) %>%
  dplyr::inner_join(., imputted, by=intersect(names(.), names(imputted)))

rm(fit1, fit2)

# SEX: GENDER OF THE CHILD, SET 0 TO MALE AND 1 TO FEMALE ----
masterRaw <- dplyr::mutate(masterRaw, SEX = dplyr::if_else(SEX==1, 0, 1, NA_real_))

# REGION: REGION IN WHICH THE CHILD IS LOCATED (MAKE THEM BINARY) -----
### NOT NECESSARY ANYMORE AS I HAD TO DO THIS FOR IMPUTATION ####

# BWTGRM_P: WEIGHT AT BIRTH, CREATE A STANDARDIZED VARIABLE (BETWEEN 0 AND 1), MAKE SURE THE VARIABLE NAME MAKES SENSE ----
masterRaw <- dplyr::mutate_at(masterRaw,
                 .vars = c("BWTGRM_P"),
                 .funs = function(x){
                   (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))
                   })

# CASSTILL TO SALLG2 ----
# CONSOLIDATE THESE VARIABLES INTO A SINGLE ONE WHERE 0 IS 'NO AUTOIMMUNE DISEASE/ALLERGY' AND 1 IS 'HAS AUTOIMMUNE/ALLERGY'
masterRaw <- dplyr::mutate_at(masterRaw, 
                 .vars=dplyr::vars(CASSTILL:SALLG2), 
                 .funs = function(x) dplyr::if_else(x != 1, 0, 1, NA_real_)) %>% 
  dplyr::mutate(ALLERGINM = rowSums(dplyr::select(., CASSTILL:SALLG2), na.rm=T)) %>% 
  dplyr::mutate(ALLERGINM = dplyr::if_else(ALLERGINM > 0, 1, 0, 0))

# EARINF1, EARINF2 # had 3+ ear infections in past 12 m '1' LE 2 yrs, '2' 3 to 17: 0 FOR 'NO', 1 FOR 'YES', 0 FOR EVERYTHING ELSE
masterRaw <- dplyr::mutate_at(masterRaw, paste0("EARINF", c(1:2)), 
                 .funs = function(x) dplyr::if_else(x != 1, 0, 1, NA_real_)) %>% 
  dplyr::mutate(EARINF = rowSums(dplyr::select(., EARINF1:EARINF2), na.rm=T)) %>%
  dplyr::mutate(EARINF = dplyr::if_else(EARINF > 0, 1, 0, 0))


# DEV_BORN: THIS IS AN UNTREATED VARIABLE EQUIVALENT TO 'REGIONBR' IN THE SAMPLE PERSONS SAMPLE FILE. 
masterRaw <- dplyr::mutate(masterRaw, DEV_BORN = dplyr::if_else(DEV_BORN %in% c(1, 4, 99), 0, 1, 1))


# P_ASSMEV TO P_AHAYFYR: CONSOLIDATDEV_BORNE THESE VARIABLES INTO A SINGLE ONE WHERE 0 IS PARENT HAS 'NO AUTOIMMUNE/ALLERGY'
# 1 OTHERWISE. FOR UKNOWN/NA/NO ANSWER,ETC. RESPONSES, ASSING 'NO AUTOIMMUNE/ALLERGY' (0)
masterRaw <- dplyr::mutate_at(masterRaw, 
                 .vars = dplyr::vars(P_ASSMEV:P_AHAYFYR), 
                 .funs = function(x) dplyr::if_else(x != 1, 0, 1, NA_real_)) %>% 
  dplyr::mutate(P_ALLERGINM = rowSums(dplyr::select(., P_ASSMEV:P_AHAYFYR), na.rm=T)) %>%
  dplyr::mutate(P_ALLERGINM = dplyr::if_else(P_ALLERGINM > 0, 1, 0, 0)) 


# P_DEV_BORN: SAME AS BORN_DEV VARIABLE FOR CHILD BUT THIS TIME FOR PARENT
masterRaw <- dplyr::mutate(masterRaw, P_DEV_BORN = dplyr::if_else(P_DEV_BORN %in% c(1, 4, 99), 0, 1, 1))

# FM_KIDS: NUMBER OF KIDS IN FAMILY. BINARIZE THE FOLLOWING GROUPS:
# 2-3 0 FOR NO, 1 TO YES
# +3: 0 FOR NO, 1 TO YES
# OMMIT THE COLUMN FOR 1 CHILD IN THE FAMILY

masterRaw <- dplyr::mutate(masterRaw, FM_KIDS = ifelse(FM_KIDS > 3, 4, FM_KIDS))

masterRaw <- binnarize(masterRaw, c("FM_KIDS"))

# INCGRP5: INCOME GROUP, BINARIZE THE FOLLOWING CATEGORIES
# 35,000 TO 74,999: 0 TO NO, 1 TO YES
# 75,000 TO 99,999: 0 TO NO, 1 TO YES
# 100,000 AND OVER: 0 TO NO, 1 TO YES
# 0 - 34,999: OMMIT THIS COLUMN (BASELINE GROUP)
# UNDEFINED: MAKE IT BELENG TO THE 0-34,999 GROUP (WILL BE OMMITED)
masterRaw <- binnarize(masterRaw, c("INCGRP5")) %>% 
  dplyr::select(-INCGRP5_96, -INCGRP5_99)#Drop columns 1 child in the family

# Remove cols ----
master <- dplyr::select(masterRaw, -(CASSTILL:EARINF2), -P_ASSMEV, -P_AHAYFYR, -P_DIBTYPE)

# organize columns
setcolorder(master, sort(names(master)))
setcolorder(master, c("ID", "ALLERGINM", setdiff(names(master), c("ID", "ALLERGINM"))))

saveRDS(master, "data/master.RDS")
