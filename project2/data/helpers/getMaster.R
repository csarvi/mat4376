getMaster <- function(){
  # package requirements (to install) ----
  if(!nzchar(system.file(package = "magrittr"))) {
    ans <- menu(choices = c("Y", "N"), title = "The package \"magrittr\" is required to run this application.\n\nDo you wish to install the package? (if \"No\", program execution stops if \"No\")")  
    if(ans == 2) stop("package will not be installed")
    install.packages("magrittr")
    rm(ans)
  }
  if(!nzchar(system.file(package = "data.table"))) {
    ans <- menu(choices = c("Y", "N"), title = "The package \"data.table\" is required to run this application.\n\nDo you wish to install the package? (if \"No\", program execution stops if \"No\")")  
    if(ans == 2) stop("package will not be installed")
    install.packages("data.table")
    rm(ans)
  }
  
  # package requirements (to load) ----
  if(!any(.packages() %in% "magrittr")) library(magrittr)
  if(!any(.packages() %in% "data.table")) library(data.table)
  
  # load csv files ----
  exdir <- paste0(tempdir(), "/nhis") # temp dir to find files
  filesPath <- dir(exdir, "(?i)\\.csv", full.names = T) # vector with files names and paths
  files <- dir(exdir, "(?i)\\.csv") # vector with files names and paths
  
  objNames <- tolower(gsub(x=files, pattern="(?i)([A-z]+)\\.csv", "\\1")) %>% 
    gsub(x=., pattern="x+", "") # remove csv to name objects
  
  # read files...
  for(i in seq_along(filesPath)){
    assign(objNames[[i]], data.table::fread(filesPath[[i]], stringsAsFactors = F))
  }
  rm(i)
   
  # remove unecessary columns in Sample Child ----
  # select columns
  samchild <- samchild[, .(SRVY_YR, HHX, FMX, FPX, REGION, SEX, AGE_P, RACERPI2, 
                           BWTGRM_P, # Birth-weight -> normalize it
                           # AUTOIMMUNE DISEASES 
                           CASSTILL, # whether SC have asthma
                           HAYF1, HAYF2, # hay fever, '1' LE 2 yrs, '2' 3 to 17
                           RALLG1, RALLG2, # respiratory allergy, '1' LE 2 yrs, '2' 3 to 17 
                           DALLG1, DALLG2, # food digestive allergy, '1' LE 2 yrs, '2' 3 to 17
                           SALLG1, SALLG2, # eczema/skin allergy, '1' LE 2 yrs, '2' 3 to 17
                           # END OF AUTOIMMUNE DISEASES
                           EARINF1, EARINF2 # had 3+ ear infections in past 12 m '1' LE 2 yrs, '2' 3 to 17
  )]  
  
  # we only want children who are children of the reference person in the survey, do merging with persons sample to determine that 
  personsChild <- persons[CSTATFLG == 1 & FRRP == 4, 
                          .(HHX, FMX, FPX, 
                            DEV_BORN = REGIONBR # variable to be treated, born in the developing world?
                          )]
  
  # merge...
  children <- samchild[personsChild, on = c("HHX", "FMX", "FPX"), nomatch=0L][, FPX:=NULL]
  
  
  # create variable for whether child has autoimmune/allergy
  # cols <- c("CASSTILL", "HAYF1", "HAYF2", "RALLG1", "RALLG2", "DALLG1", "DALLG2", "SALLG1", "SALLG2")
  # children[, condition := dplyr::if_else(children[, Reduce(`|`, lapply(.SD, `==`, 1L)), .SDcols = cols], 1L, 0L, 0L)]
  # rm(cols)
  
  # link adult to children ----
  
  # first go to adult sample, select pertinent columns and only get those who are parents of child RESIDING in the family
  samadult <-  samadult[PAR_STAT == 1 # PAR_STAT IS WHETHER ADUL IS A PARENT OF A CHILD RESIDING IN THE FAMILY
                        , .(HHX, FMX, FPX,
                            # PARENTS AUTOIMMUNE DISEASES
                            P_ASSMEV = AASMEV, # EVER HAD ASTHMA?
                            P_DIBTYPE = DIBTYPE, # TYPE OF DIABETES, WILL ONLY USE PARENTS WITH TYPE 1 (IT CAN BE CONSIDERED A RISK FACTOR)
                            P_AHAYFYR = AHAYFYR # Had hay fever past 12 mo
                            # END AUTOIMMUNE DISEASES
                        )]
  
  # merge with persons (need to know whether parent is us born or not)
  # persons
  personsAdult <- persons[ASTATFLG== 1 & # FLAG IN THE SAMPLE ADULT
                            FRRP == 1,
                          .(HHX, FMX, FPX, SRVY_YR,
                            P_DEV_BORN = REGIONBR # Region born (need this to flag whether parent came from developing country)
                          )]
  
  adults <- samadult[personsAdult, on = c("HHX", "FMX", "FPX"), nomatch=0L][, FPX := NULL]
  
  # now merge with children sample 
  master <- children[adults, on = c("HHX", "FMX", "SRVY_YR")]
  
  # link to family sample ----
  # key: household no. (HMX) and family no. (FMX)
  family <- family[, .(SRVY_YR, HHX, FMX, 
                       FM_KIDS, # of family members under 18 years of age
                       INCGRP5 # income group (see NHIS family layout)
  )]
  
  master <- master[family, on=c("HHX", "FMX", "SRVY_YR"), nomatch=0L] 
  
  return(master)
}