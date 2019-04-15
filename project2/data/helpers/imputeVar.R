imputeVar <- function(seed){
  if(!nzchar(system.file(package = "mice"))) {
    ans <- menu(choices = c("Y", "N"), title = "The package \"mice\" is required to run this application.\n\nDo you wish to install the package? (if \"No\", program execution stops if \"No\")")  
    if(ans == 2) stop("package will not be installed")
    install.packages("mice")
    rm(ans)
  }
  
  # create ID, this will be used to merge the masterRaw data and the imputed data
  masterRaw <- dplyr::mutate(masterRaw, ID = seq_len(nrow(masterRaw))) 
  
  # select vars of interest
  impMaster <- dplyr::select(masterRaw, ID, REGION, AGE_P, RACERPI2, BWTGRM_P, DEV_BORN)
  
  # load helper fun
  impMaster <- binnarize(impMaster, c("REGION", "RACERPI2"))
  
  # start imputation
  set.seed(seed)
  imp <- mice::mice(impMaster, m = 50, method = "norm.nob",maxit = 50)
  
  # do diagnostics, see if imputted values changed the correlations ----
  fit1 <- with(impMaster, 
               lm(BMI_SC ~ AGE_P + BWTGRM_P + DEV_BORN + REGION_2 + REGION_3 + REGION_4 + RACERPI2_2 + RACERPI2_3 + RACERPI2_4 + RACERPI2_5 + RACERPI2_6))
  
  fit2 <- with(imp, lm(BMI_SC ~ AGE_P + BWTGRM_P + DEV_BORN + REGION_2 + REGION_3 + REGION_4 + RACERPI2_2 + RACERPI2_3 + RACERPI2_4 + RACERPI2_5 + RACERPI2_6))
  
  fit1$coefficients
  Reduce(`+`, lapply(seq_along(fit2$analyses), function(x) return(fit2$analyses[[x]]$coefficients)))/length(fit2$analyses)
  # GOOD RESULTS! 
  
  imputted <- mice::complete(imp)
  
  
}
