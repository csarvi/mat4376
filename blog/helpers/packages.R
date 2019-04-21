### RUN THIS SCRIPT BEFORE STARTING YOUR ANALYSIS - ENSURES ALL PKGS ARE INSTALLED
pkgs <- c("magrittr", "data.table", "shiny", "flexdashboard", "caret", "glmnet", "quanteda", "tidytext", "stringr", "mccr")
for(i in seq_along(pkgs)){
 if(!nzchar(system.file(package = pkgs[[i]]))) install.packages(pkgs[[i]])
}
rm(pkgs, i)
