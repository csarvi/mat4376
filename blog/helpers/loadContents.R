# load libraries, functions and objects ----

# load libraries ----
library(shiny)
library(glmnet)


# load helper functions ----
source("helpers/getScore.R")

# load objects ----
mod <- readRDS("model/trained_model.RDS") # trained model 
dfmMod <- readRDS("model/dfm_mod.RDS") # model's document feature matrix