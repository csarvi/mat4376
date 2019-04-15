masterRaw <- readRDS("data/masterRaw.RDS")

# var descriptions ----

tab <- data.frame(
  Columns=names(masterRaw), 
  Description=c(
    "Region of the United States",
    "Child's biological sex", 
    "Child's age", 
    "Child's OMB race group", 
    "Child's weigth at birth",
    "Does your child still have asthma?",
    "Has the child had hay fever in the past 12 months (age less or equal to 2)",
    "Has the child had hay fever in the past 12 months (age greater than 2)",
    "Has the child had respiratory allergy in the past 12 months (age less or equal to 2)",
    "Has the child had respiratory allergy in the past 12 months (age greater than 2)",
    "Has the child had any kind of food/digestive allergy in the past 12 months (age less or equal to 2)",
    "Has the child had any kind of food/digestive allergy in the past 12 months (age greater than 2)",
    "Has the child had eczema in the past 12 months (age less or equal to 2)",
    "Has the child had eczema in the past 12 months (age greater than 2)",
    "Has the child had three or more ear infections in the past 12 months (age less or equal to 2)",
    "Has the child had three or more ear infections in the past 12 months (age greater than 2)", 
    "Was the child born in a developing country?", 
    "Does the parent have diabetes type I?",
    "Does the parent have asthma?",
    "Does the parent have hay fever?",
    "Was the parent born in a developed country?", 
    "Number of children in the family", 
    "Family's income"
  ),
  Code=c(
    "1: NE; 2: MW; 3: South; 4: West", 
    "1: Male; 2: Female",
    "",
    "1: White; 2: Black/African American only; 3: AIAN only; 4: Asian only; 05: Race group not releasable; 06: Multiple race",
    "",
    "1: Yes; 2: No; 7: Refused; 8: Not ascertained; 9: Don't know",
    "1: Yes; 2: No; 7: Refused; 8: Not ascertained; 9: Don't know",
    "1: Yes; 2: No; 7: Refused; 8: Not ascertained; 9: Don't know",
    "1: Yes; 2: No; 7: Refused; 8: Not ascertained; 9: Don't know",
    "1: Yes; 2: No; 7: Refused; 8: Not ascertained; 9: Don't know",
    "1: Yes; 2: No; 7: Refused; 8: Not ascertained; 9: Don't know",
    "1: Yes; 2: No; 7: Refused; 8: Not ascertained; 9: Don't know",
    "1: Yes; 2: No; 7: Refused; 8: Not ascertained; 9: Don't know",
    "1: Yes; 2: No; 7: Refused; 8: Not ascertained; 9: Don't know",
    "1: Yes; 2: No; 7: Refused; 8: Not ascertained; 9: Don't know",
    "1: Yes; 2: No; 7: Refused; 8: Not ascertained; 9: Don't know",
    "0: No; 1: Yes. Developed regions/country: US, Europe (excluding Russia and former USSR areas); 'Unkown' and 'Elsewhere'", 
    "0: No; 1: Yes",
    "0: No; 1: Yes",
    "0: No; 1: Yes",
    "Same code as child variable", 
    "",
    "1: $0 - $34,999; 2: $35,000 - $74,999; 3: $75,000 - $99,999; $100,000 and over; 96: Undefined; 99: Unknown"
  )
)

saveRDS(tab, "project_analysis/tables/descriptions.RDS")

# parameters description ----

saveRDS(data.frame(Model=c("Classification tree","KNN","Logistic","Naïve bayes"), 
                   Parameter= c("Threshold of complexity parameter (CP) in the formula: $R_{cp}(T) \\equiv R(T) + cp \\times |T| \\times R(T_{1})$\n\nMinimum number of observations in a node",
      "Number of neighbours considered ($K$)",
      "A complexity parameter ($\\lambda$)\n\n$\\alpha$ as a compromise between ridge ($\\alpha=0$) and lasso ($\\alpha=1$)",
      "A factor for Laplace correction."),
  stringsAsFactors=FALSE),
  "project_analysis/tables/paramsTab.RDS")


rm(list =ls())