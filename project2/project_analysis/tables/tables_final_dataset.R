master_final <- readRDS("data/master.RDS")


tab_b <- data.frame(
  Columns=names(master_final), 
  Description=c(
    "Unique identifier of each observation in the dataset",
    "Has the child had any type of allergy", 
    "Child's age", 
    "Child's weigth at birth",
    "Region of the world where the child was born",
    "Has the child had any type of respiratory/ear infection",
    "Two kids in the family",
    "Three kids in the family",
    "Four kids in the family",
    "Income Group (35,000 to 74,999)",
    "Income Group (75,000 to 99,999)",
    "Income Group (100,000 and over)",
    "Parent has autoimmune/allergy",
    "Region of the world where the parent was born",
    "Parent's OMB race group is Black/African American only",
    "Parent's OMB race group is AIAN only",
    "Parent's OMB race group is Asian only",
    "Parent's OMB race group is Race group not releasable",
    "Parent's OMB race group is Multiple race",
    "Child is located in the Midwest region (US)",
    "Child is located in the South region (US)",
    "Child is located in the West region (US)",
    "Child's biological sex"
  ),
  Code=c(
    "", 
    "0: Yes; 1: No",
    "",
    "Normalized variable usin range between 0 and 1",
    "0: Child was'nt born in developed world (region) 1: Child was born in developed world (region)",
    "0: Yes; 1: No",
    "0: Yes; 1: No",
    "0: Yes; 1: No",
    "0: Yes; 1: No",
    "0: Yes; 1: No",
    "0: Yes; 1: No",
    "0: Yes; 1: No",
    "0: Yes; 1: No",
    "0: Parent was'nt born in developed world (region) 1: Parent was born in developed world (region)",
    "0: Yes; 1: No",
    "0: Yes; 1: No",
    "0: Yes; 1: No",
    "0: Yes; 1: No",
    "0: Yes; 1: No",
    "0: Yes; 1: No",
    "0: Yes; 1: No",
    "0: Yes; 1: No",
    "1: Male; 2: Female"
  )
)

saveRDS(tab_b, "project_analysis/tables/descriptions_f.RDS")
rm(list =ls())
