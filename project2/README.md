# Project 2 Instructions

Go to `project_analysis/project_analysis.html` for instructions on how to fully reproduce this analysis.

## Extract the data for this project

First, set the working directory by specifying the path of the folder <em>project2</em>. This can be done in <em>R Studio</em> by typing on the console:
```
path <- "PATH/TO/project2"
setwd(path)
getwd() # should show "PATH/TO/project2". Note that "PATH/TO" is the rest of the path of project2.
```

You can also open the project file <em>project2.Rproj</em>, this automatically sets the working directory in your `R` session to the project's folder. 

#### The `data` folder

The main script is `data/processData.R`. By running this program, a file called `master.RDS` should be saved in the folder. `data/processData.R` contains modules that extract and transform the data downloaded from the CDC website. You **don't** need to download the data directly from CDC. Everything should be handled by scripts included in `data/processData.R`. The resulting `master` dataset will be saved as an `.RDS` file that is used in the machine learning exercise.