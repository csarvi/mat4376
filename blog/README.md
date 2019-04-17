# Building an application in `shiny` for binary text sentiment classification

In this tutorial, I will show you how to build a text sentiment classification application in <code>shiny</code>. This app should:

<li>Accept text supplied by the user in <code>shiny</code>'s user interface (UI);</li>

<li>Process this text into trained classification model;</li>

<li>Provide a score varying between 0 and 1 which can be interpreted as the P(Y=1);</li>

<li>Crowdsource data collection by allowing feedback on the results;</li>

The last component is of particular importance. The user may choose to accept or reject the resulting score which then prompts the application to save this information on a separate dataset. This stored data can then be used to improve the classifier.

Before starting, certify that the following packages are installed: `"shiny", "magrittr", "data.table", "caret", "glmnet", "tidytext", "stringr", "mccr"`. If you are not sure, run this code snippet before starting your analysis (see script `packages.R`:

```r
pkgs <- c("shiny", "magrittr", "data.table", "caret", "glmnet", "tidytext", "stringr", "mccr")
for(i in seq_along(pkgs)){
 if(!nzchar(system.file(package = pkgs[[i]]))) install.packages(pkgs[[i]])
}
rm(pkgs, i)
```

---