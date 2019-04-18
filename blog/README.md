# Building an application in `shiny` for binary text sentiment classification

In this tutorial, I will show you how to build a text sentiment classification application in <code>shiny</code>. This app should:

<li>Accept text supplied by the user in <code>shiny</code>'s user interface (UI);</li>

<li>Process this text into trained classification model;</li>

<li>Provide a score varying between 0 and 1 which can be interpreted as the P(Y=1);</li>

<li>Crowdsource data collection by allowing feedback on the results;</li>
<br>
The last component is of particular importance. The user may choose to accept or reject the resulting score which then prompts the application to save this information on a separate dataset. This stored data can then be used to improve the classifier.

Before starting, certify that the following packages are installed: `"shiny", "magrittr", "data.table", "caret", "glmnet", "quanteda", "tidytext", "stringr", "mccr"`. If you are not sure, run this code snippet before starting your analysis (see script `packages.R`:

```r
pkgs <- c("shiny", "magrittr", "data.table", "caret", "glmnet", "quanteda", "tidytext", "stringr", "mccr")
for(i in seq_along(pkgs)){
 if(!nzchar(system.file(package = pkgs[[i]]))) install.packages(pkgs[[i]])
}
rm(pkgs, i)
```

I am going to assume that your working directory is the folder corresponding to the `blog` directory in this document. If you are in doubt, enter in `R`: `getwd()`. The result should end with `blog`: `"path/to/blog"`. If not, set the working directory to the `blog` folder: `setwd("path/to/blog")`. For [RStudio](https://www.rstudio.com/products/rstudio/download/) users, you can create a [project](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects) which will set the directory to the project's folder.

### Building the classifier

The first step is to build a classifier. This is not done in `shiny`. In this step, all data manipulation and modelling is done in the `./model` folder.

#### Getting the data

The first step is to get the data. We use Andrew Maas' <a href="http://ai.stanford.edu/~amaas/data/sentiment/]">Large Movie Review Dataset</a>. It contains 50,000 movie reviews from the <a href="https://www.imdb.com/">IMDb</a> website. Scores vary from 0 to 5 stars with the increment unit being 0.5 stars --0 being the lowest and 10 being the best. The raw data is separated into two, balanced equal sets of 25,000 --i.e. a 50k training and test set. Our approach is to combine the training and test sets since we want more observations in the training set as opposed to have equal size for both sets. 

Reviews with a score between 4 and 7 are excluded since they tend to be more dubious (neutral) in sentiment. We consider reviews with a score between 0 and 3 to be negative whereas a score between 8 and 10 is positive. We end up with 39,866 reviews to train and test our model. We are building a classifier for a binary variable. In our dataset, 0 and 1 denote negative and positive sentiment respectively. For convenience, this data is saved in `./model/processed_data.RDS`. 

I also provide a script (`./model/data_processing.R`) that combines the raw data from the Large Movie Review Dataset into a `data.table`. I am not including the raw data in the `./folder` directory but to make this script work, you wil have to [download the data](http://ai.stanford.edu/~amaas/data/sentiment/aclImdb_v1.tar.gz) and extract the `.tar` file in the `./model` folder. The directory structure should look like: `./model/aclImdb_v1.var/aclImdb_v1/...`. Alternatively, you can manually change the paths in the script.

#### Building the model

We fit an L1 regularized logistic model with 10-fold cross validation procedure using the `cv.glmnet` function --see the [`glmnet` package vignette](https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html#spa). The reason for regularization is an attempt to alleviate the large number of features (p) over the number of columns (n). The [`caret`](https://topepo.github.io/caret/data-splitting.html) package is used to partition the data.

The file `./model/train_model.R` shows the model building process including the transformation of the combined sentiment dataset into a document frequency matrix. First, we load the processed movie review `data` and create an `ID` column that will be used in the matrix transformation process. We are using `data.table` and `magrittr` for data manipulation.

```r
# pkg requirements ----
pkgs <- c("magrittr", "data.table")
for(i in seq_along(pkgs)){
  if(!any(.packages() == pkgs[[i]])) suppressWarnings(
    suppressPackageStartupMessages(library(pkgs[[i]], character.only = T))
  )
} 
rm(pkgs, i)

# load data ----
data <- readRDS("model/processed_data.RDS")

# create ID var
data[, ID:=seq_len(nrow(data))]
data[, sentiment:=as.integer(sentiment)]
```

There are many packages that you can use to prepare your dataset for text classification. I personally like the _tidy_ way with the  [`tidytext`](https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html) package by [Julia Silge](https://juliasilge.com/) and [David Robinson](http://varianceexplained.org/). In the code below, we tokenize our dataset using `tidytext::unnest_tokens`. Tokenization splits the texts into single words, changing the dimension of the dataset from one row per text to many rows per text (or `ID` in our case). We also remove stopwords with by filtering out the rows with `stopwords::stopwords("en", "stopwords-iso")` (`stopwords` comes with `tidytext`). 

```r
# transform data into dfm ----
dfm <- copy(data) %>%
  tidytext::unnest_tokens(word, text) %>% # tokennize
  .[!(word %in% stopwords::stopwords("en", "stopwords-iso"))] %>% # remove stopwords
  .[!stringr::str_detect(word, "\\d")]  # remove digits

sparseWords <- dfm %>%
  .[, .(n=.N), by=c("ID", "word")]  # count terms by text ID

sparseWords <- data[, .(ID, sentiment)] %>% 
  .[sparseWords, on=c("ID")] # need sentiment columns, merge original data with term count data

sparseWords <- sparseWords %>% tidytext::cast_dfm(ID, word, n) # create dfm
```
We need to calculate the frequency of  produce a [document-term matrix](https://en.wikipedia.org/wiki/Document-term_matrix) 


The training set corresponds to 25% (9,967 obs.) of the total --my machine can't handle more than that. 

---
