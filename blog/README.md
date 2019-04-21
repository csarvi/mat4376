# Building an application in `shiny` for binary text sentiment classification

In this tutorial, I will show you how to build a text sentiment classification application in [`shiny`](https://shiny.rstudio.com/). This app should:

<li>Accept text supplied by the user in <code>shiny</code>'s user interface (UI);</li>

<li>Process this text into trained classification model;</li>

<li>Provide a score varying between 0 (negative) and 1 (positive);</li>

<li>Crowdsource data collection by allowing feedback on the results;</li>
<br>
The last component is of particular importance. The user may choose to accept or reject the resulting score which then prompts the application to save this information on a separate dataset. This stored data can then be used to improve the classifier.

Before starting, certify that the following packages are installed: `"shiny"`, `"flexdashboard"`, `"magrittr"`, `"data.table"`, `"caret"`, `"glmnet"`, `"quanteda"`, `"tidytext"`, `"stringr"`, `"mccr"`. If you are not sure, run this code snippet before starting your analysis (see script [`packages.R`](https://github.com/jdemello/mat4376/blob/master/blog/packages.R):

```r
### RUN THIS SCRIPT BEFORE STARTING YOUR ANALYSIS - ENSURES ALL PKGS ARE INSTALLED
pkgs <- c("magrittr", "data.table", "shiny", "flexdashboard", "caret", "glmnet", "quanteda", "tidytext", "stringr", "mccr")
for(i in seq_along(pkgs)){
 if(!nzchar(system.file(package = pkgs[[i]]))) install.packages(pkgs[[i]])
}
rm(pkgs, i)
```

I am going to assume that your working directory is the folder corresponding to the `blog` directory in this document. If you are in doubt, enter in `R`: `getwd()`. The result should end with `blog`: `"path/to/blog"`. If not, set the working directory to the `blog` folder: `setwd("path/to/blog")`. For [RStudio](https://www.rstudio.com/products/rstudio/download/) users, you can create a [project](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects) which will set the directory to the project's folder.

## Building the classifier

The first step is to build a classifier. This is not done in `shiny`. In this step, all data manipulation and modelling is done in the `./model` folder.

### Getting the data

The first step is to get the data. We use Andrew Maas' <a href="http://ai.stanford.edu/~amaas/data/sentiment/]">Large Movie Review Dataset</a>. It contains 50,000 movie reviews from the <a href="https://www.imdb.com/">IMDb</a> website. Scores vary from 0 to 5 stars with the increment unit being 0.5 stars --0 being the lowest and 10 being the best. The raw data is separated into two, balanced equal sets of 25,000 --i.e. a 50k training and test set. Our approach is to combine the training and test sets since we want more observations in the training set as opposed to have equal size for both sets. 

Reviews with a score between 4 and 7 are excluded since they tend to be more dubious (neutral) in sentiment. We consider reviews with a score between 0 and 3 to be negative whereas a score between 8 and 10 is positive. We end up with 39,866 reviews to train and test our model. We are building a classifier for a binary variable. In our dataset, 0 and 1 denote negative and positive sentiment respectively. For convenience, this data is saved in `./model/processed_data.RDS`. 

I also provide a script (`./model/data_processing.R`) that combines the raw data from the Large Movie Review Dataset into a `data.table`. I am not including the raw data in the `./folder` directory but to make this script work, you wil have to [download the data](http://ai.stanford.edu/~amaas/data/sentiment/aclImdb_v1.tar.gz) and extract the `.tar` file in the `./model` folder. The directory structure should look like: `./model/aclImdb_v1.var/aclImdb_v1/...`. Alternatively, you can manually change the paths in the script.

### <a name="dtm_trans">Document-term matrix trasnformation and model estimation</a>

We fit an L1 regularized logistic model with 10-fold cross validation procedure using the `cv.glmnet` function --see the [`glmnet` package vignette](https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html#spa). The reason for regularization is an attempt to alleviate the large number of features (p) over the number of columns (n). The [`caret`](https://topepo.github.io/caret/data-splitting.html) package is used to partition the data.

The file `./model/train_model.R` shows the model building process including the transformation of the combined sentiment dataset into a [document-term matrix](https://en.wikipedia.org/wiki/Document-term_matrix). 

First, we load the processed movie review `data` and create an `ID` column that will be used in the matrix transformation process. We are using `data.table` and `magrittr` for data manipulation.

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
```
Then we count the frequency of words per comment and use the function `tidytext::cast_dfm` to transform the tidy dataset into an object of class `dfm` of the [`quanteda`](https://quanteda.io/) package. An object of class `dfm` is a document-term matrix. The `quanteda` package simplifies the document-term matrix manipulation.

```r
sparseWords <- dfm %>%
  .[, .(n=.N), by=c("ID", "word")]  # count terms by text ID

sparseWords <- data[, .(ID, sentiment)] %>% 
  .[sparseWords, on=c("ID")] # need sentiment columns, merge original data with term count data

sparseWords <- sparseWords %>% tidytext::cast_dfm(ID, word, n) # create dfm

# save the original matrix to use to intersect with new data features ---
saveRDS(sparseWords, "model/dfm_mod.RDS")
```

<a name="dfm_mod"></a>The last line of the code above is important. We are saving the full `dfm` object for a reason. In our `shiny` app, we will need to feed the model with new information. In document-term matrices, every column is a term with the inner cells being their frequency. In our exemple, every row is a review and every cell is the count of a particular term relative to that review. We use this matrix to build our model. But then what happens when we feed new text to our app and some terms don't appear in the document-term matrix used to build the model? This is why we are saving this matrix. We will use it to intersect with the new matrix generated by the comment left by the user in the app. Only those terms showing in both the original sparse matrix used to build the model and the user supplied text will be preserved for classification. The model fit will throw an error if you try to supply new features to it.

Now we partition the data into training and test set. I tried to increase the number of observations in my training set as much as possible. Unfortunately, my machine could only handle 25% (9,967 obs.) of the total observations for model training.

```r
#### create train/test samples
set.seed(1234) # reproducibility

# data partition
indexTrain <- caret::createDataPartition(data$sentiment,p = 0.25,list=FALSE)
indexTest <- seq_len(nrow(data))[which(!(seq_len(nrow(data)) %in% indexTrain))]

#### run binomial model with cross validation

# features training set
trainSet <- sparseWords[indexTrain[, 1], ]

# response variable training set
y <- data[indexTrain[, 1], sentiment]

set.seed(1234) # reproducibility
mod <- glmnet::cv.glmnet(y = y, 
                         x = trainSet, nfolds = 10, 
                         family="binomial", type.measure = "class", alpha=1)
```

The rest of the code in `./model/train_model.R` fits the model in the test set and calculates the [Matthew's Correlation Coefficient](https://en.wikipedia.org/wiki/Matthews_correlation_coefficient) and the [true positive rate](https://en.wikipedia.org/wiki/Sensitivity_and_specificity). We then proceed to save the model to use in the `shiny` app:

```r
# create a test set
testSet <- sparseWords[indexTest, ]

#### test set model validation
# predict test set using resulting model
fit <- predict(mod, testSet, type="response")

# use a probability distribution to assign class
res <- apply(fit, 1, function(y) rbinom(1, 1, y))

# generate an actual and prediction dataset 
predictions <- data.table(Prediction=res, Actual=data[indexTest, sentiment])

# calculate the MCC 
mcc <- mccr::mccr(data[indexTest, sentiment], res) # 0.5425818

# calculate true positive rate
tablePred <- table(predictions)
tpr <- tablePred[2, 2]/(tablePred[2, 2] + tablePred[1, 2]) # ~ 77%

#### save model
saveRDS(mod, "model/trained_model.RDS")
```

Our model (`mod`) is saved as <a name="trained_model">`./model/trained_model.RDS`</a>. The last thing to do is to feed some unseen text into `mod` for classification. This part is of particular importance since this piece of code will be used in our app. We can write a function that will receive some text as input and it will manipulate this string into a document-term matrix in order to be passed onto the model for classification. Why do we need to create a function? Think of our app. We need some piece of code that performs the same task every time the user supply some text. What does this function needs to do?

1. Receive an input string and process it into a document-matrix;

2. Intersect its features with the matrix used to train the model, **only those words that exist in the training matrix will be preserved**;

3. Feed this matrix into the model and return its score.

I saved this function as `./helpers/getScore.R`. Some advice: name the file that contains your custom function with this function's name. This helps you to keep things organized and easily identifiable when you are managing your files. Unsurprisingly, my funciton is called <a name="getScore">`getScore()`</a> and here it is what it does:

```r
getScore <- function(mod, dfmMod, txt){
  # insert text in a dataframe
  txt <- data.frame(text= txt, 
                    stringsAsFactors = FALSE)
  
  # unnest tokens
  txt2 <- tidytext::unnest_tokens(txt, word, text)
  
  # remove stopwords
  txt2 <- txt2[which(!(txt2$word %in% stopwords::stopwords("en", "stopwords-iso"))), , drop=FALSE]
  # remove digits
  txt2 <- txt2[!grepl(x=txt2$word, pattern = "\\d"), , drop=FALSE]
  # count remaining words in txt
  txt2 <- data.frame(table(txt2$word), stringsAsFactors = FALSE)
  # rename cols
  names(txt2) <- c("word", "n")
  # create fake ID for dfm creation
  txt2$ID <- 1
  
  # create dfm matrix
  dfmNew <- tidytext::cast_dfm(data = txt2, document = ID, term = word, value = n)
  
  # intersect with features in the model's document feature matrix
  dfmNew <- quanteda::dfm_select(dfmNew, dfmMod, valuetype="fixed")
  
  # get feature intersection ----
  res <- predict(mod, dfmNew, type="response")
  
  return(res)
}
```

First of all, let's understand the arguments of this function. It takes three arguments:

* `mod`: this is the (saved) trained model in `./model/trained_model.RDS`;

* `dfmMod`: the `dfm` matrix used to train your model;

* `txt`: the unseen piece of text supplied by the user.

The first parts of this code is not new to you. It repeats the steps done in the [training data processing section](#dtm_trans). The only novel piece of code here is in the matrix feature selection `dfmNew <- quanteda::dfm_select(dfmNew, dfmMod, valuetype="fixed")`. The first part uses the `quanteda::dfm_select()` function. The first argument is the newly created `dfm` object from `txt`. `dfmMod` is the `pattern` argument in `quanteda::dfm_select()` --type `formals(quanteda::dfm_select)` to see which arguments this function takes. The function understands that `dfmMod` in the `pattern` argument refers to the features of the training set `dfm` object. Finally, `valuetype="fixed"` specifies that the match between features needs to be exact.

To see `getScore()` in action, type this code on your console --make sure the working directory is the `blog` folder:

```r
# load function
source("helpers/getScore.R", local=T)

# import model and training set dfm
mod <- readRDS("model/trained_model.RDS")
dfmMod <- readRDS("model/dfm_mod.RDS")

# create a sentence for classification
txt <- "I love cookie dough and brownie ice-cream!"

getScore(mod=mod, dfmMod=dfmMod, txt=txt) # returns 0.6957222
```

## `shiny` app

Before we start with a brief introduction to `shiny`, let's refresh the funcitonality of our app: 

* the user supplies some text;

* this text is used to feed the previously trained model to generate a score from 0(negative) to 1 (positive);

* Crowdsource data collection by allowing feedback by the user;

### A brief introduction to `shiny`

`shiny` enables `R` users to write interactive web-based application in `R` with little to no HTML, CSS and javascript knowledge. RStudio offers a more in-depth tutorial to `shiny` [here](https://shiny.rstudio.com/tutorial/).

On its most general level, a `shiny` app has three components:

* `ui`: this is where the user defines the page layout and its elements;

* `server`: where the "backend" stuff goes;

* a function that executes the app;

You need to have these three elements in place to make your `shiny` app work. For instance, you can put all these three elements in a single script like so:

```r
ui <- function(){
  
}

server <- function(){
  
}

shiny::shinyApp(ui=ui, server=server)
```

Make sure this script is in your current working directory. By running the code above, you should get a "Not Found" message, which is fine since your page has no layout. We need to populate the `ui` with something.

If you app grows on complexity, it is often a good idea to treat these three elements in separate scripts --this how I structured the scripts on this repo. In this case, three separate scripts are created: `ui.R`, `server.R` and `app.R`. Make sure these scripts are located in your current working directory. `ui.R` and `server.R` obey the same structure as shown above except that you don't need `shiny::sinyApp(...)` anymore. Instead, `app.R` contains one line: `shiny::runApp(launch.browser=T)`. Executing `app.R` starts the app.


#### Designing your `ui`

Fortunately, `shiny` has a suite of page layout functions to pick from. In our app, we are working the `shiny::fluidPage()` function.<sup>[1](#footnote1)</sup> This function creates a fluid page layout, with n-rows and a 12-unit wide grid columns for each row. So think about an $n \times 12$ grid layout. If we add the fluid page aspect and include `server <- function(input, output){}`, we have ourselves a (empty) page:

```r
ui <- function(){
  shiny::fluidPage()
}

server <- function(input, output){
  
}

shiny::shinyApp(ui=ui, server=server)
```

Congrats, you've written your first web page with `shiny`. It is important to note that any visual element you want to add to your page needs to go inside `shiny::fluidPage()`.

Now you may want to add a title to your page. No need for HTML, `shiny::titlePanel()` creates a title for you:

```r
ui <- function(){
  shiny::fluidPage(
    shiny::titlePanel("Text classification app")
  )
}

server <- function(input, output){
  
}

shiny::shinyApp(ui=ui, server=server)
```

Great, your webpage now has a title but why stop there? Let's transform this into a web app.

#### Widgets

`shiny` has [out-of-the-box widgets](https://shiny.rstudio.com/gallery/widget-gallery.html) that we can utilize. Widgets are elements in your webpage that interact with the user. In other words, widgets are essential to create a web-based app. In our text classification app, users need to enter some text for sentiment classification. We use `shiny::textAreaInput()`:

```r
ui <- function(){
  shiny::fluidPage(
    # webpage title
    shiny::titlePanel("Text classification app"), 
    # text box input
    shiny::textAreaInput(inputId = "inputSentence", label=NULL, resize="both", 
                         placeholder = "Write a sentence for evaluation...", 
                         width="400px", height = "225px")
  )
}

server <- function(input, output){
  
}

shiny::shinyApp(ui=ui, server=server, options=list(launch.browser=T))
```

Take note, every element that goes inside of `...` in `shiny::fluidPage()` needs to be specified in a list-like structure --i.e. separated by a comma. Our webpage now has a title and a text box. Running the whole code again should yield a webpage with a text box --feel free to type anything! I also introduced `options=list(launch.browser=T)` to `shiny::shinyApp()`. What it does is exactly what it says: it launches your app on your <strong>default browser</strong>.

By now, we need to introduce the concept of reactivity in `shiny` --see this [reactivity overview](https://shiny.rstudio.com/articles/reactivity-overview.html) for a more in-depth dicussion. In your app, the text input inside the text box is a reactive value and this value can be accessed through the `inputId` label in the function `shiny::textAreaInput(inputId="inputSentence", ...)`. The values in the text box area is reacting to the user's input. This input is changed _in loco_ as the user types something new to the text. You can access the values from the text box in your browser like so:

1. Run your app;

2. Once you are on your browser, type `Ctrl + shift + I` to access the developer tools;

3. Go to "Console" and type `$("#inputSentence").val()`, an empty string (`""`) is returned;

4. Now type something in the textbox and repeat (3); the console should return the text input in the text box;

![blog_text_reactive](https://i.imgur.com/6uBbXkT.png)


This is great. Our shiny app is working. We can see that the widget responds to the values inputted by the user. 

Now I am introducing another widget: `shiny::actionButton()`. The purpose of this widget is to trigger some action in your app once a button is pressed. Why do we need an action button? As noticed before, the reactive value in `inputSentence` changes every time the user types something on the text box. Now imagine how slow our app would be if a score had to be calculate every time something is typed inside the text box. The purpose of the action button in our app is to act as a conductor between the user input in the text box and sentiment classification: 

```r
ui <- function(){
  shiny::fluidPage(
    # webpage title
    shiny::titlePanel("Text classification app"), 
    # text box input
    shiny::textAreaInput(inputId = "inputSentence", label=NULL, resize="both", 
                         placeholder = "Write a sentence for evaluation...", 
                         width="400px", height = "225px"),
    # action button
    shiny::actionButton(inputId = "guessButton", label = "Let me guess!")
  )
}

server <- function(input, output){
}

shiny::shinyApp(ui=ui, server=server, options = list(launch.browser=T))
```

There is now a button (labelled "Let me guess!") under the textbox. If you press this button, nothing happens --reason: the action button has not been linked to the textbox input as a reactive event. This will be done in `server`.

#### Output functions

We borrow the function `gaugeOutput` from the [`flexdashboard`](https://rmarkdown.rstudio.com/flexdashboard/using.html#gauges) package. This is an `output` function (see the `output` in `server`) that generates a gauge graph:

```r
ui <- function(){
  shiny::fluidPage(
    # webpage title
    shiny::titlePanel("Text classification app"), 
    # text box input
    shiny::textAreaInput(inputId = "inputSentence", label=NULL, resize="both", 
                         placeholder = "Write a sentence for evaluation...", 
                         width="400px", height = "225px"),
    # action button
    shiny::actionButton(inputId = "guessButton", label = "Let me guess!"), 
    # gauge output
    flexdashboard::gaugeOutput("scoreGauge", height = "120px")
  )
}

server <- function(input, output){
  
}

shiny::shinyApp(ui=ui, server=server, options = list(launch.browser=T))

```

An `output` function in the `ui` is **usually** a reactive endpoint. Output functions in `ui` are placeholders for some object that will be displayed given some action done by the user. That means that the `flexdashboard::gaugeOutput()` in our app is the receiving end of some chain of reactive events. In our particular example, here is the chain: 

> user types something in the textbox --> user presses the action button --> text is shipped to the classifier which in turn produces a score --> the score is fed to the gauge output rendering function.

No gauge graph is displayed after running the app --expected since no chain has been established in `server` function. Output functions in the `ui` are merely placeholders in this case, it shows where the output appears but all the action happens in `server` that shoots back something to the user interface.

#### Pre-loaded objects

Before we move on to `server`, there is something else you may be asking yourself: how do I load the classifier in the app? You can pre-load and create any `R` object before running `ui` or `server`. In this example, some content is loaded before `ui`. Usually, you want to load packages, `source()` functions, read and create objects. The content is defined before running your app --which means that this code runs only once each time you run the app. **Be mindful of overheading**: adding a lot of stuff in the pre-loading stage will cause delay in starting your app.

It is good practice to break your code into multiple scripts whenever you can discern the different tasks in your app. I like to put these scripts in a folder called `helpers`. Anything that goes into `helpers` is an ancillary function that helps to run the main task at hand. The `R` script  [`loadContents.R`](https://github.com/jdemello/mat4376/blob/master/blog/helpers/loadContents.R):

* loads `shiny` and `glmnet`;

* loads the function [`getScore()`](#getScore) --produces a sentiment classification score;

* loads the [classifier](#trained_model) and the [training dtm](#dfm_mod) --both are used as arguments in `getScore()`; 

Finally, we place the code before defining `ui`:

```r
# load contents ----
source("helpers/loadContents.R")

# Define UI ----
ui <- function(){
  ### ui code goes here ####
  ...
}
```

#### `server`

Let's get into business here and show what goes inside `server`:

```r
server <- function(input, output, session) {
  
  # text - to be inserted in the dataset ----
  txtIn <- shiny::eventReactive(
    input$guessButton, {
      return(input$inputSentence)
    }
  )
  
  # calculate score - to be inserted in the dataset ----
  score <- shiny::eventReactive(
    input$guessButton, {
      txt <- input$inputSentence
      score <- getScore(mod=mod, dfmMod=dfmMod, txt=txt)
      return(score)
    }
  )
  
  # gauge rendering ----
  output$scoreGauge <- flexdashboard::renderGauge({
    flexdashboard::gauge(
      round(100 * score(), digits = 1), 
      symbol = "%",
      min=0,
      max=100,
      sectors = flexdashboard::gaugeSectors(
        success=c(70, 100), 
        warning=c(50,70), 
        danger = c(0,50),
        colors = c("#3CB371", "#FF7F50", "#FF69B4")
      )
    )
  })
}
```

##### Reactive values


----
<sup name="footnote1">1</sup> I use the notation `package::function` whenever possible to make clear where that function comes from. Alternatively, you can load the package using `library(package)`