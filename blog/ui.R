# load contents ----
source("helpers/loadContents.R")

# Define UI ----
ui <- function(){
  # page style and other definitions ----
  shiny::fluidPage(
    
    # load css styles, js scripts, etc.
    shiny::tagList(
      shiny::tags$head(
        shiny::tags$link(rel="stylesheet", type="text/css", href="style.css")
        )
    ),
    
    shiny::titlePanel("Text classification app"),
    shiny::textAreaInput(inputId = "inputSentence", 
                         label = NULL, resize = "both", 
                         placeholder = "Write a sentece for evaluation...", 
                         width  = "400px", height = "225px"),
    shiny::actionButton(inputId = "guessButton", label = "Let me guess!"),
    flexdashboard::gaugeOutput("scoreGauge", height = "120px")
  ) # end of fluid page tag ----
}