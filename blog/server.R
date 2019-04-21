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