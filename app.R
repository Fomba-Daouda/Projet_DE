library(shinythemes)
library(tidyverse)
library(DT)
library(dplyr)
library(tidytext)
library(wordcloud)


#Ajout partie serveur
server <- function(input, output, session) {

  # Chargement du dataset------------------Armel--------- 
  filedata <- reactive({
    infile <- input$file
    if (is.null(infile)){
      return(NULL)   
    }
    scan(infile$datapath, character(0), sep=".",quote=NULL)
  })
  #-----------------Fin chargement------Armel----------------

  #Debut Trigrams--------------------Armel
  output$triplot <- renderPlot({

    if (is.null(input$file)){
      return(NULL)      
    }
    
    LookForKeyword <- c(input$keyword)
    
    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
  })
  
  #Fin trigrams -------------Armel----

}


#Partie ui
shinyApp(ui = ui, server = server)