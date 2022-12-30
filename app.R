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
}


#Partie ui
shinyApp(ui = ui, server = server)