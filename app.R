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

  #Affichage des données
  output$tb <- DT::renderDataTable({
    LookForKeyword <- c(input$keyword)
    df <- filedata() 
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    DT::datatable(df2)
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

    trigrams_15 <- df2 %>%
      select(value) %>%
      unnest_tokens(ngram, value, token = "ngrams", n = 3) %>%
      count(ngram, sort = TRUE) %>%
      ungroup() %>%
      top_n(25) #Paramètre à ajouter dynamiquement sur la vue
    
    p6 <- ggplot(trigrams_15, aes(x=reorder(ngram, -n), y=n)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      ylab("frequency") +
      xlab("ngram")
    
    p6
    
  })
  #Fin trigrams -------------Armel----



}


#Partie ui
shinyApp(ui = ui, server = server)