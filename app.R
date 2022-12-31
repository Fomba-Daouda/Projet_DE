## Installation des libraries

library(shinythemes)
library(tidyverse)
library(DT)
library(dplyr)
library(tidytext)
library(DT)

## Début de l'interface
ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"),  
  titlePanel("Drag-and-drop textual analysis"),
  tags$div(class="header", checked=NA,
     tags$p("Upload a text file and choose a keyword below to run an exploratory textual and sentiment analysis")),
  hr()
)


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

  ##----------Data table -----------Armel--------------------
  output$tbneg <- DT::renderDataTable({
    
    if (is.null(input$file)){
      return(NULL)      
    }
    
    LookForKeyword <- c(input$keyword)
    
    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
    # tokenizedT <- df2 %>%
    #   select(value) %>%
    #   unnest_tokens(word, value) %>%
    #   count(word, sort = TRUE) %>%
    #   ungroup()
    # tokenizedT
    # 
    # tokenized_rem_stopwordsT <- tokenizedT %>%
    #   anti_join(stop_words)

    sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
    labMT <- sentiments %>%
      select(word, happs)
    
    ### Quick sentiment analysis
    
    allsentimentT <- df2 %>%
      select(value) %>%
      unnest_tokens(word, value) %>%
      anti_join(stop_words) %>%
      inner_join(labMT, by = "word") %>%
      group_by(word) %>%
      summarize(sentiment = mean(happs)) %>%
      arrange(desc(sentiment)) %>%
      mutate("sentiment2" = sentiment-5.372 )
    
    bottomsent <- head(arrange(allsentimentT,sentiment2), n = 50)
    
    DT::datatable(bottomsent)
    
  })
  # fin data table---------Armel------------------------------

  ##Debut data table-------Armel------------------------------
  output$bigramsT <- DT::renderDataTable({
    
    if (is.null(input$file)){
      return(NULL)      
    }
    
    LookForKeyword <- c(input$keyword)
    
    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
    bigrams <- df2 %>%
      select(value) %>%
      unnest_tokens(ngram, value, token = "ngrams", n = 2) %>%
      count(ngram, sort = TRUE) %>%
      ungroup()

    DT::datatable(bigrams)
  })  
  # Fin data table--------------------Armel--------------------------------

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

