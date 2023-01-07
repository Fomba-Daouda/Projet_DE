## Installation des libraries

library(shinythemes)
library(tidyverse)
library(DT)
library(dplyr)
library(tidytext)
library(wordcloud)

## Début de l'interface
ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"),  
  titlePanel("Analyse textuelle"),
  tags$div(class="header", checked=NA,
    tags$p("Téléchargez un fichier texte/csv et choisissez un mot-clé ci-dessous pour exécuter une analyse exploratoire du texte et des sentiments")
  ),
  hr(),
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    ## SidePannel--------------------Reda-----------------------
    sidebarPanel(
      fileInput("file", "Téléchargez votre fichier txt/csv, vous pouvez glisser-déposer"),
      hr(),
      textInput("keyword", "Rechercher un mot-clé", ""),
      hr(),
      tags$div(class="header", checked=NA,
        tags$p("Une fois que vous avez téléchargé un document, faites défiler vers le bas pour voir les phrases contextuelles, l'analyse des sentiments et les principaux bi- et trigrammes.")
      ),
      hr(),
      textInput("neg", "Changer la couleur négative", "red"),
      hr(),
      textInput("pos", "Changer la couleur positive", "blue"),
      hr(),
      tags$br(),
      hr(),
      #tags$div(class="header", checked=NA,
        #tags$p("This analysis uses the R package 'tidytext' and the 'labMT' sentiment dictionary from Andy Reagan. Created by Aleszu Bajak.")
      #) 
    ),
    #Fin SidePannel -------------Reda------------------
    #Début mainPannel ----------------------Daouda-------------------------
    mainPanel(
      h4("Sentences", align = "center"),
      DTOutput("tb"),
      h4("Les mots les plus négatifs et les plus positifs", align = "center"),
      plotOutput("p_sentT"),
      h4("Top 50 des mots positifs", align = "center"),
      DTOutput("tbpos"),
      h4("Top 50 des mots négatifs", align = "center"),
      DTOutput("tbneg"),
      h4("Top bigrams", align = "center"),
      DTOutput("bigramsT"),
      h4("Top bigrams", align = "center"),
      plotOutput("biplot"),
      h4("Top trigrams", align = "center"),
      DTOutput("trigramsT"),
      h4("Top trigrams", align = "center"),
      plotOutput("triplot"),
      h4("Nuage de mots", align = "center"),
      plotOutput("nuage")
      
    )
    #Fin mainPanel-------------------Daouda-----------------------------
  )
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
  #Data table --------------Reda-------------
  output$tbpos <- DT::renderDataTable({

    if (is.null(input$file)){
      return(NULL)      
    }

    LookForKeyword <- c(input$keyword)

    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
    tokenizedT <- df2 %>%
      select(value) %>%
      unnest_tokens(word, value) %>%
      count(word, sort = TRUE) %>%
      ungroup()
    tokenizedT
    
    tokenized_rem_stopwordsT <- tokenizedT %>%
      anti_join(stop_words)
    
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
    
    # Bind 10 most positive terms and 10 most negative terms

    output$nuage <- renderPlot({
      wordcloud(words = tokenized_rem_stopwordsT$word, tokenized_rem_stopwordsT$n, min.freq = 2,
      max.words=100, random.order=FALSE, rot.per=0.40, 
      colors=brewer.pal(8, "Dark2"))
    })
    
    topsent <- allsentimentT %>%
      top_n(50) 

    DT::datatable(topsent)
    
    #wcT <- wordcloud(words = tokenized_rem_stopwordsT$word, freq = tokenized_rem_stopwordsT$n, min.freq = 1,
     # max.words=100, random.order=FALSE, rot.per=0.15,
      #colors=brewer.pal(8, "RdGy")
    #)

  })
          
  ##--------Fin Data table--------Reda
  ##----------Data table -----------Armel--------------------
  output$tbneg <- DT::renderDataTable({
    
    if (is.null(input$file)){
      return(NULL)      
    }
    
    LookForKeyword <- c(input$keyword)
    
    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
    tokenizedT <- df2 %>%
      select(value) %>%
      unnest_tokens(word, value) %>%
      count(word, sort = TRUE) %>%
      ungroup()
    tokenizedT
    
    tokenized_rem_stopwordsT <- tokenizedT %>%
      anti_join(stop_words)

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
   #debut plot --------------Daouda--------------
  output$p_sentT <- renderPlot({
    
    if (is.null(input$file)){
      return(NULL)      
    }
    
    LookForKeyword <- c(input$keyword)
    
    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
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
    
    # Bind 10 most positive terms and 10 most negative terms
    
    bottomsentT <- allsentimentT %>%
      top_n(-10) 
    topsentT <- allsentimentT %>%
      top_n(10) 
    sentimentT <- bind_rows(bottomsentT,topsentT) %>%
      arrange(desc(sentiment2)) %>%
      distinct() # remove duplicates
    sentimentT
    
    p_sentT <- ggplot(sentimentT, aes(x= reorder(word, -sentiment2), 
      y = sentiment2, 
      fill = sentiment2 > 0)) + #this is midpoint of labMT sentiment dictionary
      geom_col(show.legend = FALSE) +
      coord_flip() +
      ylab("sentiment") +
      xlab("word") + 
      scale_y_continuous(limits=c(-5, 5)) +
      scale_fill_manual(values=c(input$neg,input$pos))
    p_sentT
    
  })
  
  ## Fin plot Daouda
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
# Debut plot----------Reda-----------------------------------------------
  output$biplot <- renderPlot({
    
    if (is.null(input$file)){
      return(NULL)      
    }
    
    LookForKeyword <- c(input$keyword)
    
    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
    bigrams_15 <- df2 %>%
      select(value) %>%
      unnest_tokens(ngram, value, token = "ngrams", n = 2) %>%
      count(ngram, sort = TRUE) %>%
      ungroup() %>%
      top_n(25)
    
    p7 <- ggplot(bigrams_15, aes(x=reorder(ngram, -n), y=n)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      ylab("frequency") +
      xlab("ngram")
    p7
    
  })

  #Fin plot ---------------------Reda---------------------------------
  
   
  
  # trigramsT-----------DAOUDA------------------------------
  
  output$trigramsT <- DT::renderDataTable({
    
    if (is.null(input$file)){
      return(NULL)      
    }
    
    LookForKeyword <- c(input$keyword)
    
    df <- filedata()
    df2 <- tbl_df(df[grep(paste(LookForKeyword, collapse="|"),df)])
    
    trigrams <- df2 %>%
      select(value) %>%
      unnest_tokens(ngram, value, token = "ngrams", n = 3) %>%
      count(ngram, sort = TRUE) %>%
      ungroup()
    
    DT::datatable(trigrams)
    
  })
  
  #Fin Trigrams--------------------Daouda
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