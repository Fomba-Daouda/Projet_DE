## Installation des libraries

library(shiny)
library(shinydashboard)
library(flexdashboard)
library(shinythemes)
library(tidyverse)
library(DT)
library(dplyr)
library(tidytext)
library(wordcloud)
library(tensorflow)
library(keras)
library(tfdatasets)
library(coro)
#library("data.table")
#library(DataExplorer)
## Début de l'interface





ui <- dashboardPage(skin="green",
  dashboardHeader(title="PROJET DATA ENGINEERING"),
  dashboardSidebar
    (title = "Menu",
      sidebarMenu(
        menuItem("Sentiments analysis", tabName = "dash", icon = icon("fas fa-chart-bar"))
      )
    ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML('
          /* logo */
          .skin-green .main-header .logo {
            background-color: #073d60;
            color: #fff;
            border-bottom: 0 solid transparent;
          }
          .skin-blue .main-header .logo {
            background-color: #f4b943;
          }
          .skin-green .main-header .navbar {
            background-color: #03787c;
          }
          .skin-green .left-side, .skin-green .main-sidebar, .skin-green .wrapper {
            background-color: #073d60;
          }
          .skin-green .sidebar-menu>li.active>a, .skin-green .sidebar-menu>li:hover>a {
            color: #fff;
            background: #03787c;
            border-left-color: #00a65a;
          }
          div#output_signataire {
            color: red;
            text-align: center;
          }'
        )
      )
    ),
    tags$div(class="header", checked=NA,
      theme = shinythemes::shinytheme("journal"),  
      titlePanel("Analyse textuelle"),
      tags$div(class="header", checked=NA,
        tags$h4("Téléchargez un fichier texte/csv et choisissez un mot-clé ci-dessous pour exécuter une analyse exploratoire du texte et des sentiments")
      )
    ),
    hr(),
    #Display datasets
    tabItem(tabName = "dash",
      fluidRow(
        tabsetPanel(
          tabPanel(
            "Analyse des sentiments",
            wellPanel(
              tags$h4("Dans cette partie, nous faisons une analyse de sentiments en fonction du fichier donnée au programme !", style="color: violet"),
              fluidRow(
                box(width = 3,
                  ## SidePannel--------------------Reda-----------------------
                  sidebarPanel(width=12,
                    fileInput("file", "Téléchargez votre fichier txt/csv, vous pouvez glisser-déposer"),
                    hr(),
                    textInput("keyword", "Rechercher un mot-clé (positif/négatif)", ""),
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
                    hr()
                  ),
                  #Fin SidePannel -------------Reda------------------
                ),
                box(width = 9,
                  #Début mainPannel ----------------------Daouda-------------------------
                  mainPanel(width=12,
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
            )
          ),
          tabPanel(
            "ML avec Tensorflow(Keras)",
            fluidRow(
              box(width = 12,
                wellPanel(
                  tags$h4("
                    Dans cette partie, nous entraînons un modèle d'analyse des sentiments pour classer les critiques de films comme positives ou négatives , en fonction du texte de la critique.", style="color: royalblue"
                  ),
                  tags$h4("
                    Il s'agit d'un exemple de classification binaire - ou à deux classes -, un type de problème d'apprentissage automatique important et largement applicable.", style="color: royalblue"
                  ),
                  tags$h4("Nous utiliserons l' ensemble de données Large Movie Review qui contient le texte de 50 000 critiques de films de la base de données de films Internet.", style="color: royalblue"),
                  tags$h4("Ceux-ci sont divisés en 25 000 avis pour l'entraînement et 25 000 avis pour les tests. 
                    Les ensembles de formation et de test sont équilibrés , ce qui signifie qu'ils contiennent un nombre égal d'avis positifs et négatifs.", style="color: royalblue"
                  )
                )
              ),
              wellPanel(
                box(width = 12,
                  column(12,
                    column(5,
                      box(width = 12, title = "Paramètres du réseau de neurones",
                        verbatimTextOutput("summary")
                      )
                    ),
                    column(3,
                      box(width = 12, title = "Évaluer le modèle",
                        verbatimTextOutput("evaluate")
                      )
                    ),
                    column(4,
                      box(width = 12, title = "Graphe de précision et de perte au fil du temps",
                        plotOutput("precision")
                      )
                    )
                  )
                )
              ),
             wellPanel(
               box(width = 12,
                 column(12,
                  column(6,
                   box(width = 12, title = "Entrez un commentaire dont vous aimerez prédire sa classe",
                      textInput("new_text_to_predict", label = NULL, placeholder = "Entrez un commentaire en anglais")
                   )
                  ),
                  column(6,
                   box(width = 12, title = "Nature de votre commentaire !", style="text-align: center; color: firebrick; font-weight: bold;",
                      textOutput("prediction")
                   )
                  )
                 )
               )
             )
            )
          )
        )
      )
    )
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
      dplyr::select(value) %>%
      unnest_tokens(word, value) %>%
      count(word, sort = TRUE) %>%
      ungroup()
    tokenizedT
    
    tokenized_rem_stopwordsT <- tokenizedT %>%
      anti_join(stop_words)
    
    sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
    labMT <- sentiments %>%
    dplyr::select(word, happs)
    
    ### Quick sentiment analysis
    
    allsentimentT <- df2 %>%  
      dplyr::select(value) %>%
      unnest_tokens(word, value) %>%
      anti_join(stop_words) %>%
      inner_join(labMT, by = "word") %>%
      group_by(word) %>%
      dplyr::summarize(sentiment = mean(happs)) %>%
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
      dplyr::select(value) %>%
      unnest_tokens(word, value) %>%
      count(word, sort = TRUE) %>%
      ungroup()
    tokenizedT
    
    tokenized_rem_stopwordsT <- tokenizedT %>%
      anti_join(stop_words)

    sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
    labMT <- sentiments %>%
    dplyr::select(word, happs)
    
    ### Quick sentiment analysis
    
    allsentimentT <- df2 %>%
      dplyr::select(value) %>%
      unnest_tokens(word, value) %>%
      anti_join(stop_words) %>%
      inner_join(labMT, by = "word") %>%
      group_by(word) %>%
      dplyr::summarize(sentiment = mean(happs)) %>%
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
    dplyr::select(word, happs)
    
    ### Quick sentiment analysis
    
    allsentimentT <- df2 %>%  
      dplyr::select(value) %>%
      unnest_tokens(word, value) %>%
      anti_join(stop_words) %>%
      inner_join(labMT, by = "word") %>%
      group_by(word) %>%
      dplyr::summarize(sentiment = mean(happs)) %>%
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
      dplyr::select(value) %>%
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
      dplyr::select(value) %>%
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
      dplyr::select(value) %>%
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
      dplyr::select(value) %>%
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




  #------------Ajout du modèle Début--------------#

  #Téléchargez et explorez le jeu de données IMDB
  # url <- "https://ai.stanford.edu/~amaas/data/sentiment/aclImdb_v1.tar.gz"
  # 
  # dataset <- get_file(
  #   "aclImdb_v1",
  #   url,
  #   untar = TRUE,
  #   cache_dir = '.',
  #   cache_subdir = ''
  # )

  dataset_dir <- file.path("aclImdb")

  list.files(dataset_dir)
  
  train_dir <- file.path(dataset_dir, 'train')
  list.files(train_dir)
  
  sample_file <- file.path(train_dir, 'pos/1181_9.txt')
  readr::read_file(sample_file)
  
  remove_dir <- file.path(train_dir, 'unsup')
  unlink(remove_dir, recursive = TRUE)

  #Les répertoires aclImdb/train/poset aclImdb/train/negcontiennent de nombreux fichiers texte,
  #dont chacun est une critique de film unique. Jetons un coup d'œil à l'un d'eux.


  #Créons un ensemble de validation en utilisant une répartition 80:20 des données d'apprentissage.
  batch_size <- 32
  seed <- 42
  
  raw_train_ds <- text_dataset_from_directory(
    'aclImdb/train',
    batch_size = batch_size,
    validation_split = 0.2,
    subset = 'training',
    seed = seed
  )

  batch <- raw_train_ds %>%
    reticulate::as_iterator() %>%
    coro::collect(n = 1)
  
  batch[[1]][[1]][1]

  raw_val_ds <- text_dataset_from_directory(
    'aclImdb/train',
    batch_size = batch_size,
    validation_split = 0.2,
    subset = 'validation',
    seed = seed
  )

  raw_test_ds <- text_dataset_from_directory(
    'aclImdb/test',
    batch_size = batch_size
  )

  #Préparer l'ensemble de données pour l'entraînement
  # creating a regex with all punctuation characters for replacing.

  #layer_text_vectorization
  # creating a regex with all punctuation characters for replacing.
  re <- reticulate::import("re")
  
  punctuation <- c("!", "\\", "\"", "#", "$", "%", "&", "'", "(", ")", "*",
                   "+", ",", "-", ".", "/", ":", ";", "<", "=", ">", "?", "@", "[",
                   "\\", "\\", "]", "^", "_", "`", "{", "|", "}", "~")
  
  punctuation_group <- punctuation %>%
    sapply(re$escape) %>%
    paste0(collapse = "") %>%
    sprintf("[%s]", .)
  
  custom_standardization <- function(input_data) {
    lowercase <- tf$strings$lower(input_data)
    stripped_html <- tf$strings$regex_replace(lowercase, '<br />', ' ')
    tf$strings$regex_replace(
      stripped_html,
      punctuation_group,
      ""
    )
  }
  
  max_features <- 10000
  sequence_length <- 250
  
  vectorize_layer <- layer_text_vectorization(
    standardize = custom_standardization,
    max_tokens = max_features,
    output_mode = "int",
    output_sequence_length = sequence_length
  )

  # Make a text-only dataset (without labels), then call adapt
  train_text <- raw_train_ds %>%
    dataset_map(function(text, label) text)
  vectorize_layer %>% adapt(train_text)

  vectorize_text <- function(text, label) {
    text <- tf$expand_dims(text, -1L)
    list(vectorize_layer(text), label)
  }

  # retrieve a batch (of 32 reviews and labels) from the dataset
  batch <- reticulate::as_iterator(raw_train_ds) %>%
    reticulate::iter_next()
  first_review <- as.array(batch[[1]][1])
  first_label <- as.array(batch[[2]][1])
  print(vectorize_text(first_review, first_label))
  
  #cat("Review:\n", first_review)
  #print(vectorize_text(first_review, first_label))

  train_ds <- raw_train_ds %>% dataset_map(vectorize_text)
  val_ds <- raw_val_ds %>% dataset_map(vectorize_text)
  test_ds <- raw_test_ds %>% dataset_map(vectorize_text)

  AUTOTUNE <- tf$data$AUTOTUNE
  
  train_ds <- train_ds %>%
    dataset_cache() %>%
    dataset_prefetch(buffer_size = AUTOTUNE)
  val_ds <- val_ds %>%
    dataset_cache() %>%
    dataset_prefetch(buffer_size = AUTOTUNE)
  test_ds <- test_ds %>%
    dataset_cache() %>%
    dataset_prefetch(buffer_size = AUTOTUNE)

  #Créer le modèle
  embedding_dim <- 16
  
  model <- keras_model_sequential() %>%
    layer_embedding(max_features + 1, embedding_dim) %>%
    layer_dropout(0.2) %>%
    layer_global_average_pooling_1d() %>%
    layer_dropout(0.2) %>%
    layer_dense(1)
  
  summary(model)

  output$summary <- renderPrint({
    summary(model)
  })

  #Fonction de perte et optimiseur
  model %>% compile(
    loss = loss_binary_crossentropy(from_logits = TRUE),
    optimizer = 'adam',
    metrics = metric_binary_accuracy(threshold = 0)
  )

  #Former le modèle
  epochs <- 15
  history <- model %>%
    fit(
      train_ds,
      validation_data = val_ds,
      epochs = epochs
    )

  #Évaluer le modèle
  model %>% evaluate(test_ds)
  output$evaluate <- renderPrint({
    model %>% evaluate(test_ds)
  })

  output$precision <- renderPlot({
    plot(history)
  })

  plot(history)

  #Export the model
  export_model <- keras_model_sequential() %>%
    vectorize_layer() %>%
    model() %>%
    layer_activation(activation = "sigmoid")
  
  export_model %>% compile(
    loss = loss_binary_crossentropy(from_logits = FALSE),
    optimizer = "adam",
    metrics = 'accuracy'
  )
  
  # Test it with `raw_test_ds`, which yields raw strings
  export_model %>% evaluate(raw_test_ds)
  
  # examples <- c(
  #   "The movie was great!",
  #   "The movie was okay.",
  #   "The movie was terrible..."
  # )
  
  observeEvent(input$new_text_to_predict, {
    comment <- input$new_text_to_predict
    if(!is.null(comment) && comment != " "){
      pred_result <- predict(export_model, c(as.character(comment)))
      
      output$prediction <- renderText({ 
        paste("Votre commentaire appartient à la classe 1 (Commentaire positif) avec une probabilité de: ", as.character(pred_result))
      })
    }
  })

  #------------Ajout du modèle Fin--------------#



  #KMEANS RESTAURANT

  # GET DATA
  # reviews_df<-fread('reviews.csv',encoding='UTF-8')
  # reviews_if<-fread('preprocessed_text.csv',encoding='UTF-8') 
  # reviews_df<-merge(reviews_df,reviews_if,by='id_review') #I preprocessed the text feature in https://www.kaggle.com/lazaro97/sentiment-geospatial-analysis. (Upadte: Removing english words)
  # restaurants_df<-fread('restaurants.csv',encoding='UTF-8')
  # nrc<-fread('lexico_nrc.csv', encoding = "UTF-8")
  # 
  # plot_missing(restaurants_df)
  # row.names(restaurants_df)<-restaurants_df$id
  # restaurants_df<-restaurants_df%>%select(-IDDIST,-min_price,-max_price,-platform)
}

#Partie ui
shinyApp(ui = ui, server = server)