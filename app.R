library(shinythemes)
library(tidyverse)
library(DT)
library(dplyr)
library(tidytext)
library(wordcloud)


#Ajout partie serveur
server <- function(input, output, session) {

}


#Partie ui
shinyApp(ui = ui, server = server)