## Installation des libraries

library(shinythemes)
library(dplyr)
library(tidyverse)
library(tidytext)
library(DT)

## Début de l'interface
ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"),  
  titlePanel("Drag-and-drop textual analysis"),
  tags$div(class="header", checked=NA,
           tags$p("Upload a text file and choose a keyword below to run an exploratory textual and sentiment analysis")),
  hr(),
  
)


server <- function(input, output, session) {}



shinyApp(ui = ui, server = server)