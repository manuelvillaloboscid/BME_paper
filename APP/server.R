data_db = read.csv("www/DB/BD_Biomedica - BD.csv",sep=";",header=T)
data_main = read.csv("www/DB/BD_Biomedica - Main.csv",sep=";",header=T)
data_thesaurus = read.csv("www/DB/BD_IEEE_tesauro.csv",sep=";",header=T)
data_keywords = read.csv("www/DB/BD_SCOPUS_keywords.csv",sep=";",header=T)


paleta = "Greens" #"Blues" "Zissou" "GrandBudapest"

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  ###############################
  #A) Indexes
  ###############################
  # Permite realizar el análisis de indices 
  source("www/SOURCES/SERVER/backend_indexes.R")
  backend_indexes(input,output,session,data_db,data_main)
  
  ###############################
  #B) Knowledge
  ###############################
  # Permite realizar el análisis de conocimiento 
  source("www/SOURCES/SERVER/backend_knowledge.R")
  backend_knowledge(input,output,session,data_db,data_main,data_thesaurus,data_keywords)
  
  ###############################
  #C) Collaboration
  ###############################
  # Permite realizar el análisis de colaboración 
  source("www/SOURCES/SERVER/backend_collaboration.R")
  backend_collaboration(input,output,session,data_db,data_main,data_thesaurus,data_keywords)
})
