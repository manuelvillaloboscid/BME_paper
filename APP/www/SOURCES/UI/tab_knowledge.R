tab_knowledge=tabPanel("Knowledge",icon = icon("brain"),align = "center",
                     
                       
  ###############################
  #- Principal
  ###############################                       
  br(),
                    
  fluidRow(align = "center", offset = 0,
                               
  h2("Science mapping"),
                               
  br(),
  
  tabsetPanel(type="pills",
    
    ###############################
    #A) Frequency analysis
    ###############################  
    
    tabPanel("Frequency analisys",icon = icon("bars-progress",lib = "font-awesome"),align = "center",
             
             h3("Areas of knowledge - Frequency analysis"),
             
             br(),
             
             p("This section allows visualising the frequency of the most relevant areas of knowledge associated with 
             each university and academic publications by using word clouds and treemaps. We defined the areas according to 
               the",a("IEEE Taxonomy Access",href = "https://www.ieee.org/publications/services/thesaurus-thank-you.html?",
                      target="_blank"),"and SCOPUS index keywords."),


             h4("Instructions"),
             
             tags$div(
               tags$p("1. Select the range of years to analyse."), 
               tags$p("2. Choose the type of publication and level of keywords to use."), 
               tags$p("3. Select the university or academic to create the word cloud and treemaps. If the message 'not enough 
                      data' is shown, this university or academic needs more publications or repeated keywords during the
                      period to be analysed.")
             ),
             
             br(),
             
             br(),
             
             h4("Data analisys"),
             

             sliderInput("slider_Frequency_main_years", "Years", min = 2001, max = 2022, sep = "", value = c(2020,2022)),
            
             
             selectInput("selinput_Frequency_main_publication", "Type of publication", choices = list("All","Article", 
                                                                                                      "Conference Paper")),
             
             selectInput("selinput_Frequency_main_lknowledge", "Area of the knowledge level", 
                         choices = list("IEEE Area - Level 1" = 1, "IEEE Area - Level 2" = 2,"IEEE Area - Level 3" = 3,
                                        "SCOPUS index keywords" = 4), selected = 1),      
             br(),                    
             
             #-------------------------------
             #A.1) Columna métricas Parte 1
             #-------------------------------
             column(5,align = "center", offset =1,
                    
                    selectInput("selinput_Frequency_left_university", "University", choices = list("All the universities" = 1,
                                                                  "Universidad de Valparaíso (UV)" = 2,
                                                                  "Universidad de Concepción (UdeC)" = 3, 
                                                                  "Universidad de Santiago de Chile (USACH)" = 4), selected = 1),
                    br(),
                    plotlyOutput("plot_Frequency_left_summary",height = 200),                
                    br(),
                    plotOutput("plot_Frequency_left_wordcloud"),
                    br(),
                    plotlyOutput("plot_Frequency_left_treemap")
             ), #End olumn
             
             #------------------------------
             #A.2) Columna métricas Parte 2
             #------------------------------
             column(5,align = "center", offset =1,
                    
                    uiOutput('selinput_Frequency_right_academics'),
                    
                    br(),
                    plotlyOutput("plot_Frequency_right_summary",height = 200),                
                    br(),
                    plotOutput("plot_Frequency_right_wordcloud"),
                    br(),
                    plotlyOutput("plot_Frequency_right_treemap")
                    
                    
             ), #End column                  
     
             ), #End tab panel frequency analysis
    
    ###############################
    #B) Temporal - Temporal analysis
    ############################### 
    
    tabPanel("Temporal analysis",icon = icon("clock",lib = "font-awesome"),align = "center",
             
             h3("Areas of knowledge - Comparison between years"),
             
             br(),
             
             p("This section compares the areas of knowledge associated with each university and academic
             publications considering different periods using the ", a("Chi-square metric.",
             href = "https://quanteda.io/reference/textstat_keyness.html",target="_blank"),"We defined the areas according to 
             the",a("IEEE Taxonomy Access", href = "https://www.ieee.org/publications/services/thesaurus-thank-you.html?"
                    ,target="_blank"),"and SCOPUS index keywords."),

             h4("Instructions"),
             
             tags$div(
               tags$p("1. Select both periods to compare."), 
               tags$p("2. Choose the type of publication and level of keywords to use."), 
               tags$p("3. Select the university or academic to analyse. If the message 'not enough data' is shown, 
                      this university or academic has not enough data or publications during the period to be studied.")

             ),
             
             br(),
             
             br(),
             
             h4("Data analisys"),
             
             #------------------------------
             #B.1) Columna métricas Parte 1
             #------------------------------
             column(3,align = "center", offset =3,
                    
                    sliderInput("slider_Temporal_left_years", "Period 1", min = 2001, max = 2022, sep = "", value = c(2020,2022)),
                    
             ), #End column
             
             #------------------------------
             #B.2) Columna métricas Parte 1
             #------------------------------
             column(3,align = "center", offset =0,
                    
                    sliderInput("slider_Temporal_right_years", "Period 2", min = 2001, max = 2022, sep = "", value = c(2001,2019)),
                    
             ), #End column
             
             
             br(),
             br(),
             
             
             selectInput("selinput_Temporal_main_lknowledge", "Area of the knowledge level", 
                         choices = list("IEEE Area - Level 1" = 1,  "IEEE Area - Level 2" = 2,"IEEE Area - Level 3" = 3,
                                        "SCOPUS index keywords" = 4), selected = 1),
             
             selectInput("selinput_Temporal_main_publication", "Type of publication", choices = list("All","Article", 
                                                                                                      "Conference Paper")),
             br(),
             br(),
             br(),
             br(),
             
             #-------------------------------
             #B.3) Columna métricas Parte 1
             #-------------------------------
             column(5,align = "center", offset =1,
                    
                    selectInput("selinput_Temporal_left_university", "University", choices = list("All the universities" = 1, 
                     "Universidad de Valparaíso (UV)" = 2, "Universidad de Concepción (UdeC)" = 3, 
                    "Universidad de Santiago de Chile (USACH)" = 4), selected = 1),
                    
                    br(),
                    plotlyOutput("plot_Temporal_left_summary",height = 200),                
                    br(),
                    
                    plotlyOutput("plot_Temporal_left_comparison",height = 1200),
                    
             ), #End column
             
             
             #------------------------------
             #B.4) Columna métricas Parte 2
             #------------------------------
             column(5,align = "center", offset =1,
                    
                    uiOutput('selinput_Temporal_right_academics'),
                    
                    br(),
                    plotlyOutput("plot_Temporal_right_summary",height = 200),                
                    br(),
                    
                    plotlyOutput("plot_Temporal_right_comparison",height = 1200),
                    
                    
             ), #End column               
             
      ), #End tab panel comparison analysis
     
    
    ###############################
    #C) Comparison between universities and academics
    ############################### 
     
    tabPanel("Comparison analysis",icon = icon("circle-half-stroke",lib = "font-awesome"),align = "center", #https://icons.getbootstrap.com/
    
             h3("Areas of knowledge - Comparison between universities or academics"),
             
             br(),
             
             p("This section allows comparing the areas of knowledge between university and academic publications 
               using the ", a("Chi-square metric.",href = "https://quanteda.io/reference/textstat_keyness.html",
             target="_blank"),"We defined the areas according to 
             the",a("IEEE Taxonomy Access", href = "https://www.ieee.org/publications/services/thesaurus-thank-you.html?"
                    ,target="_blank"),"and SCOPUS index keywords."),
             
             h4("Instructions"),
             
             tags$div(
               tags$p("1. Select the range of years to analyse."), 
               tags$p("2. Choose the type of publication and level of keywords to use."), 
               tags$p("3. Select the university or academic to study If the message 'not enough data' is shown, 
                      this university or academic has not enough data or publications during the period to be studied, or 
                      the same university or academic has been selected.")
               
             ),
             
             br(),
             
             br(),
             
             h4("Data analisys"),
             
             sliderInput("selinput_Comparison_main_years", "Years", min = 2001, max = 2022, sep = "", value = c(2020,2022)),
             
             selectInput("selinput_Comparison_main_publication", "Type of publication", choices = list("All","Article", 
                                                                                                     "Conference Paper")),
             
             selectInput("selinput_Comparison_main_lknowledge", "Area of the knowledge level", 
                         choices = list("IEEE Area - Level 1" = 1, "IEEE Area - Level 2" = 2,"IEEE Area - Level 3" = 3,
                                        "SCOPUS index keywords" = 4), selected = 1),      
             br(),            
             
             #-------------------------------
             #C.1) Columna métricas Parte 1
             #-------------------------------
             column(5,align = "center", offset =1,
                    
                    selectInput("selinput_Comparison_left_top_university", "University 1", choices = list("All the universities" = 1, 
                    "Universidad de Valparaíso (UV)" = 2, "Universidad de Concepción (UdeC)" = 3, 
                    "Universidad de Santiago de Chile (USACH)" = 4), selected = 1),
                    
                    br(),
                    plotlyOutput("plot_Comparison_left_top_summary",height = 200),                
                    br(),
                    
                    selectInput("selinput_Comparison_left_bottom_university", "University 2", choices = list("All the universities" = 1, 
                    "Universidad de Valparaíso (UV)" = 2, "Universidad de Concepción (UdeC)" = 3, 
                    "Universidad de Santiago de Chile (USACH)" = 4), selected = 1),
                    
                    br(),
                    plotlyOutput("plot_Comparison_left_bottom_summary",height = 200),                
                    br(),
                    
                    plotlyOutput("plot_Comparison_left_comparison",height = 1200),
                    
             ), #End column
             
             #-------------------------------
             #C.1) Columna métricas Parte 1
             #-------------------------------
             column(5,align = "center", offset =1,
                    
                    uiOutput('selinput_Comparison_right_top_academics'),
                    
                    br(),
                    plotlyOutput("plot_Comparison_right_top_summary",height = 200),                
                    br(),
                    
                    uiOutput('selinput_Comparison_right_bottom_academics'),
                    
                    br(),
                    plotlyOutput("plot_Comparison_right_bottom_summary",height = 200),                
                    br(),
                    
                    plotlyOutput("plot_Comparison_right_comparison",height = 800)
                    
             ), #End column
      
             br(),
             br(),
             br(),
             br(),
             br()       
    ),
  
    ###############################
    #D) Keyword
    ############################### 
    
    tabPanel("Keyword analysis",icon = icon("file-word", lib = "font-awesome"),align = "center", #https://icons.getbootstrap.com/
             
             h3("Areas of knowledge - Keyword analysis"),
             
             br(),
             
             p("This section allows knowing the distribution in time of a specific keyword associated with an area of knowledge.",
             "We defined the areas according to the",a("IEEE Taxonomy Access", href = "https://www.ieee.org/publications/services/thesaurus-thank-you.html?"
                    ,target="_blank"),"and SCOPUS index keywords."),
             
             h4("Instructions"),
             
             tags$div(
               tags$p("1. Select the range of years to analyse."), 
               tags$p("2. Choose the type of publication and level of keywords to use."), 
               tags$p("3. Select the keyword to study.")
             ),
             
             br(),
             
             br(),
             
             h4("Data analisys"),
             
             selectInput("selinput_Keywords_main_publication", "Type of publication", choices = list("All","Article", 
                                                                                                       "Conference Paper")),
             
             selectInput("selinput_Keywords_main_lknowledge", "Area of the knowledge level", 
                         choices = list("IEEE Area - Level 1" = 1, "IEEE Area - Level 2" = 2,"IEEE Area - Level 3" = 3,
                                        "SCOPUS index keywords" = 4), selected = 1),
             
             uiOutput('selinput_Keywords_main_keywords'),
             
             plotlyOutput("plot_Keywords_main_keywords",height = 1200,width = 1000)
             
             
    ), #End tabpanel
    
    ###############################
    #E) Journals
    ############################### 
    tabPanel("Journal analysis",icon = icon("book", lib = "font-awesome"),align = "center", #https://icons.getbootstrap.com/
             
             h3("Areas of knowledge - Journal analysis"),
             
             br(),
             
             p("This section allows knowing where people are publishing."),
             
             h4("Instructions"),
             
             tags$div(
               tags$p("1. Select the range of years to analyse."), 
               tags$p("2. Choose the type of publication."), 
               tags$p("3. Select the university o academic to analyse")
             ),
             
             br(),
             
             br(),
             
             h4("Data analisys"),
             
             sliderInput("selinput_Journal_main_years", "Years", min = 2001, max = 2022, sep = "", value = c(2020,2022)),
             
             
             selectInput("selinput_Journal_main_publication", "Type of publication", choices = list("All","Article", 
                                                                                                     "Conference Paper")),
             
             
             #-------------------------------
             #E.1) Columna métricas Parte 1
             #-------------------------------
             column(5,align = "center", offset =1,
                    
                    
                    selectInput("selinput_Journal_left_university", "University 1", choices = list("All the universities" = 1, 
                                   "Universidad de Valparaíso (UV)" = 2, "Universidad de Concepción (UdeC)" = 3, 
                                    "Universidad de Santiago de Chile (USACH)" = 4), selected = 1),
                    
                    br(),
                    plotlyOutput("plot_Journal_left_summary",height = 200),                
                    br(),
                    
                    plotlyOutput("plot_Journal_left_universities",height = 1200),
                    
                    
             ),
             
             #-------------------------------
             #E.1) Columna métricas Parte 1
             #-------------------------------
             column(5,align = "center", offset =1,
                    
                    uiOutput('selinput_Journal_right_academics'),
                    
                    br(),
                    plotlyOutput("plot_Journal_right_summary",height = 200),                
                    br(),
                    
                    plotlyOutput("plot_Journal_right_academics",height = 1200),
                    
                    
             )

    ) #End tabpanel
    
    
    
  ),  #End tabset panel
  
  ) #End fluidRow
)