tab_collaboration=tabPanel("Collaboration",icon = icon("globe",lib = "font-awesome"),align = "center",
                       
                       
                       ###############################
                       #- Principal
                       ###############################                       
                       br(),
                       
                       fluidRow(align = "center", offset = 0,
                                
                                h2("Collaboration analysis"),
                                
                                br(),
                                
                                tabsetPanel(type="pills",
                                            
                                #-----------------------------
                                # A) Cluster analisys
                                #-----------------------------
                                            
                                tabPanel("Cluster analisys",icon = icon("tree",lib = "font-awesome"),align = "center",            
                                            
                                         
                                         h3("Collaboration analysis - Cluster analysis"),
                                         
                                         br(),
                                         
                                         p("This section allows visualising the relationships between universities and academics
                                         using a hierarchical clustering technique (average) based on the areas of knowledge of 
                                         their publications. 
                                           We defined the areas according to the", a("IEEE Taxonomy Access",
                                         href = "https://www.ieee.org/publications/services/thesaurus-thank-you.html?",
                                         target="_blank"),"and SCOPUS index keywords."),
                      
                      
                                h4("Instructions"),
                      
                                tags$div(
                                  tags$p("1. Select the range of years to analyse. Visualization may take time."), 
                                  tags$p("2. Choose the type of publication and level of keywords to use."), 
                                  tags$p("3. Select the university to be studied."),
                                  tags$p("4. Select the distance to be applied.")
                                  
                                ),
                      
                                br(),
                                
                                br(),
                                
                                h4("Data analisys"),
                                
                                column(5, offset = 1,
                                
                                sliderInput("slider_Cluster_main_years", "Years", min = 2001, max = 2022, sep = "", 
                                            value = c(2020,2022)),
                                
                                selectInput("selinput_Cluster_main_publication", "Type of publication", 
                                            choices = list("All","Article", "Conference Paper")),

                                selectInput("selinput_Cluster_main_lknowledge", "Area of the knowledge level", 
                                            choices = list("IEEE Area - Level 1" = 1, "IEEE Area - Level 2" = 2,
                                                           "IEEE Area - Level 3" = 3,
                                                           "SCOPUS index keywords" = 4), selected = 1),      
                                
                                selectInput("selinput_Cluster_main_university", "University", 
                                            choices = list("All the universities" = 1,
                                                           "Universidad de Valparaíso (UV)" = 2,
                                                           "Universidad de Concepción (UdeC)" = 3, 
                                                           "Universidad de Santiago de Chile (USACH)" = 4), selected = 1),
                                
                                selectInput("selinput_Cluster_main_distance", "Type of distance", 
                                            choices = list("canberra","correlation","cosine","dice","edice","ejaccard","euclidean",
                                                           "jaccard","manhattan","maximum","minkowski"), 
                                            selected = "correlation"),
                                
                                br(),
                                
                                br(),
                                
                                plotlyOutput("plot_Cluster_main_summary")

                                
                                ),
                                
                                
                                column(5, offset = 1,
                                
                                       
                                       dendroNetworkOutput("graph_Cluster_right_universities"),
                                       
                                       #plotlyOutput("plot_Cluster_left_heatmap_universities"),
                                       
                                       dendroNetworkOutput("graph_Cluster_right_academics"),
                                       
                                       #plotlyOutput("plot_Cluster_left_heatmap_academic")
                                       
                                
                                )

                                ), #Fin tabPanel    
                                
                                #-----------------------------
                                # B) Cluster analisys
                                #-----------------------------
                                
                                tabPanel("Author analisys",icon = icon("person",lib = "font-awesome"),align = "center",
                                         
                                         
                                         h3("Collaboration analysis - Author analysis"),
                                         
                                         br(),
                                         
                                         p("This section allows visualising the relationships between authors using frequency."),
                                         
                                         h4("Instructions"),
                                         
                                         tags$div(
                                           tags$p("1. Select the range of years to analyse. Visualization may take time."), 
                                           tags$p("2. Choose the type of publication and level of keywords to use."), 
                                           tags$p("3. Select the university to be studied."),
                                           tags$p("4. Select the type of affiliation to be applied.")
                                         ),
                                         
                                         br(),
                                         
                                         br(),
                                         
                                         h4("Data analisys"),
                                         
                                         column(5, offset = 1,
                                                
                                                sliderInput("slider_Author_main_years", "Years", min = 2001, max = 2022, sep = "", 
                                                            value = c(2020,2022)),
                                                
                                                selectInput("selinput_Author_main_publication", "Type of publication", 
                                                            choices = list("All","Article", "Conference Paper")),
                                                
                                                
                                                selectInput("selinput_Author_main_university", "University", 
                                                            choices = list("All the universities" = 1,
                                                                           "Universidad de Valparaíso (UV)" = 2,
                                                                           "Universidad de Concepción (UdeC)" = 3, 
                                                                           "Universidad de Santiago de Chile (USACH)" = 4), selected = 1),
                                                
                                                selectInput("selinput_Author_main_type", "Type of affiliation", 
                                                            choices=list("UV-UdeC-USACH","All authors"), 
                                                            selected = "UV-UdeC-USACH"),
                                                
                                                br(),
                                                
                                                br(),
                                                
                                                plotlyOutput("plot_Author_main_summary")
                                                
                
                                ), #End columns
                                
                                
                                column(5, offset = 1,
                                       
                                       
                                                plotlyOutput("plot_Author_right_authors", height = "1200")
                                       
                                       
                                )
                    
                  ) #End tabpanel
                  
        ) # Fin tabsetPanel             
  
                                
  ) #Fin fluidRow
                                
                                
) #Fin tabPanel