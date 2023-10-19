tab_indexes=tabPanel("Indexes",icon = icon("table"),align = "center",
  
  ###############################
  #A) Principal
  ###############################                       
  br(),
                         
  fluidRow(align = "center", offset = 0,
        
  h2("Performance and collaboration indexes"),
    
  br(),
  
  p("This section allows to visualise the performance and collaboration indexes associated with 
             each university and academics publications"),
  
  h4("Instructions"),
  
  tags$div(
    tags$p("1. Select the range of years to analyse."), 
    tags$p("2. Choose one or all the universities to study."), 
 
  ),
  
  br(),
  
  br(),
  
  h4("Data analisys"),
  
  sliderInput("slider_Indexes_main_years", "Years", min = 2001, max = 2022, sep = "", value = c(2020,2022)),
  
  br(),
  
  selectInput("slider_Indexes_main_universities", "University", choices = list("All the universities" = 1, 
                                                                          "Universidad de Valparaíso (UV)" = 2,
                                                                  "Universidad de Concepción (UdeC)" = 3, 
                                                                  "Universidad de Santiago de Chile (USACH)" = 4), selected = 1),      

  br(),
  
    ###############################
    #A.1) Columna métricas Parte 1
    ###############################  
  column(6,align = "left", offset =1,
                     
    h3("Performance indexes"),
                                         
    p("This section describes the performance indexes considered in this analysis:"),
                                         
    br(),
                                        
    tags$ol(
                
      tags$li(p(strong("Total publications"),("(\\(TP\\))."),"Total number of publications.")), 
                                           
      tags$li(p(strong("Total journal publications"),("(\\(TP_{j}\\))."),"Total number of journal publications.")),
                                           
      tags$li(p(strong("Total conference proceeding publications"),("(\\(TP_{p}\\))."),"Total number of conference 
              proceeding publications.")),
                                           
      tags$li(p(strong("Percentage of journal publications"),("( \\( \\%TP_{j} \\))."),("\\( \\frac{TP_j}{TP} \\times 100 \\)"))),
                                           
      tags$li(p(strong("Percentage  of conference proceeding publications"),("( \\( \\%TP_{p} \\))."),("\\( \\frac{TP_p}{TP} 
                                                                                                     \\times 100 \\)"))),
                                           
      tags$li(p(strong("Average publications by academics"),("(\\(AP\\))."),"Average number of manuscripts published by academic.")), 
                                           
      tags$li(p(strong("Average journal publications by academics"),("(\\(TP_{j}\\))."),
              "Average number of manuscripts published in journals by academics.")),
                                           
      tags$li(p(strong("Average conference proceeding publications by academics"),("(\\(TP_{p}\\)).")," 
              Average number of manuscripts published in conference proceedings by academics.")),  
                                           
      tags$li(p(strong("Average publications by year"),("(\\(APY\\))."),"Average number of manuscripts published by year.")), 
                                           
      tags$li(p(strong("Average journal publications by year"),("(\\(AP_{j}Y\\))."),"Average number of manuscripts 
              published in journals by year.")),
                                           
      tags$li(p(strong("Average conference proceeding publications by year"),("(\\(AP_{p}Y\\)).")," 
              Average number of manuscripts published in conference proceedings by year.")),                   
                                           
      tags$li(p(strong("Number of active years of publications"),("(\\(NAY\\))."),"Number of years that research 
               constituent record a publication.")),
                                           
      tags$li(p(strong("Productivity per active year of publication"),("( \\(PAY \\))."),("\\( \\frac{TP}{NAY} \\)"))),
                                           
      tags$li(p(strong("Total citations"),("(\\(TC\\))."),"Total number of citations.")),
                                           
      tags$li(p(strong("Average citations by publication"),("( \\(AC \\))."),("\\( \\frac{TC}{TP} \\)"))),
                                           
      tags$li(p(strong("Number of cited publications"),("(\\(NCP\\))."),"Number of cited publications.")),
                                           
      tags$li(p(strong("Proportion of cited publications"),("( \\(PCP \\))."),("\\( \\frac{NCP}{TP} \\)"))),
                                           
      tags$li(p(strong("Citations per cited publication"),("( \\(CPP \\))."),("\\( \\frac{TC}{NCP} \\)"))),
                                           
    ) # End listado
    
  ), # end column

  ###############################
  #A.2) Columna tablas
  ###############################         
  column(4,align = "right", offset =0,
         
         dataTableOutput("table_Indexes_performance")                
         
  ),
  
  ###############################
  #A.3) Columna métricas Parte 2
  ###############################  
  
  column(6,align = "left", offset =1,                 
    br(),br(),
                 
    h3("Collaboration indexes"),
                 
    p("This section describes the collaboration indexes considered in this analysis:"),
                   
    tags$ol(
      tags$li(p(strong("Number of academics"),("(\\(NAc\\)"),"). Total number of academics.")),
                 
      tags$li(p(strong("Number of contributing authors"),("(\\(NCA\\)"),"). Total number of different authors contributing to 
                publications.")),
                 
      tags$li(p(strong("Average contributing authors"),("(\\(ANCA\\)"),("). \\( \\frac{NCA}{TP} \\)"))),
  
      tags$li(p(strong("Co-authored publications"),("(\\(CA\\)"),"). Total number of co-authored publications.")),
                      
      tags$li(p(strong("Percentage of co-authored publications"),("\\( (\\%CA \\)"),
                                ("). \\( \\frac{CA}{TP} \\times 100 \\)"))),
      
      br(),
      br(),
                      
    ) # End listado
  ), # end column
           

  ###############################
  #A.4 Columna tablas
  ###############################         
  column(4,align = "right", offset =0,
         br(),br(),
         dataTableOutput("table_Indexes_collaboration")                
         
  ),
    
            
  ) #end fluidRow
                         
)
