
#===================================
#A - Preámbulo Carga de bibliotecas
#==================================

#Fuentes
source("www/SOURCES/UI/fuentes.R")

#Archivos para visualización
source("www/SOURCES/UI/encabezado.R")
source("www/SOURCES/UI/tab_indexes.R")
source("www/SOURCES/UI/tab_knowledge.R")
source("www/SOURCES/UI/tab_collaboration.R")


#Encoding 
options(encoding="UTF-8")
  

fluidPage(theme = shinytheme("lumen"),align = "center", #lumen, flatly, spacelab, united
          #Ecuaciones
          withMathJax(),
          
           tags$head(
             includeHTML("google-analytics.html"),
             includeScript("google-analytics.js"),
             tags$style('
               ul.nav-pills{
                 display: flex;
                 justify-content: center;
               }')
             
             
          ),
          

          
          #Panel principal
          mainPanel(width = 12,align="center",
                    tags$div(id="DivEncabezado",encabezado),
                    tabsetPanel(type = "tabs",id="tab_principal",
                                tab_indexes,
                                tab_knowledge,
                                tab_collaboration
                    ) #endtabsetPanel
          ) #End mainPanel
          
) #End fuildPage