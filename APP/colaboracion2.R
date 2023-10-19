
#Librerías
library(igraph)
library(networkD3)

#Lectura de datos
datos_autores = read.csv("www/DB/BD_Biomedica - BD.csv",sep=";",header=T)
autores = data.frame(datos_autores$Nombre, datos_autores$Autor.ID,datos_autores$Autores,datos_autores$Universidad)

#Arreglo nombre columnas
colnames(autores) = c("nombre", "ID","autores","Universidad")
#Separación 
autores <- autores %>%
  mutate(autores = strsplit(as.character(autores),","))%>%
  unnest(autores)

autores = autores %>% filter(Universidad == "USACH")

#Grafico
plot <- simpleNetwork(autores, height="800px", width="800px",
                      Source = 1,                 # column number of source
                      Target = 3,                 # column number of target
                      linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                      charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                      fontSize = 14,               # size of the node names
                      fontFamily = "serif",       # font og node names
                      linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                      nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
                      opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                      zoom = T                    # Can you zoom on the figure?
)
plot