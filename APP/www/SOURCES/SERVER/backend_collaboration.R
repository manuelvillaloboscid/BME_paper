backend_collaboration = function(input,output,session,data_db,data_main,data_thesaurus,data_keywords)
{
  
  #================================================
  # A) Vista 1 - Cluster
  #================================================
  
  #---------------------------
  #A.1)  plot_Cluster_main_summary - Hace resumen de publicaciones x universidad
  #---------------------------
  
  output$plot_Cluster_main_summary = renderPlotly({
    
    #Actualizar update input
    updateSliderInput(session, inputId ="slider_Cluster_main_years",min=min(data_db$Año),max=max(data_db$Año))
    data_db_tmp = data_db
    
    #Seleccionar por publicación
    publication = input$selinput_Cluster_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
     
    university=input$selinput_Cluster_main_university
    if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
    if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
    if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}
     
    ini_year = input$slider_Cluster_main_years[1]     #Mínimo de los años ingresados
    last_year = input$slider_Cluster_main_years[2]
     
    years=data.frame(years=min(data_db$Año):max(data_db$Año))
    years_tmp=data_db_tmp %>% group_by(Año) %>%summarise(n = n())
    frequency=merge(years,years_tmp,by.x="years",by.y="Año",all.x=T)
    frequency$n[which(is.na(frequency$n))]=0
    frequency$time = "Other years"
    frequency$time[which((frequency$years>=ini_year) &  (frequency$years<=last_year))] = paste(ini_year,"-",last_year) 
    
    plot_app=ggplot(data=frequency, aes(x=years, y=n)) +   theme_classic() + ylab("Publications") + xlab("Years") + 
       geom_bar(stat="identity", aes(fill = time)) + scale_x_continuous(breaks=seq(min(data_db$Año),max(data_db$Año),1)) + 
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),line = element_blank(), legend.position='bottom') +
       guides(fill=guide_legend(title="")) + scale_fill_manual(values=c("lightblue","seashell2"))
     
    ggplotly(plot_app) %>% layout(legend = list(orientation = "h", x = 0.35, y =-1), height = 200)
    
  }) #End renderUI
  
  #---------------------------
  #A.2)  plot_Cluster_right_universities - Hace cluster de publicaciones x universidad
  #---------------------------
  
  output$graph_Cluster_right_universities = renderDendroNetwork({
    
    #Filtro por periodo inicial
    ini_year = as.numeric(input$slider_Cluster_main_years[1])
    last_year = as.numeric(input$slider_Cluster_main_years[2])
    
    data_db_tmp = data_db  %>% filter(Año %in% (ini_year:last_year))
    
    
    #Seleccionar por publicación
    publication = input$selinput_Cluster_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}

    #Define nivel de palabras claves
    level_knowledge=input$selinput_Cluster_main_lknowledge
    if (level_knowledge==1) {thesaurus = unique(data_thesaurus$Area2)}
    if (level_knowledge==2) {thesaurus = unique(data_thesaurus$Area3)}
    if (level_knowledge==3) {thesaurus = unique(data_thesaurus$Area4)}
    if (level_knowledge==4) {thesaurus = unique(data_keywords$Keywords)}
    
    
    bandera = 0
    if ((nrow(data_db_tmp)==0))
    { bandera = 1 }
    
    if (bandera!=1)
    {
      #Arregla los datos para hacer la prueba
      data_db_tmp$Index.keywords = stri_replace_all_regex(data_db_tmp$Index.keywords, "[\\p{p}\\p{S}]", " ")
      if (level_knowledge!=4){corpus_text = corpus(data_db_tmp, text_field = "Abstract")} else
      {corpus_text = corpus(data_db_tmp, text_field = "Index.keywords")}
      
      token_text = tokens(corpus_text,remove_punct = T,remove_symbols = T, remove_numbers = T, remove_url = T,
                          remove_separators = T)
      token_text = tokens_tolower(token_text)
      kwick_text = kwic(token_text, pattern = phrase(thesaurus))
      
      #Si no hay intersecciones con el diccionario
      if (length(kwick_text$docname)==0){bandera=1}
      
      if (bandera==0){
        kwick_text$from=kwick_text$from=kwick_text$to=kwick_text$pre=kwick_text$post=kwick_text$pattern=NULL
        corpus_text =summary(corpus_text,n=length(kwick_text$docname))
        kwick_text=merge(kwick_text,corpus_text[,c("Text","Año")],by.x="docname",by.y = "Text")
        kwick_text=merge(kwick_text,corpus_text[,c("Text","Universidad")],by.x="docname",by.y = "Text")
        kwick_text=merge(kwick_text,corpus_text[,c("Text","Nombre")],by.x="docname",by.y = "Text")
      }
    }
    
    
    if (bandera == 1){
      
      tstat_key = NULL
      tstat_key = data.frame(feature=c("There are no enough data to compare two periods",
                                       "It was no possible to realise the comparison"),
                             score=rbind(0,0))

    } else
      
    {
      #Arregla los datos para hacer la prueba
      if (level_knowledge!=4){corpus_text = corpus(data_db_tmp, text_field = "Abstract")} else
      {corpus_text = corpus(data_db_tmp, text_field = "Index.keywords")}
      token_text = tokens(corpus_text,remove_punct = T,remove_symbols = T, remove_numbers = T, remove_url = T,remove_separators = T)
      token_text = tokens_tolower(token_text)
      kwick_text = kwic(token_text, pattern = phrase(thesaurus))
      kwick_text$from=kwick_text$from=kwick_text$to=kwick_text$pre=kwick_text$post=kwick_text$pattern=NULL
      corpus_text =summary(corpus_text,n=length(kwick_text$docname))
      kwick_text=merge(kwick_text,corpus_text[,c("Text","Año")],by.x="docname",by.y = "Text")
      kwick_text=merge(kwick_text,corpus_text[,c("Text","Universidad")],by.x="docname",by.y = "Text")
      kwick_text=merge(kwick_text,corpus_text[,c("Text","Nombre")],by.x="docname",by.y = "Text")
      corpus_text = corpus(kwick_text,text_field="keyword")
      token_text = tokens(corpus_text)
      token_text = tokens_compound(token_text, pattern = phrase(thesaurus))
      
      df_text = dfm(token_text)
      dfmat_users = dfm_group(df_text, groups = Universidad)
      
      if (length(unique(kwick_text$Universidad))>1)
      {

      #Agrego nuevas medidas de distancia
      sel_distance=input$selinput_Cluster_main_distance
        if (sel_distance=="correlation" | sel_distance=="cosine" | sel_distance=="jaccard" | sel_distance=="ejaccard" |
            sel_distance=="dice" | sel_distance=="edice" | sel_distance=="hamann")
          
        {
          tstat_dist = 1 - as.dist(textstat_simil(dfmat_users,method=sel_distance))
          
        } else
        {tstat_dist = as.dist(textstat_dist(dfmat_users,method=sel_distance))}  
        
      user_clust = hclust(tstat_dist,method = "average")
      
      # #Crea heatmap
      # output$plot_Cluster_left_heatmap_universities = renderPlotly({
      #   
      #   
      #   dfm_df=convert(dfmat_users,to="data.frame")
      #   dfm_df = dfm_df %>% gather(key="Keywords",value="value",-doc_id)
      #   names(dfm_df) = c("Universities","Keywords","Frequency")
      #   
      #   dfm_df$Universities = factor(dfm_df$Universities, levels = unique(dfm_df$Universities[rev(user_clust$order)]))
      #   
      #   
      #   plot_app=ggplot(dfm_df, aes(Keywords,Universities, fill= Frequency)) + 
      #     geom_tile() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      #     scale_fill_gradient(low="black", high="aliceblue")
      #   
      #   ggplotly(plot_app) %>% layout(height = 400)
      #   
      # })
      # 
      colores=c("blue","pink","green")
      colores = colores[1:length(unique(user_clust$labels))]
      dendroNetwork(user_clust, linkColour  = "black" ,zoom = T, treeOrientation= "horizontal", 
                    textColour   = colores, fontSize = 20,textOpacity = 1)
    
      } 
    } # End else
    
  }) #End render
  
  
  
  #---------------------------
  #A.3)  plot_Cluster_right_academics - Hace cluster de publicaciones x académico
  #---------------------------
  
  output$graph_Cluster_right_academics = renderDendroNetwork({
    
    #Filtro por periodo inicial
    ini_year = as.numeric(input$slider_Cluster_main_years[1])
    last_year = as.numeric(input$slider_Cluster_main_years[2])

    data_db_tmp = data_db  %>% filter(Año %in% (ini_year:last_year))

    #Seleccionar por publicación
    publication = input$selinput_Cluster_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}

    #Filtro de universidad
    university=input$selinput_Cluster_main_university

    if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
    if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
    if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}


    #Define nivel de palabras claves
    level_knowledge=input$selinput_Cluster_main_lknowledge
    if (level_knowledge==1) {thesaurus = unique(data_thesaurus$Area2)}
    if (level_knowledge==2) {thesaurus = unique(data_thesaurus$Area3)}
    if (level_knowledge==3) {thesaurus = unique(data_thesaurus$Area4)}
    if (level_knowledge==4) {thesaurus = unique(data_keywords$Keywords)}


    bandera = 0
    if ((nrow(data_db_tmp)==0))
    { bandera = 1 }

    if (bandera!=1)
    {
      #Arregla los datos para hacer la prueba
      #Arregla los datos para hacer la prueba
      data_db_tmp$Index.keywords = stri_replace_all_regex(data_db_tmp$Index.keywords, "[\\p{p}\\p{S}]", " ")
      if (level_knowledge!=4){corpus_text = corpus(data_db_tmp, text_field = "Abstract")} else
      {corpus_text = corpus(data_db_tmp, text_field = "Index.keywords")}
      
      token_text = tokens(corpus_text,remove_punct = T,remove_symbols = T, remove_numbers = T, remove_url = T,remove_separators = T)
      token_text = tokens_tolower(token_text)
      kwick_text = kwic(token_text, pattern = phrase(thesaurus))

      #Si no hay intersecciones con el diccionario
      if (length(kwick_text$docname)==0){bandera=1}

      if (bandera==0){
        kwick_text$from=kwick_text$from=kwick_text$to=kwick_text$pre=kwick_text$post=kwick_text$pattern=NULL
        corpus_text =summary(corpus_text,n=length(kwick_text$docname))
        kwick_text=merge(kwick_text,corpus_text[,c("Text","Año")],by.x="docname",by.y = "Text")
        kwick_text=merge(kwick_text,corpus_text[,c("Text","Universidad")],by.x="docname",by.y = "Text")
        kwick_text=merge(kwick_text,corpus_text[,c("Text","Nombre")],by.x="docname",by.y = "Text")
      }
    }


    if (bandera == 1){

      tstat_key = NULL
      tstat_key = data.frame(feature=c("There are no enough data to compare two periods",
                                       "It was no possible to realise the comparison"),
                             score=rbind(0,0))

    } else

    {
      #Arregla los datos para hacer la prueba
      data_db_tmp$Index.keywords = stri_replace_all_regex(data_db_tmp$Index.keywords, "[\\p{p}\\p{S}]", " ")
      if (level_knowledge!=4){corpus_text = corpus(data_db_tmp, text_field = "Abstract")} else
      {corpus_text = corpus(data_db_tmp, text_field = "Index.keywords")}
      
      token_text = tokens(corpus_text,remove_punct = T,remove_symbols = T, remove_numbers = T, remove_url = T,remove_separators = T)
      token_text = tokens_tolower(token_text)
      kwick_text = kwic(token_text, pattern = phrase(thesaurus))
      kwick_text$from=kwick_text$from=kwick_text$to=kwick_text$pre=kwick_text$post=kwick_text$pattern=NULL
      corpus_text =summary(corpus_text,n=length(kwick_text$docname))
      kwick_text=merge(kwick_text,corpus_text[,c("Text","Año")],by.x="docname",by.y = "Text")
      kwick_text=merge(kwick_text,corpus_text[,c("Text","Universidad")],by.x="docname",by.y = "Text")
      kwick_text=merge(kwick_text,corpus_text[,c("Text","Nombre")],by.x="docname",by.y = "Text")
      corpus_text = corpus(kwick_text,text_field="keyword")
      token_text = tokens(corpus_text)
      token_text = tokens_compound(token_text, pattern = phrase(thesaurus))

      df_text = dfm(token_text)
      dfmat_users = dfm_group(df_text, groups = Nombre)

      if (length(unique(kwick_text$Nombre))>1)
      {

        #Agrego nuevas medidas de distancia
        sel_distance=input$selinput_Cluster_main_distance
        if (sel_distance=="correlation" | sel_distance=="cosine" | sel_distance=="jaccard" | sel_distance=="ejaccard" |
            sel_distance=="dice" | sel_distance=="edice" | sel_distance=="hamann")

        {
          tstat_dist = 1 - as.dist(textstat_simil(dfmat_users,method=sel_distance))

        } else
        {
          tstat_dist = as.dist(textstat_dist(dfmat_users,method=sel_distance))}

          nmax= min(nrow(as.matrix(tstat_dist)),8) - 1
        
        
          tstat_dist_clust=NbClust(data=NULL, diss = tstat_dist, distance=NULL, index = "silhouette",method="average",
                                 max.nc = nmax)
        
          
          user_clust = hclust(tstat_dist,method = "average")
          
                
          clust_colors = brewer.pal(length(unique(tstat_dist_clust$Best.partition)),"Dark2")
        
          
          # #Crea heatmap
          # output$plot_Cluster_left_heatmap_academic = renderPlotly({
          #   
          #   
          #   dfm_df=convert(dfmat_users,to="data.frame")
          #   dfm_df = dfm_df %>% gather(key="Keywords",value="value",-doc_id)
          #   names(dfm_df) = c("Authors","Keywords","Frequency")
          #   
          #   dfm_df$Authors = factor(dfm_df$Authors, levels = unique(dfm_df$Authors[rev(user_clust$order)]))
          #   
          #   
          #   plot_app=ggplot(dfm_df, aes(Keywords,Authors, fill= Frequency)) + 
          #     geom_tile() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
          #     scale_fill_gradient(low="black", high="aliceblue")
          #   
          #   ggplotly(plot_app) %>% layout(height = 1800)
          #   
          # })
          
          dendroNetwork(user_clust, linkColour  = "black" ,zoom = T, treeOrientation= "horizontal",
                        fontSize = 12,textOpacity = 1,height = 1200,textColour = 
                          clust_colors[cutree(user_clust,tstat_dist_clust$Best.nc[[1]])])

      }
    } # End else
    
  }) #End Cluster
  
  
  #================================================
  # B) Vista 2 - Authors
  #================================================
  
  #---------------------------
  #B.1)  plot_Author_main_summary - Hace resumen de publicaciones x universidad
  #---------------------------
  
  output$plot_Author_main_summary = renderPlotly({
    
    #Actualizar update input
    updateSliderInput(session, inputId ="slider_Author_main_years",min=min(data_db$Año),max=max(data_db$Año))
    data_db_tmp = data_db
    
    #Seleccionar por publicación
    publication = input$selinput_Author_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    university=input$selinput_Author_main_university
    if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
    if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
    if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}
    
    ini_year = input$slider_Author_main_years[1]     #Mínimo de los años ingresados
    last_year = input$slider_Author_main_years[2]
    
    years=data.frame(years=min(data_db$Año):max(data_db$Año))
    years_tmp=data_db_tmp %>% group_by(Año) %>%summarise(n = n())
    frequency=merge(years,years_tmp,by.x="years",by.y="Año",all.x=T)
    frequency$n[which(is.na(frequency$n))]=0
    frequency$time = "Other years"
    frequency$time[which((frequency$years>=ini_year) &  (frequency$years<=last_year))] = paste(ini_year,"-",last_year) 
    
    plot_app=ggplot(data=frequency, aes(x=years, y=n)) +   theme_classic() + ylab("Publications") + xlab("Years") + 
      geom_bar(stat="identity", aes(fill = time)) + scale_x_continuous(breaks=seq(min(data_db$Año),max(data_db$Año),1)) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),line = element_blank(), legend.position='bottom') +
      guides(fill=guide_legend(title="")) + scale_fill_manual(values=c("lightblue","seashell2"))
    
    ggplotly(plot_app) %>% layout(showlegend=T, legend = list(orientation = "h", x = 0.35, y =-1), height = 200)
    
  }) #End renderUI
  
  
  
  #---------------------------
  #B.2)  plot_Cluster_right_academics - Hace cluster de publicaciones x académico
  #---------------------------
  
  output$plot_Author_right_authors = renderPlotly({
    
    #Filtro por periodo inicial
    ini_year = as.numeric(input$slider_Author_main_years[1])
    last_year = as.numeric(input$slider_Author_main_years[2])
    
    data_db_tmp = data_db  %>% filter(Año %in% (ini_year:last_year))
    data_main = data_main %>% mutate(Codigo.Scopus = strsplit(as.character(Codigo.Scopus),"-"))%>%
      unnest(Codigo.Scopus)
    
    
    #Seleccionar por publicación
    publication = input$selinput_Author_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    #Filtro de universidad
    university=input$selinput_Author_main_university
    
    if (university==2) {
    data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")
    data_main = data_main  %>% filter(Universidad == "UV")
    }
    
    if (university==3) {
    data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")
    data_main = data_main  %>% filter(Universidad == "UdeC")
    }
    if (university==4) {
    data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")
    data_main = data_main  %>% filter(Universidad == "USACH")
    }
    
    diccionario = unlist(strsplit(as.character(data_main$Codigo.Scopus),"-"))
    
    
    bandera = 0
    if ((nrow(data_db_tmp)==0))
    { bandera = 1 }
    
    
    
    if (bandera == 1){
      
      tstat_key = NULL
      tstat_key = data.frame(feature=c("There are no enough data to compare two periods",
                                       "It was no possible to realise the comparison"),
                             score=rbind(0,0))
      
    } else
      
    {
      authors_tmp=NULL
      i=1
      for (a in 1:nrow(data_db_tmp))
      {
        idx = order(unlist(strsplit(as.character(data_db_tmp$Author.s..ID[[a]]),"-")))
        cod_autores = unlist(strsplit(as.character(data_db_tmp$Author.s..ID[[a]]),"-"))[idx]
        nom_autores=  stri_trans_general(unlist(strsplit(as.character(data_db_tmp$Autores[[a]]),","))[idx], id = "Latin-ASCII")
        
        if (input$selinput_Author_main_type=="All authors") 
        {idx = 1:length(nom_autores)} else
        {idx = which(!is.na(match(cod_autores,diccionario)))}
        
        if (length(idx)>1) 
        { 
          if (input$selinput_Author_main_type == "All authors")
          {authors_tmp[i]= do.call(paste, c(as.list(str_trim(nom_autores[idx])), sep = "\n"))} else
          {authors_tmp[i]=do.call(paste,(c(as.list(str_trim(
            
            paste(str_trim((data_main %>% 
                              filter(Codigo.Scopus %in% cod_autores[idx]))$Nombre),str_trim((data_main %>% 
                              filter(Codigo.Scopus %in% cod_autores[idx]))$Universidad),sep="-")
            
          )),sep="\n")))} 
          i = i + 1
        }
      }
      
      
      if (length(authors_tmp)<2)
      {authors_tmp = rep("There are no contibutions between authors during the period.",2)}

      names_tmp = names(sort(table(authors_tmp),decreasing = T))
      freq_tmp = as.numeric(sort(table(authors_tmp),decreasing = T))
      
      results=data.frame(authors= names_tmp, Freq=freq_tmp) %>% top_n(10)
      if (nrow(results)>20) {results=results[1:20,]}
      if (nrow(results)>1)
      {results$authors=factor(results$authors, levels = unique(results$authors[order(results$Freq,decreasing=F)]))}
      
      plot_app=ggplot(data=results, aes(x=Freq, y=authors)) +   theme_classic() + ylab("Publications") + xlab("n") +
        geom_bar(stat="identity",fill="pink") + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),line = element_blank(), legend.position='bottom') +
        theme(line = element_blank(), legend.position='bottom') +
        guides(fill=guide_legend(title=""))
      
      
      ggplotly(plot_app)
        
      
    } # End else
    
  }) #End Cluster
  
} #En backend collaboration

