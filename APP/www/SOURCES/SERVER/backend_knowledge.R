backend_knowledge = function(input,output,session,data_db,data_main,data_thesaurus,data_keywords)
{

#================================================
# A) Vista 1 - Frecuencia
#================================================
  
  #---------------------------
  #A.1) Render - plot_Frequency_left_summary  - Hace resumen de publicaciones x universidad
  #---------------------------
  
  
  output$plot_Frequency_left_summary = renderPlotly({
    
    #Actualizar update input
    updateSliderInput(session, inputId ="slider_Frequency_main_years",min=min(data_db$Año),max=max(data_db$Año))
    data_db_tmp = data_db
    
    #Seleccionar por publicación
    publication = input$selinput_Frequency_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    university=input$selinput_Frequency_left_university
    if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
    if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
    if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}
    
    ini_year = input$slider_Frequency_main_years[1]     #Mínimo de los años ingresados
    last_year = input$slider_Frequency_main_years[2]
    
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
    
    ggplotly(plot_app) %>% layout(legend = list(orientation = "h", x = 0.35, y =-1))
    
  }) #End renderUI


  #---------------------------
  #A.2) plot_Frequency_left_wordcloud - Hace nube de palabras x universidad
  #---------------------------

  output$plot_Frequency_left_wordcloud = renderPlot({


    #Filtro por años
    ini_year = as.numeric(input$slider_Frequency_main_years[1])
    last_year = as.numeric(input$slider_Frequency_main_years[2])
    data_db_tmp = data_db  %>% filter(Año %in% (ini_year:last_year))

    #Seleccionar por publicación
    publication = input$selinput_Frequency_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    #Filtro por universidad
    university=input$selinput_Frequency_left_university
    if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
    if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
    if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}



    #Define nivel de palabras claves
    level_knowledge=input$selinput_Frequency_main_lknowledge
    if (level_knowledge==1) {thesaurus = unique(data_thesaurus$Area2)}
    if (level_knowledge==2) {thesaurus = unique(data_thesaurus$Area3)}
    if (level_knowledge==3) {thesaurus = unique(data_thesaurus$Area4)}
    if (level_knowledge==4) {thesaurus = unique(data_keywords$Keywords)}


    #Extrae elementos y crea wordcloud
    if (nrow(data_db_tmp)<2) {text_analysis = corpus(c(rep("not enough data",3),
                                                       rep("empty",3)))} else
         {data_db_tmp$Index.keywords = stri_replace_all_regex(data_db_tmp$Index.keywords, "[\\p{p}\\p{S}]", " ")
         if (level_knowledge!=4){text_analysis = corpus(data_db_tmp, text_field = "Abstract")} else
         {text_analysis = corpus(data_db_tmp, text_field = "Index.keywords")}}                   
    
    text_analysis = tokens(text_analysis,remove_punct = T,remove_symbols = T, remove_numbers = T, remove_url = T,
                           remove_separators = T)
    text_analysis=tokens_tolower(text_analysis)
    text_analysis = kwic(text_analysis, pattern = phrase(c(thesaurus,"not enough data", "empty")))
    text_analysis = data.frame(words=text_analysis$keyword)

    text_analysis=text_analysis %>%
       count(words, sort=T)%>%
       top_n(20,n)


    set.seed(1)
    text_analysis$angle = sample(c(90,0),nrow(text_analysis),replace=T,prob = c(0.6,0.4))
    ggplot(text_analysis, aes(label = words,size = n,angle=angle,color = factor(sample.int(10, nrow(text_analysis), 
                                                                                           replace = TRUE)))) +
      geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,grid_size = 25, eccentricity = 1) +
      scale_size_area(max_size = 15) +  theme_minimal() +
      scale_color_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(32))

  }) #End renderWordcoud


  #---------------------------
  #A.3) plot_Frequency_left_treemap - Hace treemap x universidad
  #---------------------------


  output$plot_Frequency_left_treemap = renderPlotly({

    #Filtro por años
    ini_year = as.numeric(input$slider_Frequency_main_years[1])
    last_year = as.numeric(input$slider_Frequency_main_years[2])
    data_db_tmp = data_db  %>% filter(Año %in% (ini_year:last_year))

    #Seleccionar por publicación
    publication = input$selinput_Frequency_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    
    #Filtro por universidad
    university=input$selinput_Frequency_left_university
    if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
    if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
    if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}



    #Define nivel de palabras claves
    level_knowledge=input$selinput_Frequency_main_lknowledge
    if (level_knowledge==1) {thesaurus = unique(data_thesaurus$Area2)}
    if (level_knowledge==2) {thesaurus = unique(data_thesaurus$Area3)}
    if (level_knowledge==3) {thesaurus = unique(data_thesaurus$Area4)}
    if (level_knowledge==4) {thesaurus = unique(data_keywords$Keywords)}


    #Extrae elementos y crea treemap
    if (nrow(data_db_tmp)<2) {text_analysis = corpus(c(rep("not enough data",3),
                                                       rep("empty",3)))} else
                                                       {data_db_tmp$Index.keywords = stri_replace_all_regex(data_db_tmp$Index.keywords, "[\\p{p}\\p{S}]", " ")
                                                       if (level_knowledge!=4){text_analysis = corpus(data_db_tmp, text_field = "Abstract")} else
                                                       {text_analysis = corpus(data_db_tmp, text_field = "Index.keywords")}} 
    text_analysis = tokens(text_analysis,remove_punct = T,remove_symbols = T, remove_numbers = T, remove_url = T,
                           remove_separators = T)
    text_analysis=tokens_tolower(text_analysis)
    text_analysis = kwic(text_analysis, pattern = phrase(c(thesaurus,"not enough data", "empty")))
    text_analysis = data.frame(words=text_analysis$keyword)

    text_analysis= text_analysis %>%count(words, sort=T)%>%top_n(20,n)

    plot_ly(
      text_analysis,
      labels  = ~ words,
      parents = NA,
      values = ~ n,
      type = 'treemap',
      #marker=list(colorscale='Picnic'),
      hovertemplate = "Ingredient: %{label}<br>Count: %{value}<extra></extra>"
    )

  }) #End Renderplot

  #---------------------------
  #A.4)  Render - selinput_Frequency_left_university - Filtra lista de académicos(as)
  #---------------------------

  observeEvent(input$selinput_Frequency_left_university, {
    output$selinput_Frequency_right_academics <- renderUI({

      data_db_tmp = data_db

      university=input$selinput_Frequency_left_university
      if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
      if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
      if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}

      selectInput("d_selinput_Frequency_right_academics",label="Academic", choices = sort(unique(data_db_tmp$Nombre)),
                  selected = 1,width = "40%")
    }) #End renderUI
  })

  
   
  #---------------------------
  #A.5) plot_Frequency_right_summary - Hace resumen x académico(a)
  #---------------------------

  output$plot_Frequency_right_summary = renderPlotly({

    data_db_tmp = data_db

    #Seleccionar por publicación
    publication = input$selinput_Frequency_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    #Filtro por academico
    academic=input$d_selinput_Frequency_right_academics
    if (is.null(academic)){academic="Aqueveque Navarro Pablo Esteban"}
    data_db_tmp = data_db_tmp  %>% filter(Nombre==academic)

    ini_year = input$slider_Frequency_main_years[1]     #Mínimo de los años ingresados
    last_year = input$slider_Frequency_main_years[2]

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


    ggplotly(plot_app) %>% layout(legend = list(orientation = "h", x = 0.35, y =-1))

  }) #End renderUI

  #---------------------------
  #A.6) plot_Frequency_right_wordcloud - Hace nube de palabras x académico(a)
  #---------------------------

  output$plot_Frequency_right_wordcloud = renderPlot({


    #Filtro por años
    ini_year = as.numeric(input$slider_Frequency_main_years[1])
    last_year = as.numeric(input$slider_Frequency_main_years[2])
    data_db_tmp = data_db  %>% filter(Año %in% (ini_year:last_year))

    #Seleccionar por publicación
    publication = input$selinput_Frequency_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    #Filtro por académico
    academic=input$d_selinput_Frequency_right_academics
    if (is.null(academic)){academic="Aqueveque Navarro Pablo Esteban"}
    data_db_tmp = data_db_tmp  %>% filter(Nombre==academic)

    #Define nivel de palabras claves
    level_knowledge=input$selinput_Frequency_main_lknowledge
    if (level_knowledge==1) {thesaurus = unique(data_thesaurus$Area2)}
    if (level_knowledge==2) {thesaurus = unique(data_thesaurus$Area3)}
    if (level_knowledge==3) {thesaurus = unique(data_thesaurus$Area4)}
    if (level_knowledge==4) {thesaurus = unique(data_keywords$Keywords)}

    #Extrae elementos y crea wordcloud
    if (nrow(data_db_tmp)<2) {text_analysis = corpus(c(rep("not enough data",3),
                                                       rep("empty",3)))} else
                                                       {data_db_tmp$Index.keywords = stri_replace_all_regex(data_db_tmp$Index.keywords, "[\\p{p}\\p{S}]", " ")
                                                       if (level_knowledge!=4){text_analysis = corpus(data_db_tmp, text_field = "Abstract")} else
                                                       {text_analysis = corpus(data_db_tmp, text_field = "Index.keywords")}} 
    text_analysis = tokens(text_analysis,remove_punct = T,remove_symbols = T, remove_numbers = T, remove_url = T,remove_separators = T)
    text_analysis=tokens_tolower(text_analysis)
    text_analysis = kwic(text_analysis, pattern = phrase(c(thesaurus,"not enough data", "empty")))
    text_analysis = data.frame(words=text_analysis$keyword)

    text_analysis=text_analysis %>%
      count(words, sort=T)%>%
      top_n(20,n)


    set.seed(1)
    text_analysis$angle = sample(c(90,0),nrow(text_analysis),replace=T,prob = c(0.6,0.4))
    ggplot(text_analysis, aes(label = words,size = n,angle=angle,color = factor(sample.int(10, nrow(text_analysis), replace = TRUE)))) +
      geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,grid_size = 25, eccentricity = 1) +
      scale_size_area(max_size = 15) +  theme_minimal() +
      scale_color_manual(values = colorRampPalette(brewer.pal(8, "Set1"))(32))

  }) #End renderWordcoud


  #---------------------------
  #A.7) plot_Frequency_right_treemap - Hace treemap x académico(a)
  #---------------------------


  output$plot_Frequency_right_treemap = renderPlotly({

    #Filtro por años
    ini_year = as.numeric(input$slider_Frequency_main_years[1])
    last_year = as.numeric(input$slider_Frequency_main_years[2])
    data_db_tmp = data_db  %>% filter(Año %in% (ini_year:last_year))

    #Seleccionar por publicación
    publication = input$selinput_Frequency_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    #Filtro por académico
    academic=input$d_selinput_Frequency_right_academics
    if (is.null(academic)){academic="Aqueveque Navarro Pablo Esteban"}
    data_db_tmp = data_db_tmp  %>% filter(Nombre==academic)


    #Define nivel de palabras claves
    level_knowledge=input$selinput_Frequency_main_lknowledge
    if (level_knowledge==1) {thesaurus = unique(data_thesaurus$Area2)}
    if (level_knowledge==2) {thesaurus = unique(data_thesaurus$Area3)}
    if (level_knowledge==3) {thesaurus = unique(data_thesaurus$Area4)}
    if (level_knowledge==4) {thesaurus = unique(data_keywords$Keywords)}


    #Extrae elementos y crea treemap
    if (nrow(data_db_tmp)<2) {text_analysis = corpus(c(rep("not enough data",3),
                                                       rep("empty",3)))} else
                                                       {data_db_tmp$Index.keywords = stri_replace_all_regex(data_db_tmp$Index.keywords, "[\\p{p}\\p{S}]", " ")
                                                       if (level_knowledge!=4){text_analysis = corpus(data_db_tmp, text_field = "Abstract")} else
                                                       {text_analysis = corpus(data_db_tmp, text_field = "Index.keywords")}} 
    text_analysis = tokens(text_analysis,remove_punct = T,remove_symbols = T, remove_numbers = T, remove_url = T,remove_separators = T)
    text_analysis=tokens_tolower(text_analysis)
    text_analysis = kwic(text_analysis, pattern = phrase(c(thesaurus,"not enough data", "empty")))
    text_analysis = data.frame(words=text_analysis$keyword)

    text_analysis= text_analysis %>%count(words, sort=T)%>%top_n(20,n)

    plot_ly(
      text_analysis,
      labels  = ~ words,
      parents = NA,
      values = ~ n,
      type = 'treemap',
      #marker=list(colorscale='Picnic'),
      hovertemplate = "Ingredient: %{label}<br>Count: %{value}<extra></extra>"
    )

  }) #End Renderplot

#================================================
# B) Vista 2 - Comparación por tiempo
#================================================

  #---------------------------
  #B.1)  Render - d_selinput_Temporal_right_academics - Actualiza listado de académicos y fechas
  #---------------------------

  observeEvent(input$selinput_Temporal_left_university, {

    
    #Actualizar update input
    updateSliderInput(session, inputId ="slider_Temporal_left_years",min=min(data_db$Año),max=max(data_db$Año))
    data_db_tmp = data_db
    
    
    #Actualizar update input
    updateSliderInput(session, inputId ="slider_Temporal_right_years",min=min(data_db$Año),max=max(data_db$Año))
    data_db_tmp = data_db
    
    
    output$selinput_Temporal_right_academics <- renderUI({

      data_db_tmp = data_db

      university=input$selinput_Temporal_left_university
      if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
      if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
      if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}

      selectInput("d_selinput_Temporal_right_academics",label="Academic", choices = sort(unique(data_db_tmp$Nombre)),
                  selected = 1,width = "40%")
    }) #End renderUI
  })


  #---------------------------
  #B.2) plot_Temporal_left_summary - Hace  resumen x universidad
  #---------------------------


  output$plot_Temporal_left_summary = renderPlotly({


    data_db_tmp = data_db

    #Seleccionar por publicación
    publication = input$selinput_Temporal_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    
    university=input$selinput_Temporal_left_university
    if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
    if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
    if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}

    ini_year1 = input$slider_Temporal_left_years[1]     #Mínimo de los años ingresados
    last_year1 = input$slider_Temporal_left_years[2]

    ini_year2 = input$slider_Temporal_right_years[1]     #Mínimo de los años ingresados
    last_year2 = input$slider_Temporal_right_years[2]



    years=data.frame(years=min(data_db$Año):max(data_db$Año))
    years_tmp=data_db_tmp %>% group_by(Año) %>%summarise(n = n())
    frequency=merge(years,years_tmp,by.x="years",by.y="Año",all.x=T)
    frequency$n[which(is.na(frequency$n))]=0
    frequency$time = "Other years"
    frequency$time[which((frequency$years>=ini_year1) &  (frequency$years<=last_year1))] = paste(ini_year1,"-",last_year1)
    frequency$time[which((frequency$years>=ini_year2) &  (frequency$years<=last_year2))] = paste(ini_year2,"-",last_year2)


    plot_app=ggplot(data=frequency, aes(x=years, y=n)) +   theme_classic() + ylab("Publications") + xlab("Years") +
      geom_bar(stat="identity", aes(fill = time)) + scale_x_continuous(breaks=seq(min(data_db$Año),max(data_db$Año),1)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),line = element_blank(), legend.position='bottom') +
      guides(fill=guide_legend(title="")) + scale_fill_manual(labels= c("Other years",paste(ini_year1,"-",last_year1),
                                                                        paste(ini_year2,"-",last_year2)),
                                                              values=c("pink","lightblue","seashell2"))

    ggplotly(plot_app) %>% layout(legend = list(orientation = "h", x = 0.35, y =-1))

  }) #End renderUI


  #---------------------------
  #B.3) plot_Temporal_right_summary - - Hace resumen x académico
  #---------------------------


  output$plot_Temporal_right_summary = renderPlotly({


    data_db_tmp = data_db

    #Seleccionar por publicación
    publication = input$selinput_Temporal_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    
    #Filtro por academico
    academic=input$d_selinput_Temporal_right_academics
    if (is.null(academic)){academic="Aqueveque Navarro Pablo Esteban"}
    data_db_tmp = data_db_tmp  %>% filter(Nombre==academic)

    ini_year1 = input$slider_Temporal_left_years[1]     #Mínimo de los años ingresados
    last_year1 = input$slider_Temporal_left_years[2]

    ini_year2 = input$slider_Temporal_right_years[1]     #Mínimo de los años ingresados
    last_year2 = input$slider_Temporal_right_years[2]

    years=data.frame(years=min(data_db$Año):max(data_db$Año))
    years_tmp=data_db_tmp %>% group_by(Año) %>%summarise(n = n())
    frequency=merge(years,years_tmp,by.x="years",by.y="Año",all.x=T)
    frequency$n[which(is.na(frequency$n))]=0
    frequency$time = "Other years"
    frequency$time[which((frequency$years>=ini_year1) &  (frequency$years<=last_year1))] = paste(ini_year1,"-",last_year1)
    frequency$time[which((frequency$years>=ini_year2) &  (frequency$years<=last_year2))] = paste(ini_year2,"-",last_year2)


    plot_app=ggplot(data=frequency, aes(x=years, y=n)) +   theme_classic() + ylab("Publications") + xlab("Years") +
      geom_bar(stat="identity", aes(fill = time)) + scale_x_continuous(breaks=seq(min(data_db$Año),max(data_db$Año),1)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),line = element_blank(), legend.position='bottom') +
      guides(fill=guide_legend(title="")) + scale_fill_manual(labels= c("Other years",paste(ini_year1,"-",last_year1),
                                                                        paste(ini_year2,"-",last_year2)),
                                                              values=c("pink","lightblue","seashell2"))

    ggplotly(plot_app) %>% layout(legend = list(orientation = "h", x = 0.35, y =-1))

  }) #End renderUI


  #---------------------------
  #B.4)  plot_Temporal_left_comparison - Hace comparación de palabras x universidad
  #---------------------------

  output$plot_Temporal_left_comparison = renderPlotly({

    #Filtro por periodo inicial
    ini_year1 = as.numeric(input$slider_Temporal_left_years[1])
    last_year1 = as.numeric(input$slider_Temporal_left_years[2])

    #Filtro por periodo final
    ini_year2 = as.numeric(input$slider_Temporal_right_years[1])
    last_year2 = as.numeric(input$slider_Temporal_right_years[2])


    ini_year= min(ini_year1,ini_year2)
    last_year=max(last_year1,last_year2)
    data_db_tmp = data_db  %>% filter(Año %in% (ini_year:last_year))

    #Seleccionar por publicación
    publication = input$selinput_Temporal_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    #Filtro de universidad
    university=input$selinput_Temporal_left_university
    
    if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
    if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
    if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}


    #Define nivel de palabras claves
    level_knowledge=input$selinput_Temporal_main_lknowledge
    if (level_knowledge==1) {thesaurus = unique(data_thesaurus$Area2)}
    if (level_knowledge==2) {thesaurus = unique(data_thesaurus$Area3)}
    if (level_knowledge==3) {thesaurus = unique(data_thesaurus$Area4)}
    if (level_knowledge==4) {thesaurus = unique(data_keywords$Keywords)}


    bandera = 0
    if ((nrow(data_db_tmp)==0) | (length(unique(c(ini_year1,ini_year2,last_year1,last_year2)))==1))
    { bandera = 1 }

    if (bandera!=1)
    {
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

        #Revisa condiciones para establecer la clase

        if ((length(which(kwick_text$Año>=ini_year1 & kwick_text$Año<=last_year1))==0) | (length(which(kwick_text$Año>=ini_year2 &
            kwick_text$Año<=last_year2))==0) | (length(which(kwick_text$Año>=ini_year1 & kwick_text$Año<=last_year1))==nrow(kwick_text)) |
            (length(which(kwick_text$Año>=ini_year2 & kwick_text$Año<=last_year2))==nrow(kwick_text)))
        {bandera = 1}
      }


    if (bandera == 1){

        tstat_key = NULL
        tstat_key = data.frame(feature=c("There are no enough data to compare two periods",
                                         "It was no possible to realise the comparison"),
                               score=rbind(0,0))
        tstat_key$class = paste(ini_year1,"-",last_year1)

    } else

      {
        kwick_text$clase=0
        kwick_text$clase[which(kwick_text$Año>=ini_year1 & kwick_text$Año<=last_year1)]=paste(ini_year1,"-",last_year1)
        kwick_text$clase[which(kwick_text$Año>=ini_year2 & kwick_text$Año<=last_year2)]=paste(ini_year2,"-",last_year2)

        corpus_text = corpus(kwick_text,text_field="keyword")
        token_text = tokens(corpus_text)
        token_text = tokens_compound(token_text, pattern = phrase(thesaurus))

        df_text = dfm(token_text)

        tstat_key = textstat_keyness(df_text,target = df_text$clase== paste(ini_year1,"-",last_year1))

        names(tstat_key)[2]="score"

        tstat_key_top =  tstat_key[(which(tstat_key$score>=0)),]
        if (nrow(tstat_key_top)>20){tstat_key_top=tstat_key_top %>% top_n(n=20,score)}
        tstat_key_top$class=paste(ini_year1,"-",last_year1)

        tstat_key_down =  tstat_key[(which(tstat_key$score<0)),]
        if (nrow(tstat_key_down)>20){tstat_key_down=tstat_key_down %>% top_n(n=-20,score)}
        tstat_key_down$class=paste(ini_year2,"-",last_year2)

        tstat_key = rbind(tstat_key_top,tstat_key_down)
        tstat_key = tstat_key[,c(-3,-4,-5)]
      }

    if (length(which(duplicated(tstat_key$feature)))>0) {tstat_key=tstat_key[-which(duplicated(tstat_key$feature)),]}


    color = ifelse(tstat_key$class == paste(ini_year1,"-",last_year1), "pink", "lightblue")

    plot_app = ggplot(tstat_key, aes(x = reorder(feature, score), y = score)) +
      geom_bar(stat = "identity",
               show.legend = TRUE,
               color = "white", aes(fill = class)) + # Border color
      theme(legend.position = "top") + guides(fill=guide_legend(title="Periods")) +
      scale_y_continuous(limits = c(min(tstat_key$score) - 0.5,
                                    max(tstat_key$score) + 0.5)) +
      coord_flip() + theme_minimal() +   scale_fill_manual(values = c("pink", "lightblue")) + xlab("Chi-square value")

    ggplotly(plot_app) %>% layout(xaxis  = list(title = "Chi-square value"), yaxis = list(title = "Keywords"))

  })


  #---------------------------
  #B.5)  plot_Temporal_right_comparison - Hace comparación de palabras x académico
  #---------------------------

  output$plot_Temporal_right_comparison = renderPlotly({

    #Filtro por periodo inicial
    ini_year1 = as.numeric(input$slider_Temporal_left_years[1])
    last_year1 = as.numeric(input$slider_Temporal_left_years[2])

    #Filtro por periodo final
    ini_year2 = as.numeric(input$slider_Temporal_right_years[1])
    last_year2 = as.numeric(input$slider_Temporal_right_years[2])


    ini_year= min(ini_year1,ini_year2)
    last_year=max(last_year1,last_year2)
    data_db_tmp = data_db  %>% filter(Año %in% (ini_year:last_year))
    
    #Seleccionar por publicación
    publication = input$selinput_Temporal_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    

    #Filtro por académico
    academic=input$d_selinput_Temporal_right_academics
    if (is.null(academic)){academic="Aqueveque Navarro Pablo Esteban"}
    data_db_tmp = data_db_tmp  %>% filter(Nombre==academic)

    bandera = 0
    if ((nrow(data_db_tmp)==0) | (length(unique(c(ini_year1,ini_year2,last_year1,last_year2)))==1))
    { bandera = 1 }

    #Define nivel de palabras claves
    level_knowledge=input$selinput_Temporal_main_lknowledge
    if (level_knowledge==1) {thesaurus = unique(data_thesaurus$Area2)}
    if (level_knowledge==2) {thesaurus = unique(data_thesaurus$Area3)}
    if (level_knowledge==3) {thesaurus = unique(data_thesaurus$Area4)}
    if (level_knowledge==4) {thesaurus = unique(data_keywords$Keywords)}


    if (bandera!=1)
    {

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


      #Revisa condiciones para establecer la clase

      if ((length(which(kwick_text$Año>=ini_year1 & kwick_text$Año<=last_year1))==0) | (length(which(kwick_text$Año>=ini_year2 &
                                                                                                     kwick_text$Año<=last_year2))==0) | (length(which(kwick_text$Año>=ini_year1 & kwick_text$Año<=last_year1))==nrow(kwick_text)) |
          (length(which(kwick_text$Año>=ini_year2 & kwick_text$Año<=last_year2))==nrow(kwick_text)))
      {bandera = 1}
    }


    if (bandera == 1){

      tstat_key = NULL
      tstat_key = data.frame(feature=c("There are no enough data to compare two periods","It was no possible
                                       to realise the comparison"),
                             score=rbind(0,0))
      tstat_key$class = paste(ini_year1,"-",last_year1)

    } else

    {
      kwick_text$clase=0
      kwick_text$clase[which(kwick_text$Año>=ini_year1 & kwick_text$Año<=last_year1)]=paste(ini_year1,"-",last_year1)
      kwick_text$clase[which(kwick_text$Año>=ini_year2 & kwick_text$Año<=last_year2)]=paste(ini_year2,"-",last_year2)

      corpus_text = corpus(kwick_text,text_field="keyword")
      token_text = tokens(corpus_text)
      token_text = tokens_compound(token_text, pattern = phrase(thesaurus))

      df_text = dfm(token_text)

      tstat_key = textstat_keyness(df_text,target = df_text$clase== paste(ini_year1,"-",last_year1))

      names(tstat_key)[2]="score"
      tstat_key_top =  tstat_key[(which(tstat_key$score>=0)),]
      if (nrow(tstat_key_top)>20){tstat_key_top=tstat_key_top %>% top_n(n=20,score)}

      tstat_key_top$class=paste(ini_year1,"-",last_year1)
      tstat_key_down =tstat_key[(which(tstat_key$score<0)),]
      if (nrow(tstat_key_down)>20){tstat_key_down=tstat_key_down %>% top_n(n=-20,score)}

      tstat_key_down$class=paste(ini_year2,"-",last_year2)

      tstat_key = rbind(tstat_key_top,tstat_key_down)
      tstat_key = tstat_key[,c(-3,-4,-5)]
    }

    if (length(which(duplicated(tstat_key$feature)))>0) {tstat_key=tstat_key[-which(duplicated(tstat_key$feature)),]}

    color = ifelse(tstat_key$class == paste(ini_year1,"-",last_year1), "pink", "lightblue")

    plot_app = ggplot(tstat_key, aes(x = reorder(feature, score), y = score)) +
      geom_bar(stat = "identity",
               show.legend = TRUE,
               color = "white", aes(fill = class)) + # Border color
      xlab("Group") +
      ylab("Value") + theme(legend.position = "top") +
      scale_y_continuous(limits = c(min(tstat_key$score) - 0.5,
                                    max(tstat_key$score) + 0.5)) + guides(fill=guide_legend(title="Periods")) +
      coord_flip() + theme_minimal() +   scale_fill_manual(values = c("pink", "lightblue")) + xlab("Chi-square value")

    ggplotly(plot_app) %>% layout(xaxis  = list(title = "Chi-square value"), yaxis = list(title = "Keywords"))
    

  })
  
#================================
# C. Vista 3 - Compración entre universidades o académicos
#================================

  #---------------------------
  #C.1) plot_Comparison_left_top_summary - Hace resumen Universidad 1
  #---------------------------

  output$plot_Comparison_left_top_summary = renderPlotly({

    #Actualizar update input
    updateSliderInput(session, inputId ="selinput_Comparison_main_years",min=min(data_db$Año),max=max(data_db$Año))
    data_db_tmp = data_db

    data_db_tmp = data_db

    #Seleccionar por publicación
    publication = input$selinput_Comparison_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    
    university=input$selinput_Comparison_left_top_university
    if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
    if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
    if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}

    ini_year = input$selinput_Comparison_main_years[1]     #Mínimo de los años ingresados
    last_year = input$selinput_Comparison_main_years[2]

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

    ggplotly(plot_app) %>% layout(legend = list(orientation = "h", x = 0.35, y =-1))

  }) #End renderUI



  #---------------------------
  #C.2) plot_Comparison_left_bottom_summary - Hace resumen universidad 2
  #---------------------------

  output$plot_Comparison_left_bottom_summary = renderPlotly({


    data_db_tmp = data_db

    #Seleccionar por publicación
    publication = input$selinput_Comparison_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    
    university=input$selinput_Comparison_left_bottom_university
    if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
    if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
    if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}

    ini_year = input$selinput_Comparison_main_years[1]     #Mínimo de los años ingresados
    last_year = input$selinput_Comparison_main_years[2]

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

    ggplotly(plot_app) %>% layout(legend = list(orientation = "h", x = 0.35, y =-1))

  }) #End renderUI


  #---------------------------
  #C.3)  Aplot_Comparison_left_comparison - Hace comparación entre universidades
  #---------------------------

  output$plot_Comparison_left_comparison = renderPlotly({

    #Filtro por año 1
    ini_year = input$selinput_Comparison_main_years[1]     #Mínimo de los años ingresados
    last_year = input$selinput_Comparison_main_years[2]
    data_db_tmp = data_db  %>% filter(Año %in% (ini_year:last_year))

    #Seleccionar por publicación
    publication = input$selinput_Comparison_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    

    #Filtro de universidad
    university1_tmp=input$selinput_Comparison_left_top_university
    university1="ALL"
    if (university1_tmp==2) {university1="UV"}
    if (university1_tmp==3) {university1="UdeC"}
    if (university1_tmp==4) {university1="USACH"}

    #Filtro de universidad
    university2="ALL"
    university2_tmp=input$selinput_Comparison_left_bottom_university
    if (university2_tmp==2) {university2="UV"}
    if (university2_tmp==3) {university2="UdeC"}
    if (university2_tmp==4) {university2="USACH"}

    if ( (university1_tmp!=1) & (university2_tmp!=1)  )
    {data_db_tmp = data_db_tmp  %>% filter(Universidad==university1 | Universidad==university2)}


    bandera = 0
    if ((nrow(data_db_tmp)==0) | (length(unique(c(university1,university2)))==1))
    { bandera = 1 }

    #Define nivel de palabras claves
    level_knowledge=input$selinput_Comparison_main_lknowledge
    if (level_knowledge==1) {thesaurus = unique(data_thesaurus$Area2)}
    if (level_knowledge==2) {thesaurus = unique(data_thesaurus$Area3)}
    if (level_knowledge==3) {thesaurus = unique(data_thesaurus$Area4)}
    if (level_knowledge==4) {thesaurus = unique(data_keywords$Keywords)}


    if (bandera!=1)
    {

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

      #Revisa condiciones para establecer la clase
      if (length(unique(kwick_text$Universidad))==1){bandera=1}
    }

    if (bandera == 1){

      tstat_key = NULL
      tstat_key = data.frame(feature=c("There are no  enough data to compare two periods",
                                       "It was no possible to realise the comparison"),
                             score=rbind(0,0))
      tstat_key$class = university1

    } else

    {

      kwick_text$clase=0
      if (university1!="ALL"){
        kwick_text$clase[which(kwick_text$Universidad==university1)]=paste(university1)
        kwick_text$clase[which(kwick_text$Universidad!=university1)]=paste(university2)}

      if (university2!="ALL"){
        kwick_text$clase[which(kwick_text$Universidad==university2)]=paste(university2)
        kwick_text$clase[which(kwick_text$Universidad!=university2)]=paste(university1)}


      corpus_text = corpus(kwick_text,text_field="keyword")
      token_text = tokens(corpus_text)
      token_text = tokens_compound(token_text, pattern = phrase(thesaurus))

      df_text = dfm(token_text)

      tstat_key = textstat_keyness(df_text,target = df_text$clase== university1)

      names(tstat_key)[2]="score"
      tstat_key_top = tstat_key[(which(tstat_key$score>0)),]
      if (nrow(tstat_key_top)>20){tstat_key_top=tstat_key_top %>% top_n(n=20,score)}
      tstat_key_top$class=university1
      tstat_key_down = tstat_key[(which(tstat_key$score<0)),]
      if (nrow(tstat_key_down)>20){tstat_key_down=tstat_key_down %>% top_n(n=-20,score)}
      tstat_key_down$class=university2

      tstat_key = rbind(tstat_key_top,tstat_key_down)
      tstat_key = tstat_key[,c(-3,-4,-5)]
    }

    if (length(which(duplicated(tstat_key$feature)))>0) {tstat_key=tstat_key[-which(duplicated(tstat_key$feature)),]}


    color = ifelse(tstat_key$class == university1, "pink", "lightblue")

    plot_app = ggplot(tstat_key, aes(x = reorder(feature, score), y = score)) +
      geom_bar(stat = "identity",
               show.legend = TRUE,
               color = "white", aes(fill = class)) + # Border color
      xlab("Group") +
      ylab("Value") + theme(legend.position = "top") + guides(fill=guide_legend(title="Periods")) +
      scale_y_continuous(limits = c(min(tstat_key$score) - 0.5,
                                    max(tstat_key$score) + 0.5)) +
      coord_flip() + theme_minimal() +   scale_fill_manual(values = c("pink", "lightblue")) + xlab("Chi-square value")

    ggplotly(plot_app) %>% layout(xaxis  = list(title = "Chi-square value"), yaxis = list(title = "Keywords"))
    
  }) #End renderUI



  #---------------------------
  #C.4)  Render - selinput_Comparison_left_top_university - Limita Selector Académicos 1
  #---------------------------

  observeEvent(input$selinput_Comparison_left_top_university, {

    output$selinput_Comparison_right_top_academics <- renderUI({

      data_db_tmp = data_db

      university=input$selinput_Comparison_left_top_university
      if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
      if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
      if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}

      selectInput("d_selinput_Comparison_right_top_academics",label="Academic 1", choices = sort(unique(data_db_tmp$Nombre)),
                  selected = 1,width = "40%")
    }) #End renderUI
  })

  

  #---------------------------
  #C.5) plot_Comparison_right_top_summary - Hace resumen de académico 1
  #---------------------------

  output$plot_Comparison_right_top_summary = renderPlotly({

    data_db_tmp = data_db

    #Seleccionar por publicación
    publication = input$selinput_Comparison_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    
    #Filtro por academico
    academic=input$d_selinput_Comparison_right_top_academics
    if (is.null(academic)){academic="Aqueveque Navarro Pablo Esteban"}
    data_db_tmp = data_db_tmp  %>% filter(Nombre==academic)

    ini_year = input$selinput_Comparison_main_years[1]     #Mínimo de los años ingresados
    last_year = input$selinput_Comparison_main_years[2]

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


    ggplotly(plot_app) %>% layout(legend = list(orientation = "h", x = 0.35, y =-1))

  }) #End renderUI



  #---------------------------
  #C.6)  Render - selinput_Comparison_right_bottom_academics -  Limita Selector Académico 2
  #---------------------------

  observeEvent(input$selinput_Comparison_left_bottom_university, {

    output$selinput_Comparison_right_bottom_academics <- renderUI({

      data_db_tmp = data_db

      university=input$selinput_Comparison_left_bottom_university
      if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
      if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
      if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}

      selectInput("d_selinput_Comparison_right_bottom_academics",label="Academic 2", choices = sort(unique(data_db_tmp$Nombre)),
                  selected = 1,width = "40%")
    }) #End renderUI
  })



  #---------------------------
  #C.7) plot_Comparison_right_bottom_summary - Hace resumen de académico 2
  #---------------------------

  output$plot_Comparison_right_bottom_summary = renderPlotly({

    data_db_tmp = data_db

    #Seleccionar por publicación
    publication = input$selinput_Comparison_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    
    
    #Filtro por academico
    academic=input$d_selinput_Comparison_right_bottom_academics
    if (is.null(academic)){academic="Aqueveque Navarro Pablo Esteban"}
    data_db_tmp = data_db_tmp  %>% filter(Nombre==academic)

    ini_year = input$selinput_Comparison_main_years[1]     #Mínimo de los años ingresados
    last_year = input$selinput_Comparison_main_years[2]

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


    ggplotly(plot_app) %>% layout(legend = list(orientation = "h", x = 0.35, y =-1))

  }) #End renderUI


  #---------------------------
  #C.8)  plot_Comparison_right_comparison - Grafica la comparación entre académicos
  #---------------------------

  output$plot_Comparison_right_comparison = renderPlotly({

    #Filtro por año 1
    ini_year = input$selinput_Comparison_main_years[1]     #Mínimo de los años ingresados
    last_year = input$selinput_Comparison_main_years[2]
    data_db_tmp = data_db  %>% filter(Año %in% (ini_year:last_year))

    #Filtro por académico
    academic1=input$d_selinput_Comparison_right_top_academics
    academic2=input$d_selinput_Comparison_right_bottom_academics

    #Seleccionar por publicación
    publication = input$selinput_Comparison_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
    

    if (is.null(academic1) | is.null(academic2))
    {academic1="Aqueveque Navarro Pablo Esteban"
    academic2="Arias Parada Luis Emiliano"
    }

    bandera = 0
    if ((nrow(data_db_tmp)<2) | (length(unique(c(academic1,academic2)))==1))
    { bandera = 1 }

    #Define nivel de palabras claves
    level_knowledge=input$selinput_Comparison_main_lknowledge
    if (level_knowledge==1) {thesaurus = unique(data_thesaurus$Area2)}
    if (level_knowledge==2) {thesaurus = unique(data_thesaurus$Area3)}
    if (level_knowledge==3) {thesaurus = unique(data_thesaurus$Area4)}
    if (level_knowledge==4) {thesaurus = unique(data_keywords$Keywords)}

    if (bandera!=1)
    {
      data_db_tmp = data_db_tmp  %>% filter(Nombre==academic1 | Nombre==academic2)
      
      
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
            #Revisa condiciones para establecer la clase
      if (length(unique(kwick_text$Nombre))==1){bandera=1}
    }

    if (bandera == 1){

      tstat_key = NULL
      tstat_key = data.frame(feature=c("There are no  enough data to compare two periods","It was no possible to realise the comparison"),
                             score=rbind(0,0))
      tstat_key$class = academic1

    } else

    {

      kwick_text$clase=0
      kwick_text$clase[which(kwick_text$Nombre==academic1)]=paste(academic1)
      kwick_text$clase[which(kwick_text$Nombre==academic2)]=paste(academic2)

      corpus_text = corpus(kwick_text,text_field="keyword")
      token_text = tokens(corpus_text)
      token_text = tokens_compound(token_text, pattern = phrase(thesaurus))

      df_text = dfm(token_text)

      tstat_key = textstat_keyness(df_text,target = df_text$clase== academic1)
      
      names(tstat_key)[2]="score"
      
      tstat_key_top = tstat_key[(which(tstat_key$score>=0)),]
      if (nrow(tstat_key_top)>20){tstat_key_top=tstat_key_top %>% top_n(n=20,score)}
      tstat_key_top$class=academic1
      tstat_key_down = tstat_key[(which(tstat_key$score<0)),]
      if (nrow(tstat_key_down)>20){tstat_key_down=tstat_key_down %>% top_n(n=-20,score)}
      tstat_key_down$class=academic2
      
      
      tstat_key = rbind(tstat_key_top,tstat_key_down)
      tstat_key = tstat_key[,c(-3,-4,-5)]
    }

    if (length(which(duplicated(tstat_key$feature)))>0) {tstat_key=tstat_key[-which(duplicated(tstat_key$feature)),]}
 
    

    color = ifelse(tstat_key$class == academic1, "pink", "lightblue")

    plot_app = ggplot(tstat_key, aes(x = reorder(feature, score), y = score)) +
      geom_bar(stat = "identity",
               show.legend = TRUE,
               color = "white", aes(fill = class)) + # Border color
      xlab("Group") +
      ylab("Value") + theme(legend.position = "top") + guides(fill=guide_legend(title="Periods")) +
      scale_y_continuous(limits = c(min(tstat_key$score) - 0.5,
                                    max(tstat_key$score) + 0.5)) +
      coord_flip() + theme_minimal() +   scale_fill_manual(values = c("pink", "lightblue")) + xlab("Chi-square value")

    ggplotly(plot_app) %>% layout(xaxis  = list(title = "Chi-square value"), yaxis = list(title = "Keywords"))
    
    }) #End renderUI

  #================================
  # D. Vista 4 - Keyword analysis
  #================================

  #---------------------------
  #D.1)  Render - selinput_Keywords_main_publication - Ajusta las keywords
  #---------------------------

  observeEvent(c(input$selinput_Keywords_main_lknowledge, input$selinput_Keywords_main_publication), {

    output$selinput_Keywords_main_keywords <- renderUI({
      
      data_db_tmp = data_db

      #Seleccionar por publicación
      publication = input$selinput_Keywords_main_publication
      if (publication!="All")
      {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
      
      level_knowledge=input$selinput_Keywords_main_lknowledge
      if (level_knowledge==1) {thesaurus = unique(data_thesaurus$Area2)}
      if (level_knowledge==2) {thesaurus = unique(data_thesaurus$Area3)}
      if (level_knowledge==3) {thesaurus = unique(data_thesaurus$Area4)}
      if (level_knowledge==4) {thesaurus = unique(data_keywords$Keywords)}

      #Arregla los datos para hacer la prueba
      data_db_tmp$Index.keywords = stri_replace_all_regex(data_db_tmp$Index.keywords, "[\\p{p}\\p{S}]", " ")
      if (level_knowledge!=4){corpus_text = corpus(data_db_tmp, text_field = "Abstract")} else
      {corpus_text = corpus(data_db_tmp, text_field = "Index.keywords")}
      
      token_text = tokens(corpus_text,remove_punct = T,remove_symbols = T, remove_numbers = T, remove_url = T,remove_separators = T)
      token_text = tokens_tolower(token_text)
      kwick_text = kwic(token_text, pattern = phrase(thesaurus))

      selectizeInput("d_selinput_Keywords_main_keywords",label="Keywords", options= list(maxOptions = 10000), 
                  choices = sort(unique(kwick_text$keyword)),
                  selected = 1)
    }) #End renderUI
  })
 
     
   #---------------------------
   #D.2)  Render - plot_Keywords_main_keywords - Grafica las keywords
   #---------------------------
 
   output$plot_Keywords_main_keywords = renderPlotly({
 
     keyword_search = input$d_selinput_Keywords_main_keywords
     if (is.null(keyword_search)){keyword_search="accuracy"}

    #datos
    data_db_tmp = data_db

    #Seleccionar por publicación
    publication = input$selinput_Keywords_main_publication
    if (publication!="All")
    {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}

    level_knowledge=input$selinput_Keywords_main_lknowledge
    if (level_knowledge==1) {thesaurus = unique(data_thesaurus$Area2)}
    if (level_knowledge==2) {thesaurus = unique(data_thesaurus$Area3)}
    if (level_knowledge==3) {thesaurus = unique(data_thesaurus$Area4)}
    if (level_knowledge==4) {thesaurus = unique(data_keywords$Keywords)}


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


    kwick_text = kwick_text  %>% filter(keyword == keyword_search)
    kwick_text=data.frame(kwick_text %>% group_by(Año,Universidad,Nombre) %>% count())
    kwick_text$Nombre=factor(kwick_text$Nombre, levels = unique(sort(kwick_text$Nombre,decreasing = T)))

    if(nrow(kwick_text)!=0)
    {

    plot_app = ggplot(kwick_text, aes(x=Año-0.5, xend=Año+0.5, y=Nombre, yend=Nombre, color=Universidad)) +
      geom_segment(size=4) + theme(legend.position="none") + theme_minimal() +
      xlim(min(data_db$Año)-1, max(data_db$Año)+1) + scale_color_manual(values=c("lightblue","pink","lightgreen")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position='bottom') +
      xlab("Years") + ylab("Academics")

    ggplotly(plot_app) %>% layout(
      xaxis = list(
        dtick = 1,
        tick0 = min(data_db$Año),
        tickmode = "linear"
      ))
    }

  }) # End render
   
  
  #================================
  # E. Vista 5 - Journal analysis
  #================================
   
   #---------------------------
   #E.1)  Render - selinput_Journal_right_academics - Filtra el listado de académicos x universidad
   #---------------------------
   
   observeEvent(input$selinput_Journal_left_university, {
     
     output$selinput_Journal_right_academics <- renderUI({
       
       data_db_tmp = data_db
       
       university=input$selinput_Journal_left_university
       if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
       if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
       if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}
       
       selectInput("d_selinput_Journal_right_academics",label="Academic 1", choices = sort(unique(data_db_tmp$Nombre)),
                   selected = 1,width = "40%")
     }) #End renderUI
   })
   
   
   #---------------------------
   #E.2) plot_Journal_left_summary - Grafica Resumen x universidad
   #---------------------------
   
   output$plot_Journal_left_summary = renderPlotly({
     
     
     data_db_tmp = data_db
     
     #Seleccionar por publicación
     publication = input$selinput_Journal_main_publication
     if (publication!="All")
     {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
     
      university=input$selinput_Journal_left_university
      if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
      if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
      if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}
      
     ini_year = input$selinput_Journal_main_years[1]     #Mínimo de los años ingresados
     last_year = input$selinput_Journal_main_years[2]
     
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
     
     ggplotly(plot_app) %>% layout(legend = list(orientation = "h", x = 0.35, y =-1))
     
   }) #End renderUI
   
   
   #---------------------------
   #E.3) plot_Journal_left_universities - Grafica journals x universidad
   #---------------------------
   output$plot_Journal_left_universities = renderPlotly({
     
     #Actualizar update input
     updateSliderInput(session, inputId ="selinput_Journal_main_years",min=min(data_db$Año),max=max(data_db$Año))
     data_db_tmp = data_db
     
     #Filtro por año
     ini_year = input$selinput_Journal_main_years[1]     #Mínimo de los años ingresados
     last_year = input$selinput_Journal_main_years[2]
     data_db_tmp = data_db_tmp  %>% filter(Año %in% (ini_year:last_year))
     
     
     #Seleccionar por publicación
     publication = input$selinput_Journal_main_publication
     if (publication!="All")
     {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
     
     #Filtrar por universidad
     university=input$selinput_Journal_left_university
     if (university==2) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UV")}
     if (university==3) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="UdeC")}
     if (university==4) {data_db_tmp = data_db_tmp  %>% filter(Universidad=="USACH")}
     
     data_db_tmp=data_db_tmp %>% group_by(Universidad,Fuente) %>% count(Fuente, sort=F)
     data_db_tmp=data.frame(data_db_tmp)
     data_db_tmp=data_db_tmp[order(data_db_tmp$n,decreasing=T),] %>% top_n(10,n)
     
     
     if (nrow(data_db_tmp)<1)
     {data_db_tmp = data.frame(Fuente="There are no data recorded", Universidad = "ALL", n =1)}
     
     
     #Ordenar factores
     data_db_tmp$Fuente=factor(data_db_tmp$Fuente, levels = unique(data_db_tmp$Fuente[order(data_db_tmp$n,decreasing=F)]))
    
          
     plot_app=ggplot(data=data_db_tmp, aes(x=n, y=Fuente)) +   theme_classic() + ylab("Publications") + xlab("n") +
       geom_bar(stat="identity", aes(fill = Universidad))  +
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),line = element_blank(), legend.position='bottom') +
       theme(line = element_blank(), legend.position='bottom') +
       guides(fill=guide_legend(title="")) + scale_fill_manual(values=c("lightblue","pink","lightgreen"))
     ggplotly(plot_app)
     
   })#Fin render
   
   
   #---------------------------
   #E.4) plot_Comparison_right_bottom_summary - Hace resumen x Académico
   #---------------------------
   
   output$plot_Journal_right_summary = renderPlotly({
     
     data_db_tmp = data_db
     
     #Seleccionar por publicación
     publication = input$selinput_Journal_main_publication
     if (publication!="All")
     {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
     
     
     #Filtro por academico
     academic=input$selinput_Journal_right_academics
     if (is.null(academic)){academic="Aqueveque Navarro Pablo Esteban"}
     data_db_tmp = data_db_tmp  %>% filter(Nombre==academic)
     
     ini_year = input$selinput_Journal_main_years[1]     #Mínimo de los años ingresados
     last_year = input$selinput_Journal_main_years[2]
     
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
     
     
     ggplotly(plot_app) %>% layout(legend = list(orientation = "h", x = 0.35, y =-1))
     
   }) #End renderUI
   
   
   #---------------------------
   #E.5) Rplot_Journal_right_academics - Grafica journals de académicos
   #---------------------------
   output$plot_Journal_right_academics = renderPlotly({
     
     data_db_tmp = data_db
     
     
     #Filtro por año
     ini_year = input$selinput_Journal_main_years[1]     #Mínimo de los años ingresados
     last_year = input$selinput_Journal_main_years[2]
     data_db_tmp = data_db_tmp  %>% filter(Año %in% (ini_year:last_year))
     
     
     #Seleccionar por publicación
     publication = input$selinput_Journal_main_publication
     if (publication!="All")
     {data_db_tmp = data_db_tmp  %>% filter(Tipo.de.documento==publication)}
     
     #Filtro por académico
     academic=input$d_selinput_Journal_right_academics
     if (is.null(academic)){academic="Aqueveque Navarro Pablo Esteban"}
     data_db_tmp = data_db_tmp  %>% filter(Nombre==academic)
     
     data_db_tmp=data_db_tmp %>% group_by(Universidad,Fuente) %>% count(Fuente, sort=F)
     data_db_tmp=data.frame(data_db_tmp)
     data_db_tmp=data_db_tmp[order(data_db_tmp$n,decreasing=T),] %>% top_n(10,n)
     
     
     if (nrow(data_db_tmp)<1)
     {data_db_tmp = data.frame(Fuente="There are no data recorded", Universidad = "ALL", n =0)}
     
     
     #Ordenar factores
     data_db_tmp$Fuente=factor(data_db_tmp$Fuente, levels = unique(data_db_tmp$Fuente[order(data_db_tmp$n,decreasing=F)]))
     
     #Plot
     plot_app=ggplot(data=data_db_tmp, aes(x=n, y=Fuente)) +   theme_classic() + ylab("Publications") + xlab("n") +
       geom_bar(stat="identity",fill="lightblue")  +
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),line = element_blank(), legend.position='bottom') +
       theme(line = element_blank(), legend.position='bottom') +
       guides(fill=guide_legend(title=""))
     
     ggplotly(plot_app)
     
     
     
     
   })#Fin render
   
}