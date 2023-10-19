backend_indexes = function(input,output,session,data_db,data_main)
{
  
  output$table_Indexes_performance = renderDataTable({
    
    #Actualizar update input
    updateSliderInput(session, inputId ="slider_Indexes_main_years",min=min(data_db$Año),max=max(data_db$Año))
    
    
    #Filtro por años
    ini_year = as.numeric(input$slider_Indexes_main_years[1])
    last_year = as.numeric(input$slider_Indexes_main_years[2])
    data_db_tmp = data_db  %>% filter(Año %in% (ini_year:last_year))
    

    if (input$slider_Indexes_main_universities=="1")
    {  
      
      #-------------------
      # 1) TP - Total publications
      #-------------------
      TP = rep(3,0)
      TP[1] = nrow(data_db_tmp %>% filter(Universidad=="UV"))
      TP[2] = nrow(data_db_tmp %>% filter(Universidad=="UdeC"))
      TP[3] = nrow(data_db_tmp %>% filter(Universidad=="USACH")) 
      
      
      #------------------
      # 2) TPj - Total journal publications
      #-------------------
      TPj = rep(3,0)
      TPj[1] = nrow(data_db_tmp %>% filter(Universidad=="UV" & Tipo.de.documento =="Article"))
      TPj[2] = nrow(data_db_tmp %>% filter(Universidad=="UdeC" & Tipo.de.documento =="Article"))
      TPj[3] = nrow(data_db_tmp %>% filter(Universidad=="USACH" & Tipo.de.documento =="Article"))
      
      #------------------
      # 3) TPp - Total conference proceeding publications
      #-------------------
      TPp = rep(3,0)
      TPp[1] = nrow(data_db_tmp %>% filter(Universidad=="UV" & Tipo.de.documento =="Conference Paper"))
      TPp[2] = nrow(data_db_tmp %>% filter(Universidad=="UdeC" & Tipo.de.documento =="Conference Paper"))
      TPp[3] = nrow(data_db_tmp %>% filter(Universidad=="USACH" & Tipo.de.documento =="Conference Paper"))
      
      
      #------------------
      # 4) %TP$_j$ - Percentage of journal publications
      #-------------------
      Per_TPj = rep(3,0)
      Per_TPj[1] = round((TPj[1]/TP[1]) * 100,1)
      Per_TPj[2] = round((TPj[2]/TP[2]) * 100,1)
      Per_TPj[3] = round((TPj[3]/TP[3]) * 100,1)
      
      #------------------
      # 5) %TP$_p$ - Percentage of conference proceeding publications
      #-------------------
      Per_TPp= rep(3,0)
      Per_TPp[1] = round((TPp[1]/TP[1]) * 100,1)
      Per_TPp[2] = round((TPp[2]/TP[2]) * 100,1)
      Per_TPp[3] = round((TPp[3]/TP[3]) * 100,1)
      
      #------------------
      # 6) AP - Average publications by academics
      #-------------------
      AP  = rep(3,0)
      AP[1] = round(TP[1]/length(unique((data_db_tmp %>% filter(Universidad=="UV"))$Nombre)),1)
      AP[2] = round(TP[2]/length(unique((data_db_tmp %>% filter(Universidad=="UdeC"))$Nombre)),1)
      AP[3] = round(TP[3]/length(unique((data_db_tmp %>% filter(Universidad=="USACH"))$Nombre)),1)
      
      
      #------------------
      # 7) APj - Average journal publications by academics
      #-------------------
      APj  = rep(3,0)
      APj[1] = round(TPj[1]/length(unique((data_db_tmp %>% filter(Universidad=="UV"))$Nombre)),1)
      APj[2] = round(TPj[2]/length(unique((data_db_tmp %>% filter(Universidad=="UdeC"))$Nombre)),1)
      APj[3] = round(TPj[3]/length(unique((data_db_tmp %>% filter(Universidad=="USACH"))$Nombre)),1)
      
      
      #------------------
      # 8) APp - Average conference proceeding publications by academic
      #-------------------
      APp  = rep(3,0)
      APp[1] = round(TPp[1]/length(unique((data_db_tmp %>% filter(Universidad=="UV"))$Nombre)),1)
      APp[2] = round(TPp[2]/length(unique((data_db_tmp %>% filter(Universidad=="UdeC"))$Nombre)),1)
      APp[3] = round(TPp[3]/length(unique((data_db_tmp %>% filter(Universidad=="USACH"))$Nombre)),1)
      
      #------------------
      # 9) APY - Average publications by year
      #-------------------
      APY = rep(3,0)
      rango=max((data_db_tmp %>% filter(Universidad=="UV"))$Año)-min((data_db_tmp %>% filter(Universidad=="UV"))$Año)+1
      APY[1] = round(TP[1]/rango,1)
      rango=max((data_db_tmp %>% filter(Universidad=="UdeC"))$Año)-min((data_db_tmp %>% filter(Universidad=="UdeC"))$Año)+1
      APY[2] = round(TP[2]/rango,1)
      rango=max((data_db_tmp %>% filter(Universidad=="USACH"))$Año)-min((data_db_tmp %>% filter(Universidad=="USACH"))$Año)+1
      APY[3] = round(TP[3]/rango,1)
      
      
      #------------------
      # 10) APjY - Average journal publications by year 
      #-------------------
      APjY = rep(3,0)
      rango=max((data_db_tmp %>% filter(Universidad=="UV"))$Año)-min((data_db_tmp %>% filter(Universidad=="UV"))$Año)+1
      APjY[1] = round(TPj[1]/rango,1)
      rango=max((data_db_tmp %>% filter(Universidad=="UdeC"))$Año)-min((data_db_tmp %>% filter(Universidad=="UdeC"))$Año)+1
      APjY[2] = round(TPj[2]/rango,1)
      rango=max((data_db_tmp %>% filter(Universidad=="USACH"))$Año)-min((data_db_tmp %>% filter(Universidad=="USACH"))$Año)+1
      APjY[3] = round(TPj[3]/rango,1)
      
      #------------------
      # 11) APpY - Average conference proceeding publications by year 
      #-------------------
      APpY = rep(3,0)
      rango=max((data_db_tmp %>% filter(Universidad=="UV"))$Año)-min((data_db_tmp %>% filter(Universidad=="UV"))$Año)+1
      APpY[1] = round(TPp[1]/rango,1)
      rango=max((data_db_tmp %>% filter(Universidad=="UdeC"))$Año)-min((data_db_tmp %>% filter(Universidad=="UdeC"))$Año)+1
      APpY[2] = round(TPp[2]/rango,1)
      rango=max((data_db_tmp %>% filter(Universidad=="USACH"))$Año)-min((data_db_tmp %>% filter(Universidad=="USACH"))$Año)+1
      APpY[3] = round(TPp[3]/rango,1)
      
      #------------------
      # 12) NAY - Number of active years of publications
      #-------------------
      NAY = rep(3,0)
      NAY[1] = length(unique((data_db_tmp %>% filter(Universidad=="UV"))$Año))
      NAY[2] = length(unique((data_db_tmp %>% filter(Universidad=="UdeC"))$Año))
      NAY[3] = length(unique((data_db_tmp %>% filter(Universidad=="USACH"))$Año))
      
      #------------------
      # 13) PAY - Productivity per active year of publication
      #-------------------
      PAY = rep(3,0)
      PAY[1] = round(TP[1]/NAY[1],1)
      PAY[2] = round(TP[2]/NAY[2],1)
      PAY[3] = round(TP[3]/NAY[3],1)
      
      #------------------
      # 14) TC - Total number of citations
      #-------------------
      TC = rep(3,0)
      TC[1] = sum((data_db_tmp %>% filter(Universidad=="UV"))$Citado.por)
      TC[2] = sum((data_db_tmp %>% filter(Universidad=="UdeC"))$Citado.por)
      TC[3] = sum((data_db_tmp %>% filter(Universidad=="USACH"))$Citado.por)
      
      #------------------
      # 15) AC - Average citations by publication
      #-------------------
      AC = rep(3,0)
      AC[1] = round(TC[1]/TP[1],1)
      AC[2] = round(TC[2]/TP[2],1)
      AC[3] = round(TC[3]/TP[3],1)
      
      #------------------
      # 16) NCP - Number of cited publications
      #-------------------
      NCP = rep(3,0)
      NCP[1] =  length(which((data_db_tmp %>% filter(Universidad=="UV"))$Citado.por>0))
      NCP[2] =  length(which((data_db_tmp %>% filter(Universidad=="UdeC"))$Citado.por>0))
      NCP[3] =  length(which((data_db_tmp %>% filter(Universidad=="USACH"))$Citado.por>0))
      
      #------------------
      # 17) PCP - Proportion of cited publications
      #------------------- 
      PCP = rep(3,0)
      PCP[1] = round(NCP[1]/TP[1],1)
      PCP[2] = round(NCP[2]/TP[2],1)
      PCP[3] = round(NCP[3]/TP[3],1)
      
      #------------------
      # 18) CPP - Citations per cited publication
      #------------------- 
      CPP = rep(3,0)
      CPP[1] = round(TC[1]/NCP[1],1)
      CPP[2] = round(TC[2]/NCP[2],1)
      CPP[3] = round(TC[3]/NCP[3],1)
      
      
      #Generación de tabla
      indexes = c("Total publications","Total journal publications","Total conference proceeding publications",
                  "Percentage of journal publications","Percentage of conference proceeding publications",
                  "Average publications by academics","Average journal publications by academics",
                  "Average conference proceeding publications by academic","Average publications by year",
                  "Average journal publications by year","Average conference proceeding publications by year",
                  "Number of active years of publications","Productivity per active year of publication",
                  "Total number of citations","Average citations by publication","Number of cited publications",
                  "Proportion of cited publications","Citations per cited publication")
      
      data_matrix = rbind(TP,TPj,TPp,Per_TPj,Per_TPp,AP,APj,APp,APY,APjY,APpY,NAY,PAY,TC,AC,NCP,PCP,CPP)
      colnames(data_matrix)=c("UV","UdeC","USACH")
      rownames(data_matrix)=indexes
      data_matrix[which(is.infinite(data_matrix))]=0
      data_matrix[which(is.na(data_matrix))]=0
      data_matrix = tbl_df(data_matrix)
      rownames(data_matrix) = indexes
      
      
      datatable(data_matrix, class = 'display', options = list(pageLength = 20, dom = 'Brt', autoWidth = TRUE, ordering = F))
      
    } else #Fin universidad = 1
    {
      if (input$slider_Indexes_main_universities=="2"){university="UV"}
      if (input$slider_Indexes_main_universities=="3"){university="UdeC"}
      if (input$slider_Indexes_main_universities=="4"){university="USACH"}
      
      data_db_tmp = data_db_tmp  %>% filter(Universidad==university)
      data_matrix=NULL
      
      
      academics = unique(data_db_tmp$Nombre)
      if (length(academics)==0) {academics="There were no academics during the selected period"}
      
      for (a in 1:length(academics))
      {
        #-------------------
        # 1) TP - Total publications
        #-------------------
        TP = nrow(data_db_tmp %>% filter(Nombre == academics[a]))
        
        #------------------
        # 2) TPj - Total journal publications
        #-------------------
        TPj = nrow(data_db_tmp %>% filter(Nombre == academics[a] & Tipo.de.documento=="Article"))
        
        #------------------
        # 3) TPp - Total conference proceeding publications
        #-------------------
        TPp = nrow(data_db_tmp %>% filter(Nombre == academics[a] & Tipo.de.documento=="Conference Paper"))
        
        #-------------------
        #4) %TP$_j$ - Percentage of journal publications
        #-------------------
        Per_TPj = round((TPj/TP)*100,1)
        
        
        #------------------
        # 5) %TP$_p$ - Percentage of conference proceeding publications
        #-------------------
        Per_TPp = round((TPp/TP)*100,1)
        
        #------------------
        # 6) APY - Average publications by year
        #-------------------
        rango = max((data_db_tmp %>% filter(Nombre == academics[a]))$Año)-min((data_db_tmp %>% filter(Nombre == academics[a]))$Año)+1
        APY = rango
        
        #------------------
        # 7) APjY - Average journal publications by year
        #-------------------
        APjY = round(TPj/rango,1)
        
        
        #------------------
        # 8) APpY - Average journal publications by year
        #-------------------
        APpY = round(TPp/rango,1)
        
        
        #------------------
        # 9 NAY - Number of active years of publications
        #-------------------
        NAY = length(unique((data_db_tmp %>% filter(Nombre == academics[a]))$Año))
        
        #------------------
        # 10) PAY - Productivity per active year of publication
        #-------------------
        PAY = round(TP/NAY,1)
        
        
        #------------------
        # 11) TC - Total number of citations
        #-------------------
        TC = sum((data_db_tmp %>% filter(Nombre == academics[a]))$Citado.por)
        
        #------------------
        # 12) AC - Average citations by publication
        #-------------------
        AC = round(TC/TP,1)
        
        #------------------
        # 13) NCP - Number of cited publications
        #-------------------
        NCP = length(which((data_db_tmp %>% filter(Nombre == academics[a]))$Citado.por>0))
        
        #------------------
        # 14) PCP - Proportion of cited publications
        #-------------------
        PCP = round(NCP/TP,1)
        
        #------------------
        # 15) CPP - Citations per cited publication
        #-------------------
        CPP = round(TC/NCP,1)
        
        
        data_matrix = rbind(data_matrix,cbind(TP,TPj,TPp,Per_TPj,Per_TPp,APY,APjY,APpY,NAY,PAY,TC,AC,NCP,PCP,CPP))
        data_matrix[which(is.infinite(data_matrix))]=0
        data_matrix[which(is.na(data_matrix))]=0
        
      }
      
      
      data_matrix = data.frame(Academics=academics,data_matrix)
      data_matrix = data_matrix[order(data_matrix$TP,data_matrix$TPj,decreasing = T),]
      
      data_matrix = tbl_df(data_matrix)
      rownames(data_matrix) = NULL 
      
      datatable(data_matrix, class = 'display nowrap', options = list(pageLength = 20, dom = "Bfrtp",autoWidth = T,scrollX = TRUE, 
                                                                      columnDefs = list(list(width = '100%', targets = 1))))
      
    } # Fin if !=1
    
  }) #Fin observe DataTAble 1
  
  output$table_Indexes_collaboration = renderDataTable({
    
    #Filtro por años
    ini_year = as.numeric(input$slider_Indexes_main_years[1])
    last_year = as.numeric(input$slider_Indexes_main_years[2])
    data_db_tmp = data_db  %>% filter(Año %in% (ini_year:last_year))
    
    if (input$slider_Indexes_main_universities=="1")
    {  
      
      #------------------
      #1. Number of academics (NAc)
      #------------------
      NAc = rep(3,0)
      NAc[1] = length(unique((data_db_tmp %>% filter(Universidad=="UV"))$Nombre))
      NAc[2] = length(unique((data_db_tmp %>% filter(Universidad=="UdeC"))$Nombre))
      NAc[3] = length(unique((data_db_tmp %>% filter(Universidad=="USACH"))$Nombre))
      
      #------------------
      #2. Number of contributing authors (NCA)
      #------------------
      NCA = rep(3,0)
      NCA[1] = sum(str_count((data_db_tmp %>% filter(Universidad=="UV"))$Autores,",")+1)
      NCA[2] = sum(str_count((data_db_tmp %>% filter(Universidad=="UdeC"))$Autores,",")+1)
      NCA[3] = sum(str_count((data_db_tmp %>% filter(Universidad=="USACH"))$Autores,",")+1)
      
      #------------------
      #3. Average contributing authors (ANCA)
      #------------------
      ANCA = rep(3,0)
      ANCA[1] = round(mean(str_count((data_db_tmp %>% filter(Universidad=="UV"))$Autores,",")+1),1)
      ANCA[2] = round(mean(str_count((data_db_tmp %>% filter(Universidad=="UdeC"))$Autores,",")+1),1)
      ANCA[3] = round(mean(str_count((data_db_tmp %>% filter(Universidad=="USACH"))$Autores,",")+1),1)
      
      #------------------
      #4. Co-authored publications (CA)
      #------------------
      CA = rep(3,0)
      CA[1] = length(which((str_count((data_db_tmp %>% filter(Universidad=="UV"))$Autores,",")+1)>1))
      CA[2] = length(which((str_count((data_db_tmp %>% filter(Universidad=="UdeC"))$Autores,",")+1)>1))
      CA[3] = length(which((str_count((data_db_tmp %>% filter(Universidad=="USACH"))$Autores,",")+1)>1))
      
      #------------------
      #5. Percentage of co-authored publications (CA)
      #------------------
      Per_CA = rep(3,0)
      Per_CA[1] = round((CA[1]/length(which((str_count((data_db_tmp %>% filter(Universidad=="UV"))$Autores,",")+1)>0)))*100,1)
      Per_CA[2] = round((CA[2]/length(which((str_count((data_db_tmp %>% filter(Universidad=="UdeC"))$Autores,",")+1)>0))*100),1)
      Per_CA[3] = round((CA[3]/length(which((str_count((data_db_tmp %>% filter(Universidad=="USACH"))$Autores,",")+1)>0)))*100,1)
      
      indexes = c("Number of academics","Number of contributing authors","Average contributing authors","Co-authored publications",
                  "Percentage of co-authored publications")
      data_matrix = rbind(NAc,NCA,ANCA,CA,Per_CA)
      colnames(data_matrix)=c("UV","UdeC","USACH")
      rownames(data_matrix)=indexes
      data_matrix[which(is.infinite(data_matrix))]=0
      data_matrix[which(is.na(data_matrix))]=0
      data_matrix = tbl_df(data_matrix)
      rownames(data_matrix) = indexes
      
      
      datatable(data_matrix, class = 'display', options = list(pageLength = 20, dom = 'Brt', autoWidth = TRUE, ordering = F))
      
    } else #Fin universidad = 1
    {
      if (input$slider_Indexes_main_universities=="2"){university="UV"}
      if (input$slider_Indexes_main_universities=="3"){university="UdeC"}
      if (input$slider_Indexes_main_universities=="4"){university="USACH"}
      
      data_db_tmp = data_db_tmp  %>% filter(Universidad==university)
      data_matrix=NULL
      
      
      academics = unique(data_db_tmp$Nombre)
      if (length(academics)==0) {academics="There were no academics during the selected period"}
      
      for (a in 1:length(academics))
      {
        #-------------------
        # 1) TP - Total publications
        #-------------------
        TP = nrow(data_db_tmp %>% filter(Nombre == academics[a]))
        
        #------------------
        #1. Number of academics (NAc)
        #------------------
        NAc = length(unique((data_db_tmp %>% filter(Nombre == academics[a]))$Nombre))
        
        #------------------
        #2. Number of contributing authors (NCA)
        #------------------
        NCA = length(unique(unlist(str_split((data_db_tmp %>% filter(Nombre == academics[a]))$Autores,","))))
        
        #------------------
        #3. Average contributing authors (ANCA)
        #------------------
        ANCA = round(mean(str_count((data_db_tmp %>% filter(Nombre == academics[a]))$Autores,",")+1),1)
        
        #------------------
        #4. Co-authored publications (CA)
        #------------------
        CA = length(which((str_count((data_db_tmp %>% filter(Nombre == academics[a]))$Autores,",")+1)>1))
        
        #------------------
        #5. Percentage of co-authored publications (CA)
        #------------------
        Per_CA = round((CA/length(which((str_count((data_db_tmp %>% filter(Nombre == academics[a]))$Autores,",")+1)>0)))*100,1)
        
        
        data_matrix = rbind(data_matrix,cbind(TP,NAc,NCA,ANCA,CA,Per_CA))
        data_matrix[which(is.infinite(data_matrix))]=0
        data_matrix[which(is.na(data_matrix))]=0
        
      }
      
      data_matrix = data.frame(Academics=academics,data_matrix)
      data_matrix = data_matrix[order(data_matrix$TP,data_matrix$NCA,decreasing = T),]
      
      data_matrix = tbl_df(data_matrix)
      rownames(data_matrix) = NULL 
      
      datatable(data_matrix, class = 'display nowrap', options = list(pageLength = 20, dom = "Bfrtp",autoWidth = T,scrollX = TRUE, 
                                                                      columnDefs = list(list(width = '100%', targets = 1))))
      
    } #Fin else
    
  }) #Fin observe DataTAble 2
  
} # Fin funcion backend_indexes