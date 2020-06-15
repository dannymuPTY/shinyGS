########################################################################################
#Written by Danny Murillo          
#Written by UTP-Ridda2
#Version: 4.0 RC                                         
#License: Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)  
#Date: 2018/10/01
########################################################################################

#-----------------------------------------------------
#----------------funciones en R------------------------
#------------------------------------------------------

PubGS_library<-function(){
  # INSTALAR PAQUETES DE FUNCIONES
  #-----------------------------------------
  # Lista de paquetes de funciones a instalar
  packages = c("xml2","rvest","plyr", "wordcloud","tm","NLP","dplyr","curl", "ggplot2","stringr","tidyverse","purrr","rlang")
  
  # Instala los paquetes sinï¿½ los tienes instalados
  .inst <- packages %in% installed.packages()
  if(length(packages[!.inst]) > 0) install.packages(.packages[!.inst])
  
  # CARGAR PAQUETES O CREAR FUNCIONES
  #-----------------------------------------
  # Carga los paquetes sinï¿½ los tienes cargados
  lapply(packages, require, character.only=TRUE)
  #-----------------------------------------
  
}


#----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------
PubGS_publications_get<-function(id_user){
  #
  #/citations?user=w7Wc8w4AAAAJ&hl=es
  #id_user<-"fRRMQjoAAAAJ"
  #id_user<-"OhoCAoAAAAJ"
  #id_user<-"0-Gc2aEAAAAJ"
  # leer web  de perfil 
  
 # t <- proc.time() # Inicia el cronómetro
  
  
  
  #url="https://scholar.google.es/citations?hl=es&view_op=search_authors&mauthors=idiap.gob.pa&btnG="
  url_google<-"https://scholar.google.com"
  url<-paste(url_google,"/citations?user=",id_user,"&hl=es")
  url <-gsub(" ", "",url, fixed=TRUE)
  perfil_PB <- read_html(url)
  data_PB<-html_nodes(perfil_PB,"#gsc_a_b > tr")
  data_PB_<-html_nodes(perfil_PB,"#gsc_a_b > tr>td")
  
  num<-length(data_PB)
  num_<-length(data_PB_)
  publicaciones_GS<-data.frame()
  publicaciones_GS_total<-data.frame()
  #x<-12
  for (x in 1:num) {
    #print(x)
    title<-xml_text(xml_nodes(data_PB[x],"td.gsc_a_t a"))
    title_url<-xml_attr(xml_nodes(data_PB[x],"td.gsc_a_t a"), "data-href")
    cita<-xml_text(xml_nodes(data_PB[x],"td.gsc_a_c a"))
    cita<-ifelse(nchar(cita[1])>0,cita[1],NA)
    
    #datos de coautores y nombre de la revista
    coautores<-xml_text(xml_nodes(data_PB[x],"td.gsc_a_t div:nth-child(2)"))
    coautores<-ifelse(nchar(coautores)>0, coautores, NA)
    
    revistaName<-xml_text(xml_nodes(data_PB[x],"td.gsc_a_t div:nth-child(3)"))
    revistaName<-ifelse(nchar(revistaName)>0, revistaName, NA)
    
    #-------------------------------
    cita_no1<-xml_attr(xml_nodes(data_PB[x],"td.gsc_a_c a"),"class")
    cita_no2<-length(as.character(grep("gsc_a_acm",cita_no1)))
    #cita_no<-is.na(strtoi(cita_no2))
    
    if(cita_no2==0){
      cita_no<-0
    }else{
      cita_no<-1
    }
    #print(x)
    cita_url<-xml_attr(xml_nodes(data_PB[x],"td.gsc_a_c a"), "href")
    cita_url<-ifelse(nchar(cita_url[1])>0, cita_url[1], NA)
    
    anio<-xml_text(xml_nodes(data_PB[x],"td.gsc_a_y"))
    anio<-ifelse(nchar(anio)>0, anio,NA)
    
    publicaciones_GS<-data.frame(title,title_url,cita,cita_no, cita_url,coautores,revistaName,anio)
    publicaciones_GS_total<-rbind.data.frame(publicaciones_GS_total,publicaciones_GS)
    
  }
  #View(publicaciones_GS_total)
  #8. Leer enlaces de boton de pÃ¡gina siguiente de datos - PAGINACIÃ“N
  count<-0
  contar<-1
  
  repeat{
    
    botones <- html_nodes(perfil_PB,"button")
    boton_paginacion<-botones[length(botones)]
    
    #9. VALIDAR si es la ultima pÃ¡gina de la afiliacion
    verifica<-length(grep("disabled",boton_paginacion))
    
    #10. No es necesario Esta linea de impresiÃ³n
    print(paste("page", " " , contar))
    
    
    if(verifica==0){
      count<-count+20
      contar<-contar+1
      perfil_PB_next <- paste(url,"&cstart=",count,"&pagesize=20")
      perfil_PB_next <-gsub(" ", "",perfil_PB_next, fixed=TRUE)
      url<-perfil_PB_next
      perfil_PB <- read_html(url)
      data_PB<-html_nodes(perfil_PB,"#gsc_a_b > tr")
      data_PB_<-html_nodes(perfil_PB,"#gsc_a_b > tr>td")
      #data_PB <-append(data_PB,data_PB_new)
      
      #almacenar datos de publicaciones
      num<-length(data_PB)
      num_<-length(data_PB_)
      
      #print(paste("num=",num, "num_",num))
      #x<-178
      if(num_!=1){
        #print("entro")
        for (x in 1:num) {
          title<-xml_text(xml_nodes(data_PB[x],"td.gsc_a_t a"))
          
          title_url<-xml_attr(xml_nodes(data_PB[x],"td.gsc_a_t a"), "data-href")
          
          cita<-xml_text(xml_nodes(data_PB[x],"td.gsc_a_c a"))
          cita<-ifelse(nchar(cita[1])>0,cita[1],NA)
          #verificar si citaciones son eliminadas
          cita_no1<-xml_attr(xml_nodes(data_PB[x],"td.gsc_a_c a"),"class")
          cita_no2<-length(as.character(grep("gsc_a_acm",cita_no1)))
          #cita_no<-is.na(strtoi(cita_no2))
          
          if(cita_no2==0){
            cita_no<-0
          }else{
            cita_no<-1
          }
          
          cita_url<-xml_attr(xml_nodes(data_PB[x],"td.gsc_a_c a"), "href")
          cita_url<-ifelse(nchar(cita_url[1])>0, cita_url[1], NA)
          
          anio<-xml_text(xml_nodes(data_PB[x],"td.gsc_a_y"))
          anio<-ifelse(nchar(anio)>0, anio,NA)
          #datos de coautores y nombre de la revista
          coautores<-xml_text(xml_nodes(data_PB[x],"td.gsc_a_t div:nth-child(2)"))
          coautores<-ifelse(nchar(coautores)>0, coautores, NA)
          
          revistaName<-xml_text(xml_nodes(data_PB[x],"td.gsc_a_t div:nth-child(3)"))
          revistaName<-ifelse(nchar(revistaName)>0, revistaName, NA)
          #publicaciones_GS<-data.frame(as.character(title),title_url,is.nan(as.integer(cita)), cita_url, is.nan(as.integer(anio)))
          publicaciones_GS<-data.frame(title,title_url,cita,cita_no, cita_url, coautores,revistaName,anio)
          publicaciones_GS_total<-rbind.data.frame(publicaciones_GS_total,publicaciones_GS)
        }
      }
      
      #length(data_PB)
    }else{
      break
    } 
    
  }# FIN DE REPEAT
  
  names(publicaciones_GS_total)<-c("Titulo","Titulo_url", "citas" ,"cita_no", "citas_url","Coautores","RevistaName","anio")
  publicaciones_GS_total1<-publicaciones_GS_total[publicaciones_GS_total$citas!="*",]
  publicaciones_GS_total1$citas<-as.character(publicaciones_GS_total1$citas)
  #is.na(strtoi(publicaciones_GS_total1$citas))
  nume<-nrow(publicaciones_GS_total1)
  #publicaciones_GS_total2<-publicaciones_GS_total1[!is.na(publicaciones_GS_total1$citas),]
  #publicaciones_GS_total2<-publicaciones_GS_total2[publicaciones_GS_total2$cita_no==0,]
  #sum(publicaciones_GS_total1$citas)
  #View(publicaciones_GS_total)
  #publicaciones_GS_total1$citas<-as.numeric(as.character(publicaciones_GS_total2$citas))
  
  #str(publicaciones_GS_total)
  publicaciones_GS_total$citas<-as.integer(as.character(publicaciones_GS_total$citas)) 
  publicaciones_GS_total$anio<-as.integer(as.character(publicaciones_GS_total$anio)) 
  
  publicaciones_GS_total$Titulo<-as.character(publicaciones_GS_total$Titulo)
  publicaciones_GS_total$Titulo_url<-as.character(publicaciones_GS_total$Titulo_url)
  publicaciones_GS_total$citas_url<-as.character(publicaciones_GS_total$citas_url)
  publicaciones_GS_total$Coautores<-as.character(publicaciones_GS_total$Coautores)
  publicaciones_GS_total$RevistaName<-as.character(publicaciones_GS_total$RevistaName)
 #for (x in 1:nume) {
  #  cita_<-publicaciones_GS_total1$citas[x]
  #  print(paste(x," ",cita_))
  #  if(is.na(strtoi(cita_))){
  #    cita_<-0
  #    publicaciones_GS_total1$citas[x]<-as.numeric(cita_)
  #  }else{
  #    publicaciones_GS_total1$citas[x]<-as.numeric(cita_)
  #  }
  #}
  

   
  
  return(publicaciones_GS_total)
  #str(publicaciones_GS_total1)
  
  
} #fin de funcion



#----------------------------------------------------------------
#----------------------------------------------------------------
#-------Función de nombre y afiiacion del perfil ----------------
#----------------------------------------------------------------

PubGs_afiliacion<-function(url_GS){
  #url_GS<-"https://scholar.google.com/citations?hl=es&user=BJFarhcAAAAJ"
  
  google_url <- read_html(url_GS)
  
  google_autor <- html_text(html_nodes(google_url,"#gsc_prf_in"))
  google_afiliacion <- html_text(html_nodes(google_url,"#gsc_prf_i > div:nth-child(2)"))
  #google_afiliation <- html_text(html_nodes(google_url,"h2.gsc_authors_header"))
  #afiliation_num_char <- nchar(google_afiliation)
  #afiliation_num_char1 <- (nchar(html_text(html_nodes(google_url,"h2.gsc_authors_header a"))))+1
  #afiliation<-substr(google_afiliation,1,afiliation_num_char-afiliation_num_char1)
  
  afiliation <- list(google_autor, google_afiliacion)
  
  return(afiliation)
}


#------------------------------------------------------------------------------
#------------ Extraer listado de investigadores de Google Scholar---------------


PubGS_research <- function(url_GS){
  
  #url_GS<-"https://scholar.google.es/citations?hl=es&view_op=search_authors&mauthors=idiap.gob.pa&btnG="
  #url_GS<-"https://scholar.google.es/citations?view_op=view_org&org=4736061867397421563&hl=es&oi=io"
  #1. Leer contenido de la pÃ¡gina de GOOGLE SCHOLAR  - AfiliaciÃ³n
  google_url <- read_html(url_GS)
  #2. Seleccionar enlaces con ID de usuarios de GOOGLE SCHOLAR 
  google_url_perfiles <- html_nodes(google_url,"div.gsc_1usr")
  
  #validar paginacion
  
  #botones_paginacion<-html_nodes(google_url,"button")
  #boton_ultimo<-botones_paginacion[length(botones_paginacion)]
  #pagina<-length(boton_ultimo)
  botones_paginacion<-length(html_nodes(google_url,"#gsc_authors_bottom_pag > div > span") %>% html_text())
  pagina<-ifelse(botones_paginacion==0, 0, 1)
  
  cantidad<-length(google_url_perfiles)
  google_utp_id_user1<-data.frame()
  google_utp_id_userF<-data.frame()
  google_utp_id_user<-data.frame()
  
  
  #3. Extraer datos de Nombre, Palabras clabes, hindex  de cada perfil
  #y<-11
  for (y in 1:cantidad) {
    #print(y)
    #4.Bloque de datos de cada perfil en GS
    google_UTP_href1<-html_nodes(google_url_perfiles[y],"h3 a")
    #print( google_UTP_href1)
    # numero de citaciones por autor
    google_UTP_citas<-html_text(html_nodes(google_url_perfiles[y],"div.gs_ai_cby"))
    cadena_cita<-nchar(google_UTP_citas)

    if( (length(cadena_cita) > 0) & (cadena_cita>1))
    {
      separar_citas <- strsplit(google_UTP_citas, split=" ")
      separar_vector <-unlist(separar_citas)
      num_vector<-length(separar_vector)
      cites<-separar_vector[num_vector]
#      print(cites)
    }else
    {
      cites<-0
    }
    #5. Seleccionar solo la cadena de HREF del ID
    google_utp_id1<-html_attr(google_UTP_href1,"href")
    #EXTRAER DATO DEL ID CAPTURADO
    #patron de codigo scholar
    #pattern_ <- "([a-zA-Z0-9]{10,12})"
    pattern_ <- "([a-zA-Z0-9_-]{10,12})"
    codID<-str_extract(google_utp_id1, pattern = pattern_)
    
    
    google_palabras1<-html_text(html_nodes(google_url_perfiles[y],"div.gs_ai_int >a"))
    google_palabras2<-toString(google_palabras1)
    #6.Crear tabla con id de usuario y nombre
    google_utp_id_user1<-data.frame(html_text(google_UTP_href1),as.integer(cites),as.character(google_palabras2),google_utp_id1,codID)
    google_utp_id_userF<-rbind.data.frame(google_utp_id_userF,google_utp_id_user1)
  } #FIN DE CICLO FOR
  
  #7.cambiar etiquetas de las tablas 
  names(google_utp_id_userF)<-c("Nombre","citas", "word_key" , "url_user","Id_user")
  google_utp_id_user<-google_utp_id_userF
  
  #
  #FIN
  #EXTRACCIÃ“N DE DATOS DE LA PRIMERA PÃGINA DE GS
  #
  
  
  #-------------------------------------------------
  #----------REPETIR CICLO DE EXTRACCIï¿½N DE DATOS
  #-------------------------------------------------
  verifica<-10
  
  
  #
  #INICIO
  #EXTRACCIÃ“N DE DATOS DE PAGINACIÃ“N
  #validar si afiliaciï¿½n solo tiene una pï¿½gina   
  #
  if(pagina==1)
  {
    
    repeat{
      
      #8. Leer enlaces de boton de pÃ¡gina siguiente de datos - PAGINACIÃ“N
      botones_paginacion<-html_nodes(google_url,"button")
      boton_ultimo<-botones_paginacion[length(botones_paginacion)]
      
      #9. VALIDAR si es la ultima pÃ¡gina de la afiliacion
      verifica<-length(grep("disabled",boton_ultimo))
      
      #10. No es necesario Esta linea de impresiÃ³n
      print(verifica)
      
      #11. Validar si es la Ultima pÃ¡gina de El perfil de AfiliaciÃ³n
      
      if(verifica==0){
        
        #12. extraer datos del boton de paginacion al siguiente bloque de ususarios
        boton_next<-html_attr(botones_paginacion[length(botones_paginacion)],"onclick")
        
        #13 substraer cadena de la citacion a la siguiente pagina - PAGINACIÃ“N
        boton_next_cita<-substr(boton_next,18,151)
        
        #14. Limpiar enlace que se extrajo del botÃ³n de paginaciÃ³n
        enlace_cita_paginacion<-gsub("\\x3d", "=", boton_next_cita, fixed=TRUE)
        enlace_cita_paginacion<-gsub("\\x26", "&", enlace_cita_paginacion, fixed=TRUE)
        
        #15. unir enlace de cita con enlace de google scholar
        url_google<-"https://scholar.google.es"
        url_google_utp_paginacion<-paste(url_google,enlace_cita_paginacion,collapse = NULL)
        
        #16 Eliminar espacios en blanco de URL completa
        url_google_utp_paginacion<-gsub(" ", "", url_google_utp_paginacion, fixed=TRUE)
        
        #17 Asignar url de PaginaciÃ³n a variable para repetir Ciclo de SCRAPER
        url_GS_UTP<-url_google_utp_paginacion
        
        #18.Leer URL para Scrapear datos de afiliation en GS
        google_url<- read_html(url_GS_UTP)
        
        
        #19. Seleccionar bloque de datos con ID de usurios de GOOGLE SCHOLAR - UTP  
        google_url_perfiles <- html_nodes(google_url,"div.gsc_1usr")
        
        #20. Leer cantidda de bloques de pergil que hay en cada pÃ¡gina
        cantidad<-length(google_url_perfiles)
        google_utp_id_user1<-data.frame()
        google_utp_id_userF<-data.frame()
        
        #21. Extraer datos por cada Bloque de perfil en GS
        y<-1
        for (y in 1:cantidad) {
          #22. Extraer datos por cada Bloque que se encuenre en las etiquetas H3 a
          google_UTP_href1<-html_nodes(google_url_perfiles[y],"h3 a")
          #print( google_UTP_href1)
          # numero de citaciones por autor
          google_UTP_citas<-html_text(html_nodes(google_url_perfiles[y],"div.gs_ai_cby"))
          cadena_cita<-nchar(google_UTP_citas)
          #print(google_UTP_citas)
          if( (length(cadena_cita) > 0) & (cadena_cita>1))
          {
            separar_citas <- strsplit(google_UTP_citas, split=" ")
            separar_vector <-unlist(separar_citas)
            num_vector<-length(separar_vector)
            cites<-separar_vector[num_vector]
          }else
          {
            cites<-0
          }
          
          
          
          #23. Seleccionar solo la cadena del enlace HREF
          google_utp_id1<-html_attr(google_UTP_href1,"href")
          #pattern_ <- "([a-zA-Z0-9]{10,12})"
          pattern_ <- "([a-zA-Z0-9_-]{10,12})"
          codID<-str_extract(google_utp_id1, pattern = pattern_)
          
          
          google_palabras1<-html_text(html_nodes(google_url_perfiles[y],"div.gs_ai_int >a"))
          google_palabras2<-toString(google_palabras1)
          #6.Crear tabla con id de usuario y nombre
          google_utp_id_user1<-data.frame(html_text(google_UTP_href1),as.integer(cites),as.character(google_palabras2),google_utp_id1,codID)
          google_utp_id_userF<-rbind.data.frame(google_utp_id_userF,google_utp_id_user1)
          
        }#FIN DE CICLO FOR
        
        #25.cambiar etiquetas de las tablas 
        names(google_utp_id_userF)<-c("Nombre","citas","word_key" , "url_user","Id_user")
        #26 unir data frame
        google_utp_id_user<-rbind.data.frame(google_utp_id_user,google_utp_id_userF)
        
        
      } #Fin IF VALIDAR
      
      
      #27. verificar si es la ultima paginacion
      if(verifica==0){
        #print("next")
      }else{
        #print("finish") 
        break
      } 
      
    }# FIN DE REPEAT
    
  } #fin de if para validar si tiene una pï¿½gina
  
  #
  #FIN
  #EXTRACCIÃ“N DE DATOS DE PAGINACIÃ“N
  #
  
  #28. Retornar datos a un data frame , si ha sido asigando
  return(google_utp_id_user)
  
} # FIN DE FUNCION




#----------------------------------------------------------
#------------ Unir todo los perfiles y su valor de hindex
#----------------------------------------------------------

PubGS_hindex<-function(google_utp_id_user){
  
  #1. Contar numero de filas del data frame de listado de Perfil
  #google_utp_id_user<-GS_UTP2
  gs_filas=nrow(google_utp_id_user)
  gs_publi_index<-data.frame()
  gs_publica_index<-data.frame()
  hindex<-data.frame()
  tempo<-list()
  url_user_hi<-NA
  
  #2. Extraer datos de citaciones y hindex de cada perfil
  url_google<-"https://scholar.google.es"
  
  for(x1 in 1:gs_filas)  {  
    
    #x1<-77
    
    #3. Imprimir nombre de usuario - OPCIONAL
    print(paste(x1,".",as.character(google_utp_id_user$Nombre[x1])))
    
    #prueba - borrar
    #formulario_google_scholar_sni_2020$perfil_GS[10]
    #formulario_google_scholar_sni_2020$url_user[10]
    #url_user_hi<-formulario_google_scholar_sni_2020$url_user[10]
    #4. Asignar id de usuario para extraer datos
    url_user_hi<-google_utp_id_user$url_user[x1]
    url_google_hpa<-paste(url_google,url_user_hi,collapse = NULL)
    
    #5. Eliminar espacion en blanco de URL completa
    url_google_hpa<-gsub(" ", "", url_google_hpa, fixed=TRUE)
    url_GS_indexh<-url_google_hpa
    
    try (
    #7.Scrapear afiliation de UTP en google
    google_indexh<- read_html(url_GS_indexh)
    )
    
    #read_html("https://scholar.google.com/citations?user=QOEUMiYAAAAJ&hl=es")
    
    #8. Seleccionar enlaces con ID de usuarios de GOOGLE SCHOLAR - UTP  
    google_index_tabla<-html_nodes(google_indexh,"#gsc_rsb_st")
    vacio<-length(google_index_tabla)
    
    if(vacio!=0){
      google_index_tablad_datos<-html_nodes(google_index_tabla,"td.gsc_rsb_std")
      canti<-length(google_index_tablad_datos)
      
      #9. Extraer Palabras clabes de cada perfil
      for (num in 1:canti) {
        tempo[num]<-as.numeric(html_text(google_index_tablad_datos[num]));
        
      }
    }else{
      num<-1:6
      tempo[num]<-list(0)
    }
    
    
    #10. REnombrar nombres de etiqueta de la tabla
    hindex<-data.frame(c(tempo))
    names(hindex)<-c("cita","cita_2015","hindex","hindex_2015","index10","indexi10_2015")
    gs_publi_index<-google_utp_id_user[x1,]
    gs_publi_index<-data.frame(gs_publi_index,c(hindex))
    
    #11. Unir datos extraidos
    gs_publica_index<-rbind.data.frame(gs_publica_index,gs_publi_index)
    
  } # FIN DE FOR PRINCIPAL
  
  
  return(gs_publica_index)  
  
} #FIN DE FUNCION HINDEX



#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#------------------- unir publicaiones de afiliacion ----------------------
#--------------------------------------------------------------------------

#google_utp_id_user<-Gs_total_panama
#x<-74

PubGS_publications<-function(google_utp_id_user){
  
  # google_utp_id_user<-GS_UTP
  #1. Contar numero de filas del data frame de perfiles
  gs_filas=nrow(google_utp_id_user)
  
  #2. Inicializat data frame temporal
  gs_publicaciones<-data.frame()
  #x<-1
  #3. Unir todo las publicaciones de usuarios de Google scholar en el data frame "google_UTP1"
  for(x in 1:gs_filas){  
    
    #4 Extraerla spublicaciones por cada perfil  de GS
    gs_tem<-data.frame()
    
    gs_tem=PubGS_publications_get(as.character(google_utp_id_user$Id_user[x])) 
    tamano_arreglo<-nrow(gs_tem)
    print(paste(x,"."," ",google_utp_id_user$Id_user[x]," / ",tamano_arreglo))
    
    id_user_<-google_utp_id_user$Id_user[x]
    id_user<-rep(id_user_,tamano_arreglo)
    gs_tem<-data.frame(gs_tem,id_user)
    
    #5. Añadir nombre a cada Perfil de GS
    Nombre_userr_<-google_utp_id_user$Nombre[x]
    Nombre_user<-rep(Nombre_userr_,tamano_arreglo)
    gs_tem<-data.frame(gs_tem,Nombre_user)
    
    #6. Agrupar publicaciones en un Datframe
    gs_publicaciones<-rbind.data.frame(gs_publicaciones,gs_tem)
    
  }#FIN DE FOR
  return(gs_publicaciones)  
  
  
}#FIN DE FUNCION HINDEX



#-------------------------------------------------------------
#--------------------------------------------------------------
#----------- mostar publicaciones más citadas  -----------------
#--------------------------------------------------------------

PubGS_citas<-function(data_publicaciones, num){
  
  data_publicaciones$citas<-as.integer(data_publicaciones$citas)
  #x<-arrange(data_publicaciones, desc(citas))
  #head(x)
  #citas<-arrange(data_publicaciones, desc(citas)) %>%  rename(autor = Nombre_user )
  citas<-citas[1:num,-c(2,4,5)]
  return(citas)
  
}


#-------------------------------------------------------------
#--------------------------------------------------------------
#----------- extraer publicaciones individual -----------------
#--------------------------------------------------------------


PubGS_perfil<-function(url){
  
  n<-nchar(url)
  
  # verificar tamaño del patron 
  if(n>12){
    
    #patron de codigo scholar
    #pattern_ <- "([a-zA-Z0-9]{10,12})"
    pattern_ <- "([a-zA-Z0-9_-]{10,12})"
    cod<-str_extract(url, pattern = pattern_)
    
  }else{
    cod<-url
  }
  
  t <-  proc.time()
  print(paste("Inicia Tiempo=", t ))
  publica<-PubGS_publications_get(cod)
  # Detiene el cronómetro
  print(paste("finaliza tiempo individual", proc.time()-t ))
  
  return(publica)
}



#-------------------------------------------------------------
#--------------------------------------------------------------
#----------- publicaciones agrupadas po el filtro de año -----------------
#--------------------------------------------------------------



PubGs_agrupar_perfiles<-function(data_publicaciones,anio1){
  
  data_publicaciones_<-data_publicaciones
  data_publicaciones_$anio<-as.numeric(as.character(data_publicaciones_$anio))
  data_publicaciones_$citas<-as.numeric(as.character(data_publicaciones_$citas))
  
  data_publicaciones_anio<- data_publicaciones_ %>% filter(anio >= anio1)
  data_publicaciones_grupo <- data_publicaciones_anio %>% group_by(Nombre_user) %>% summarise(citas = sum(na.omit(citas)), publica_anio = n())
  #data_publicaciones_anio %>% group_by(Nombre_user) %>% summarise(citas = sum(citas), n = n())
  data_publicaciones_grupo <- rename(data_publicaciones_grupo, Nombre = Nombre_user)
  data_publicaciones_grupo <- arrange(data_publicaciones_grupo, desc(publica_anio))
  
  return(data_publicaciones_grupo)
  
}

#write.csv(data_publicaciones_grupo,paste("data_publicaciones_grupo_","utp_",format(Sys.Date(),"%Y_%m_%d") , ".csv"))

#--------------------------------------------------
#--------------------------------------------------
#--------------------------------------------------
#----------- funcion para ordenarcitas-------------






PubGS_totalCitas<- function(gs_publicaciones,orden){
  #agrupar citas por nombre
  x<-is.na(gs_publicaciones$citas)
  vacios<-which(x==TRUE)
  gs_publicaciones$citas[vacios]<-0
  gs_publicaciones$citas<-as.integer(gs_publicaciones$citas)
  #str(gs_publicaciones)
  
  citas_user<-ddply(gs_publicaciones,c("id_user","Nombre_user"), summarise, total_citas = sum(citas),total_publi = length(Titulo))
  citas_user$Nombre_user<-as.character(citas_user$Nombre_user)
  #citas_user<-citas_user[-1]
  
  if(orden=="citas"){
    citas_user<-citas_user[rev(order(citas_user$total_citas)),]
  }else if(orden=="publi"){
    citas_user<-citas_user[rev(order(citas_user$total_publi)),]
  }else if(orden=="name"){
    citas_user<-citas_user[(order(citas_user$Nombre_user)),]
  }else{
    citas_user<-citas_user
  }
  
  return(citas_user)
  
} #fin de funcion


PubGS_GraficaCitas<-function(gs_publicaciones,afiliation){
  
  
  #limpiar datos
  
  x<-is.na(data_publicaciones$citas)
  vacios<-which(x==TRUE)
  data_publicaciones$citas[vacios]<-0
  data_publicaciones$citas<-as.integer(data_publicaciones$citas)
  
  citas_anio<-ddply(data_publicaciones,c("anio"), summarise, total_citas = sum(citas),total_publi = length(Titulo))
  citas_anio<-citas_anio[-1,]
  
  #write.csv(citas_anio,"citas-anio-GS-03-08-2019.csv")
  #calculos
  citas_anio$anio<-as.numeric(as.character(citas_anio$anio))
  citas_anio<-citas_anio[order(citas_anio$anio),]
  
  plot(citas_anio$anio,citas_anio$total_citas, col="red", pch =20 , type ="o", lty="dotted",main =paste("Citas y Publicaciones por anio"), xlab="Anios" , ylab="citas y Publicaciones")
  points (citas_anio$anio,citas_anio$total_publi, pch =18 , col=" blue", type ="o", lty="dotted")
  
  anioMax<-citas_anio$anio[citas_anio$total_citas==max(citas_anio$total_citas)]
  citasMax<-max(citas_anio$total_citas)
  
  anioPuMax<-citas_anio$anio[citas_anio$total_publi==max(citas_anio$total_publi)]
  publiMax<-max(citas_anio$total_publi)
  
  points(anioMax,citasMax, pch =19 , col="black")
  text (anioMax,citasMax+25, labels =citasMax)
  
  points(anioPuMax,publiMax, pch =19 , col="black")
  text (anioPuMax,publiMax+25, labels =publiMax)
  
  legend("topleft", legend=c("Citas", "Publicaciones", "Valores Maximos"), col=c("red", "blue","Black"), lty=1:3, cex=0.8)
}




PubGS_hindexUser<-function(url_GS_indexh){
  
  google_indexh<- read_html(url_GS_indexh)
  
  #3. Seleccionar enlaces con ID de usurios de GOOGLE SCHOLAR - UTP  
  google_index_nombre<-html_text(html_nodes(google_indexh,"#gsc_prf_in"))
  
  # Seleccionar afiliaciï¿½n
  google_afiliacion<-html_text(html_nodes(google_indexh,"#gsc_prf_i > div:nth-child(3) > a"))
  if(length(google_afiliacion)==0){
    google_afiliacion<-NA
  }
  #seleccionar url del perfil 
  google_url<- html_attr(html_nodes(google_indexh,"#gsc_prf_ivh > a"),"href")
  if(length(google_url)==0){
    google_url<-NA
  }
  #4. extraer palabras del perfil
  google_palabras1<-html_text(html_nodes(google_indexh,"#gsc_prf_i > div:nth-child(4) > a"))
  google_palabras2<-toString(google_palabras1)
  
  google_index_palabras<-google_palabras2
  if(length(google_index_palabras)==0){
    google_index_palabras<-NA
  }
  #3. Seleccionar enlaces con ID de usurios de GOOGLE SCHOLAR - UTP  
  google_index_tabla<-html_nodes(google_indexh,"#gsc_rsb_st")
  
  google_index_tablad_datos<-html_nodes(google_index_tabla,"td.gsc_rsb_std")
  canti<-length(google_index_tablad_datos)+4
  
  hindex<-data.frame()
  tempo<-list()
  tempo[1]<-google_index_nombre
  tempo[2]<-google_afiliacion
  tempo[3]<-google_index_palabras
  tempo[4]<-google_url
  
  for (num in 5:canti) {
    tempo[num]<-as.numeric(html_text(google_index_tablad_datos[num-4]));
    
  }
  
  hindex<-data.frame(c(tempo))
  
  #hindex<-data.frame(tempo[1],tempo[2],tempo[3],tempo[4],tempo[5],tempo[6])
  names(hindex)<-c("Nombre","Afiliacion","word_key","Pagina principal","citaciones","cita_2011","hindex","hindex_2011","index10","indexi10_2011")
  
  
  return(hindex)
  
}

#--------------------------------------------------------------
#--------------------------------------------------------------
#tranformar palabras

nube_palabras<-function(data_hindex1){
  
  data_hindexp<-data_hindex1
  #trasnformar factor a caracter
  data_hindexp$word_key<-as.character(data_hindexp$word_key)
  
  nump<-nrow(data_hindexp)
  #inicializar data frame de palabras
  palabras_clabes<-data.frame()
  
  #agrupar lista de palabras de cada perfil en data frame
  for (x in 1:nump) {
    texto_split = strsplit(data_hindexp$word_key[x], split=",")
    texto_columnas = data.frame(unlist(texto_split))
    names(texto_columnas)<-"word_key"
    palabras_clabes<-rbind(palabras_clabes,texto_columnas)
  }
  
  #eliminar palabras duplicadas
  #nrow(palabras_clabes) #244
  palabras_clabesU<-unique(palabras_clabes)
  palabras_clabesUP<-unique(palabras_clabes)
  palabras_clabesU$word_key<-as.character(palabras_clabesU$word_key)
  
  #nrow(palabras_clabesU) #234
  #str(palabras_clabesU)
  
  #limpiar cadena de palabras
  palabras_clabesU<- tolower(palabras_clabesU)
  #palabras_clabesU<- removePunctuation(palabras_clabesU)
  palabras_clabesU<- removeWords(palabras_clabesU,c("and","los","de","y","las","la","el"))
  
  #generar nube de palabras
  wordcloud(palabras_clabesU, scale =c(3,0.5) , max.word =200 , rot.per =0.25, colors = brewer.pal (8, "Dark2"))
  
}

#-------------------------------------------------------------
#-------------------grafico perfil individual -----------------
#-----------------------------------------------------------
graficoIndividual <-function(google_url, afiliacion){
  #afiliacion <- PubGs_afiliacion(url)
  # usar dos variable colocar stat="identity"
  #google_url<-"https://scholar.google.com/citations?hl=es&user=vJ_Sk8EAAAAJ"
  #google_url <-"https://scholar.google.com/citations?user=hSDkDKoAAAAJ"
  google_url <- read_html(google_url)
  
  #extaer anio del grafico
  google_anio <- html_nodes(google_url,"#gsc_rsb_cit > div > div.gsc_md_hist_w > div>span")
  numAnio <- length(google_anio)
  numeros <- numAnio:1
  anioText <- html_text(google_anio[numeros])
  #cantidad de numeros
  google_num<-numAnio:1
  
  
  #buscar numeración de grafico
  google_numeracion <- html_nodes(google_url,"#gsc_rsb_cit > div > div.gsc_md_hist_w > div>a") %>% html_attr("style")
  numeracion_split <- strsplit(google_numeracion, split = ";")
  num <- length(numeracion_split)
  #x<-1
  numero <- character()
  num_inversa <- vector()
  for (x in 1:num) {
    lista <- unlist(numeracion_split[x])
    pattern_ <- "([0-9]{1,2})"
    num_inversa[x] <- str_extract(lista[4], pattern = pattern_) %>% as.numeric()
  }
  
  #buscar numero faltante para colocar valo de 0
  faltante<-  setdiff(google_num,num_inversa)
  
  #extaer citas del grafico
  google_data<- html_nodes(google_url,"#gsc_rsb_cit > div > div.gsc_md_hist_w > div>a>span")
  numData <- length(google_data)
  numerosD <- 1:numData
  dataText <- as.numeric(as.character(html_text(google_data[numerosD])))
  #invertir vector
  google_dataNum <- rev(dataText)
  #crear dataFrame nuevo de citas y numeraciones
  gooDataCitas <- data.frame("id"=num_inversa, "citas"=dataText)
  #añadir elemnto a vector
  #append(google_dataNum,2500)
  gooDataCitas_<- c(faltante,0)
  # verificar que hay numeros faltantes por año en el grafico
  if(length(faltante)>0){
    gooDataCitas <- rbind(gooDataCitas, gooDataCitas_)
  }
  gooDataCitas <-arrange(gooDataCitas,id)
  
  #---------------------------------------------------------------------------
  #unir en datFrame datos de anio y citas ordenados por numero y datos vacios
  #---------------------------------------------------------------------------
  gooDataCitas <- cbind(gooDataCitas, anioText)
  publicaGra <- data.frame(anio=gooDataCitas$anioText,citas=gooDataCitas$citas)
  
  return(publicaGra)
}

 #--------------------------------------------------
 #---------------- Generar grafica -----------------
 #--------------------------------------------------

Generagrafico <-function(publicaGra, afiliacion){
  #publicaGra <- publicaGra %>% rename(anio = anioText, citas = dataText)
  #publicaGra<- publica %>% group_bgooDataCitasy (anio) %>%  na.omit() %>% summarise(citas = sum(citas))
  #publicaGra$citas <- as.numeric(publicaGra$citas)
  #str(publicaGra)
  
  gg <- ggplot(publicaGra, aes(anio ,citas)) + 
    geom_bar(stat = "identity", fill = "#155d89") + 
    geom_text(aes(label = citas), vjust = -0.3, size = 5.5) +
    labs(title = enc2native("Gráfico de citaciones por año"), 
         subtitle =paste0("Perfil de",afiliacion[1], "\n",afiliacion[2]), 
         caption = "Data source: Google Scholar",
         x = enc2utf8("Año"), 
         y = enc2utf8("Citaciones")) +
    #theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    theme_gray() + theme(legend.position = "none") +
    theme(axis.text=element_text(size=17), #tamaño letra eje x y eje y
          #axis.title=element_text(size=12,face="bold", vjust = 0.3)) #tamaño letra etiquetas X / Y
          axis.title.x=element_text(size=17,face="bold", vjust = 0.3), #tamaño letra etiquetas X
          axis.title.y=element_text(size=17,face="bold", vjust = 3)) #tamaño letra etiquetas  Y
  #https://stackoverflow.com/questions/14942681/change-size-of-axes-title-and-labels-in-ggplot2
  
  #print("grafico")
  #data <- list(gg,publicaGra)
  #data[[2]]
  #as.data.frame(data[2])
  
  return(gg)
}



