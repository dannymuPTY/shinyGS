# Ejecutar la función  PubGS_library()  si no tiene instalado la siguientes librerias:
# xml2,rvest,plyr, wordcloud,tm,NLP,dplyr,curl, ggplot2,stringr,tidyverse,purrr,rlang
#PubGS_library() 

# Carga de paquetes ----


library(shiny)
library(shinyjs)
library(shinythemes)
library(stringr)
library(xml2)
library(ggplot2)
#library(rvest)
#library(tidyverse)
#library(DT)
#library(plyr)
#library(purrr)
#library(dplyr)



source("funciones-gs.R")

#---------------------------------------------------


ui <- fluidPage(theme = shinytheme("simplex"),
  
    fluidRow(
      column(width = 12, 
      titlePanel("Extraer datos de Perfil de Google Scholar")
      )
    ),
 
  mainPanel(
    textInput("caption", "Ingresar URL","", placeholder =  "https://scholar.google.com/citations?user=YNx08l0AAAAJ&hl=es&oi=ao", width = "100%"),
    radioButtons("escanear", "Tipo de Extracción:",
                 c("Publicaciones de perfil Individual" = "individual",
                   "Listado de perfiles Institucional" = "institucional",
                   "Publicaciones de perfiles Institucional" = "institucionalFull")),
    
    #seleccionar mostrar tabla de resultados
    checkboxInput("showTable", "Mostrar tabla de resultados",FALSE),
    
    #mostrar botones de extraer y descargar
    actionButton("extraerButton", "Scrapear", class = "extra"),
    tags$head(tags$style(".extra{background:#007bff;} .extra{color:#ffffff;} .extra{font-family: Courier New}")),
    tags$head(tags$style(".butt1{background:#fd7e14;} .butt1{color:#ffffff;} .butt1{font-family: Courier New; margin-left:10px;}")),
    tags$head(tags$style(".butt0{background:#28a745; color:#ffffff; font-family: Courier New; margin-left:10px;}")),
    tags$head(tags$style(".butt2{background:#39589b; color:#ffffff; font-family: Courier New; margin-left:10px;}")),
    tags$head(tags$style(".butt3{background:#39589b; color:#ffffff; font-family: Courier New; margin-left:10px;}")),
    
   #Establecer un output de texto con el valor de la variable var
    br(),
    br(),
    #pre(id = "console"),
    textOutput("var_tiempoExtra"),
    #verbatimTextOutput("console"),
    # mostrar nombre de perfil ------------------
    textOutput("var_descargar"),
    # mostrar numero de documentos extraidos
    textOutput("var_seleccionada"),
    #verbatimTextOutput("value"),
    br(),
    br(),
    plotOutput("grafico", width = "100%", height = "500px"),
    br(),
    br(),
     #imprimir resultados de la tabla
      #tableOutput("resultados")
      DT::dataTableOutput("resultados"),
    br(),
    br(),
    )
)
  


server <- function(input, output){
  
  # VARIABLE REACTIVA QUE ALMACENARÁ LAS PUBLICACIONES SCRAPE,ADAS
  scraper <- reactiveValues(Pgrafico = NULL, dataAnio = NULL, publicaciones = NULL, nume = NULL,
                            listaNum = NULL, afiliacion = NULL, tiempoExtra= NULL,  afiliacionT=NULL)
  
  #Tenemos que renderizar la variable de texto var_seleccionada
  output$var_seleccionada <- renderText(
    #paste("Has seleccionado:", input$caption)
    paste(scraper$nume)
  )
  
  output$var_descargar <- renderText(
    #paste("Has seleccionado:", input$caption)
    paste(scraper$tiempoExtra)
  )
  
  output$var_tiempoExtra<- renderText(
    #paste("Has seleccionado:", input$caption)
    paste(scraper$afiliacion[1])
  )
  
  # verificar si se selecciono scanear perfil individual / institucional / completo
  
  
  #observar evento del boton extraer
  observeEvent(input$extraerButton,{
    
    print(input$escanear)
    #inicia tiempo de scraping
    t <-  proc.time()
   afiliacionT <- PubGs_afiliacion(input$caption)
    
    showModal(modalDialog(
      title = "Mensaje",
      HTML(paste("<b>Extrayendo datos de Google Scholar.</b> <br>", "Perfil de ", afiliacionT[1], ", ", afiliacionT[2])),
      footer = actionButton("confirm", "Close"),
      easyClose = TRUE
    ))

    
    #withConsoleRedirect("console", {
      #scraper$nume <- "EXTRAYENDO REGISTROS"
      #scraper$publicaciones <- PubGS_perfil(input$caption)
      #scraper$publicaciones <- PubGS_research(input$caption)
   
      
      scraper$publicaciones <- switch(input$escanear, 
          individual =  PubGS_perfil(input$caption), 
          institucional = {listaPerfil <- PubGS_research(input$caption)
                                          PubGS_hindex(listaPerfil)},
          institucionalFull = {listaPerfil <- PubGS_research(input$caption)
                               listaNum <- PubGS_hindex(listaPerfil)
                               PubGS_publications(listaNum)}
          )
      
    #})
    
   
    
    #---------------- validar si se debe generar el boton de descargar perfiles full-------------------------
    if(input$escanear == "institucionalFull"){
      # insertar boton descargar publicaciones dinamicamente
      insertUI("#extraerButton", "afterEnd",
               downloadButton('downloadDataPe', 'Descargar perfiles CSV', class = "butt0"),
               tags$head(tags$style(".butt0{background:#FC0D00;} .butt0{color:#ffffff} .butt0{font-family: Courier New}"))
               )
      # insertar boton descagar publicaciones dinamicamente -----------------------------------------
      insertUI("#extraerButton", "afterEnd",
               downloadButton('downloadData', 'Descargar publicaciones CSV', class = "butt1"),
               tags$head(tags$style(".butt1{background:#042FB3;} .butt1{color:#ffffff;} .butt1{font-family: Courier New}"))
      )
      removeUI(selector = "#grafico")
    }
      
      #----------------- validar seleccion de opcion boton institucional -------------------------------------------
      if(input$escanear == "institucional"){
        # insertar boton descagar publicaciones dinamicamente -----------------------------------------
        insertUI("#extraerButton", "afterEnd",
                 downloadButton('downloadData', 'Descargar perfiles CSV', class = "butt1"),
                 tags$head(tags$style(".butt1{background:#042FB3;} .butt1{color:#ffffff;} .butt1{font-family: Courier New}"))
        )
        removeUI(selector = "#grafico")
      }
    
    #----------------- validar selccion de opcion boton individual -------------------------------------------
    if(input$escanear == "individual"){
    # insertar boton descagar publicaciones dinamicamente -----------------------------------------
    insertUI("#extraerButton", "afterEnd",
             downloadButton('downloadData', 'Publicaciones CSV', class = "butt1"),
             tags$head(tags$style(".butt1{background:#042FB3;} .butt1{color:#ffffff;} .butt1{font-family: Courier New}"))
             )
      
      insertUI("#downloadData", "afterEnd",
               downloadButton('downloadPlot', 'Gráfico Citas', class = "butt2"),
               tags$head(tags$style(".butt2{background:#39589b;} .butt2{color:#ffffff;} .butt2{font-family: Courier New}"))
      )
      
      insertUI("#extraerButton", "afterEnd",
               downloadButton('downloadDataCitas', HTML('Citas/año CSV'), class = "butt3"),
               tags$head(tags$style(".butt3{background:#042FB3;} .butt3{color:#ffffff;} .butt3{font-family: Courier New}"))
      )
      #-------------- extraer nombre del perfil de afiliación----------------
      afiliacion <- PubGs_afiliacion(input$caption)
      scraper$afiliacion <- paste("Perfil de ", afiliacion[1], ", ", afiliacion[2])
      print(scraper$afiliacion)
      #llamar grafico-----------------------
      #graficoI <- graficoIndividual(publicaciones, afiliacion)
     
      dataGrafico <- graficoIndividual(input$caption, afiliacion)
      Pgrafico <- Generagrafico(dataGrafico,afiliacion)
      #output$grafico <- renderPlot(graficoIndividual(input$caption, afiliacion))
      output$grafico <- renderPlot(Pgrafico)
      scraper$dataAnio <- as.data.frame(dataGrafico)
      scraper$Pgrafico <-  Pgrafico
      
    }
    
      
      
      #--------------- extraer numero de publicaciones ----------------------
      scraper$nume <- paste("Registros extraidos:",nrow(scraper$publicaciones))
      print(scraper$nume)
    
    # validar si se selecciono el checkbox de mostrar tabla
    # validar si se muestra la tabla con mÃ¡s de 1 resultado
    if (input$showTable == TRUE) {
      if (scraper$nume > 0) {
        print("dentro")
        
        # mostrar resultados en datatable dinamica cada 6 filas
        #output$resultados <- DT::renderDataTable(
         # scraper$publicaciones[,-c(2,4)],
         #options = list(pageLength = 6)
        #)
        
        output$resultados <- switch(input$escanear, 
                         individual = DT::renderDataTable(
                            scraper$publicaciones[,-c(2,4)],
                            options = list(pageLength = 6)), 
                         institucional = DT::renderDataTable(
                              scraper$publicaciones[,-c(2)],
                              options = list(pageLength = 10)),
                         institucionalFull = DT::renderDataTable(
                              scraper$publicaciones[,-c(2,4)],
                              options = list(pageLength = 15))
                         )
        
        #output$resultados <- renderTable(
        #  scraper$publicaciones[,-c(2,4)]
        #) pageLength = 15
      }
    }
      # tiempo final de scrapin institucional
      print(paste("Fin tiempo scraping", proc.time() - t ))
      scraper$tiempoExtra <- paste("Fin tiempo scraping", proc.time() - t ) 
      
      #remover ventana
      removeModal()
  })
  
 
  
  #descargar tabla generada de publicaciones
  output$downloadData <- downloadHandler(
    #pattern_ <- "([a-zA-Z0-9_-]{10,12})",
    #codID<-str_extract(input$caption, pattern = pattern_),
    filename <- function() { 
      codID <- str_extract(input$caption, pattern = "([a-zA-Z0-9_-]{10,12})")
      paste("GoogleScholar_publications_",codID,'.csv', sep = '') 
      },
    content <-  function(file) {
      write.csv(scraper$publicaciones, file)
    }
  )
  
  
  #descargar tabla generada de citas por año
  output$downloadDataCitas <- downloadHandler(
    #pattern_ <- "([a-zA-Z0-9_-]{10,12})",
    #codID<-str_extract(input$caption, pattern = pattern_),
    filename <- function() { 
      codID <- str_extract(input$caption, pattern = "([a-zA-Z0-9_-]{10,12})")
      paste("GoogleScholar_citasAnios_",codID,'.csv', sep = '') 
    },
    content <-  function(file) {
      write.csv(scraper$dataAnio, file)
    }
  )
  
  
  #descargar tabla generada de publicaciones
  output$downloadDataPe <- downloadHandler(
    #pattern_ <- "([a-zA-Z0-9_-]{10,12})",
    #codID<-str_extract(input$caption, pattern = pattern_),
    filename <- function() { 
      codID <- str_extract(input$caption, pattern = "([a-zA-Z0-9_-]{10,12})")
      paste("GoogleScholar_research_",codID,'.csv', sep = '') 
    },
    content <-  function(file) {
      write.csv(scraper$listaNum, file)
    }
  )
  
  
  #descargar imagen del grafico
  output$downloadPlot <- downloadHandler(
    #filename = function() { paste("Grafico_citas.png", sep = '') },
    filename <- function() { 
      #codID <- str_extract(input$caption, pattern = "([a-zA-Z0-9_-]{10,12})")
      #paste("GoogleScholar_graficoCitas_",codID,'.png', sep = '') 
      paste("Grafico-citas","png", sep = '.')
    },
    content = function(file) {
      # ESTE NO - ggsave(file,plot = renderPlot(graficoIndividual(input$caption, afiliacion)), device = "png")
      ggsave(file,scraper$Pgrafico, width = 16,
             height = 10.4,dpi=96,
             units = 'in')
      
      #png(file)
      #scraper$Pgrafico
      #dev.off()
      
      print("descargar grafico")
    }
  )
  
}

shinyApp(ui = ui, server = server)
