## app.R ##
library(shiny)
library(shinydashboard)
library(shinyBS)
library(ggplot2)
library(leaflet)
library(ggalt)
library(lubridate)
library(dplyr)
library(shinyjs)
library(qpcR)
library(markdown)
library(shinyalert)
library(sqldf)

# setwd("E:/ESTEBAN/DE LA U/02 ESPECIALIZACION/SEMESTRE 1/01 CIENCIA DE LOS DATOS/Shiny/trabajo_final/accidentes")

# ******* CARGA Y LECTURA DE DATOS ***********

datos_accidentes<-readRDS(file="datos.rds", refhook = NULL)
# opciones_comuna <- read.csv.sql("accidentalidad_2014_2019.csv", "select DISTINCT COMUNA from file", sep=";")


# Data frame para leer la información de la hoja "Tendencias", ya que estos datos no dependen de los filtros.
datos_completos<-subset(datos_accidentes,subset = (datos_accidentes$PERIODO) %in% c(2014,2015,2016,2017,2018,2019))


  ### Lee los datos del CSV
     # datos_accidentes1<-read.csv("accidentalidad_2014_2019.csv", header = TRUE, sep = ";")
  
  ### Guardar los datos en el formato comprimido rds para mejorar el rendimiento de la aplicación
    # saveRDS(datos_accidentes1, file = "datos.rds", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)
  
  ### Lee los datos del formato rds para optmizar la aplicación
   
    # datos_accidentes <- data.frame(read.csv.sql("accidentalidad_2014_2019.csv", "select * from file", sep=";") )

# datos_accidentes<-load("datos2.rds",envir = parent.frame(), verbose = FALSE)
# datos_accidentes<-datos_accidentes3


### Inicialización de variables
 fechas<-as.Date(datos_accidentes$FECHA)
 fecha_min<-min(fechas)
 fecha_max<-max(fechas)
# fecha_min<-"01/01/2014"
# fecha_max<-"30/06/2019"


# Listar las comunas
 opciones_comuna<-c("", sort(as.character(unique(datos_accidentes$COMUNA))),"TODAS")
 # opciones_comuna<-c("TODAS","Aranjuez")

youtube_video <- '<iframe width="853" height="480" src="https://www.youtube.com/embed/kUZCrEWzXII" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'
style <- "my_css2.css"
footer<-'<div class="row">
          <div class="col-sm-6">
            <p class="footer-links">
        
              <a href="https://github.com/juanrpo19/trabajo_ciencia_datos.git">Acceder al repositorio de git.</a>
            </p>
          </div>
          <div class="col-sm-6">
            <p class="footer-company-name">Accidentalidad en Medellín</p>
          </div>
        </div>'



### Creación de la interfaz
ui <- dashboardPage(
  
    dashboardHeader(title = "Accidentalidad Medellín"),
    dashboardSidebar(
        
      ### Botones del menú izquierdo
        sidebarMenu(  #<i class="fas fa-chart-line"></i>
            menuItem("Inicio",tabName = "home_page", icon=icon("fab fa-youtube"))
            ,menuItem("Análisis de accidentes",tabName = "analisis_accidentes",icon = icon("fas fa-poll"))
            ,menuItem("Tendencias",tabName = "tendencia_accidentes",icon = icon("fas fa-chart-line"))
            ,menuItem("Informe Técnico",tabName = "informe_tecnico",icon = icon("fas fa-chart-line"))
        )
    ),
    
    ### Elementos de la parte derecha de la aplicación
    dashboardBody(
      useShinyalert(),    
      tabItems(
            
            ### Tab que corresponde al menuItem "Pagina Inicial"
            tabItem(tabName = "home_page",
                    fluidRow(
                        box(title = "Video",HTML(youtube_video),width=700)
                    ),
                    fluidRow(
                        HTML(footer)
                    )
            ),
            
            ###Tab que corresponde al menuItem "analisis_accidentes"
            tabItem(tabName = "analisis_accidentes",
                    fluidRow(
                       box(width = 12,
                           
                           ### Lista de comunas
                            column(width = 4,
                                selectInput("lista_comuna","Comuna:",choices =  opciones_comuna),
                                actionButton("calcular","Calcular")
                            ),
                           
                           ### Calendarios
                            column(width = 8,
                                dateRangeInput("rango_fechas", "Ranngo de fechas:", start = fecha_min, end = fecha_max,
                                                min = fecha_min, max = fecha_max,
                                                separator = " hasta ", format = "dd/mm/yyyy",
                                                startview = 'year', language = 'es', weekstart = 1)
                                
                            )
                           
                           ### Checks de los dias de la semana
                           ,fluidRow(
                               column(width = 1, checkboxInput("chkLunes","Lunes",value=TRUE,width = '100%'))
                               ,column(width = 1 ,checkboxInput("chkMartes","Martes",value=TRUE,width = '100%'))
                               ,column(width = 1,checkboxInput("chkMiercoles","Miércoles",value=TRUE,width = '100%'))
                               ,column(width = 1 ,checkboxInput("chkJueves","Jueves",value=TRUE,width = '100%'))
                               ,column(width = 1 ,checkboxInput("chkViernes","Viernes",value=TRUE,width = '100%'))
                               ,column(width = 1 ,checkboxInput("chkSabado","Sábado",value=TRUE,width = '100%'))
                               ,column(width = 1,checkboxInput("chkDomingo","Domingo",value=TRUE,width = '100%'))
                            )
                           ,fluidRow(
                             
                           )
                       )
                    ),
                    
                    ### Etiquetas de conteo de eventos
                    fluidRow(
                        infoBoxOutput("conteo_muertes",width = 3)
                        ,infoBoxOutput("conteo_heridos",width = 3)
                        ,infoBoxOutput("conteo_danios",width = 3)
                        ,infoBoxOutput("conteo_total",width = 3)
                    ),
                    
                    ### Gráficas y mapa de el tab "Análisis"
                    fluidRow(
                        box(title = "Georeferenciación",leafletOutput("graficarMapa"), height = 600)
                        ,box(title ="Distribución de accidentes por día" , plotOutput("graficarBarras", height = 230))
                        ,box(title ="Accidentes por rango de hora" , plotOutput("graficarLineas", height = 230))
                    )
            )
            
            # corresponde al menuItem "Tendencias" y muestra las gráficas
            ,tabItem(tabName = "tendencia_accidentes",
                    fluidRow(
                        box(title ="Variación Mensual", plotOutput("graficarTendenciaAnual", height = 370))
                        ,box(title ="Variación Anual", plotOutput("graficarTendenciaPorAnio", height = 370))
                        ,box(title ="Accidentalidad por comuna", plotOutput("grfTendenciaPorComuna", height = 370))
                    )
            )
            
            ### Tab del informe técnico
            ,tabItem(tabName = "informe_tecnico",
                     uiOutput('pagina_web')
            )
            
         
        )
    )
)


server <- function(input, output) { 

    # ...::: Valida que se seleccione un rango de fechas valido :::...
    observeEvent(input$calcular, {
      
      fecha_ini<-(input$rango_fechas[1])
      fecha_fin<-(input$rango_fechas[2])
      comuna<-input$lista_comuna
      
      if (comuna==""){
        shinyalert("Error", "Seleccione una comuna...", type = "error")
        shinyjs::enable("calcular")
        return()
      }
      
      if (is.na(fecha_ini) | is.na(fecha_fin)){
        shinyalert("Error", "Por favor ingrese un rango de fecha.", type = "error")
        shinyjs::enable("calcular")
        return()
      }
      
      if (fecha_ini > fecha_fin){
        shinyalert("Error", "La fecha inicial debe ser menor o igual a la fecha final.", type = "error")
        shinyjs::enable("calcular")
        return()
      }
      
    })
  
  
    
    # ...::: Inserta la pagina Web que contiene el informe técnico :::...
    output$pagina_web<-renderUI({
      includeHTML('www/App_UN_Accidentalidad.html')
    })
 
  
    # ...::: Actualiza la información de los infoBox de Muertes :::...
    data_conteo_muertes<-eventReactive(input$calcular,{
      
      ### Validacíon de parametros
      req(input$rango_fechas[1],input$rango_fechas[2],input$lista_comuna)
      
      ### Datos de entrada
      fecha_ini<-(input$rango_fechas[1])
      fecha_fin<-(input$rango_fechas[2])
      comuna<-as.character(input$lista_comuna)
      
      ### Filtrado de datos
      if (input$lista_comuna != "TODAS")
              {datos_accidentes1<-(subset(datos_accidentes,subset = (datos_accidentes$COMUNA==comuna)))
              
      }

      withProgress(message = "Calculando etiquetas (1/4)", {
        incProgress(amount = 0)
        
        
        ### Crea un vector con los días de la semana seleccionados en los checks
        vectorDias<-"0"
        vectorDias<-append(c("0"), c("0"), after = length(vectorDias))
        if (input$chkLunes==TRUE){ vectorDias<-append(vectorDias, c("LUNES"), after = length(vectorDias))}
        if (input$chkMartes==TRUE){ vectorDias<-append(vectorDias, c("MARTES"), after = length(vectorDias))}
        if (input$chkMiercoles==TRUE){ vectorDias<-append(vectorDias, c("MIERCOLES"), after = length(vectorDias))}
        if (input$chkJueves==TRUE){ vectorDias<-append(vectorDias, c("JUEVES"), after = length(vectorDias))}
        if (input$chkViernes==TRUE){ vectorDias<-append(vectorDias, c("VIERNES"), after = length(vectorDias))}
        if (input$chkSabado==TRUE){ vectorDias<-append(vectorDias, c("SABADO"), after = length(vectorDias))}
        if (input$chkDomingo==TRUE){ vectorDias<-append(vectorDias, c("DOMINGO"), after = length(vectorDias))}
        
        
        ### Si selecciona la opción TODAS de la lista de comunas no realiza el filtro de COMUNA, de lo contrario
        ### realiza el filtro por la comuna seleccionada
        ifelse(comuna=="TODAS"
               ,registro_muertes<-(subset(datos_accidentes
                                          ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                     & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                     & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                     & datos_accidentes$GRAVEDAD=="MUERTO")
                                          ,select = c("GRAVEDAD")))
               ,registro_muertes<-(subset(datos_accidentes1
                                          ,subset = (as.Date(datos_accidentes1$FECHA) >= as.Date(fecha_ini)
                                                     & as.Date(datos_accidentes1$FECHA) <= as.Date(fecha_fin)
                                                     & datos_accidentes1$COMUNA==comuna
                                                     & datos_accidentes1$DIA_NOMBRE %in% as.array(vectorDias)
                                                     & datos_accidentes1$GRAVEDAD=="MUERTO") 
                                          ,select = c("GRAVEDAD")))
         )        
        
        
        ### Valida si el subset anterior devuelve registros
        ifelse(NROW(registro_muertes$GRAVEDAD)==0
               ,cant_muertes<-0
               ,cant_muertes<-aggregate(registro_muertes$GRAVEDAD~registro_muertes$GRAVEDAD,FUN = length)
        )
        
        
        ### Imprime el dato de muertes en la etiqueta correspondiente
        infoBox("Muertes", tags$p(paste0(cant_muertes),style = "font-size: 200%;"),  icon = icon("fas fa-dizzy"), color = "red", fill = TRUE)
        
      })
      
    })

    
    # ...::: Actualiza la información de los infoBox de Heridos:::...
    data_conteo_heridos<-eventReactive(input$calcular,{
      
      ### Validacíon de parametros
      req(input$rango_fechas[1],input$rango_fechas[2],input$lista_comuna)
      
      ### Datos de entrada
      fecha_ini<-(input$rango_fechas[1])
      fecha_fin<-(input$rango_fechas[2])
      comuna<-as.character(input$lista_comuna)
      
      ### Filtrado de datos
      ### Filtrado de datos
      if (input$lista_comuna != "TODAS")
      {datos_accidentes1<-(subset(datos_accidentes,subset = (datos_accidentes$COMUNA==comuna)))
      
      }
      
      withProgress(message = "Calculando etiquetas (2/4)", {
        incProgress(0.2)
        
        
        ### Crea un vector con los días de la semana seleccionados en los checks
        vectorDias<-"0"
        vectorDias<-append(c("0"), c("0"), after = length(vectorDias))
        if (input$chkLunes==TRUE){ vectorDias<-append(vectorDias, c("LUNES"), after = length(vectorDias))}
        if (input$chkMartes==TRUE){ vectorDias<-append(vectorDias, c("MARTES"), after = length(vectorDias))}
        if (input$chkMiercoles==TRUE){ vectorDias<-append(vectorDias, c("MIERCOLES"), after = length(vectorDias))}
        if (input$chkJueves==TRUE){ vectorDias<-append(vectorDias, c("JUEVES"), after = length(vectorDias))}
        if (input$chkViernes==TRUE){ vectorDias<-append(vectorDias, c("VIERNES"), after = length(vectorDias))}
        if (input$chkSabado==TRUE){ vectorDias<-append(vectorDias, c("SABADO"), after = length(vectorDias))}
        if (input$chkDomingo==TRUE){ vectorDias<-append(vectorDias, c("DOMINGO"), after = length(vectorDias))}
        
        
        
        ### Si selecciona la opción TODAS de la lista de comunas no realiza el filtro de COMUNA, de lo contrario
        ### realiza el filtro por la comuna seleccionada		
        ifelse(comuna=="TODAS"
               ,registro_heridos<-(subset(datos_accidentes
                                          ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                     & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                     & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                     & datos_accidentes$GRAVEDAD=="HERIDO") 
                                          ,select = c("GRAVEDAD")))
               ,registro_heridos<-(subset(datos_accidentes1
                                          ,subset = (as.Date(datos_accidentes1$FECHA) >= as.Date(fecha_ini)
                                                     & as.Date(datos_accidentes1$FECHA) <= as.Date(fecha_fin)
                                                     & datos_accidentes1$COMUNA==comuna
                                                     & datos_accidentes1$DIA_NOMBRE %in% vectorDias
                                                     & datos_accidentes1$GRAVEDAD=="HERIDO") 
                                          ,select = c("GRAVEDAD")))
        )
        
        
        ### Valida si el subset anterior devuelve registros
        ifelse(NROW(registro_heridos$GRAVEDAD)==0
               ,cant_heridos<-0
               ,cant_heridos<-aggregate(registro_heridos$GRAVEDAD~registro_heridos$GRAVEDAD,FUN = length)
        )
        
        ### Imprime el dato de muertes en la etiqueta correspondiente
        infoBox("Heridos", tags$p(paste0(cant_heridos),style = "font-size: 200%;"), icon = icon("fas fa-ambulance"),color = "orange")


      })
      
    })
    
    
    # ...::: Actualiza la información de los infoBox de Solo Daños :::...
    data_conteo_danios<-eventReactive(input$calcular,{
      
      ### Validacíon de parametros
      req(input$rango_fechas[1],input$rango_fechas[2],input$lista_comuna)
      
      
      ### Datos de entrada
      fecha_ini<-(input$rango_fechas[1])
      fecha_fin<-(input$rango_fechas[2])
      comuna<-as.character(input$lista_comuna)
      
      ### Filtrado de datos
      ### Filtrado de datos
      if (input$lista_comuna != "TODAS")
        {datos_accidentes1<-(subset(datos_accidentes,subset = (datos_accidentes$COMUNA==comuna)))
      }
      
      
      withProgress(message = "Calculando etiquetas (3/4)", {
        incProgress(amount = 0.3)
        
        
        ### Crea un vector con los días de la semana seleccionados en los checks 
        vectorDias<-"0"
        vectorDias<-append(c("0"), c("0"), after = length(vectorDias))
        if (input$chkLunes==TRUE){ vectorDias<-append(vectorDias, c("LUNES"), after = length(vectorDias))}
        if (input$chkMartes==TRUE){ vectorDias<-append(vectorDias, c("MARTES"), after = length(vectorDias))}
        if (input$chkMiercoles==TRUE){ vectorDias<-append(vectorDias, c("MIERCOLES"), after = length(vectorDias))}
        if (input$chkJueves==TRUE){ vectorDias<-append(vectorDias, c("JUEVES"), after = length(vectorDias))}
        if (input$chkViernes==TRUE){ vectorDias<-append(vectorDias, c("VIERNES"), after = length(vectorDias))}
        if (input$chkSabado==TRUE){ vectorDias<-append(vectorDias, c("SABADO"), after = length(vectorDias))}
        if (input$chkDomingo==TRUE){ vectorDias<-append(vectorDias, c("DOMINGO"), after = length(vectorDias))}
        
        
        ### Si selecciona la opción TODAS de la lista de comunas no realiza el filtro de COMUNA, de lo contrario
        ### realiza el filtro por la comuna seleccionada		
        ifelse(comuna=="TODAS"
               ,registro_danios<-(subset(datos_accidentes
                                         ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                    & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                    & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                                    & datos_accidentes$GRAVEDAD=="SOLO DAÑOS") 
                                         ,select = c("GRAVEDAD")))
               ,registro_danios<-(subset(datos_accidentes1
                                         ,subset = (as.Date(datos_accidentes1$FECHA) >= as.Date(fecha_ini)
                                                    & as.Date(datos_accidentes1$FECHA) <= as.Date(fecha_fin)
                                                    # & datos_accidentes1$COMUNA==comuna
                                                    & datos_accidentes1$DIA_NOMBRE %in% as.array(vectorDias)
                                                    & datos_accidentes1$GRAVEDAD=="SOLO DAÑOS") 
                                         ,select = c("GRAVEDAD")))
        )
        
        ### Valida si el subset anterior devuelve registros
        ifelse(NROW(registro_danios$GRAVEDAD)==0
               ,cant_danios<-0
               ,cant_danios<-aggregate(registro_danios$GRAVEDAD~registro_danios$GRAVEDAD,FUN = length)
        )
        
        ### Imprime el dato de muertes en la etiqueta correspondiente
        infoBox("Solo Daños", tags$p(paste0(cant_danios), style = "font-size: 200%;"),  icon = icon("fas fa-car-crash"), color = "yellow")
        
      })
      
    })
    
    
    # ...::: Actualiza la información de los infoBox de Total :::...
    data_conteo_total<-eventReactive(input$calcular,{
      
      ### Validacíon de parametros
      req(input$rango_fechas[1],input$rango_fechas[2],input$lista_comuna)
      
      ### Datos de entrada
      fecha_ini<-(input$rango_fechas[1])
      fecha_fin<-(input$rango_fechas[2])
      comuna<-as.character(input$lista_comuna)
      
      ### Filtrado de datos
      if (input$lista_comuna != "TODAS")
        {datos_accidentes1<-(subset(datos_accidentes,subset = (datos_accidentes$COMUNA==comuna)))
      }
      
      withProgress(message = "Calculando etiquetas (4/4)", {
        incProgress(amount = 0.4)
        
        
        ### Crea un vector con los días de la semana seleccionados en los checks   
        vectorDias<-"0"
        vectorDias<-append(c("0"), c("0"), after = length(vectorDias))
        if (input$chkLunes==TRUE){ vectorDias<-append(vectorDias, c("LUNES"), after = length(vectorDias))}
        if (input$chkMartes==TRUE){ vectorDias<-append(vectorDias, c("MARTES"), after = length(vectorDias))}
        if (input$chkMiercoles==TRUE){ vectorDias<-append(vectorDias, c("MIERCOLES"), after = length(vectorDias))}
        if (input$chkJueves==TRUE){ vectorDias<-append(vectorDias, c("JUEVES"), after = length(vectorDias))}
        if (input$chkViernes==TRUE){ vectorDias<-append(vectorDias, c("VIERNES"), after = length(vectorDias))}
        if (input$chkSabado==TRUE){ vectorDias<-append(vectorDias, c("SABADO"), after = length(vectorDias))}
        if (input$chkDomingo==TRUE){ vectorDias<-append(vectorDias, c("DOMINGO"), after = length(vectorDias))}
        
        
        
        ### Si selecciona la opción TODAS de la lista de comunas no realiza el filtro de COMUNA, de lo contrario
        ### realiza el filtro por la comuna seleccionada		
        ifelse(comuna=="TODAS"
               ,registro_total<-(subset(datos_accidentes
                                        ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                   & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                   & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                        ) 
                                        ,select = c("GRAVEDAD")))
               ,registro_total<-(subset(datos_accidentes1
                                        ,subset = (as.Date(datos_accidentes1$FECHA) >= as.Date(fecha_ini)
                                                   & as.Date(datos_accidentes1$FECHA) <= as.Date(fecha_fin)
                                                   # & datos_accidentes1$COMUNA==comuna
                                                   & datos_accidentes1$DIA_NOMBRE %in% as.array(vectorDias)
                                        ) 
                                        ,select = c("GRAVEDAD")))
        )
        
        
        ### Valida si el subset anterior devuelve registros
        ifelse(NROW(registro_total$GRAVEDAD)==0
               ,cant_total<-0
               ,cant_total<-aggregate(registro_total$GRAVEDAD~registro_total$GRAVEDAD,FUN = length)
        )
        
        ### Imprime el dato de muertes en la etiqueta correspondiente
        infoBox("Total", tags$p(paste0(cant_total), style = "font-size: 200%;"),  icon = icon("list"), color = "purple")
        
      })
      
    })
    
    
    # ...::: Crea el mapa con los puntos a mostrar :::...
    data_mapa<-eventReactive(input$calcular,{
      
      ### Validacíon de parametros
      req(input$rango_fechas[1],input$rango_fechas[2],input$lista_comuna)
      
      ### Parametros de entrada
      ### Datos de entrada
      fecha_ini<-(input$rango_fechas[1])
      fecha_fin<-(input$rango_fechas[2])
      comuna<-as.character(input$lista_comuna)
      
      ### Filtrado de datos
      if (input$lista_comuna != "TODAS")
        {datos_accidentes1<-(subset(datos_accidentes,subset = (datos_accidentes$COMUNA==comuna)))
      }
      
      
      withProgress(message = "Actualizando mapa", {
        incProgress(0.6)
        
        
        ### Crea un vector con los días de la semana seleccionados en los checks  
        vectorDias<-"0"
        vectorDias<-append(c("0"), c("0"), after = length(vectorDias))
        if (input$chkLunes==TRUE){ vectorDias<-append(vectorDias, c("LUNES"), after = length(vectorDias))}
        if (input$chkMartes==TRUE){ vectorDias<-append(vectorDias, c("MARTES"), after = length(vectorDias))}
        if (input$chkMiercoles==TRUE){ vectorDias<-append(vectorDias, c("MIERCOLES"), after = length(vectorDias))}
        if (input$chkJueves==TRUE){ vectorDias<-append(vectorDias, c("JUEVES"), after = length(vectorDias))}
        if (input$chkViernes==TRUE){ vectorDias<-append(vectorDias, c("VIERNES"), after = length(vectorDias))}
        if (input$chkSabado==TRUE){ vectorDias<-append(vectorDias, c("SABADO"), after = length(vectorDias))}
        if (input$chkDomingo==TRUE){ vectorDias<-append(vectorDias, c("DOMINGO"), after = length(vectorDias))}
        
        
        ### Si selecciona la opción TODAS de la lista de comunas no realiza el filtro de COMUNA, de lo contrario
        ### realiza el filtro por la comuna seleccionada		
         ifelse(comuna=="TODAS"
               ,puntos_accidentes<-(subset(datos_accidentes
                                           ,subset = (as.Date(datos_accidentes$FECHA) >= as.Date(fecha_ini)
                                                      & as.Date(datos_accidentes$FECHA) <= as.Date(fecha_fin)
                                                      & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
                                           ) 
                                           ,select = c("LATITUD","LONGITUD")))
               
               ,puntos_accidentes<-(subset(datos_accidentes1
                                           ,subset = (as.Date(datos_accidentes1$FECHA) >= as.Date(fecha_ini)
                                                      & as.Date(datos_accidentes1$FECHA) <= as.Date(fecha_fin)
                                                      & datos_accidentes1$DIA_NOMBRE %in% as.array(vectorDias))
                                                      # & datos_accidentes1$COMUNA==comuna)
                                           ,select = c("LATITUD","LONGITUD")))
        )
        
        ### Captura los puntos máximos y minimos para delimitar el mapa
        lngA=min(puntos_accidentes$LONGITUD)
        lngB=max(puntos_accidentes$LONGITUD)
        latA=min(puntos_accidentes$LATITUD)
        latB=max(puntos_accidentes$LATITUD)
        
        ### Genera el mapa con los puntos filtrados
        mapa<-leaflet()
        mapa<-addTiles(mapa)
        mapa<-addProviderTiles(mapa,provider <- "OpenStreetMap.Mapnik")
        mapa<-fitBounds(mapa,lng1=lngA,lng2=lngB,lat1=latA,lat2=latB)
        mapa<-addMarkers(mapa,lng=puntos_accidentes$LONGITUD,lat=puntos_accidentes$LATITUD,clusterOptions = markerClusterOptions())
        mapa
      })
      
      
    })
    
    
    #...::: Imprime grafico de barras con los dias de la semana :::...
    data_grafico_dias_sem<-eventReactive(input$calcular,{
      
      ### Validacíon de parametros
      req(input$rango_fechas[1],input$rango_fechas[2],input$lista_comuna)
      
      
      ### Datos de entrada
      fecha_ini<-(input$rango_fechas[1])
      fecha_fin<-(input$rango_fechas[2])
      comuna<-as.character(input$lista_comuna)
      
      ### Filtrado de datos
      if (input$lista_comuna != "TODAS")
        {datos_accidentes1<-(subset(datos_accidentes,subset = (datos_accidentes$COMUNA==comuna)))
      }
      
      withProgress(message = "Actualizando gráfico de semana", {
        incProgress(0.75)
        
        
        ### Crea un vector con los días de la semana seleccionados en los checks    
        vectorDias<-c("0")
        vectorDias<-append(c("0"), c("0"), after = length(vectorDias))
        if (input$chkLunes==TRUE){ vectorDias<-append(vectorDias, c("LUNES"), after = length(vectorDias))}
        if (input$chkMartes==TRUE){ vectorDias<-append(vectorDias, c("MARTES"), after = length(vectorDias))}
        if (input$chkMiercoles==TRUE){ vectorDias<-append(vectorDias, c("MIERCOLES"), after = length(vectorDias))}
        if (input$chkJueves==TRUE){ vectorDias<-append(vectorDias, c("JUEVES"), after = length(vectorDias))}
        if (input$chkViernes==TRUE){ vectorDias<-append(vectorDias, c("VIERNES"), after = length(vectorDias))}
        if (input$chkSabado==TRUE){ vectorDias<-append(vectorDias, c("SABADO"), after = length(vectorDias))}
        if (input$chkDomingo==TRUE){ vectorDias<-append(vectorDias, c("DOMINGO"), after = length(vectorDias))}
        
        
        ### Si selecciona la opción TODAS de la lista de comunas no realiza el filtro de COMUNA, de lo contrario
        ### realiza el filtro por la comuna seleccionada		
        ifelse(comuna=="TODAS"
               ,datos<-subset(datos_accidentes,subset = (as.Date(datos_accidentes$FECHA)>=as.Date(fecha_ini)
                                                         & as.Date(datos_accidentes$FECHA)<=as.Date(fecha_fin)
                                                         & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias)
               ))
               
               ,datos<-subset(datos_accidentes1,subset = (as.Date(datos_accidentes1$FECHA)>=as.Date(fecha_ini)
                                                         & as.Date(datos_accidentes1$FECHA)<=as.Date(fecha_fin)
                                                         & datos_accidentes1$DIA_NOMBRE %in% as.array(vectorDias)))
                                                         # & datos_accidentes1$COMUNA==comuna)
                                                          # )
        )

        ### Valida si el subset anterior devuelve registros
        if(NROW(datos)>0){
          datos$DIA_SEMANA<-as.character(datos$DIA_NOMBRE)
          datos_conteo<-aggregate(DIA_SEMANA~DIA_NOMBRE, data=datos,FUN=length)
          names(datos_conteo)<-c("DIA_SEMANA","CONTEO")
          
          ### Asigna un valor entre 1 a 7 a cada uno de los días de la semana para organizar la grafica
          datos_conteo$NUM_DIA<- ifelse(datos_conteo$DIA_SEMANA=="DOMINGO",7,
                                        ifelse(datos_conteo$DIA_SEMANA=="SABADO",6,
                                               ifelse(datos_conteo$DIA_SEMANA=="VIERNES",5,
                                                      ifelse(datos_conteo$DIA_SEMANA=="JUEVES",4,
                                                             ifelse(datos_conteo$DIA_SEMANA=="MIERCOLES",3,
                                                                    ifelse(datos_conteo$DIA_SEMANA=="MARTES",2,
                                                                           ifelse(datos_conteo$DIA_SEMANA=="LUNES",1,0)
                                                                    )
                                                             )
                                                      )
                                               )
                                        )
          )
          
          ### Ordena los datos por día para la grafica
          datos_conteo<-datos_conteo[order(datos_conteo$NUM_DIA),]
          names(datos_conteo)=c("DIA","CUENTA","NRO_DIA")
          
          ### Genera la grafica
          ggplot(data=datos_conteo,aes(x=reorder(datos_conteo$DIA,datos_conteo$NRO_DIA), y=datos_conteo$CUENTA)) +
            geom_bar(stat="identity", fill="steelblue")+
            xlab("DÍA")+
            ylab("CANTIDAD DE ACCIDENTES")+
            theme(plot.title = element_text(hjust = 0.5,size = 10, face = "bold"))+
            geom_text(aes(label=datos_conteo$CUENTA), vjust=1.6, color="white", size=5)
          
        }
        
      })
      
    })
    
    
    # ...::: Imprime grafico de lineas con las horas del dia :::...
    data_grafico_horas<-eventReactive(input$calcular,{
      
      ### Validacíon de parametros
      req(input$rango_fechas[1],input$rango_fechas[2],input$lista_comuna)
      
      
      ### Datos de entrada
      fecha_ini<-(input$rango_fechas[1])
      fecha_fin<-(input$rango_fechas[2])
      comuna<-as.character(input$lista_comuna)
      
      ### Filtrado de datos
      if (input$lista_comuna != "TODAS")
        {datos_accidentes1<-(subset(datos_accidentes,subset = (datos_accidentes$COMUNA==comuna)))
      }
      
      withProgress(message = "Actualizando gráfico de horas", {
        incProgress(amount=0.9)
        
        ### Crea un vector con los días de la semana seleccionados en los checks
        vectorDias<-"0"
        vectorDias<-append(c("0"), c("0"), after = length(vectorDias))
        if (input$chkLunes==TRUE){ vectorDias<-append(vectorDias, c("LUNES"), after = length(vectorDias))}
        if (input$chkMartes==TRUE){ vectorDias<-append(vectorDias, c("MARTES"), after = length(vectorDias))}
        if (input$chkMiercoles==TRUE){ vectorDias<-append(vectorDias, c("MIERCOLES"), after = length(vectorDias))}
        if (input$chkJueves==TRUE){ vectorDias<-append(vectorDias, c("JUEVES"), after = length(vectorDias))}
        if (input$chkViernes==TRUE){ vectorDias<-append(vectorDias, c("VIERNES"), after = length(vectorDias))}
        if (input$chkSabado==TRUE){ vectorDias<-append(vectorDias, c("SABADO"), after = length(vectorDias))}
        if (input$chkDomingo==TRUE){ vectorDias<-append(vectorDias, c("DOMINGO"), after = length(vectorDias))}
        
        
        ### Si selecciona la opción TODAS de la lista de comunas no realiza el filtro de COMUNA, de lo contrario
        ### realiza el filtro por la comuna seleccionada		
        ifelse(comuna=="TODAS"
               ,intradia<-subset(datos_accidentes,subset = (as.Date(datos_accidentes$FECHA)>=as.Date(fecha_ini)
                                                            & as.Date(datos_accidentes$FECHA)<=as.Date(fecha_fin)
                                                            & datos_accidentes$DIA_NOMBRE %in% as.array(vectorDias))
                                                            ,select = c("RANGO_HORA","DIA_NOMBRE"))
               
               ,intradia<-subset(datos_accidentes1,subset = (as.Date(datos_accidentes1$FECHA)>=as.Date(fecha_ini)
                                                            & as.Date(datos_accidentes1$FECHA)<=as.Date(fecha_fin)
                                                            # & datos_accidentes1$COMUNA==comuna
                                                            & datos_accidentes1$DIA_NOMBRE %in% as.array(vectorDias))
                                                            ,select = c("RANGO_HORA","DIA_NOMBRE"))
        )
        
        ### Valida si el subset anterior devuelve registros, agrupa los datos y genera la grafica
        if(NROW(intradia$RANGO_HORA)>0){        
          datos_horas <-  intradia %>%
            
            group_by(RANGO_HORA, DIA_NOMBRE)%>%
            
            summarise(
              CUENTA_DATOS = n(),
            )
          
          ### Genera la grafica
          p<-ggplot(data =datos_horas, aes(y= CUENTA_DATOS, x = RANGO_HORA,color=factor(DIA_NOMBRE)))+ 
            geom_line()+ theme(legend.position="top")+
            theme_bw()+
            labs(color = "Año")+
            scale_x_discrete("Mes", limits=c(0:23))
          print(p)
          
        }
        incProgress(amount=1)
      })
      
    })
    
    
    ### ...::: Gráficos de tendencias :::....
    output$graficarTendenciaAnual<-renderPlot({
      
      ### Agrupa los datos y cuenta las ocurrencias por mes y periodo
      table.temp <-  datos_completos %>%group_by(MES, PERIODO)%>%
        summarise(
          frecuencia = n(),
        )
      
      ### Genera la grafica
      p<-ggplot(data =table.temp, aes(y= frecuencia, x = MES,color=factor(PERIODO)))+ 
        geom_line()+ 
        theme(legend.position="top")+
        theme_bw()+
        labs(color = "Año")+
        scale_x_discrete("Mes", limits=c(0:12))
      print(p)
    })
    
    output$graficarTendenciaPorAnio<-renderPlot({
      
      ### Agrupa los datos y los sumariza por periodo
      datos_completos$OBSERVACION<-1
      datos_completos %>% group_by(PERIODO) %>% 
        summarise(ex = length(OBSERVACION))
      
      datos_resumidos<-datos_completos %>% group_by(PERIODO)%>%summarise(CUENTA = length(OBSERVACION))
      
      ### Genera la grafica
      ggplot(datos_resumidos,aes(x=datos_resumidos$PERIODO,y=datos_resumidos$CUENTA)) +
        geom_bar(stat="identity", fill="steelblue")+
        xlab("AÑO")+
        ylab("CANTIDAD DE ACCIDENTES")+
        theme(plot.title = element_text(hjust = 0.5,size = 10, face = "bold"))+
        geom_text(aes(label=datos_resumidos$CUENTA), vjust=1.6, color="white", size=5)+
        scale_x_continuous(breaks=unique(datos_resumidos$PERIODO))
      
    })
    
    output$grfTendenciaPorComuna<-renderPlot({
      
      datos_completos<-subset(datos_accidentes,subset = (datos_accidentes$PERIODO) %in% c(2014,2015,2016,2017,2018,2019)
                              , select = "COMUNA")
      
      
      ### Sumariza por comuna y cuenta la cantidad de accidentes
      datos_comuna <- datos_completos%>%
        group_by(COMUNA)%>%
        summarise(
          Accidentes = n(),)
      
      ### Genera la grafica
      ggplot(data= datos_comuna, aes(x=reorder(COMUNA,Accidentes), y=Accidentes))+
        geom_bar(stat="identity", fill= c("Green","Green","Green","Green","Green","Green","Green","Yellow","Yellow","Yellow","Yellow","Yellow","Orange","Orange","Orange","Orange","Orange","Red","Red","Red","Red","Red"))+
        theme_minimal()+
        theme_bw()+
        coord_flip() +
        theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
        labs(title="Accidentalidad por comuna", y = "Número accidentes", x = "Comuna")
      
    })
    
    
    
    
    output$conteo_muertes <- renderInfoBox({
      data_conteo_muertes()
    })
    
    output$conteo_heridos <- renderInfoBox({
      data_conteo_heridos()
    })
  
    output$conteo_danios <- renderInfoBox({
      data_conteo_danios()
    })

    output$conteo_total <- renderInfoBox({
      data_conteo_total()
    })

    output$graficarMapa<-renderLeaflet({
      data_mapa()
    })

    output$graficarBarras<-renderPlot({
      data_grafico_dias_sem()
    })

    output$graficarLineas<-renderPlot({
      data_grafico_horas()
    })

    

}

shinyApp(ui, server)
