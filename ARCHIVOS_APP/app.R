

library(shiny)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(rgdal)
library(leaflet)

load(file = "DATA_MAPA")
load(file = "barrios")
load(file = "datos")


DATA = read.csv(
    file = 'predicciones.csv',
    header = TRUE,
    sep = ";",
    encoding = "UTF-8") 

DATA_MES = read.csv(
    file = 'datos_mes.csv',
    header = TRUE,
    sep = ";",
    encoding = "UTF-8") 

nombres = colnames(DATA_MES)
nombres[1] = "GRAVEDAD"
colnames(DATA_MES) = nombres
DATA_MES$FECHA = as.Date(DATA_MES$FECHA)
DATA_MES$MOSTRAR = paste(DATA_MES$NOMBRE_MES,DATA_MES$ANO,sep=" - ")

nombres = colnames(DATA)
nombres[1]="FECHA"
colnames(DATA) = nombres
rm(nombres)

DATA$FECHA = as.Date(DATA$FECHA)
DATA$DIA_SEM = factor(DATA$DIA_SEM, levels = c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"))
DATA$MES = factor(DATA$MES, levels = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"))
DATA$SEMANA_MES = factor(DATA$SEMANA_MES, levels = c("1","2","3","4"))
DATA$FIN_SEMANA = factor(DATA$FIN_SEMANA, levels = c("No","Si"))
DATA$FESTIVO = factor(DATA$FESTIVO, levels = c("No","Si"))
DATA$FERIA_FLORES = factor(DATA$FERIA_FLORES, levels = c("No","Si"))

ui = navbarPage("Accidentalidad en Medellín", 
                collapsible = TRUE, 
                inverse = TRUE, 
                theme = shinytheme("spacelab"),
                
                tabPanel("Visualización de accidentalidad",
                         
                         fluidPage(
                             
                             sidebarLayout(
                                 
                                 sidebarPanel(
                                     
                                     width = 2,
                                     
                                     #Gravededad de visualización
                                     selectInput(inputId = "GRAVEDAD_MAPA", 
                                                 label = "Gravedad del accidente",
                                                 c("Todas" = "T",
                                                   "Solo Daños" = "DANO",
                                                   "Herido" = "HERIDO",
                                                   "Muerto" = "MUERTO")),
                                     
                                     #Año de visualización
                                     selectInput(inputId = "ANO_MAPA", 
                                                 label = "Año",
                                                 c("Todos" = "T",
                                                   "2014" = "2014",
                                                   "2015" = "2015",
                                                   "2016" = "2016",
                                                   "2017" = "2017",
                                                   "2018" = "2018")),
                                     
                                     #Mes de visualización
                                     selectInput(inputId = "MES_MAPA",
                                                 label = "Mes",
                                                 c("Todos" = "T",
                                                   "Enero" = "ENERO",
                                                   "Febrero" = "FEBRERO",
                                                   "Marzo" = "MARZO",
                                                   "Abril" = "ABRIL",
                                                   "Mayo" = "MAYO",
                                                   "Junio" = "JUNIO",
                                                   "Julio" = "JULIO",
                                                   "Agosto" = "AGOSTO",
                                                   "Septiembre" = "SEPTIEMBRE",
                                                   "Octubre" = "OCTUBRE",
                                                   "Noviembre" = "NOVIEMBRE",
                                                   "Diciembre" = "DICIEMBRE")),
                                     
                                     #DIA DE LA SEMANA
                                     selectInput(inputId = "SEMANA_MAPA", 
                                                 label = "Día de la semana",
                                                 c("Todos" = "T",
                                                   "Lunes" = "LUNES",
                                                   "Martes" = "MARTES",
                                                   "Miércoles" = "MIERCOLES",
                                                   "Jueves" = "JUEVES",
                                                   "Viernes" = "VIERNES",
                                                   "Sábado" = "SABADO",
                                                   "Domingo" = "DOMINGO")),
                                     
                                     #Momento del día
                                     selectInput(inputId = "MOMENTO_MAPA", 
                                                 label = "Momento del día",
                                                 c("Todos" = "T",
                                                   "Madrugada" = "MADRUGADA",
                                                   "Mañana" = "MAÑANA",
                                                   "Tarde" = "TARDE",
                                                   "Noche" = "NOCHE")),
                                     
                                 ),
                                 
                                 mainPanel(
                                     
                                     leafletOutput("MAPA", height = "750px")
                                     
                                 ),
                                 
                             )
                         )
                ),
                
                tabPanel("Predicción",
                         
                         fluidPage(
                             
                             sidebarLayout(
                                 
                                 sidebarPanel(
                                     
                                     width = 2,
                                     
                                     #Botones para escoger la ventana de tiempo de predicciÃ³n
                                     radioButtons(inputId = "TIEMPO_PREDICCION", 
                                                  label = "Ventana de tiempo",
                                                  c("Diario" = "DIA",
                                                    "Semanal" = "SEMANA",
                                                    "Mensual" = "MES")),
                                     
                                     #Botones para escoger la gravedad del incidente
                                     radioButtons(inputId = "GRAVEDAD_PREDICCION", 
                                                  label = "Gravedad del accidente",
                                                  c("Todas" = "TODAS",
                                                    "Solo Daños" = "DANO",
                                                    "Herido" = "HERIDO",
                                                    "Muerto" = "MUERTO")),
                                     
                                     conditionalPanel(
                                         condition = "input.TIEMPO_PREDICCION === 'DIA' && input.GRAVEDAD_PREDICCION != 'TODAS'",
                                         selectInput(inputId = "TIPO_PREDICCION",
                                                     label = "Tipo de gráfico",
                                                     c("General" = "GENERAL",
                                                       "Día semana" = "DIA",
                                                       "Mes" = "MES"))),
                                     
                                     conditionalPanel(
                                         condition = "input.TIEMPO_PREDICCION != 'MES'",
                                         dateRangeInput(inputId = "FECHA",
                                                        label = "Rango de predicción",
                                                        start = "2019-01-01",
                                                        end   = "2019-01-31",
                                                        min = "2019-01-01",
                                                        max = "2020-12-31",
                                                        language = "es",
                                                        weekstart = 1,
                                                        separator = " hasta ")),
                                     
                                     conditionalPanel(
                                         
                                         condition = "input.TIEMPO_PREDICCION === 'MES'",
                                         
                                         selectInput(inputId = "MES_DESDE",
                                                     label ="Desde",
                                                     c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")),
                                         
                                         radioButtons(inputId = "ANO_DESDE", 
                                                      label = NULL,
                                                      c("2019" = 2019,
                                                        "2020" = 2020),
                                                      inline=T),
                                         
                                         selectInput(inputId = "MES_HASTA",
                                                     label ="Hasta",
                                                     selected = "Junio",
                                                     c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")),
                                         
                                         radioButtons(inputId = "ANO_HASTA", 
                                                      label = NULL,
                                                      c("2019" = 2019,
                                                        "2020" = 2020),
                                                      inline=T)
                                         
                                     )
                                     
                                 ),
                                 
                                 
                                 mainPanel(
                                     
                                     plotlyOutput(outputId = "GRAFICA_PREDICCION", height = "750px")
                                     
                                 )
                                 
                             )
                             
                         )
                         
                ),
                
                tabPanel("Agrupamiento",
                         
                         fluidPage(
                             
                             sidebarLayout(
                                 
                                 sidebarPanel(
                                     
                                     width = 2,
                                     
                                     #Tipo de agrupamiento
                                     radioButtons(inputId = "TIPO_AGRUPAMIENTO", 
                                                  label = "Tipo de agrupamiento",
                                                  c("Accidentalidad total" = "TOTAL",
                                                    "Accidentalidad en hora pico" = "PICO",
                                                    "Accidentalidad por gravedad" = "GRAVEDAD"),
                                                  inline=F),
                                     
                                     conditionalPanel(
                                         
                                         condition = "input.TIPO_AGRUPAMIENTO === 'TOTAL'",
                                         
                                         #Año de visualización
                                         selectInput(inputId = "FECHA_AGRUPACION", 
                                                     label = "Año",
                                                     c("Todos" = "TODO",
                                                       "2014" = "2014",
                                                       "2015" = "2015",
                                                       "2016" = "2016",
                                                       "2017" = "2017",
                                                       "2018" = "2018"))
                                     ),
                                     
                                     conditionalPanel(
                                         
                                         condition = "input.TIPO_AGRUPAMIENTO != 'TOTAL'",
                                         
                                         #Tipo de gravedad
                                         radioButtons(inputId = "GRAVEDAD_AGRUPACION", 
                                                      label = "Tipo de gravedad",
                                                      c("Muertos & Heridos" = "HM",
                                                        "Solo daños" = "DANO"),
                                                      inline=F),
                                         
                                         
                                         
                                     )
                                     
                                 ),
                                 
                                 mainPanel(
                                     
                                     leafletOutput("AGRUPAMIENTO", height = "750px")
                                     
                                 ),
                                 
                             )
                         )
                )
)


server <- function(input, output){
    
    output$GRAFICA_PREDICCION = renderPlotly({
        
        if(input$TIEMPO_PREDICCION == "DIA"){
            
            if(input$GRAVEDAD_PREDICCION == "DANO"){
                
                GRAFICAR_PREDICCION = (DATA[DATA$FECHA >= input$FECHA[1] & DATA$FECHA <= input$FECHA[2],])
                
                GRAFICAR_PREDICCION = GRAFICAR_PREDICCION[GRAFICAR_PREDICCION$GRAVEDAD == "Solo Daño",]  
                
                if(input$TIPO_PREDICCION == "GENERAL"){
                    
                    plot_ly(x = GRAFICAR_PREDICCION$FECHA,
                            hoverinfo = 'text',
                            text = paste('</br>Cantidad:', GRAFICAR_PREDICCION$CANTIDAD,
                                         '</br>Mes: ', GRAFICAR_PREDICCION$MES,
                                         '</br>Día mes: ', GRAFICAR_PREDICCION$DIA_MES,
                                         '</br>Diá de la semana: ', GRAFICAR_PREDICCION$DIA_SEM,
                                         '</br>Festivo: ', GRAFICAR_PREDICCION$FESTIVO)) %>% 
                        
                        add_trace(y = GRAFICAR_PREDICCION$CANTIDAD,
                                  type = "scatter", 
                                  mode = "markers+lines") %>%
                        
                        layout(xaxis = list(title = '<b> Fecha',
                                            showgrid = F),
                               yaxis = list(title = '<b> Incidentes'),
                               title = "<b> Cantidad de incidentes diarios - Gravedad <i>Solo Daños</i>",
                               plot_bgcolor  = "rgba(0, 0, 0, 0)",
                               paper_bgcolor = "rgba(0, 0, 0, 0)")
                    
                } else if(input$TIPO_PREDICCION == "DIA"){
                    
                    plot_ly(x = GRAFICAR_PREDICCION$FECHA,
                            color = GRAFICAR_PREDICCION$DIA_SEM,
                            hoverinfo = 'text',
                            text = paste('</br>Cantidad:', GRAFICAR_PREDICCION$CANTIDAD,
                                         '</br>Mes: ', GRAFICAR_PREDICCION$MES,
                                         '</br>Día mes: ', GRAFICAR_PREDICCION$DIA_MES,
                                         '</br>Festivo: ', GRAFICAR_PREDICCION$FESTIVO)) %>% 
                        
                        add_trace(y = GRAFICAR_PREDICCION$CANTIDAD,
                                  type = "scatter", 
                                  mode = "markers+lines") %>%
                        
                        layout(xaxis = list(title = '<b> Fecha',
                                            showgrid = F),
                               yaxis = list(title = '<b> Incidentes'),
                               title = "<b> Cantidad de incidentes diarios - Gravedad <i>Solo Daños</i>",
                               plot_bgcolor  = "rgba(0, 0, 0, 0)",
                               paper_bgcolor = "rgba(0, 0, 0, 0)")
                    
                } else if(input$TIPO_PREDICCION == "MES"){
                    
                    meses = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
                    
                    anos = c(2019,2020)
                    
                    fig = plot_ly() 
                    
                    for(ano in anos){
                        
                        for(mes in meses){
                            
                            fig = fig %>% add_trace(x = GRAFICAR_PREDICCION$FECHA[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                    y =  GRAFICAR_PREDICCION$CANTIDAD[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                    color = GRAFICAR_PREDICCION$MES[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                    name = mes,
                                                    type = "scatter",
                                                    mode = "markers+lines",
                                                    hoverinfo = 'text',
                                                    text = paste('</br>Cantidad: ', GRAFICAR_PREDICCION$CANTIDAD[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                                 '</br>Mes: ', GRAFICAR_PREDICCION$MES[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                                 '</br>Día mes: ', GRAFICAR_PREDICCION$DIA_MES[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                                 '</br>Festivo: ', GRAFICAR_PREDICCION$FESTIVO[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano]))
                            
                        }
                        
                    } 
                    
                    fig = fig %>% layout(xaxis = list(title = '<b> Fecha',
                                                      showgrid = F),
                                         yaxis = list(title = '<b> Incidentes'),
                                         title = "<b> Cantidad de incidentes diarios - Gravedad <i>Solo Daños</i>",
                                         plot_bgcolor  = "rgba(0, 0, 0, 0)",
                                         paper_bgcolor = "rgba(0, 0, 0, 0)")
                    
                }
                
            } else if (input$GRAVEDAD_PREDICCION == "MUERTO"){
                
                GRAFICAR_PREDICCION = (DATA[DATA$FECHA >= input$FECHA[1] & DATA$FECHA <= input$FECHA[2],])
                
                GRAFICAR_PREDICCION = GRAFICAR_PREDICCION[GRAFICAR_PREDICCION$GRAVEDAD == "Muerto",]
                
                if(input$TIPO_PREDICCION == "GENERAL"){
                    
                    plot_ly(x = GRAFICAR_PREDICCION$FECHA,
                            hoverinfo = 'text',
                            text = paste('</br>Cantidad:', GRAFICAR_PREDICCION$CANTIDAD,
                                         '</br>Mes: ', GRAFICAR_PREDICCION$MES,
                                         '</br>Día mes: ', GRAFICAR_PREDICCION$DIA_MES,
                                         '</br>Diá de la semana: ', GRAFICAR_PREDICCION$DIA_SEM,
                                         '</br>Festivo: ', GRAFICAR_PREDICCION$FESTIVO)) %>% 
                        
                        add_trace(y = GRAFICAR_PREDICCION$CANTIDAD,
                                  type = "scatter", 
                                  mode = "markers+lines") %>%
                        
                        layout(xaxis = list(title = '<b> Fecha',
                                            showgrid = F),
                               yaxis = list(title = '<b> Incidentes', autotick = F),
                               title = "<b> Cantidad de incidentes diarios - Gravedad <i>Muerto</i>",
                               plot_bgcolor  = "rgba(0, 0, 0, 0)",
                               paper_bgcolor = "rgba(0, 0, 0, 0)")
                    
                } else if(input$TIPO_PREDICCION == "DIA"){
                    
                    plot_ly(x = GRAFICAR_PREDICCION$FECHA,
                            color = GRAFICAR_PREDICCION$DIA_SEM,
                            hoverinfo = 'text',
                            text = paste('</br>Cantidad:', GRAFICAR_PREDICCION$CANTIDAD,
                                         '</br>Mes: ', GRAFICAR_PREDICCION$MES,
                                         '</br>Día mes: ', GRAFICAR_PREDICCION$DIA_MES,
                                         '</br>Festivo: ', GRAFICAR_PREDICCION$FESTIVO)) %>% 
                        
                        add_trace(y = GRAFICAR_PREDICCION$CANTIDAD,
                                  type = "scatter", 
                                  mode = "markers+lines") %>%
                        
                        layout(xaxis = list(title = '<b> Fecha',
                                            showgrid = F),
                               yaxis = list(title = '<b> Incidentes', autotick = F),
                               title = "<b> Cantidad de incidentes diarios - Gravedad <i>Muerto</i>",
                               plot_bgcolor  = "rgba(0, 0, 0, 0)",
                               paper_bgcolor = "rgba(0, 0, 0, 0)")
                    
                } else if(input$TIPO_PREDICCION == "MES"){
                    
                    meses = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
                    
                    anos = c(2019,2020)
                    
                    fig = plot_ly() 
                    
                    for(ano in anos){
                        
                        for(mes in meses){
                            
                            fig = fig %>% add_trace(x = GRAFICAR_PREDICCION$FECHA[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                    y =  GRAFICAR_PREDICCION$CANTIDAD[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                    color = GRAFICAR_PREDICCION$MES[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                    name = mes,
                                                    type = "scatter",
                                                    mode = "markers+lines",
                                                    hoverinfo = 'text',
                                                    text = paste('</br>Cantidad: ', GRAFICAR_PREDICCION$CANTIDAD[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                                 '</br>Mes: ', GRAFICAR_PREDICCION$MES[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                                 '</br>Día mes: ', GRAFICAR_PREDICCION$DIA_MES[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                                 '</br>Festivo: ', GRAFICAR_PREDICCION$FESTIVO[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano]))
                            
                        }
                        
                    } 
                    
                    fig = fig %>% layout(xaxis = list(title = '<b> Fecha',
                                                      showgrid = F),
                                         yaxis = list(title = '<b> Incidentes', autotick = F),
                                         title = "<b> Cantidad de incidentes diarios - Gravedad <i>Muerto</i>",
                                         plot_bgcolor  = "rgba(0, 0, 0, 0)",
                                         paper_bgcolor = "rgba(0, 0, 0, 0)")
                    
                }
                
            } else if (input$GRAVEDAD_PREDICCION == "HERIDO") {
                
                GRAFICAR_PREDICCION = (DATA[DATA$FECHA >= input$FECHA[1] & DATA$FECHA <= input$FECHA[2],])
                
                GRAFICAR_PREDICCION = GRAFICAR_PREDICCION[GRAFICAR_PREDICCION$GRAVEDAD == "Herido",]
                
                if(input$TIPO_PREDICCION == "GENERAL"){
                    
                    plot_ly(x = GRAFICAR_PREDICCION$FECHA,
                            hoverinfo = 'text',
                            text = paste('</br>Cantidad:', GRAFICAR_PREDICCION$CANTIDAD,
                                         '</br>Mes: ', GRAFICAR_PREDICCION$MES,
                                         '</br>Día mes: ', GRAFICAR_PREDICCION$DIA_MES,
                                         '</br>Diá de la semana: ', GRAFICAR_PREDICCION$DIA_SEM,
                                         '</br>Festivo: ', GRAFICAR_PREDICCION$FESTIVO)) %>% 
                        
                        add_trace(y = GRAFICAR_PREDICCION$CANTIDAD,
                                  type = "scatter", 
                                  mode = "markers+lines") %>%
                        
                        layout(xaxis = list(title = '<b> Fecha',
                                            showgrid = F),
                               yaxis = list(title = '<b> Incidentes'),
                               title = "<b> Cantidad de incidentes diarios - Gravedad <i>Herido</i>",
                               plot_bgcolor  = "rgba(0, 0, 0, 0)",
                               paper_bgcolor = "rgba(0, 0, 0, 0)")
                    
                } else if(input$TIPO_PREDICCION == "DIA"){
                    
                    plot_ly(x = GRAFICAR_PREDICCION$FECHA,
                            color = GRAFICAR_PREDICCION$DIA_SEM,
                            hoverinfo = 'text',
                            text = paste('</br>Cantidad:', GRAFICAR_PREDICCION$CANTIDAD,
                                         '</br>Mes: ', GRAFICAR_PREDICCION$MES,
                                         '</br>Día mes: ', GRAFICAR_PREDICCION$DIA_MES,
                                         '</br>Festivo: ', GRAFICAR_PREDICCION$FESTIVO)) %>% 
                        
                        add_trace(y = GRAFICAR_PREDICCION$CANTIDAD,
                                  type = "scatter", 
                                  mode = "markers+lines") %>%
                        
                        layout(xaxis = list(title = '<b> Fecha',
                                            showgrid = F),
                               yaxis = list(title = '<b> Incidentes'),
                               title = "<b> Cantidad de incidentes diarios - Gravedad <i>Herido</i>",
                               plot_bgcolor  = "rgba(0, 0, 0, 0)",
                               paper_bgcolor = "rgba(0, 0, 0, 0)")
                    
                } else if(input$TIPO_PREDICCION == "MES"){
                    
                    meses = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
                    
                    anos = c(2019,2020)
                    
                    fig = plot_ly() 
                    
                    for(ano in anos){
                        
                        for(mes in meses){
                            
                            fig = fig %>% add_trace(x = GRAFICAR_PREDICCION$FECHA[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                    y =  GRAFICAR_PREDICCION$CANTIDAD[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                    color = GRAFICAR_PREDICCION$MES[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                    name = mes,
                                                    type = "scatter",
                                                    mode = "markers+lines",
                                                    hoverinfo = 'text',
                                                    text = paste('</br>Cantidad: ', GRAFICAR_PREDICCION$CANTIDAD[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                                 '</br>Mes: ', GRAFICAR_PREDICCION$MES[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                                 '</br>Día mes: ', GRAFICAR_PREDICCION$DIA_MES[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano],
                                                                 '</br>Festivo: ', GRAFICAR_PREDICCION$FESTIVO[GRAFICAR_PREDICCION$MES == mes & GRAFICAR_PREDICCION$ANO == ano]))
                            
                        }
                        
                    } 
                    
                    fig = fig %>% layout(xaxis = list(title = '<b> Fecha',
                                                      showgrid = F),
                                         yaxis = list(title = '<b> Incidentes'),
                                         title = "<b> Cantidad de incidentes diarios - Gravedad <i>Herido</i>",
                                         plot_bgcolor  = "rgba(0, 0, 0, 0)",
                                         paper_bgcolor = "rgba(0, 0, 0, 0)")
                    
                }
                
            } else {
                
                GRAFICAR_PREDICCION = (DATA[DATA$FECHA >= input$FECHA[1] & DATA$FECHA <= input$FECHA[2],])
                
                plot_ly(x = GRAFICAR_PREDICCION$FECHA,
                        color = GRAFICAR_PREDICCION$GRAVEDAD,
                        hoverinfo = 'text',
                        text = paste('</br>Cantidad: ', GRAFICAR_PREDICCION$CANTIDAD,
                                     '</br>Mes: ', GRAFICAR_PREDICCION$MES,
                                     '</br>Día mes: ', GRAFICAR_PREDICCION$DIA_MES,
                                     '</br>Diá de la semana: ', GRAFICAR_PREDICCION$DIA_SEM,
                                     '</br>Festivo: ', GRAFICAR_PREDICCION$FESTIVO)) %>% 
                    
                    add_trace(y = GRAFICAR_PREDICCION$CANTIDAD,
                              type = "scatter", 
                              mode = "markers+lines") %>%
                    
                    layout(xaxis = list(title = '<b> Fecha',
                                        showgrid = F),
                           yaxis = list(title = '<b> Incidentes'),
                           title = "<b> Cantidad de incidentes diarios",
                           plot_bgcolor  = "rgba(0, 0, 0, 0)",
                           paper_bgcolor = "rgba(0, 0, 0, 0)")
                
                
            }
            
        } else if(input$TIEMPO_PREDICCION == "SEMANA"){
            
            GRAFICAR_PREDICCION = (DATA[DATA$FECHA >= input$FECHA[1] & DATA$FECHA <= input$FECHA[2],])
            #AGRUPANDO LAS OBSERVACIONES POR SEMANA
            GRAFICAR_PREDICCION$SEMANA = 0
            
            for(gravedad in unique(GRAFICAR_PREDICCION$GRAVEDAD)){
                
                s = 1
                fin = 0
                inicio = 0
                
                for(i in 1:length(GRAFICAR_PREDICCION$GRAVEDAD[GRAFICAR_PREDICCION$GRAVEDAD == gravedad])){
                    dia = GRAFICAR_PREDICCION$DIA_SEM[GRAFICAR_PREDICCION$GRAVEDAD == gravedad][i]
                    if(dia == "Domingo"){
                        fin = i
                    }
                    if(dia == "Lunes"){
                        inicio = i
                    }
                    if(inicio != 0 & fin != 0){
                        GRAFICAR_PREDICCION$SEMANA[GRAFICAR_PREDICCION$GRAVEDAD == gravedad][inicio:fin] = s
                        inicio = 0
                        fin = 0
                        s = s + 1
                    }
                    if(s == 1 & dia == "Domingo" & inicio == 0){
                        GRAFICAR_PREDICCION$SEMANA[GRAFICAR_PREDICCION$GRAVEDAD == gravedad][1:i] = s
                        s = s+1
                        fin=0
                    }
                }
                GRAFICAR_PREDICCION$SEMANA[GRAFICAR_PREDICCION$SEMANA == 0 & GRAFICAR_PREDICCION$GRAVEDAD == gravedad] = s
                rm(inicio,fin,dia,i,s)
                
            }
            
            GRAFICAR_PREDICCION$AUX_FEST = ifelse(GRAFICAR_PREDICCION$FESTIVO == "No",0,1)
            GRAFICAR_PREDICCION$FESTIVO = 0
            for(i in unique(GRAFICAR_PREDICCION$SEMANA)){
                GRAFICAR_PREDICCION$MINF[GRAFICAR_PREDICCION$SEMANA == i] = as.character(min(GRAFICAR_PREDICCION$FECHA[GRAFICAR_PREDICCION$SEMANA == i]))
                GRAFICAR_PREDICCION$MAXF[GRAFICAR_PREDICCION$SEMANA == i] = as.character(max(GRAFICAR_PREDICCION$FECHA[GRAFICAR_PREDICCION$SEMANA == i]))
                GRAFICAR_PREDICCION$FESTIVO[GRAFICAR_PREDICCION$SEMANA == i] = max(GRAFICAR_PREDICCION$AUX_FEST[GRAFICAR_PREDICCION$SEMANA == i])
            }
            
            GRAFICAR_PREDICCION$FESTIVO = ifelse(GRAFICAR_PREDICCION$FESTIVO == 1,"Si","No")
            GRAFICAR_PREDICCION$MAXF = as.Date(GRAFICAR_PREDICCION$MAXF)
            GRAFICAR_PREDICCION$MINF = as.Date(GRAFICAR_PREDICCION$MINF)  
            
            GRAFICAR_PREDICCION = aggregate(CANTIDAD~SEMANA+GRAVEDAD+MINF+MAXF+FESTIVO,data = GRAFICAR_PREDICCION,FUN = sum)
            GRAFICAR_PREDICCION = GRAFICAR_PREDICCION[order(GRAFICAR_PREDICCION$SEMANA),]
            
            if(input$GRAVEDAD_PREDICCION == "TODAS"){
                
                plot_ly(x = GRAFICAR_PREDICCION$MINF,
                        color = GRAFICAR_PREDICCION$GRAVEDAD,
                        hoverinfo = 'text',
                        text = paste('</br>Cantidad:', GRAFICAR_PREDICCION$CANTIDAD,
                                     '</br>Inicio: ', GRAFICAR_PREDICCION$MINF,
                                     '</br>Fin: ', GRAFICAR_PREDICCION$MAXF,
                                     '</br>¿Festivo?: ', GRAFICAR_PREDICCION$FESTIVO)) %>% 
                    
                    add_trace(y = GRAFICAR_PREDICCION$CANTIDAD,
                              type = "scatter", 
                              mode = "markers+lines") %>%
                    
                    layout(xaxis = list(title = '<b> Fecha', showgrid = F),
                           yaxis = list(title = '<b> Incidentes'),
                           title = "<b> Cantidad de incidentes semanales",
                           plot_bgcolor  = "rgba(0, 0, 0, 0)",
                           paper_bgcolor = "rgba(0, 0, 0, 0)")
                
            } else if (input$GRAVEDAD_PREDICCION == "HERIDO"){
                
                GRAFICAR_PREDICCION = GRAFICAR_PREDICCION[GRAFICAR_PREDICCION$GRAVEDAD == "Herido",]
                
                plot_ly(x = GRAFICAR_PREDICCION$MINF,
                        hoverinfo = 'text',
                        text = paste('</br>Cantidad:', GRAFICAR_PREDICCION$CANTIDAD,
                                     '</br>Inicio: ', GRAFICAR_PREDICCION$MINF,
                                     '</br>Fin: ', GRAFICAR_PREDICCION$MAXF,
                                     '</br>¿Festivo?: ', GRAFICAR_PREDICCION$FESTIVO)) %>% 
                    
                    add_trace(y = GRAFICAR_PREDICCION$CANTIDAD,
                              type = "scatter", 
                              mode = "markers+lines") %>%
                    
                    layout(xaxis = list(title = '<b> Fecha', showgrid = F),
                           yaxis = list(title = '<b> Incidentes'),
                           title = "<b> Cantidad de incidentes semanales - Gravedad <i>Herido</i>",
                           plot_bgcolor  = "rgba(0, 0, 0, 0)",
                           paper_bgcolor = "rgba(0, 0, 0, 0)")
                
            } else if (input$GRAVEDAD_PREDICCION == "MUERTO"){
                
                GRAFICAR_PREDICCION = GRAFICAR_PREDICCION[GRAFICAR_PREDICCION$GRAVEDAD == "Muerto",]
                
                plot_ly(x = GRAFICAR_PREDICCION$MINF,
                        hoverinfo = 'text',
                        text = paste('</br>Cantidad:', GRAFICAR_PREDICCION$CANTIDAD,
                                     '</br>Inicio: ', GRAFICAR_PREDICCION$MINF,
                                     '</br>Fin: ', GRAFICAR_PREDICCION$MAXF,
                                     '</br>¿Festivo?: ', GRAFICAR_PREDICCION$FESTIVO)) %>% 
                    
                    add_trace(y = GRAFICAR_PREDICCION$CANTIDAD,
                              type = "scatter", 
                              mode = "markers+lines") %>%
                    
                    layout(xaxis = list(title = '<b> Fecha', showgrid = F),
                           yaxis = list(title = '<b> Incidentes', autotick = F),
                           title = "<b> Cantidad de incidentes semanales - Gravedad <i>Muerto</i>",
                           plot_bgcolor  = "rgba(0, 0, 0, 0)",
                           paper_bgcolor = "rgba(0, 0, 0, 0)")
                
            } else {
                
                GRAFICAR_PREDICCION = GRAFICAR_PREDICCION[GRAFICAR_PREDICCION$GRAVEDAD == "Solo Daño",]
                
                plot_ly(x = GRAFICAR_PREDICCION$MINF,
                        hoverinfo = 'text',
                        text = paste('</br>Cantidad:', GRAFICAR_PREDICCION$CANTIDAD,
                                     '</br>Inicio: ', GRAFICAR_PREDICCION$MINF,
                                     '</br>Fin: ', GRAFICAR_PREDICCION$MAXF,
                                     '</br>¿Festivo?: ', GRAFICAR_PREDICCION$FESTIVO)) %>% 
                    
                    add_trace(y = GRAFICAR_PREDICCION$CANTIDAD,
                              type = "scatter", 
                              mode = "markers+lines") %>%
                    
                    layout(xaxis = list(title = '<b> Fecha', showgrid = F),
                           yaxis = list(title = '<b> Incidentes'),
                           title = "<b> Cantidad de incidentes semanales - Gravedad <i>Solo Daño</i>",
                           plot_bgcolor  = "rgba(0, 0, 0, 0)",
                           paper_bgcolor = "rgba(0, 0, 0, 0)")
                
            }
            
        } else {
            
            inicio_herido = as.numeric(rownames(DATA_MES[DATA_MES$NOMBRE_MES == input$MES_DESDE & 
                                                             DATA_MES$ANO == input$ANO_DESDE &
                                                             DATA_MES$GRAVEDAD == "Herido",]))
            
            fin_herido = as.numeric(rownames(DATA_MES[DATA_MES$NOMBRE_MES == input$MES_HASTA & 
                                                          DATA_MES$ANO == input$ANO_HASTA &
                                                          DATA_MES$GRAVEDAD == "Herido",]))
            
            inicio_muerto = as.numeric(rownames(DATA_MES[DATA_MES$NOMBRE_MES == input$MES_DESDE & 
                                                             DATA_MES$ANO == input$ANO_DESDE &
                                                             DATA_MES$GRAVEDAD == "Muerto",]))
            
            fin_muerto = as.numeric(rownames(DATA_MES[DATA_MES$NOMBRE_MES == input$MES_HASTA & 
                                                          DATA_MES$ANO == input$ANO_HASTA &
                                                          DATA_MES$GRAVEDAD == "Muerto",]))
            
            inicio_dano = as.numeric(rownames(DATA_MES[DATA_MES$NOMBRE_MES == input$MES_DESDE & 
                                                           DATA_MES$ANO == input$ANO_DESDE &
                                                           DATA_MES$GRAVEDAD == "Solo Daño",]))
            
            fin_dano = as.numeric(rownames(DATA_MES[DATA_MES$NOMBRE_MES == input$MES_HASTA & 
                                                        DATA_MES$ANO == input$ANO_HASTA &
                                                        DATA_MES$GRAVEDAD == "Solo Daño",]))
            
            if(input$GRAVEDAD_PREDICCION == "TODAS"){
                
                plot_ly(x = DATA_MES$FECHA[inicio_dano:fin_dano]) %>% 
                    add_trace(y = DATA_MES$CANTIDAD[inicio_dano:fin_dano],
                              name = "Solo Daño",
                              hoverinfo = 'text',
                              text = paste('</br>Cantidad:', DATA_MES$CANTIDAD[inicio_dano:fin_dano],
                                           '</br>Mes: ', DATA_MES$MOSTRAR[inicio_dano:fin_dano]),
                              type = "scatter", 
                              mode = "markers+lines") %>%
                    add_trace(y = DATA_MES$CANTIDAD[inicio_muerto:fin_muerto],
                              name = "Muerto",
                              hoverinfo = 'text',
                              text = paste('</br>Cantidad:', DATA_MES$CANTIDAD[inicio_muerto:fin_muerto],
                                           '</br>Mes: ', DATA_MES$MOSTRAR[inicio_muerto:fin_muerto]),
                              type = "scatter", 
                              mode = "markers+lines") %>%
                    add_trace(y = DATA_MES$CANTIDAD[inicio_herido:fin_herido],
                              name = "Herido",
                              hoverinfo = 'text',
                              text = paste('</br>Cantidad:', DATA_MES$CANTIDAD[inicio_herido:fin_herido],
                                           '</br>Mes: ', DATA_MES$MOSTRAR[inicio_herido:fin_herido]),
                              type = "scatter", 
                              mode = "markers+lines") %>%
                    layout(xaxis = list(title = '<b> Fecha',
                                        showgrid  = F,
                                        showticklabels = T),
                           yaxis = list(title = '<b> Incidentes'),
                           title = "<b> Cantidad de incidentes mensuales",
                           plot_bgcolor  = "rgba(0, 0, 0, 0)",
                           paper_bgcolor = "rgba(0, 0, 0, 0)")
                
            } else if(input$GRAVEDAD_PREDICCION == "HERIDO"){
                
                plot_ly(x = DATA_MES$FECHA[inicio_herido:fin_herido]) %>% 
                    add_trace(y = DATA_MES$CANTIDAD[inicio_herido:fin_herido],
                              name = "Herido",
                              hoverinfo = 'text',
                              text = paste('</br>Cantidad:', DATA_MES$CANTIDAD[inicio_herido:fin_herido],
                                           '</br>Mes: ', DATA_MES$MOSTRAR[inicio_herido:fin_herido]),
                              type = "scatter", 
                              mode = "markers+lines") %>%
                    layout(xaxis = list(title = '<b> Fecha',
                                        showgrid  = F,
                                        showticklabels = T),
                           yaxis = list(title = '<b> Incidentes'),
                           title = "<b> Cantidad de incidentes mensuales - Gravedad <i>Herido</i>",
                           plot_bgcolor  = "rgba(0, 0, 0, 0)",
                           paper_bgcolor = "rgba(0, 0, 0, 0)")
                
            } else if(input$GRAVEDAD_PREDICCION == "DANO"){
                
                plot_ly(x = DATA_MES$FECHA[inicio_dano:fin_dano]) %>% 
                    add_trace(y = DATA_MES$CANTIDAD[inicio_dano:fin_dano],
                              name = "Herido",
                              hoverinfo = 'text',
                              text = paste('</br>Cantidad:', DATA_MES$CANTIDAD[inicio_dano:fin_dano],
                                           '</br>Mes: ', DATA_MES$MOSTRAR[inicio_dano:fin_dano]),
                              type = "scatter", 
                              mode = "markers+lines") %>%
                    layout(xaxis = list(title = '<b> Fecha',
                                        showgrid  = F,
                                        showticklabels = T),
                           yaxis = list(title = '<b> Incidentes'),
                           title = "<b> Cantidad de incidentes mensuales - Gravedad <i>Solo Daño</i>",
                           plot_bgcolor  = "rgba(0, 0, 0, 0)",
                           paper_bgcolor = "rgba(0, 0, 0, 0)")
                
            } else if(input$GRAVEDAD_PREDICCION == "MUERTO"){
                
                plot_ly(x = DATA_MES$FECHA[inicio_muerto:fin_muerto]) %>% 
                    add_trace(y = DATA_MES$CANTIDAD[inicio_muerto:fin_muerto],
                              name = "Herido",
                              hoverinfo = 'text',
                              text = paste('</br>Cantidad:', DATA_MES$CANTIDAD[inicio_muerto:fin_muerto],
                                           '</br>Mes: ', DATA_MES$MOSTRAR[inicio_muerto:fin_muerto]),
                              type = "scatter", 
                              mode = "markers+lines") %>%
                    layout(xaxis = list(title = '<b> Fecha',
                                        showgrid  = F,
                                        showticklabels = T),
                           yaxis = list(title = '<b> Incidentes',
                                        autotick = F),
                           title = "<b> Cantidad de incidentes mensuales - Gravedad <i>Muerto</i>",
                           plot_bgcolor  = "rgba(0, 0, 0, 0)",
                           paper_bgcolor = "rgba(0, 0, 0, 0)")
                
            }
            
        }
        
    })
    
    output$MAPA = renderLeaflet({
        
        ano_mapa = ifelse(input$ANO_MAPA == "T","",paste(input$ANO_MAPA,"_",sep=""))
        
        mes_mapa = ifelse(input$MES_MAPA == "T","",paste(input$MES_MAPA,"_",sep=""))
        
        gravedad_mapa = ifelse(input$GRAVEDAD_MAPA == "T","",paste(input$GRAVEDAD_MAPA,"_",sep=""))
        
        semana_mapa = ifelse(input$SEMANA_MAPA == "T","",paste(input$SEMANA_MAPA,"_",sep=""))
        
        momento_mapa = ifelse(input$MOMENTO_MAPA == "T","",paste(input$MOMENTO_MAPA,"_",sep=""))
        
        pal1 = colorNumeric("viridis",NULL)
        pal2 = colorNumeric("Blues",NULL)
        
        name = paste("CANTIDAD_TOTAL_",ano_mapa,mes_mapa,gravedad_mapa,semana_mapa,momento_mapa,sep="")
        
        labels = sprintf("<strong>%s</strong><br/>Cantidad de incidentes: %g",
                         DATA_MAPA$NOMBRE_BARRIO, DATA_MAPA@data[,name]
        ) %>% lapply(htmltools::HTML)
        
        leaflet(DATA_MAPA) %>%
            addTiles() %>%
            addProviderTiles(providers$HERE.basicMap, 
                             options = providerTileOptions(
                                 id = "mapbox.light",
                                 accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
            addPolygons(stroke = T,
                        weight = 1,
                        fillColor = ifelse(DATA_MAPA@data[,name]>0,pal1(DATA_MAPA@data[,name]),pal2(DATA_MAPA@data[,name])),
                        opacity = 0.6,
                        color = "white",
                        dashArray = "0",
                        fillOpacity = 0.6,
                        label = labels,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "black",
                            dashArray = "",
                            fillOpacity = 0.8,
                            bringToFront = TRUE) )%>% 
            addLegend(pal = pal1, 
                      values = DATA_MAPA@data[,name],
                      opacity = 0.5, 
                      title = ("<center> <b> Accidentalidad total \n"),
                      position = "topright")
        
        
        
    })
    
    output$AGRUPAMIENTO = renderLeaflet({
        
        mapa_agrupacion <- leaflet(barrios) %>% 
            addTiles() 
        
        if(input$TIPO_AGRUPAMIENTO == "TOTAL"){
            
            if(input$FECHA_AGRUPACION == "2014"){
                
                datosAg[datosAg$Cantidad_2014 == 0,"Tipo_Accidentalidad_2014"] = "Sin accidentalidad"
                datosAg[datosAg$Cantidad_2014 == 0,"Grupo_2014"] = 0
                datosAg$pal = "#B2182B"
                datosAg$pal[datosAg$Grupo_2014 == 0] = "white"
                datosAg$pal[datosAg$Grupo_2014 == 1] = "#4393C3"
                datosAg$pal[datosAg$Grupo_2014 == 2] = "#F4A582"
                labels<- paste("<strong>",datosAg$barrios,"</strong><br/>",
                               "<strong>Cantidad de incidentes: </strong>",datosAg$Cantidad_2014,
                               "<br/><strong>Categoría: </strong>",datosAg$Tipo_Accidentalidad_2014,
                               sep="")
                
                
                mapa_agrupacion %>% 
                    addPolygons(weight= 0.5,
                                smoothFactor=1,
                                color= "black",
                                dashArray = "0",
                                opacity = 1,
                                fillOpacity = 0.8,
                                fillColor = datosAg$pal,
                                highlight= highlightOptions(
                                    weight=4,
                                    color="orange",
                                    fillOpacity = 0.7,
                                    bringToFront = TRUE
                                ),
                                label = lapply(labels, HTML)
                    ) %>%
                    addLegend(colors =c("#B2182B","#F4A582","#4393C3","white" ),
                              labels = c("Accidentalidad alta",
                                         "Accidentalidad media",
                                         "Accidentalidad baja",
                                         "Sin accidentalidad"),
                              title = "<center>Categorías de accidentalidad</br>2014",
                              opacity = 1,
                              position = "topright")
                
                
            } else if(input$FECHA_AGRUPACION == "2015"){
                
                datosAg[datosAg$Cantidad_2015 == 0,"Tipo_Accidentalidad_2015"] = "Sin accidentalidad"
                datosAg[datosAg$Cantidad_2015 == 0,"Grupo_2015"] = 0
                datosAg$pal = "#B2182B"
                datosAg$pal[datosAg$Grupo_2015 == 0] = "white"
                datosAg$pal[datosAg$Grupo_2015 == 1] = "#4393C3"
                datosAg$pal[datosAg$Grupo_2015 == 2] = "#F4A582"
                labels<- paste("<strong>",datosAg$barrios,"</strong><br/>",
                               "<strong>Cantidad de incidentes: </strong>",datosAg$Cantidad_2015,
                               "<br/><strong>Categoría: </strong>",datosAg$Tipo_Accidentalidad_2015,
                               sep="")
                
                mapa_agrupacion %>% 
                    addPolygons(weight= 0.5,
                                smoothFactor=1,
                                color= "black",
                                dashArray = "0",
                                opacity = 1,
                                fillOpacity = 0.8,
                                fillColor = datosAg$pal,
                                highlight= highlightOptions(
                                    weight=4,
                                    color="orange",
                                    fillOpacity = 0.7,
                                    bringToFront = TRUE
                                ),
                                label = lapply(labels, HTML)
                    ) %>%
                    addLegend(colors =c("#B2182B","#F4A582","#4393C3","white" ),
                              labels = c("Accidentalidad alta",
                                         "Accidentalidad media",
                                         "Accidentalidad baja",
                                         "Sin accidentalidad"),
                              title = "<center>Categorías de accidentalidad</br>2015",
                              opacity = 1,
                              position = "topright")
                
                
                
            } else if(input$FECHA_AGRUPACION == "2016"){
                
                datosAg[datosAg$Cantidad_2016 == 0,"Tipo_Accidentalidad_2016"] = "Sin accidentalidad"
                datosAg[datosAg$Cantidad_2016 == 0,"Grupo_2016"] = 0
                datosAg$pal = "#B2182B"
                datosAg$pal[datosAg$Grupo_2016 == 0] = "white"
                datosAg$pal[datosAg$Grupo_2016 == 1] = "#4393C3"
                datosAg$pal[datosAg$Grupo_2016 == 2] = "#F4A582"
                labels<- paste("<strong>",datosAg$barrios,"</strong><br/>",
                               "<strong>Cantidad de incidentes: </strong>",datosAg$Cantidad_2016,
                               "<br/><strong>Categoría: </strong>",datosAg$Tipo_Accidentalidad_2016,
                               sep="")
                
                mapa_agrupacion %>% 
                    addPolygons(weight= 0.5,
                                smoothFactor=1,
                                color= "black",
                                dashArray = "0",
                                opacity = 1,
                                fillOpacity = 0.8,
                                fillColor = datosAg$pal,
                                highlight= highlightOptions(
                                    weight=4,
                                    color="orange",
                                    fillOpacity = 0.7,
                                    bringToFront = TRUE
                                ),
                                label = lapply(labels, HTML)
                    ) %>%
                    addLegend(colors =c("#B2182B","#F4A582","#4393C3","white" ),
                              labels = c("Accidentalidad alta",
                                         "Accidentalidad media",
                                         "Accidentalidad baja",
                                         "Sin accidentalidad"),
                              title = "<center>Categorías de accidentalidad</br>2016",
                              opacity = 1,
                              position = "topright")
                
                
            } else if(input$FECHA_AGRUPACION == "2017"){
                
                datosAg[datosAg$Cantidad_2017 == 0,"Tipo_Accidentalidad_2017"] = "Sin accidentalidad"
                datosAg[datosAg$Cantidad_2017 == 0,"Grupo_2017"] = 0
                datosAg$pal = "#B2182B"
                datosAg$pal[datosAg$Grupo_2017 == 0] = "white"
                datosAg$pal[datosAg$Grupo_2017 == 1] = "#4393C3"
                datosAg$pal[datosAg$Grupo_2017 == 2] = "#F4A582"
                labels<- paste("<strong>",datosAg$barrios,"</strong><br/>",
                               "<strong>Cantidad de incidentes: </strong>",datosAg$Cantidad_2017,
                               "<br/><strong>Categoría: </strong>",datosAg$Tipo_Accidentalidad_2017,
                               sep="")
                
                mapa_agrupacion %>%
                    addPolygons(weight= 0.5,
                                smoothFactor=1,
                                color= "black",
                                dashArray = "0",
                                opacity = 1,
                                fillOpacity = 0.8,
                                fillColor = datosAg$pal,
                                highlight= highlightOptions(
                                    weight=4,
                                    color="orange",
                                    fillOpacity = 0.7,
                                    bringToFront = TRUE
                                ),
                                label = lapply(labels, HTML)
                    ) %>%
                    addLegend(colors =c("#B2182B","#F4A582","#4393C3","white" ),
                              labels = c("Accidentalidad alta",
                                         "Accidentalidad media",
                                         "Accidentalidad baja",
                                         "Sin accidentalidad"),
                              title = "<center>Categorías de accidentalidad</br>2017",
                              opacity = 1,
                              position = "topright")
                
            } else if(input$FECHA_AGRUPACION == "2018"){
                
                datosAg[datosAg$Cantidad_2018 == 0,"Tipo_Accidentalidad_2018"] = "Sin accidentalidad"
                datosAg[datosAg$Cantidad_2018 == 0,"Grupo_2018"] = 0
                datosAg$pal = "#B2182B"
                datosAg$pal[datosAg$Grupo_2018 == 0] = "white"
                datosAg$pal[datosAg$Grupo_2018 == 1] = "#4393C3"
                datosAg$pal[datosAg$Grupo_2018 == 2] = "#F4A582"
                labels<- paste("<strong>",datosAg$barrios,"</strong><br/>",
                               "<strong>Cantidad de incidentes: </strong>",datosAg$Cantidad_2018,
                               "<br/><strong>Categoría: </strong>",datosAg$Tipo_Accidentalidad_2018,
                               sep="")
                
                mapa_agrupacion %>%
                    addPolygons(weight= 0.5,
                                smoothFactor=1,
                                color= "black",
                                dashArray = "0",
                                opacity = 1,
                                fillOpacity = 0.8,
                                fillColor = datosAg$pal,
                                highlight= highlightOptions(
                                    weight=4,
                                    color="orange",
                                    fillOpacity = 0.7,
                                    bringToFront = TRUE
                                ),
                                label = lapply(labels, HTML)
                    ) %>%
                    addLegend(colors =c("#B2182B","#F4A582","#4393C3","white" ),
                              labels = c("Accidentalidad alta",
                                         "Accidentalidad media",
                                         "Accidentalidad baja",
                                         "Sin accidentalidad"),
                              title = "<center>Categorías de accidentalidad</br>2018",
                              opacity = 1,
                              position = "topright")
                
            } else {
                
                datosAg[datosAg$Cantidad_2014_2018 == 0,"Tipo_Accidentalidad_2014_2018"] = "Sin accidentalidad"
                datosAg[datosAg$Cantidad_2014_2018 == 0,"Grupo_2014_2018"] = 0
                datosAg$pal = "#B2182B"
                datosAg$pal[datosAg$Grupo_2014_2018 == 0] = "white"
                datosAg$pal[datosAg$Grupo_2014_2018 == 1] = "#4393C3"
                datosAg$pal[datosAg$Grupo_2014_2018 == 2] = "#F4A582"
                labels<- paste("<strong>",datosAg$barrios,"</strong><br/>",
                               "<strong>Cantidad de incidentes: </strong>",datosAg$Cantidad_2014_2018,
                               "<br/><strong>Categoría: </strong>",datosAg$Tipo_Accidentalidad_2014_2018,
                               sep="")
                
                mapa_agrupacion %>%
                    addPolygons(weight= 0.5,
                                smoothFactor=1,
                                color= "black",
                                dashArray = "0",
                                opacity = 1,
                                fillOpacity = 0.8,
                                fillColor = datosAg$pal,
                                highlight= highlightOptions(
                                    weight=4,
                                    color="orange",
                                    fillOpacity = 0.7,
                                    bringToFront = TRUE
                                ),
                                label = lapply(labels, HTML)
                    ) %>%
                    addLegend(colors =c("#B2182B","#F4A582","#4393C3","white" ),
                              labels = c("Accidentalidad alta",
                                         "Accidentalidad media",
                                         "Accidentalidad baja",
                                         "Sin accidentalidad"),
                              title = "<center>Categorías de accidentalidad</br>2014 - 2018",
                              opacity = 1,
                              position = "topright")
                
            }
            
        } else if(input$TIPO_AGRUPAMIENTO == "GRAVEDAD"){
            
            if(input$GRAVEDAD_AGRUPACION == "DANO"){
                
                datosAg[datosAg$Accidentes_Hora_Pico_2014_2018 == 0,"Tipo_Accidentalidad_DANOS_2014_2018"] = "Sin accidentalidad"
                datosAg[datosAg$Accidentes_Hora_Pico_2014_2018 == 0,"grupo_DANOS_2014_2018"] = 0
                datosAg$pal = "#B2182B"
                datosAg$pal[datosAg$grupo_HoraPico_DANOS_2014_2018 == 0] = "white"
                datosAg$pal[datosAg$grupo_HoraPico_DANOS_2014_2018 == 1] = "#4393C3"
                datosAg$pal[datosAg$grupo_HoraPico_DANOS_2014_2018 == 2] = "#F4A582"
                labels<- paste("<strong>",datosAg$barrios,"</strong><br/>",
                               "<strong>Cantidad de incidentes: </strong>",datosAg$Accidentes_Solo_Danos_2014_2018,
                               "<br/><strong>Categoría: </strong>",datosAg$Tipo_Accidentalidad_DANOS_2014_2018,
                               sep="")
                
                mapa_agrupacion %>%
                    addPolygons(weight= 0.5,
                                smoothFactor=1,
                                color= "black",
                                dashArray = "0",
                                opacity = 1,
                                fillOpacity = 0.8,
                                fillColor = datosAg$pal,
                                highlight= highlightOptions(
                                    weight=4,
                                    color="orange",
                                    fillOpacity = 0.7,
                                    bringToFront = TRUE
                                ),
                                label = lapply(labels, HTML)
                    ) %>%
                    addLegend(colors =c("#B2182B","#F4A582","#4393C3","white" ),
                              labels = c("Accidentalidad alta",
                                         "Accidentalidad media",
                                         "Accidentalidad baja",
                                         "Sin accidentalidad"),
                              title = "<center>Categorías de accidentalidad</br>2014 - 2018 </br> Solo Daños",
                              opacity = 1,
                              position = "topright")
                
            } else {
                
                datosAg[datosAg$Accidentes_Gravedad_Humano_2014_2018== 0,"Tipo_Accidentalidad_Gravedad_2014_2018"] = "Sin accidentalidad"
                datosAg[datosAg$Accidentes_Gravedad_Humano_2014_2018== 0,"grupo_Gravedad_2014_2018"] = 0
                datosAg$pal = "#B2182B"
                datosAg$pal[datosAg$grupo_Gravedad_2014_2018 == 0] = "white"
                datosAg$pal[datosAg$grupo_Gravedad_2014_2018 == 1] = "#4393C3"
                datosAg$pal[datosAg$grupo_Gravedad_2014_2018 == 2] = "#F4A582"
                labels<- paste("<strong>",datosAg$barrios,"</strong><br/>",
                               "<strong>Cantidad de incidentes: </strong>",datosAg$Tipo_Accidentalidad_Gravedad_2014_2018,
                               "<br/><strong>Categoría: </strong>",datosAg$Tipo_Accidentalidad_Gravedad_2014_2018,
                               sep="")
                
                mapa_agrupacion %>%
                    addPolygons(weight= 0.5,
                                smoothFactor=1,
                                color= "black",
                                dashArray = "0",
                                opacity = 1,
                                fillOpacity = 0.8,
                                fillColor = datosAg$pal,
                                highlight= highlightOptions(
                                    weight=4,
                                    color="orange",
                                    fillOpacity = 0.7,
                                    bringToFront = TRUE
                                ),
                                label = lapply(labels, HTML)
                    ) %>%
                    addLegend(colors =c("#B2182B","#F4A582","#4393C3","white" ),
                              labels = c("Accidentalidad alta",
                                         "Accidentalidad media",
                                         "Accidentalidad baja",
                                         "Sin accidentalidad"),
                              title = "<center>Categorías de accidentalidad</br>2014 - 2018 </br>Muertos & Heridos",
                              opacity = 1,
                              position = "topright")
                
            }
            
        } else {
            
            if(input$GRAVEDAD_AGRUPACION == "DANO"){
                
                datosAg[datosAg$Accidentes_Hora_Pico_2014_2018 == 0,"Tipo_Accidentalidad_HoraPico_Gravedad_2014_2018"] = "Sin accidentalidad"
                datosAg[datosAg$Accidentes_Hora_Pico_2014_2018 == 0,"grupo_HoraPico_DANOS_2014_2018"] = 0
                datosAg$pal = "#B2182B"
                datosAg$pal[datosAg$grupo_HoraPico_DANOS_2014_2018 == 0] = "white"
                datosAg$pal[datosAg$grupo_HoraPico_DANOS_2014_2018 == 1] = "#4393C3"
                datosAg$pal[datosAg$grupo_HoraPico_DANOS_2014_2018 == 2] = "#F4A582"
                labels<- paste("<strong>",datosAg$barrios,"</strong><br/>",
                               "<strong>Cantidad de incidentes: </strong>",datosAg$Accidentes_HoraPico_Solo_Danos_2014_2018,
                               "<br/><strong>Categoría: </strong>",datosAg$Tipo_Accidentalidad_HoraPico_Gravedad_2014_2018,
                               sep="")
                
                mapa_agrupacion %>%
                    addTiles() %>% 
                    addPolygons(weight= 0.5,
                                smoothFactor=1,
                                color= "black",
                                dashArray = "0",
                                opacity = 1,
                                fillOpacity = 0.8,
                                fillColor = datosAg$pal,
                                highlight= highlightOptions(
                                    weight=4,
                                    color="orange",
                                    fillOpacity = 0.7,
                                    bringToFront = TRUE
                                ),
                                label = lapply(labels, HTML)
                    ) %>%
                    addLegend(colors =c("#B2182B","#F4A582","#4393C3","white" ),
                              labels = c("Accidentalidad alta",
                                         "Accidentalidad media",
                                         "Accidentalidad baja",
                                         "Sin accidentalidad"),
                              title = "<center>Categorías de accidentalidad</br>2014 - 2018 | Horas pico</br>Solo Daños",
                              opacity = 1,
                              position = "topright")
                
            } else {
                
                datosAg[datosAg$Accidentes_Hora_Pico_2014_2018 == 0,"Tipo_Accidentalidad_HoraPico_Gravedad_2014_2018"] = "Sin accidentalidad"
                datosAg[datosAg$Accidentes_Hora_Pico_2014_2018 == 0,"grupo_HoraPico_Gravedad_2014_2018"] = 0
                datosAg$pal = "#B2182B"
                datosAg$pal[datosAg$grupo_HoraPico_Gravedad_2014_2018 == 0] = "white"
                datosAg$pal[datosAg$grupo_HoraPico_Gravedad_2014_2018 == 1] = "#4393C3"
                datosAg$pal[datosAg$grupo_HoraPico_Gravedad_2014_2018 == 2] = "#F4A582"
                labels<- paste("<strong>",datosAg$barrios,"</strong><br/>",
                               "<strong>Cantidad de incidentes: </strong>",datosAg$Accidentes_HoraPico_Gravedad_Humano_2014_2018 ,
                               "<br/><strong>Categoría: </strong>",datosAg$Tipo_Accidentalidad_HoraPico_Gravedad_2014_2018,
                               sep="")
                
                mapa_agrupacion %>%
                    addPolygons(weight= 0.5,
                                smoothFactor=1,
                                color= "black",
                                dashArray = "0",
                                opacity = 1,
                                fillOpacity = 0.8,
                                fillColor = datosAg$pal,
                                highlight= highlightOptions(
                                    weight=4,
                                    color="orange",
                                    fillOpacity = 0.7,
                                    bringToFront = TRUE
                                ),
                                label = lapply(labels, HTML)
                    ) %>%
                    addLegend(colors =c("#B2182B","#F4A582","#4393C3","white" ),
                              labels = c("Accidentalidad alta",
                                         "Accidentalidad media",
                                         "Accidentalidad baja",
                                         "Sin accidentalidad"),
                              title = "<center>Categorías de accidentalidad</br>2014 - 2018 | Horas pico</br>Muertos & Heridos",
                              opacity = 1,
                              position = "topright")
                
            }
            
        }
        
    })
    
}


shinyApp(ui = ui, server = server)
