# Script del dashboard para JMMI Colombia componente comerciantes
# You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
################################################################################
rm(list = ls())
#### 1 LOAD PACKAGES ###########################################################

library(dplyr)                                                                    # data wrangling work horse
library(tidyr)                                                                    # additional data wrangling
library(tidytidbits)                                                              # for conditional piping
library(stringr)                                                                  # to do some operations with strings
library(shiny)                                                                    # for shiny app functions
library(shinyWidgets)                                                             # additional UI options for shiny
library(shinythemes)                                                              # to apply a theme to the shiny app
library(sf)                                                                       # to read/manipulate shapefiles
library(leaflet)                                                                  # to display maps
library(leaflet.extras)                                                           # additional options for leaflet
library(highcharter)                                                              # to build plots
library(DT)                                                                       # for datatable in data explorer
library(kableExtra)                                                               # to make tables
library(scales)                                                                   # to define percentages

source("source/execute_if.R", encoding = "UTF-8")
source("source/execute_in_pipeline.R", encoding = "UTF-8")

#### 2 LOAD DATA ###############################################################

data <- read.csv("data/longterm_data.csv", na.strings = c("NA", ""), encoding="latin1", sep = ";")   # load JMMI dataset
list_existencia <- read.csv("data/list_existencia.csv", na.strings = c("NA", ""), encoding="latin1", sep = ";") 
item_list   <- read.csv("data/item_list.csv", encoding="latin1")                                     # load item list
item_list_1 <-  read.csv("data/item_list_1.csv", encoding="latin1")

# Cargar shps
departamento <- st_read("gis/Admin1_UnodcOcha_01012009.shp")
country     <- st_read("gis/World_admin0_countries_py_WFP_nd.shp")              # load shapefile with country border

# Ajustar a tipo Date
data$mes <- lubridate::dmy(data$mes)

### Traer las tablas RDS #######################################################
prices_long <- readRDS("data/prices_long.RData")
full <- readRDS("data/full.RData")
indicators2 <- readRDS("data/indicators2.RData")
stock <- readRDS("data/stock.RData")
indicator_list <- readRDS("data/indicator_list.RData")
dias_stock <- readRDS("data/dias_stock.RData")


#### 3 REFERENCES ##############################################################

max(unique(prices_long$Fecha))

dates <- sort(unique(prices_long$Fecha))                                          # define list with date range in data
dates_min  <- min(prices_long$Fecha)                                              # set minimum date to be displayed
dates_max  <- max(prices_long$Fecha)                                              # maximum date in data                   
dates_stock <- sort(unique(stock$Fecha))
dates_max_s <- max(stock$Fecha)


plot_location_list <- prices_long %>%                                             # define location list (which is later used as choice filter options)
  ungroup() %>%
  select(Departamento, Municipio) %>%                                             # extract governorate and district columns
  arrange(Departamento, Municipio) %>%                                            # list alphabetically
  filter(!duplicated(Departamento))                                               # remove duplicates

indicator_list <- names(indicators2)                                              # extract additional indicator list

map_location_list <- prices_long %>%                                              # define location list (which is later used as choice filter options)
  ungroup() %>%
  select(Departamento, Municipio, Fecha) %>%                                             # extract governorate and district columns
  arrange(Departamento, Municipio) %>%                                            # list alphabetically
  filter(!duplicated(Departamento))  # remove duplicates

cols      <- c("rgb(238,88,89)",   "rgb(88,88,90)",    "rgb(165,201,161)",        # define color palette for plot lines
               "rgb(86,179,205)",  "rgb(246,158,97)",  "rgb(255,246,122)",        # están los colores reach + una paleta de amarillos y naranjas
               "rgb(210,203,184)", "rgb(247,172,172)", "rgb(172,172,173)",        # azul claro y distintos tonos de beige
               "rgb(210,228,208)", "rgb(171,217,230)", "rgb(251,207,176)",
               "rgb(255,251,189)", "rgb(233,229,220)")

full_list <- data.frame(Item = indicator_list,                                    # create full indicator list
                        Group = "Indicadores") %>% arrange(Item)
full_list <- rbind(item_list, full_list)                                          # combine prices and additional indicators


#### 4 UI ######################################################################
ui <- bootstrapPage(
  navbarPage("JMMI COLOMBIA",                                                                                     # Nombre del DASHBOARD o del panel navegador
             theme = shinytheme("simplex"),                                                                       # Tema del navbarpage
             
             
             #### 1ra página  INFORMACIÓN ######################################
             tabPanel("General",
                      icon = icon("pen-to-square"),
                      div(class ="generaltabla",
                          tags$head(includeCSS("styles.css")),
                          
                          leafletOutput("map_home", width = "100%", height = "100%"),
                          
                          absolutePanel(id = "homegeneral", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                                        left = 10, right = 10, top = 40, bottom = 10,
                                        
                                        column(4,
                                               h4('Acerca del Dashboard'),
                                               p("El propósito de este dashboard es informar a la comunidad humanitaria, de forma práctica e interactiva, acerca de los indicadores 
                              más relevantes de la Iniciativa Conjunta para el Monitoreo de Mercados de Colombia (JMMI), en su componente de comerciantes: precios
                              de los productos monitoreados, días de existencia (stock) y desafíos de reabastecimiento reportados por los comerciantes. ",
                                                 style="text-align:justify;margin-bottom:20px"),
                                               
                                               h4('Navegación'),
                                               p("En la primera pestaña, “Dashboard”, encontrará los precios de cada uno de los productos monitoreados (alimentarios y no alimentarios),
                              por departamentos, en un mapa coroplético. Además, en el botón inferior izquierdo (i), podrá consultar el enlace a las hojas informativas de las 
                              diferentes rondas de recolección; en la segunda pestaña, “Gráfica de Precios”, encontrará también los precios de los productos
                              monitoreados por departamento en una gráfica de líneas; la tercera pestaña, “Mapa”, describe a través de un mapa coroplético las existencias
                              (stock) de los productos monitoreados y los desafíos de reabastecimiento reportados por los y las comerciantes. Cada uno de estos datos es 
                              reportado a nivel departamental; finalmente, en la pestaña “ Data Price Explorer” encontrará en una base de datos, los precios de cada uno de
                              los productos monitoreados a nivel departamental y municipal.", 
                                                 style="text-align:justify;margin-bottom:20px"),
                                               p("Es importante resaltar que los datos presentados en este dashboard corresponden a todas las rondas monitoreadas desde el año 2020 hasta agosto
                              de 2022 por el equipo de REACH, con el apoyo de los socios del Grupo de Transferencias Monetarias de Colombia (GTM).", style="text-align:justify;margin-bottom:20px"),
                                               
                                        ),
                                        
                                        column(4,
                                               h4('Contexto'),
                                               p("Desde el 2015, Venezuela ha sufrido una grave crisis política y económica ocasionando el desplazamiento de millones 
                              de personas en todo el mundo. En la actualidad se estima que más de 2.4 millones de migrantes han llegado a Colombia
                              para satisfacer sus necesidades básicas y requieren asistencia humanitaria.", style="text-align:justify;margin-bottom:5px"),
                                               p("Con el fin de abordar las necesidades, grupos humanitarios están implementando intervenciones basadas en efectivo como medio
                              para ayudar a los hogares vulnerables. Sin embargo, las intervenciones basadas en dinero en efectivo requieren información precisa
                              de las cadenas de suministro y mercados que funcionen adecuadamente y que proporciones productos básicos de forma continua.", 
                                                 style="text-align:justify;margin-bottom:5px"),
                                               p("Para abordar las brechas de información, REACH en colaboración con el Grupo de Trabajo de Transferencias Monetarias (GTM)
                              lanzó la Iniciativa Conjunta de Monitoreo del Mercado de Colombia (JMMI-COL) desde marzo del 2020, entrevistando a comerciantes 
                              para entender la situación del mercado, su capacidad de satisfacer las necesidades mínimas
                              y el acceso o barreras que enfrentaban los consumidores al mismo.", style="text-align:justify;margin-bottom:5px"),
                                               
                                               h4('Metodología'),
                                               p("En colaboración con las organizaciones socias del GTM, bajo el componente de productos básicos, se entrevistaron a varios
                              comerciantes en sus comercios o telefónicamente en diferentes municipios del país a través de un cuestionario con enfoque cuantitativo.
                              De forma general, en cada ronda se intentó dentro de cada municipio recolectar por lo menos tres precios por cada artículo evaluado,
                              registrando el precio de la marca comercial más vendida en el negocio.", style="text-align:justify;margin-bottom:5px"),
                                               
                                              
                                               
                                        ),
                                        column(4,
                                               h4('Limitaciones'),
                                               p("Las conclusiones para el componente de mercados de productos básicos de esta evaluación, en todas sus rondas, son indicativas,
                              ya que la cantidad de datos reunidos no es una muestra representativa, por lo que los resultados no pueden extrapolarse y no son
                              generalizables a las poblaciones de interés. Además, para cada una de las rondas monitoreadas, no se incluyeron aquellos artículos
                              para los cuales no fue posible recolectar al menos cuatro precios.", style="text-align:justify;margin-bottom:5px"),
                                               
                                               h4('Socios participantes'),
                                               tags$a(href="https://www.accioncontraelhambre.org/es", "Acción contra el Hambre", target = "_blank"),br(),
                                               tags$a(href="https://www.acnur.org", "Alto Comisionado de las Naciones Unidas para Refugiados (ACNUR)", target = "_blank"),br(),
                                               tags$a(href="https://apoyar.org", "APOYAR", target = "_blank"),br(),
                                               tags$a(href="https://www.caritas.ch/en/", "Cáritas Suiza", target = "_blank"),br(),
                                               tags$a(href="https://pro.drc.ngo/where-we-work/americas/colombia/", "Consejo Danés para Refugiados (DRC)", target = "_blank"),br(),
                                               tags$a(href="https://nrc.org.co", "Consejo Noruego para Refugiados (NRC)", target = "_blank"), br(),
                                               tags$a(href="https://www.rescue.org/country/colombia", "Comité Internacional de Rescate (IRC)", target = "_blank"), br(),
                                               tags$a(href="https://www.cruzrojacolombiana.org", "Cruz Roja Colombiana", target = "_blank"), br(),
                                               tags$a(href="https://www.goalglobal.org/countries/colombia/", "GOAL", target = "_blank"),br(),
                                               tags$a(href="https://mercycorps.org.co", "Mercy Corps", target = "_blank"), br(),
                                               tags$a(href="https://www.iom.int/es", "Organización Internacional para las Migraciones (OIM)", target = "_blank"), br(),
                                               tags$a(href="https://savethechildren.org.co", "Save the Children", target = "_blank"), br(),
                                               tags$a(href="https://es.wfp.org", "World Food Program (WFP)", target = "_blank"), br(),
                                               tags$a(href="https://www.worldvision.co", "World Vision", target = "_blank"), br(),
                                               tags$a(href="https://www.zoa-international.com/colombia", "ZOA", target = "_blank"), br()
                                                                                             )
                                        
                          ))),
                          
                          
             
             
             #### 2da página  DASHBOARD ########################################
             
             tabPanel("Dashboard",
                      icon = icon("tachometer-alt"),
                      div(class="dashboard",                                                            # set dashboard class from CSS file
                          
                          tags$head(includeCSS("styles.css")),                                          # load CSS stylesheet
                          
                          leafletOutput("map", width = "100%", height = "100%"),                        # Cargar el mapa
                          
                          absolutePanel(id = "dropdown", bottom = 20, left = 20, width = 200,                            # define blue info button
                                        fixed=TRUE, draggable = FALSE, height = "auto",
                                        dropdown(
                                          
                                          h6(strong('Consulta')),
                                          
                                          p("La temporalidad usada en el dashboard hace referencia a cada una de las rondas de recolección, por lo que, el número de la 
                                            muestra (cantidad de departamentos consultados) puede variar entre cada periodo analizado. Para consultar con
                                            mayor detalle la información de cada ronda, visite nuestro", 
                                            tags$a(href="https://www.reachresourcecentre.info/country/colombia/theme/cash/cycle/27074/?toip-group=publications&toip=factsheet#cycle-27074",
                                                   "centro de recursos"), "."),
                                          width = "650px",
                                          tooltip = tooltipOptions(title = "Botón informativo"),
                                          size = "xs",
                                          up = TRUE,
                                          style = "jelly", icon = icon("info"),
                                          animate = animateOptions(
                                            enter = "fadeInLeftBig",
                                            exit  = "fadeOutLeft",
                                            duration = 0.5)
                                        )
                          ),
                          
                          ##########################
                          absolutePanel(
                            id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE, top = "130", left = "12", right = "auto", bottom = "auto",
                            width = 330, height = "auto",
                            
                            pickerInput("select_item",                                                         # pickerInput lista y radioGroupButtons opciones a lo largo
                                        label = "Grupo de productos:",
                                        choices = lapply(split(item_list_1$Item, item_list_1$Group), as.list),
                                        options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                        selected = full_list$Item[1],
                                        multiple = FALSE
                            ),
                            
                            hr(),
                            
                            sliderTextInput("map_date_select",
                                            "Mes de recolección:",
                                            force_edges = TRUE,
                                            choices = dates,
                                            selected = dates_max,
                                            animate = TRUE
                            )                                                                              # close sliderTextInput
                            
                          ),                                                                                 # close absolutePanel
                          
                          absolutePanel(id = "no_data", fixed = TRUE, draggable = FALSE, top = 50, left = 0, right = 00, bottom = 0,
                                        width = "550", height = "20",
                                        tags$i(h4(textOutput("map_text"), style = "color: red; background-color: white;"))
                          )
                      )),
             
             
             #### 3ra página  Grafica de Precios################################
             
             tabPanel("Grafica de Precios",
                      icon = icon("chart-line"),                                                                  # Nombre del primer panel
                      chooseSliderSkin(skin = "Flat", color = NULL),
                      sidebarLayout(
                        
                        sidebarPanel(
                          tags$i(h6("Nota: Los precios reportados solamente son indicativos.", style="color:#045a8d")),        # Texto y color del texto
                          
                          
                          
                          pickerInput("plot_aggregation",                                                                         # pickerInput lista y radioGroupButtons opciones a lo largo
                                      label = "Nivel de agregación: ",
                                      choices = c("Departamento", "Municipio"),
                                      selected = "Departamento",
                                      multiple = FALSE
                          ),
                          
                          hr(),                                                                                                # Linea separadora
                          # Cuando se selecciona Departamento
                          conditionalPanel(condition = "input.plot_aggregation == 'Departamento'",
                                           radioGroupButtons("plot_by_departamento_item",
                                                             label = "Agrupar por:",
                                                             choices = c("Item", "Departamento"),
                                                             selected = "Item",
                                                             justified = TRUE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'Departamento' & input.plot_by_departamento_item == 'Departamento'",
                                           pickerInput("select_bydepartamento_departamento2",
                                                       label = "Departamento(s):",
                                                       choices = unique(plot_location_list$Departamento),
                                                       options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                       selected = c("Bogota, D.C."),
                                                       multiple = TRUE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'Municipio'",
                                           pickerInput("select_bymunicipio_municipio",
                                                       label = "Municipio(s):",
                                                       choices = lapply(split(plot_location_list$Municipio, plot_location_list$Departamento), as.list),
                                                       options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                       selected = c("Bogota, D.C."),
                                                       multiple = TRUE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'Departamento' | input.plot_aggregation == 'Municipio'",
                                           pickerInput("select_item2",                                                         # pickerInput lista y radioGroupButtons opciones a lo largo
                                                       label = "Grupo de productos: ",
                                                       choices = lapply(split(full_list$Item, full_list$Group), as.list),
                                                       options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                       selected = full_list$Item[1],
                                                       multiple = TRUE
                                           )
                          ),
                          hr(),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'Departamento' | input.plot_aggregation == 'Municipio'",
                                           sliderTextInput("select_date",                                                # set date slider
                                                           "Meses:",
                                                           force_edges = TRUE,
                                                           choices = dates,
                                                           selected = c(dates_min, dates_max)
                                           )
                          ),
                          
                          hr(),
                          
                          absolutePanel(id = "dropdown", bottom = 20, left = 20, width = 200,                            # define blue info button
                                        fixed=TRUE, draggable = FALSE, height = "auto",
                                        dropdown(
                                          h4("Nota"),
                                          column(p(h6("Por el tipo de muestreo, hay zonas geográficas que no tendrán cobertura de precios en algunas rondas")),
                                                 width = 5),
                                          width = "650px",
                                          tooltip = tooltipOptions(title = "Botón informativo"),
                                          size = "xs",
                                          up = TRUE,
                                          style = "jelly", icon = icon("info"),
                                          animate = animateOptions(
                                            enter = "fadeInLeftBig",
                                            exit  = "fadeOutLeft",
                                            duration = 0.5)
                                        )
                          ),
                          
                          width = 3                                                                    # Cambiar el ancho del recuadro
                          
                        ),
                        
                        mainPanel(                                                                      # Cuando va sidebarLayout debe ir mainPanel
                          br(),
                          tags$i(textOutput("plot_text"), style = "color: red"),                        # display error message displayed if there is no data available
                          highchartOutput("graph", width = "100%", height = "600px"),                   # display large chart
                          width = 8                                                                     # set width of main panel (out of 12, as per bootstrap logic)
                        ))),
             
             ## 4ta VENTANA MAPA ###############################################
             
             tabPanel("Mapa",
                      icon = icon("map"),
                      div(class="outer",
                          
                          tags$head(includeCSS("styles.css")),
                          
                          leafletOutput("mapa", width = "100%", height = "100%"),
                          
                          absolutePanel(
                            id = "control_mapa", class = "panel panel-default", fixed = TRUE, draggable = FALSE, top = "130", left = "12", right = "auto", bottom = "auto",
                            width = 330, height = "auto",
                            
                            pickerInput(inputId =  "mapa_indicadores",
                                        label = "Indicadores:",
                                        choices = c("Existencias", "Desafíos de reabastecimiento"),
                                        selected = "Existencias",
                                        multiple = FALSE
                            ),
                            conditionalPanel(condition = "input.mapa_indicadores == 'Existencias'",
                                             pickerInput(inputId = "map_item_select",
                                                         label = HTML("Existencias:<br><i>(días de existencia de productos alimentarios)</i>"),
                                                         choices = sort(list_existencia$Group),
                                                         selected = "Dias de existencia: Aceite",
                                                         options = list(`actions-box` = TRUE),
                                                         multiple = FALSE
                                             )
                                             
                            ),
                            conditionalPanel(condition = "input.mapa_indicadores == 'Desafíos de reabastecimiento'",
                                             pickerInput(inputId =  "map_addindicator_select",
                                                         label = HTML("Desafíos de reabastecimiento:<br><i>(% de comerciantes que reportaron)</i>"),
                                                         choices = sort(indicator_list),
                                                         selected = "% de comerciantes que reportaron enfrentar desafíos en los últimos 30 días",
                                                         options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                         multiple = FALSE
                                             )
                            ),
                            hr(),
                            
                            sliderTextInput(inputId = "mapa_fecha_seleccionada",
                                            label = "Meses:",
                                            force_edges = TRUE,
                                            choices = dates_stock,
                                            selected = dates_max_s,
                                            animate = TRUE)                                                                              # close sliderTextInput
                            
                          ),   
                          absolutePanel(id = "no_data", fixed = TRUE, draggable = FALSE, top = 50, left = 0, right = 0, bottom = 0,
                                        width = "550", height = "20",
                                        tags$i(h4(textOutput("mapa_texto"), style = "color: red; background-color: white;"))
                                        
                          )
                          
                          
                      )),
             
             # 5ta Panel del Explorador de Datos #########################
             tabPanel("Data Price Explorer", 
                      icon = icon("table"),
                      tags$head(includeCSS("styles.css")), 
                      
                      absolutePanel(                                                                # define introduction box
                        id = "control_table", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                        top = "50", left = "10", right = "auto", bottom = "auto", width = "370", height = "300",
                        
                        radioGroupButtons("table_aggregation",                                                                   # pickerInput lista y radioGroupButtons opciones a lo largo
                                          label = "Nivel de agregación: ",
                                          choices = c("Departamento", "Municipio"),
                                          selected = "Departamento",
                                          justified = TRUE
                        ),
                        
                        #Cuando se selecciona Departamento
                        conditionalPanel(condition = "input.table_aggregation == 'Departamento'",
                                         pickerInput("table_show_vars",
                                                     label = "Productos:",
                                                     choices = lapply(split(full_list$Item, full_list$Group), as.list),
                                                     options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                     selected = c("Alimentos", "No alimentos"),
                                                     multiple = TRUE
                                         )
                        ),
                        
                        #Cuando se selecciona municipio
                        conditionalPanel(condition = "input.table_aggregation == 'Municipio'",
                                         pickerInput("table_show_vars_municipio",
                                                     label = "Productos:",
                                                     choices = lapply(split(full_list$Item, full_list$Group), as.list),
                                                     options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                     selected = c("Alimentos", "No alimentos"),
                                                     multiple = TRUE
                                         )
                        ),
                        
                        #Meses/Rondas
                        sliderTextInput("table_data",
                                        "Rondas:",
                                        force_edges = TRUE,
                                        choices = dates,
                                        selected = c(dates_min, dates_max))
                        
                      ),
                      
                      
                      absolutePanel(id = "tabla", fixed = TRUE, draggable = FALSE, top = 50, left = 390, right = 0, bottom = 0,
                                    style = "overflow-y: scroll;",
                                    tags$i(textOutput("tabla_text"), style = "color: red"),                        # display error message displayed if there is no data available
                                    DT::dataTableOutput("table", width = "100%", height = "100%")
                                    
                      )
             )
             
             
  ))

#### 7 SERVER ##################################################################

server <- function(input, output, session) {
  
  ####### 1ra ventana general  #################################################
  
  output$map_home <- renderLeaflet({
    map_home <- leaflet(options = leafletOptions(attributionControl=FALSE, zoomControl = FALSE, dragging = FALSE, minZoom = 5, maxZoom = 5)) |> 
      setView(lng = -74.102360, lat = 4.629492, zoom = 12) |> 
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB",
                       options = providerTileOptions(opacity = 0.8))
  })
  
  
  
  #########   2da ventana DASHBOARD ############################################
  
  output$map_text <- renderText({""})
  
  
  
  output$map <- renderLeaflet({
    
    prices_map <- prices_long %>% select(-Municipio) %>% group_by(Fecha, Departamento, Item) %>%
      summarise_all(median, na.rm = TRUE) %>% filter(Fecha == input$map_date_select, Item == "Arroz (1 kg)")
    
    departamento <- left_join(departamento, prices_map, by = c("admin1Name" = "Departamento"))
    
    labels <- sprintf("<strong>%s</strong><br/>%s COP (%s)", departamento$admin1Name, format(departamento$'Price', big.mark=","), format(departamento$Fecha, "%b %Y")) %>% lapply(htmltools::HTML)
    pal <- colorNumeric(palette = c("#FFF98C", "#E0C45C", "#CB3B3B", "#85203B"),
                        domain = departamento$'Price', na.color = "transparent")
    
    # Coordenadas
    bounds <- departamento %>% 
      st_bbox() %>% 
      as.character()
    
    map <- leaflet(options = leafletOptions(attributionControl=FALSE, )) |> 
      fitBounds((as.numeric(bounds[1])-15), bounds[2], bounds[3], bounds[4]) |> 
      addMapPane(name = "base", zIndex = 410) |>  
      addMapPane(name = "polygons", zIndex = 420) |>  
      addMapPane(name = "label", zIndex = 430) |>  
      addPolygons(data = country, group = "País", fill = FALSE, stroke = TRUE, color = "#58585A", weight = 1.2, opacity = 1) |> 
      setMapWidgetStyle(style = list(background = "transparent")) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "Base map",
                       options = c(providerTileOptions(opacity = 0.6),
                                   leafletOptions(pane = "base"))) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "Labels",
                       options = c(providerTileOptions(opacity = 1),
                                   leafletOptions(pane = "label"))) %>%
      addLayersControl(overlayGroups = c("Labels", "País", "Departamento", "Base map"))|> 
      addPolygons(data = departamento, group = "Departamento", fill = TRUE, fillOpacity = 0.7, fillColor = ~pal(departamento$'Price'),
                  stroke = TRUE, color = "#58585A", weight = 0.3, opacity = 1,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666666",
                    fillOpacity = 0.75,
                    bringToFront = TRUE
                  ),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  options = leafletOptions(pane = "polygons")
      ) |> 
      addLegend("bottomright", pal = pal, values = departamento$'Price',
                title = "Precio:",
                labFormat = labelFormat(prefix = "COP "),
                opacity = 1)
    
    
  })
  
  observeEvent(input$select_item,{
    
    prices_map <- prices_long %>% select(-Municipio) %>% group_by(Fecha, Departamento, Item) %>%
      summarise_all(median, na.rm = TRUE) %>% filter(Fecha == input$map_date_select, Item == input$select_item)
    
    departamento <- left_join(departamento, prices_map, by = c("admin1Name" = "Departamento"))
    
    
    output$map_text <- renderText({
      if (all(is.na(departamento[["Price"]])) == TRUE) {
        "There is no data for this selection. Select another month or indicator."} else {NULL}
    })
    
    if (all(is.na(departamento[["Price"]])) == TRUE) {
      return(NULL)
    } 
    
    labels <- sprintf("<strong>%s</strong><br/>%s COP (%s)", departamento$admin1Name, format(departamento$'Price', big.mark=","), format(departamento$Fecha, "%b %Y")) %>% lapply(htmltools::HTML)
    pal <- colorNumeric(palette = c("#FFF98C", "#E0C45C", "#CB3B3B", "#85203B"),
                        domain = departamento$'Price', na.color = "transparent")
    
    leafletProxy("map") |> 
      clearControls() |> 
      #clearGroup("polygons") |> 
      clearGroup("Departamento") |> 
      addPolygons(data = departamento, group = "Departamento", fill = TRUE, fillOpacity = 0.7, fillColor = ~pal(departamento$'Price'),
                  stroke = TRUE, color = "#58585A", weight = 0.3, opacity = 1,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666666",
                    fillOpacity = 0.75,
                    bringToFront = TRUE
                  ),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
                  options = leafletOptions(pane = "polygons")
      ) |> 
      addLegend("bottomright", pal = pal, values = departamento$'Price',
                title = "Precio:",
                labFormat = labelFormat(prefix = "COP "),
                opacity = 1)  
  })
  
  
  #########   3ra ventana ######################################################
  
  # La informacion que se va a mostrar
  plot_datasetInput <- reactive({
    
    if (input$plot_aggregation == "Departamento" & input$plot_by_departamento_item == "Item") {
      prices_long %>% filter(Fecha >= input$select_date[1] & Fecha <= input$select_date[2]) %>% 
        select(-Departamento, -Municipio) %>% group_by(Fecha, Item) %>% 
        summarise_all(median, na.rm = TRUE) %>% 
        filter(Item %in% input$select_item2)
    } else if (input$plot_aggregation == "Departamento" & input$plot_by_departamento_item == "Departamento") {
      prices_long %>% filter(Fecha >= input$select_date[1] & Fecha <= input$select_date[2], Item %in% input$select_item2) %>% 
        select(-Item, -Municipio) %>% group_by(Fecha, Departamento) %>%
        summarise_all(median, na.rm = TRUE) %>% 
        filter(Departamento %in% input$select_bydepartamento_departamento2)
    } else if (input$plot_aggregation == "Municipio") {
      prices_long %>% filter(Fecha >= input$select_date[1] & Fecha <= input$select_date[2], Item %in% input$select_item2) %>% 
        select(-Item) %>% group_by(Fecha, Departamento, Municipio) %>%
        summarise_all(median, na.rm = TRUE) %>% 
        filter(Municipio %in% input$select_bymunicipio_municipio)
    }
  })
  
  output$plot_text <- renderText({
    if (nrow(plot_datasetInput()) == 0) {
      "There is no data for this selection. Change the time frame or select another indicator/location."}
  })
  
  output$graph <- renderHighchart({
    
    if (input$plot_aggregation == "Departamento" & input$plot_by_departamento_item == "Item") {
      graph <- hchart(plot_datasetInput(), "line", hcaes(x = sort(Fecha), y = Price, group = Item))
      
    } else if (input$plot_aggregation == "Departamento" & input$plot_by_departamento_item == "Departamento"){
      graph <- hchart(plot_datasetInput(), "line", hcaes(x = sort(Fecha), y = Price, group = Departamento))
    } else if (input$plot_aggregation == "Municipio"){
      graph <- hchart(plot_datasetInput(), "line", hcaes(x = sort(Fecha), y = Price, group = Municipio))
    } else{
      "Sigue intentando"
    }
    graph <- graph %>%
      hc_xAxis(title = "") %>%
      hc_colors(cols) %>%
      hc_exporting(
        enabled = TRUE,
        filename = paste0("IRQ-JPMI-linegraph_export-", Sys.Date()),
        buttons = list(contextButton = list(menuItems = list("downloadPNG", "downloadPDF", "downloadCSV"))),
        sourceWidth = 1000,
        sourceHeight = 600
      )
  })
  
  ## 4ta VENTANA MAPA ##########################################################
  output$map_text <- renderText({""})
  
  # Se hace para que se almacene la variable de manera más facil y tomarla mas adelante
  mapa_indicadores <- reactive({
    if (input$mapa_indicadores == "Existencias") {input$map_item_select} else {input$map_addindicator_select}
  })
  
  output$mapa <- renderLeaflet({
    
    
    # Coordenadas
    bounds <- departamento %>% 
      st_bbox() %>% 
      as.character()
    
    if (input$mapa_indicadores == "Existencias"){  
      stock_mapa <- stock %>% filter(Fecha == input$mapa_fecha_seleccionada)
      dias_stock_mapa <- dias_stock %>% filter(mes == input$mapa_fecha_seleccionada)
      
      departamento <- left_join(departamento, stock_mapa, by =c("admin1Name" = "Departamento"))
      departamento <- left_join(departamento, dias_stock_mapa, by =c("admin1Name" = "departamento"))
      
      labels <- sprintf("<strong>%s</strong><br/>%s Stock (%s)<br/> Datos %s", departamento$admin1Name, format(departamento[[mapa_indicadores()]], big.mark=","), format(departamento$Fecha, "%b %Y"), departamento$n_datos) %>% lapply(htmltools::HTML)
      pal <- colorNumeric(palette = c("#FFF98C", "#E0C45C", "#CB3B3B", "#85203B"),
                          domain = departamento[[mapa_indicadores()]], na.color = "transparent"
      )
      
      mapa <- leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
        fitBounds((as.numeric(bounds[1])-15), bounds[2], bounds[3], bounds[4]) %>%
        addMapPane(name = "base", zIndex = 410) %>%
        addMapPane(name = "polygons", zIndex = 420) %>%
        addMapPane(name = "label", zIndex = 430) %>%
        addPolygons(data = country, group = "Country", fill = FALSE, stroke = TRUE, color = "#58585A", weight = 1.2, opacity = 1) %>%
        setMapWidgetStyle(style = list(background = "transparent")) %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels, group = "Base map",
                         options = c(providerTileOptions(opacity = 0.6),
                                     leafletOptions(pane = "base"))) %>%
        addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "Labels",
                         options = c(providerTileOptions(opacity = 1),
                                     leafletOptions(pane = "label"))) %>%
        addLayersControl(overlayGroups = c("Labels", "Country", "Departamento", "Base map")) |> 
        addPolygons(data = departamento, group = "Departamento", fill = TRUE, fillOpacity = 0.7, fillColor = ~pal(departamento[[mapa_indicadores()]]),
                    stroke = TRUE, color = "#58585A", weight = 0.3, opacity = 1,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666666",
                      fillOpacity = 0.75,
                      bringToFront = TRUE
                    ),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    options = leafletOptions(pane = "polygons")
        ) |> 
        addLegend("bottomright", pal = pal, values = departamento[[mapa_indicadores()]],
                  title = "Stock:",
                  labFormat = labelFormat(prefix = "Días:"),
                  opacity = 1
                  
        )
      
      
    } else {
      stock_mapa <- indicators2 %>% filter(Fecha == input$mapa_fecha_seleccionada)
      departamento <- left_join(departamento, stock_mapa, by = c("admin1Name" = "Departamento"))
      
      labels <- sprintf("<strong>%s</strong><br/>%s %s (%s)", departamento$admin1Name ,format(departamento[[mapa_indicadores()]], big.mark=","), '%', format(departamento$Fecha, "%b %Y")) %>% lapply(htmltools::HTML)
      pal <- colorNumeric(palette = c("#FFF98C", "#E0C45C", "#CB3B3B", "#85203B"),
                          domain = departamento[[mapa_indicadores()]], na.color = "transparent")
      
      mapa <- leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
        fitBounds((as.numeric(bounds[1])-15), bounds[2], bounds[3], bounds[4]) %>%
        addMapPane(name = "base", zIndex = 410) %>%
        addMapPane(name = "polygons", zIndex = 420) %>%
        addMapPane(name = "label", zIndex = 430) %>%
        addPolygons(data = country, group = "Country", fill = FALSE, stroke = TRUE, color = "#58585A", weight = 1.2, opacity = 1) %>%
        setMapWidgetStyle(style = list(background = "transparent")) %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels, group = "Base map",
                         options = c(providerTileOptions(opacity = 0.6),
                                     leafletOptions(pane = "base"))) %>%
        addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "Labels",
                         options = c(providerTileOptions(opacity = 1),
                                     leafletOptions(pane = "label"))) %>%
        addLayersControl(overlayGroups = c("Labels", "Country", "Departamento", "Base map")) |> 
        addPolygons(data = departamento, group = "Departamento", fill = TRUE, fillOpacity = 0.7, fillColor = ~pal(departamento[[mapa_indicadores()]]),
                    stroke = TRUE, color = "#58585A", weight = 0.3, opacity = 1,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666666",
                      fillOpacity = 0.75,
                      bringToFront = TRUE
                    ),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    options = leafletOptions(pane = "polygons")
        )|> 
        addLegend("bottomright", pal = pal, values = departamento[[mapa_indicadores()]],
                  title = "Reportado:",
                  labFormat = labelFormat(prefix = "%:"),
                  opacity = 1
        )
    }
  })
  
  
  observeEvent(input$mapa_indicadores,{
    
    if (input$mapa_indicadores == "Existencias"){  
      stock_mapa <- stock %>% filter(Fecha == input$mapa_fecha_seleccionada)
      dias_stock_mapa <- dias_stock %>% filter(mes == input$mapa_fecha_seleccionada)
      
      departamento <- left_join(departamento, stock_mapa, by =c("admin1Name" = "Departamento"))
      departamento <- left_join(departamento, dias_stock_mapa, by =c("admin1Name" = "departamento"))
      
      labels <- sprintf("<strong>%s</strong><br/>%s Stock (%s)<br/> Datos %s", departamento$admin1Name, format(departamento[[mapa_indicadores()]], big.mark=","), format(departamento$Fecha, "%b %Y"), departamento$n_datos) %>% lapply(htmltools::HTML)
      pal <- colorNumeric(palette = c("#FFF98C", "#E0C45C", "#CB3B3B", "#85203B"),
                          domain = departamento[[mapa_indicadores()]], na.color = "transparent"
      )
      leafletProxy("mapa") |> 
        clearControls() |> 
        #clearGroup("polygons")|> 
        clearGroup("Departamento") |> 
        addPolygons(data = departamento, group = "Departamento", fill = TRUE, fillOpacity = 0.7, fillColor = ~pal(departamento[[mapa_indicadores()]]),
                    stroke = TRUE, color = "#58585A", weight = 0.3, opacity = 1,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666666",
                      fillOpacity = 0.75,
                      bringToFront = TRUE
                    ),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    options = leafletOptions(pane = "polygons")
        ) |> 
        addLegend("bottomright", pal = pal, values = departamento[[mapa_indicadores()]],
                  title = "Stock:",
                  labFormat = labelFormat(prefix = "Días:"),
                  opacity = 1
                  
        )
      
      
    } else {
      stock_mapa <- indicators2 %>% filter(Fecha == input$mapa_fecha_seleccionada)
      departamento <- left_join(departamento, stock_mapa, by = c("admin1Name" = "Departamento"))
      
      labels <- sprintf("<strong>%s</strong><br/>%s %s (%s)", departamento$admin1Name ,format(departamento[[mapa_indicadores()]], big.mark=","), '%', format(departamento$Fecha, "%b %Y")) %>% lapply(htmltools::HTML)
      pal <- colorNumeric(palette = c("#FFF98C", "#E0C45C", "#CB3B3B", "#85203B"),
                          domain = departamento[[mapa_indicadores()]], na.color = "transparent"
      )
      
      leafletProxy("mapa") |> 
        clearControls() |> 
        #clearGroup("polygons") |> 
        clearGroup("Departamento") |> 
        addPolygons(data = departamento, group = "Departamento", fill = TRUE, fillOpacity = 0.7, fillColor = ~pal(departamento[[mapa_indicadores()]]),
                    stroke = TRUE, color = "#58585A", weight = 0.3, opacity = 1,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666666",
                      fillOpacity = 0.75,
                      bringToFront = TRUE
                    ),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"),
                    options = leafletOptions(pane = "polygons")
        )|> 
        addLegend("bottomright", pal = pal, values = departamento[[mapa_indicadores()]],
                  title = "Reportado:",
                  labFormat = labelFormat(prefix = "%:"),
                  opacity = 1
        )
      
      
    }
    
    
  })
  
  
  
  
  ## 5ta ventana Dataexplorer ################################################## 
  
  ## Dataexplorer 
  
  table_datasetInput <- reactive({
    if (input$table_aggregation == "Departamento") {
      full %>% filter(
        Fecha >= input$table_data[1] & Fecha <= input$table_data[2]
      ) %>%
        select(Fecha, Departamento, input$table_show_vars) %>% 
        group_by(Fecha, Departamento) %>% 
        summarise_all(median, na.rm = TRUE) # Si se quiere ver la mediana o promedio realizar el cambio de la funcion
    } else {
      full %>% filter(
        #is.null(input$table_municipio) | Municipio %in% input$table_municipio,  # Tener cuidado, que es lo que se quiere filtrar
        Fecha >= input$table_data[1] & Fecha <= input$table_data[2]
      ) %>%
        select("Fecha", "Departamento", "Municipio", input$table_show_vars_municipio)
    }
  })
  
  # Por si se quiere agragar algun error
  output$tabla_text <- renderText({""})
  
  output$table <- renderDT({
    DT::datatable(
      table_datasetInput(),
      extensions = c('ColReorder'),
      options = list(autoWidth = TRUE, dom = 't', paging = FALSE, colReorder= TRUE, fixedHeader = TRUE, scrollX = TRUE, fixedColumns = list(leftColumns = 3)),
      rownames = FALSE,
      style = 'bootstrap', class = 'table-condensed table-hover table-striped'
    ) %>%
      formatRound(
        # Empezar a hacer el formatRound a partir de la 3ra o 4ta columna
        if (input$table_aggregation == "Departamento") {3:ncol(table_datasetInput())} else {4:ncol(table_datasetInput())},
        digits = 0,
        interval = 3,
        mark = ",",
        dec.mark = getOption("OutDec")
      ) %>%
      formatStyle(names(table_datasetInput()), "white-space"="nowrap")
  })
  
}




# Run the application 
shinyApp(ui = ui, server = server)
