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

data <- read.csv("data/longterm_data.csv", na.strings = c("NA", ""), encoding="latin1", sep = ";")   # load JPMI dataset

item_list   <- read.csv("data/item_list.csv", encoding="latin1")                                     # load item list

# Cargar shps
departamento <- st_read("gis/Admin1_UnodcOcha_01012009.shp")
country     <- st_read("gis/World_admin0_countries_py_WFP_nd.shp")              # load shapefile with country border

# Ajustar a tipo Date
data$mes <- lubridate::dmy(data$mes)


# data to numeric ---------------------------------------------------------

numerico <- c("precio_aceite", "precio_arroz", "precio_atun", "precio_cloro", 
              "precio_crema_dental", "precio_frijoles", "precio_harina",        
              "precio_huevos", "precio_jabon_personal", "precio_jabon_ropa", 
              "precio_leche", "precio_lentejas", "precio_panales", "precio_papa", 
              "precio_papel_higienico", "precio_pasta", "precio_platano",        
              "precio_pollo", "precio_tabapocas", "precio_toallas_higienicas",
              "precio_tomate", "precio_yuca", "dias_exisnc_aceite", "dias_exisnc_arroz",
              "dias_exisnc_atun", "dias_exisnc_cloro", "dias_exisnc_crema_dental",
              "dias_exisnc_frijoles", "dias_exisnc_harina", "dias_exisnc_huevos",
              "dias_exisnc_jabon_personal", "dias_exisnc_jabon_ropa", "dias_exisnc_leche",             
              "dias_exisnc_lentejas", "dias_exisnc_panales", "dias_exisnc_papa",
              "dias_exisnc_pasta", "dias_exisnc_platano", "dias_exisnc_pollo",
              "dias_exisnc_tabapocas", "dias_exisnc_toallas_higienicas",
              "dias_exisnc_tomate", "dias_exisnc_yuca")   

for (i in numerico) {
  data[i] <- as.numeric(data[[i]])
}




#### 3 AGGREGATION #############################################################

#### * 3.1 Prices ##############################################################

prices <- data %>%
  select(mes, departamento, municipio, starts_with("precio_")) %>%   # calculate site medians
  group_by(mes, departamento, municipio) %>% 
  summarise_all(median, na.rm = TRUE) %>%
  select(mes, departamento, municipio, starts_with("precio_")) %>%         # calculate departamento medians
  group_by(mes, departamento, municipio) %>% 
  summarise_all(median, na.rm = TRUE) %>%
  setNames(gsub("precio_","",names(.))) %>%                                            # rename columns names
  ungroup() %>%
  group_by(mes) %>%            
  mutate(Lista_alimentos      = arroz + lentejas + huevos + pollo + frijoles +         # calculate  values
           pasta + harina + platano + papa + yuca + aceite + tomate + leche + atun ,
         Lista_noalimentarios = papel_higienico + toallas_higienicas + jabon_ropa + crema_dental +
           jabon_personal + panales + cloro + tabapocas,
         Lista_total          = Lista_alimentos + Lista_noalimentarios
  ) %>%
  mutate_if(is.numeric, round, 0) %>%                                                  # round values
  rename(Fecha = mes, Departamento = departamento, Municipio = municipio,              # rename column names 
         "Lista de productos alimentarios"    = Lista_alimentos,
         "Lista de productos no alimentarios" = Lista_noalimentarios,
         "Lista de productos compilados"      = Lista_total,
         "Arroz (1 kg)"       = arroz,
         "Lentejas (1 libra)" = lentejas,
         "Huevos rojos tipo AA (1 unidad)"         = huevos,
         "Pechuga de pollo (1 libra)"              = pollo,
         "Fríjoles bola roja (1 libra)"            = frijoles,
         "Pastas tipo espagueti (1 libra)"         = pasta,
         "Harina de maíz (1 kg)"                   = harina,
         "Plátano (1 kg)"     = platano,
         "Papa pastusa (1 libra)"  = papa,
         "Yuca (1 libra)"     = yuca,
         "Aceite (1 Lt)"      = aceite,
         "Tomate chonto (1 libra)"     = tomate,
         "Leche (1 Lt)"       = leche,
         "Lomitos de atún en lata en aceite (170 gr)"   = atun,
         "Papel higiénico doble hoja (1 unidad )"       = papel_higienico,
         "Toallas higiénicas (1 paquete)"               = toallas_higienicas,
         "Jabón de ropa en barra (1 barra de 300gr)"    = jabon_ropa,
         "Crema de dientes (1 tubo de 100gr)"      = crema_dental,
         "Jabón personal en barra (1 barra de 75gr)"    = jabon_personal,
         "Pañales (1 paquete de 30 unidades)"      = panales,
         "Cloro (1 Lt)"          = cloro,
         "Tapabocas quirúrgico (1 unidad )"        = tabapocas
  )


prices_long <- gather(prices, Item, Price, 4:ncol(prices))                        # change dataframe to long format
#DF, columname1, columname2, estas dos columnas se crean a partir de la 4:ncol(prices)

#### * 3.2 Indicators ##########################################################

indicators <- data %>%
  select(mes, departamento, municipio, starts_with("desafios_"),  
         -ends_with("_arroz"), -ends_with("_lentejas"), -ends_with("_huevos"), -ends_with("_pollo"),
         -ends_with("_frijoles"), -ends_with("_pasta"), -ends_with("_harina"), -ends_with("_platano"),
         -ends_with("_papa"), -ends_with("_yuca"), -ends_with("_aceite"), -ends_with("_tomate"),
         -ends_with("_leche"), -ends_with("_atun"))



indicators2 <- indicators %>%
  gather(Indicator, Value, 4:(ncol(indicators))) %>%
  group_by(mes, departamento, municipio, Indicator) %>%
  summarise(freq = sum(Value == 1 | Value == "Si", na.rm = TRUE) / sum(!is.na(Value)) * 100) %>%
  mutate_if(is.numeric, round, 0) %>% 
  spread(Indicator, freq) %>%
  rename(Fecha = mes,
         Departamento = departamento,
         Municipio = municipio,
         '% de comerciantes que reportaron enfrentar desafíos en los últimos 30 días' = desafios_30ds,
         '% de comerciantes que reportaron desafíos por: Ninguna vía de acceso' = desafios_1,
         '% de comerciantes que reportaron desafíos por: Bloqueos en las vías' = desafios_2,
         '% de comerciantes que reportaron desafíos por: Restricciones en las fronteras' = desafios_3,
         '% de comerciantes que reportaron desafíos por: Inseguridad en la ruta de abastecimiento' =  desafios_4,
         '% de comerciantes que reportaron desafíos por: Malas condiciones físicas de las carreteras' = desafios_5,
         '% de comerciantes que reportaron desafíos por: Los proveedores detuvieron sus produccioness' = desafios_6,
         '% de comerciantes que reportaron desafíos por: Los proveedores no tienen esos productos' = desafios_7,
         '% de comerciantes que reportaron desafíos por: El tiempo entre el pedido y la entrega es más largo' = desafios_8,
         '% de comerciantes que reportaron desafíos por: Los proveedores no están produciendo estos productos' = desafios_9,
         '% de comerciantes que reportaron desafíos por: Hay escasez de transportistas' = desafios_10,
         '% de comerciantes que reportaron desafíos por: El precio de la gasolina es muy alto' = desafios_11,
         '% de comerciantes que reportaron desafíos por: El precio del transporte es muy alto' = desafios_12,
         '% de comerciantes que reportaron desafíos por: El principal proveedor ya no da acceso a crédito' = desafios_13,
         '% de comerciantes que reportaron desafíos por: El negocio no tiene suficiente liquidez para reabastecerse' = desafios_14,
         '% de comerciantes que reportaron desafíos por: No hay suficiente capacidad de almacenamiento' = desafios_15,
         '% de comerciantes que reportaron desafíos por: Los agentes comerciales ya no visitan los comercios de la zona' = desafios_16,
         '% de comerciantes que reportaron desafíos por: Mala relación con el proveedor' = desafios_17
  )

full <- left_join(prices, indicators2, by = c("Fecha", "Departamento", "Municipio"))


#### * 3.3 Stock ###############################################################
stock <- data %>%
  select(mes, departamento, municipio, starts_with("dias_exisnc"))

stock <- stock %>% 
  group_by(mes, departamento, municipio) %>% 
  summarise_all(median, na.rm = TRUE) %>%
  select(mes, departamento, municipio, starts_with("dias_exisnc")) %>%          # calculate departamento medians
  group_by(mes, departamento, municipio) %>% 
  summarise_all(median, na.rm = TRUE) %>%
  setNames(gsub("dias_exisnc_","",names(.))) %>%                                 # rename columns names
  ungroup() %>%
  group_by(mes) %>%  
  rename(Fecha = mes, Departamento = departamento, Municipio = municipio,       # rename column names 
         "Días de existencia: Arroz"      = arroz,
         "Días de existencia: Lentejas"   = lentejas,
         "Días de existencia: Huevos rojos tipo AA"         = huevos,
         "Días de existencia: Pechuga de pollo"             = pollo,
         "Días de existencia: Fríjoles bola roja"           = frijoles,
         "Días de existencia: Pastas tipo espagueti"        = pasta,
         "Días de existencia: Harina de maíz"               = harina,
         "Días de existencia: Plátano"       = platano,
         "Días de existencia: Papa pastusa"  = papa,
         "Días de existencia: Yuca"          = yuca,
         "Días de existencia: Aceite"        = aceite,
         "Días de existencia: Tomate chonto"     = tomate,
         "Días de existencia: Leche"             = leche,
         "Días de existencia: Lomitos de atún en lata en aceite"   = atun)


#### 4 REFERENCES ##############################################################

max(unique(prices_long$Fecha))



dates <- sort(unique(prices_long$Fecha))                                          # define list with date range in data
dates_min  <- min(prices_long$Fecha)                                              # set minimum date to be displayed
dates_max  <- max(prices_long$Fecha)                                              # maximum date in data
dates_max2 <- sort(unique(prices_long$Fecha), decreasing=T)[2]                    # second-latest date

dates_max_1y <- as.POSIXlt(dates_max)                                             # most recent month minus 1 year
dates_max_1y$year <- dates_max_1y$year-1
dates_max_1y <- as.Date(format(dates_max_1y, "%Y-%m-01"), format = "%Y-%m-%d")


plot_location_list <- prices_long %>%                                           # define location list (which is later used as choice filter options)
  ungroup() %>%
  select(Departamento, Municipio) %>%                                             # extract governorate and district columns
  arrange(Departamento, Municipio) %>%                                            # list alphabetically
  filter(!duplicated(Departamento))  # remove duplicates

map_location_list <- prices_long %>%                                           # define location list (which is later used as choice filter options)
  ungroup() %>%
  select(Departamento, Municipio, Fecha) %>%                                             # extract governorate and district columns
  arrange(Departamento, Municipio) %>%                                            # list alphabetically
  filter(!duplicated(Departamento))  # remove duplicates

indicator_list <- names(indicators) %>%
  str_subset(c("Fecha", "Departamento", "Municipio"), negate = TRUE)              # extract additional indicator list

cols      <- c("rgb(238,88,89)",   "rgb(88,88,90)",    "rgb(165,201,161)",        # define color palette for plot lines
               "rgb(86,179,205)",  "rgb(246,158,97)",  "rgb(255,246,122)",        # están los colores reach + una paleta de amarillos y naranjas
               "rgb(210,203,184)", "rgb(247,172,172)", "rgb(172,172,173)",        # azul claro y distintos tonos de beige
               "rgb(210,228,208)", "rgb(171,217,230)", "rgb(251,207,176)",
               "rgb(255,251,189)", "rgb(233,229,220)")

full_list <- data.frame(Item = indicator_list,                                    # create full indicator list
                        Group = "Indicadores") %>% arrange(Item)
full_list <- rbind(item_list, full_list)     # combine prices and additional indicators

#### * 5.1 Define dataframe ####################################################

prices_country <- prices %>%                                                      # aggregate price data at country level
  select(-Departamento, -Municipio) %>%
  group_by(Fecha) %>% 
  summarise_all(median, na.rm = TRUE)
prices_country_long <- gather(prices_country, Item, Price, 2:ncol(prices_country))# transform country-level price data to long format

prices_country_home <- prices_country %>%                                         # filter out SMEB data from country level price data
  filter(Fecha >= dates_min) %>%
  select(Fecha, `Lista de productos alimentarios`, `Lista de productos no alimentarios`, 
         `Lista de productos compilados`) %>%
  gather(Item, Price, 'Lista de productos alimentarios':'Lista de productos compilados')                                    # transform SMEB data to long format so highcharter can read dataframe

one_year_from_current_date <- prices_country_home$Fecha %>% max() - 366

prices_country_home <- prices_country_home %>% filter(Fecha > one_year_from_current_date)


#### 6 UI ######################################################################
ui <- bootstrapPage(
  navbarPage("JMMI COLOMBIA",                                                                                     # Nombre del DASHBOARD o del panel navegador
             theme = shinytheme("simplex"),                                                                       # Tema del navbarpage
             
             
             #### 1ra página  DASHBOARD ########################################
             
             tabPanel("Dashboard",
                      icon = icon("tachometer-alt"),
                      div(class="dashboard",                                                            # set dashboard class from CSS file
                          
                          tags$head(includeCSS("styles.css")),                                          # load CSS stylesheet
                          
                          leafletOutput("map", width = "100%", height = "100%"),                        # Cargar el mapa
                          
                          absolutePanel(                                                                # define introduction box
                            id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                            top = "10", left = "10", right = "auto", bottom = "auto", width = "370", height = "600",
                            style = "overflow-y: scroll;",
                            
                            h5("Contexto"),
                            
                            p("Desde el 2015, Venezuela ha sufrido una grave crisis política y económica ocasionando el desplazamiento de millones 
                              de personas en todo el mundo. En la actualidad se estima que más de 2.4 millones de migrantes han llegado a Colombia
                              para satisfacer sus necesidades básicas y requieren asistencia humanitaria.", style="text-align:justify;margin-bottom:20px"),
                            
                            p("Con el fin de abordar las necesidades, grupos humanitarios están implementando intervenciones basadas en efectivo como medio
                              para ayudar a los hogares vulnerables. Sin embargo, las intervenciones basadas en dinero en efectivo requieren información precisa
                              de las cadenas de suministro y mercados que funcionen adecuadamente y que proporciones productos básicos de forma continua.", 
                              style="text-align:justify;margin-bottom:20px"),
                            
                            p("Para abordar las brechas de información, REACH en colaboración con el Grupo de Trabajo de Transferencias Monetarias (GTM)
                              lanzó la Iniciativa Conjunta de Monitoreo del Mercado de Colombia (JMMI- COL) desde marzo del 2020, entrevistando tanto a
                              consumidores como comerciantes para entender la situación actual del mercado, su capacidad de satisfacer las necesidades mínimas
                              y el acceso o barreras que enfrentaban los consumidores al mismo.", style="text-align:justify;margin-bottom:20px"),
                            
                            hr(),
                            
                            h5("Metodología"),
                            
                            p("En colaboración con las organizaciones socias del GTM, bajo el componente de productos básicos, se entrevistaron a varios
                              comerciantes en sus comercios o telefónicamente en diferentes municipios del país a través de un cuestionario con enfoque cuantitativo.
                              De forma general, en cada ronda se intentó dentro de cada municipio recolectar por lo menos tres precios por cada artículo evaluado,
                              registrando el precio de la marca comercial más vendida en el negocio.", style="text-align:justify;margin-bottom:20px"),
                            
                            hr(),
                            
                            h5("Limitaciones"),
                            
                            p("Las conclusiones para el componente de mercados de productos básicos de esta evaluación, en todas sus rondas, son indicativas,
                              ya que la cantidad de datos reunidos no es una muestra representativa, por lo que los resultados no pueden extrapolarse y no son
                              generalizables a las poblaciones de interés.", style="text-align:justify;margin-bottom:20px"),
                          ),
                          
                          ##########################
                          absolutePanel(
                            id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE, top = "50", left = "380", right = "auto", bottom = "auto",
                            width = 330, height = "auto",
                            
                            
                            pickerInput("map_indicator_select",
                                        label = "Agrupar por:",
                                        choices = c("Item", "Departamento"),
                                        selected = "Item",
                                        multiple = FALSE
                            ),
                            hr(),
                            
                            
                            conditionalPanel(condition = "input.map_indicator_select == 'Departamento'",
                                             pickerInput("select_bydepartamento_departamento",
                                                         label = "Departamento(s):",
                                                         choices = unique(map_location_list$Departamento),
                                                         options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                         selected = c("Bogota, D.C."),
                                                         multiple = TRUE
                                             )
                            ),
                            
                            conditionalPanel(condition = "input.map_indicator_select == 'Item'",
                                             pickerInput("select_item",                                                         # pickerInput lista y radioGroupButtons opciones a lo largo
                                                         label = "Grupo de productos:",
                                                         choices = lapply(split(item_list$Item, item_list$Group), as.list),
                                                         options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                         selected = full_list$Item[1],
                                                         multiple = FALSE
                                             )
                            ),
                            
                            hr(),
                            
                            sliderTextInput("map_date_select",
                                            "Month:",
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
                          
             
             #### 2da página  Grafica de Precios################################
             
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
                                           pickerInput("select_bydepartamento_departamento",
                                                       label = "Departamento(s):",
                                                       choices = unique(plot_location_list$Departamento),
                                                       options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                       selected = c("Bogota, D.C."),
                                                       multiple = TRUE
                                           )
                          ),
                          
                          # Cuando se selecciona Municipio
                          conditionalPanel(condition = "input.plot_aggregation == 'Municipio'",
                                           radioGroupButtons("plot_by_municipio_item",
                                                             label = "Agrupar por:",
                                                             choices = c("Item", "Municipio"),
                                                             selected = "Item",
                                                             justified = TRUE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'Municipio' & input.plot_by_municipio_item == 'Municipio'",
                                           pickerInput("select_bymunicipio_municipio",
                                                       label = "Municipio(s):",
                                                       choices = lapply(split(plot_location_list$Municipio, plot_location_list$Departamento), as.list),
                                                       options = list(title = "Select", `actions-box` = TRUE, `live-search` = TRUE),
                                                       selected = c("Bogota, D.C."),
                                                       multiple = TRUE
                                           )
                          ),
                          
                          conditionalPanel(condition = "input.plot_aggregation == 'Departamento' | input.plot_aggregation == 'Municipio'",
                                           pickerInput("select_item",                                                         # pickerInput lista y radioGroupButtons opciones a lo largo
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
                          
                          h6("Algo de carreta si se quiere o se puede eliminar esto"),
                          
                          absolutePanel(id = "dropdown", bottom = 20, left = 20, width = 200,                            # define blue info button
                                        fixed=TRUE, draggable = FALSE, height = "auto",
                                        dropdown(
                                          h4("Se requiere boton informativo"),
                                          column(
                                            HTML("Una tabla puede ir aqui???"),
                                            width = 6),
                                          column(p(h6("textooooooo")),
                                                 p(h6("más texto")),
                                                 p(h6("super textoooooo")),
                                                 p(h6("More details on the SMEB can be found here:",
                                                      tags$a(href="https://www.impact-repository.org/toolkit/file-storage-and-management/",
                                                             "Prueba de link"), ".")),
                                                 width = 5),
                                          width = "650px",
                                          tooltip = tooltipOptions(title = "Queremos utilizar un boton informativo?"),
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
             
             # Nuevo tab Panel del Explorador de Datos##################################################################
             tabPanel("Data Explorer", 
                      icon = icon("table"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          
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
                        
                        
                        
                        
                        mainPanel(
                          
                          DT::dataTableOutput("table", width = "100%", height = "100%"),
                          width = 9
                        )
                      ))
             
             
             )
)

#### 7 SERVER ##################################################################

server <- function(input, output, session) {
  
  #########   1ra ventana DASHBOARD ############################################
  
  output$map_text <- renderText({""})
  
  output$map <- renderLeaflet({
    
    #prices_map <- prices_long %>% filter(Fecha == dates_max)
    
    if (input$map_indicator_select == "Item") {
      prices_map <- prices_long %>% select(-Municipio) %>% group_by(Fecha, Departamento, Item) %>%
        summarise_all(median, na.rm = TRUE) %>% filter(Fecha == input$map_date_select, Item == input$select_item)
    } else {
      prices_map <- prices_long %>% select(-Municipio, -Item) %>% filter(Fecha == input$map_date_select, Departamento %in% input$select_bydepartamento_departamento) %>% 
        group_by(Fecha, Departamento) %>% summarise_all(median, na.rm = TRUE) 
      
    }
    
    #prices_map <- prices_long %>% select(-Municipio, -Item) %>% filter(Fecha == "2020-11-01", Departamento == "Antioquia") %>% 
    #  group_by(Fecha, Departamento) %>% summarise_all(median, na.rm = TRUE) 
    
    
    #prices_map <- prices_long %>% select(-Municipio) %>% group_by(Fecha, Departamento, Item) %>%
    #  summarise_all(median, na.rm = TRUE) %>% filter(Fecha == input$map_date_select, Item == input$select_item)
    
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
                        domain = departamento$'Price', na.color = "transparent"
    )
    
    # Coordenadas
    bounds <- departamento %>% 
      st_bbox() %>% 
      as.character()
    
    map <- leaflet(options = leafletOptions(attributionControl=FALSE, )) %>%
      fitBounds((as.numeric(bounds[1])-15), bounds[2], bounds[3], bounds[4]) %>% 
      addMapPane(name = "base", zIndex = 410) %>% 
      addMapPane(name = "polygons", zIndex = 420) %>% 
      addMapPane(name = "label", zIndex = 430) %>%
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
      ) %>% 
      addPolygons(data = country, group = "País", fill = FALSE, stroke = TRUE, color = "#58585A", weight = 1.2, opacity = 1) %>%
      addLegend("bottomright", pal = pal, values = departamento$'Price',
                title = "Precio:",
                labFormat = labelFormat(prefix = "COP "),
                opacity = 1
      )%>%
      setMapWidgetStyle(style = list(background = "transparent")) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "Base map",
                       options = c(providerTileOptions(opacity = 0.6),
                                   leafletOptions(pane = "base"))) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "Labels",
                       options = c(providerTileOptions(opacity = 1),
                                   leafletOptions(pane = "label"))) %>%
      addLayersControl(overlayGroups = c("Labels", "País", "Departamento", "Base map"))
    
    
  })
  
  #########   2da ventana ######################################################
  # Funciones para imprimir la informacion
  # imprimir departamento si se tiene solo Item
  plot_departamento_select <- reactive({
    if (input$plot_by_departamento_item == "Item") {input$select_byitem_departamento} else {input$select_bydepartamento_departamento}
  })
  # imprimir municipio si se tiene solo Item
  plot_municipio_select <- reactive({
    if (input$plot_by_municipio_item == "Item") {input$select_byitem_municipio} else {input$select_bymunicipio_municipio}
  })
  # imprimir municipio o departamento cuando se escogio algun Item
  plot_item_select <- reactive({
    if ((input$plot_aggregation == 'Municipio' & input$plot_by_municipio_item == 'Item') | (input$plot_aggregation == 'Departamento' & input$plot_by_departamento_item == 'Item')) {input$select_item}
  })
  
  # La informacion que se va a mostrar
  plot_datasetInput <- reactive({prices_long %>%
      filter(
        is.null(plot_item_select()) | Item %in% plot_item_select()
      ) %>%
      execute_if(input$plot_by_municipio_item == 'Item' | input$plot_by_departamento_item == 'Item',
                 filter(
                   lubridate::dmy(Fecha) >= input$select_date[1] & lubridate::dmy(Fecha) <= input$select_date[2]
                 )) %>% 
      execute_if(input$plot_aggregation == 'Departamento',    filter(is.null(plot_departamento_select()) | Departamento %in% plot_departamento_select())) %>% 
      execute_if(input$plot_aggregation == 'Municipio', select(-Departamento)) %>%
      execute_if(input$plot_aggregation == 'Municipio', group_by(lubridate::dmy(Fecha),Municipio)) %>%
      execute_if(input$plot_aggregation == 'Municipio', summarise_all(median, na.rm = TRUE)) %>%
      execute_if(input$plot_aggregation == 'Municipio', filter(is.null(plot_municipio_select()) | Municipio %in% plot_municipio_select())) %>%
      filter(!is.na(Price))

  })
  
  output$plot_text <- renderText({
    if (nrow(plot_datasetInput()) == 0) {
      "There is no data for this selection. Change the time frame or select another indicator/location."} else {"vamos bien perrito"}
  })
  
  output$graph <- renderHighchart({
    
    if ((input$plot_aggregation == "Departamento" & input$plot_by_departamento_item == "Item") | (input$plot_aggregation == "Municipio" & input$plot_by_municipio_item == "Item")) {
      graph <- hchart(plot_datasetInput(), "point", hcaes(x = as.character(sort(lubridate::dmy(Fecha))), y = Price, group = Item))
      
    } else if (input$plot_aggregation == "Departamento"){
      graph <- hchart(plot_datasetInput(), "line", hcaes(x = as.character(sort(lubridate::dmy(Fecha))), y = Price, group = Departamento))
      
    } else {
      graph <- hchart(plot_datasetInput(), "line", hcaes(x = as.character(sort(lubridate::dmy(Fecha))), y = Price, group = Municipio))
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
  
  ## Dataexplorer 
  
  table_datasetInput1 <- reactive({
    full %>% filter(
      lubridate::dmy(Fecha) >= input$table_data[1] & lubridate::dmy(Fecha) <= input$table_data[2]
    ) %>%
      select(Fecha, Departamento, input$table_show_vars) %>% 
      group_by(Fecha, Departamento) %>% 
      summarise_all(median, na.rm = TRUE) # Si se quiere ver la mediana o promedio realizar el cambio de la funcion
    })
  

  table_datasetInput2 <- reactive({
    full %>% filter(
      #is.null(input$table_municipio) | Municipio %in% input$table_municipio,  # Tener cuidado, que es lo que se quiere filtrar
      lubridate::dmy(Fecha) >= input$table_data[1] & lubridate::dmy(Fecha) <= input$table_data[2]
    ) %>%
      select("Fecha", "Departamento", "Municipio", input$table_show_vars_municipio)
  })
  
  
  table_datasetInput <- reactive({
    if (input$table_aggregation == "Departamento") {table_datasetInput1()} else {table_datasetInput2()}
  })
  
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
  
  observe({
    input$table_reset
    #updatePickerInput(session, "table_show_vars", selected = plot_location_list$Departamento)
    #updatePickerInput(session, "table_show_vars_municipio", selected = c("Fecha", "Departamento", "Municipio"),)
    #updatePickerInput(session, "table_show_vars_municipio", selected = names(data),)
    #updateSliderTextInput(session, "table_data", selected = c(dates_min, dates_max))
  }) 
  
}




# Run the application 
shinyApp(ui = ui, server = server)





             
             
             
             
             
             
             
             