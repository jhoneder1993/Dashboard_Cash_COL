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

data <- read.csv("data/longterm_data.csv", na.strings = c("NA", ""), encoding="latin1", sep = ";")       # load JPMI dataset

item_list   <- read.csv("data/item_list.csv", encoding="latin1")                                     # load item list

country     <- st_read("gis/World_admin0_countries_py_WFP_nd.shp")              # load shapefile with country border
#governorate <- st_read("gis/irq_admbnda_adm1_cso_20190603.shp")                   # load shapefile with governorate borders
#district    <- st_read("gis/irq_admbnda_adm2_cso_20190603.shp")                   # load shapefile with district borders

# Cargar shps
departamento <- st_read("gis/Admin1_UnodcOcha_01012009.shp")


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
             
             
             #### 1ra página ################################################
             
             # Nuevo tab Panel del Explorador de Datos #########################
             tabPanel("Data Explorer", 
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
             )
  )



#### 7 SERVER ##################################################################

server <- function(input, output, session){
  
  
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

