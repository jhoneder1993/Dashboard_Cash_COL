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

list_existencia <- read.csv("data/list_existencia.csv", na.strings = c("NA", ""), encoding="latin1", sep = ";") 

country <- st_read("gis/World_admin0_countries_py_WFP_nd.shp") 
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
         "Fr??joles bola roja (1 libra)"            = frijoles,
         "Pastas tipo espagueti (1 libra)"         = pasta,
         "Harina de ma??z (1 kg)"                   = harina,
         "Pl??tano (1 kg)"     = platano,
         "Papa pastusa (1 libra)"  = papa,
         "Yuca (1 libra)"     = yuca,
         "Aceite (1 Lt)"      = aceite,
         "Tomate chonto (1 libra)"     = tomate,
         "Leche (1 Lt)"       = leche,
         "Lomitos de at??n en lata en aceite (170 gr)"   = atun,
         "Papel higi??nico doble hoja (1 unidad )"       = papel_higienico,
         "Toallas higi??nicas (1 paquete)"               = toallas_higienicas,
         "Jab??n de ropa en barra (1 barra de 300gr)"    = jabon_ropa,
         "Crema de dientes (1 tubo de 100gr)"      = crema_dental,
         "Jab??n personal en barra (1 barra de 75gr)"    = jabon_personal,
         "Pa??ales (1 paquete de 30 unidades)"      = panales,
         "Cloro (1 Lt)"          = cloro,
         "Tapabocas quir??rgico (1 unidad )"        = tabapocas
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
         '% de comerciantes que reportaron enfrentar desaf??os en los ??ltimos 30 d??as' = desafios_30ds,
         '% de comerciantes que reportaron desaf??os por: Ninguna v??a de acceso' = desafios_1,
         '% de comerciantes que reportaron desaf??os por: Bloqueos en las v??as' = desafios_2,
         '% de comerciantes que reportaron desaf??os por: Restricciones en las fronteras' = desafios_3,
         '% de comerciantes que reportaron desaf??os por: Inseguridad en la ruta de abastecimiento' =  desafios_4,
         '% de comerciantes que reportaron desaf??os por: Malas condiciones f??sicas de las carreteras' = desafios_5,
         '% de comerciantes que reportaron desaf??os por: Los proveedores detuvieron sus producciones' = desafios_6,
         '% de comerciantes que reportaron desaf??os por: Los proveedores no tienen esos productos' = desafios_7,
         '% de comerciantes que reportaron desaf??os por: El tiempo entre el pedido y la entrega es m??s largo' = desafios_8,
         '% de comerciantes que reportaron desaf??os por: Los proveedores no est??n produciendo estos productos' = desafios_9,
         '% de comerciantes que reportaron desaf??os por: Hay escasez de transportistas' = desafios_10,
         '% de comerciantes que reportaron desaf??os por: El precio de la gasolina es muy alto' = desafios_11,
         '% de comerciantes que reportaron desaf??os por: El precio del transporte es muy alto' = desafios_12,
         '% de comerciantes que reportaron desaf??os por: El principal proveedor ya no da acceso a cr??dito' = desafios_13,
         '% de comerciantes que reportaron desaf??os por: El negocio no tiene suficiente liquidez para reabastecerse' = desafios_14,
         '% de comerciantes que reportaron desaf??os por: No hay suficiente capacidad de almacenamiento' = desafios_15,
         '% de comerciantes que reportaron desaf??os por: Los agentes comerciales ya no visitan los comercios de la zona' = desafios_16,
         '% de comerciantes que reportaron desaf??os por: Mala relaci??n con el proveedor' = desafios_17
  )

full <- left_join(prices, indicators2, by = c("Fecha", "Departamento", "Municipio"))


#### * 3.3 Stock ###############################################################
stock <- data %>%
  select(mes, departamento, municipio, starts_with("dias_exisnc"),
         -ends_with("_papel_higienico"), -ends_with("_toallas_higienicas"), -ends_with("_jabon_ropa"),
         -ends_with("_jabon_personal"), -ends_with("_panales"), -ends_with("_cloro"),
         -ends_with("_tabapocas"), -ends_with("crema_dental"))

stock <- stock %>% 
  group_by(mes, departamento, municipio) %>% 
  summarise_all(median, na.rm = TRUE) %>%
  select(mes, departamento, municipio, starts_with("dias_exisnc")) %>%          # calculate departamento medians
  group_by(mes, departamento, municipio) %>% 
  summarise_all(median, na.rm = TRUE) %>%
  setNames(gsub("dias_exisnc_","",names(.))) %>%                                # rename columns names
  ungroup() %>%
  group_by(mes) %>%  
  rename(Fecha = mes, Departamento = departamento, Municipio = municipio,       # rename column names 
         "D??as de existencia: Arroz"      = arroz,
         "D??as de existencia: Lentejas"   = lentejas,
         "D??as de existencia: Huevos rojos tipo AA"         = huevos,
         "D??as de existencia: Pechuga de pollo"             = pollo,
         "D??as de existencia: Fr??joles bola roja"           = frijoles,
         "D??as de existencia: Pastas tipo espagueti"        = pasta,
         "D??as de existencia: Harina de ma??z"               = harina,
         "D??as de existencia: Pl??tano"       = platano,
         "D??as de existencia: Papa pastusa"  = papa,
         "D??as de existencia: Yuca"          = yuca,
         "D??as de existencia: Aceite"        = aceite,
         "D??as de existencia: Tomate chonto"     = tomate,
         "D??as de existencia: Leche"             = leche,
         "D??as de existencia: Lomitos de at??n en lata en aceite"   = atun)


full2 <- left_join(stock, indicators2, by = c("Fecha", "Departamento", "Municipio"))

#full <- left_join(prices, indicators2, by = c("Fecha", "Departamento", "Municipio"))

#### 4 REFERENCES ##############################################################

max(unique(prices_long$Fecha))

dates_full <- sort(unique(full2$Fecha))
dates_max_f <- max(full2$Fecha)
dates_stock <- sort(unique(stock$Fecha))
dates_max_s <- max(stock$Fecha)
dates <- sort(unique(prices_long$Fecha))                                          # define list with date range in data
dates_min  <- min(prices_long$Fecha)                              # set minimum date to be displayed
dates_min_s <- min(stock$Fecha)
dates_max  <- max(prices_long$Fecha)                              # maximum date in data
dates_max2 <- sort(unique(prices_long$Fecha), decreasing=T)[2]    # second-latest date

dates_max_1y <- as.POSIXlt(dates_max)                                             # most recent month minus 1 year
dates_max_1y$year <- dates_max_1y$year-1
dates_max_1y <- as.Date(format(dates_max_1y, "%Y-%m-01"), format = "%Y-%m-%d")


plot_location_list <- prices_long %>%                                           # define location list (which is later used as choice filter options)
  ungroup() %>%
  select(Departamento, Municipio) %>%                                             # extract governorate and district columns
  arrange(Departamento, Municipio) %>%                                            # list alphabetically
  filter(!duplicated(Departamento))                                               # remove duplicates

indicator_list <- names(indicators2)             # extract additional indicator list

cols      <- c("rgb(238,88,89)",   "rgb(88,88,90)",    "rgb(165,201,161)",        # define color palette for plot lines
               "rgb(86,179,205)",  "rgb(246,158,97)",  "rgb(255,246,122)",        # est??n los colores reach + una paleta de amarillos y naranjas
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
  filter (Fecha >= dates_min )%>%
  select(Fecha, `Lista de productos alimentarios`, `Lista de productos no alimentarios`, 
         `Lista de productos compilados`) %>%
  gather(Item, Price, 'Lista de productos alimentarios':'Lista de productos compilados')                                    # transform SMEB data to long format so highcharter can read dataframe

one_year_from_current_date <- prices_country_home$Fecha %>% max() - 366

prices_country_home <- prices_country_home %>% filter(Fecha > one_year_from_current_date)


#### 6 UI ######################################################################
ui <- bootstrapPage(
  navbarPage("JMMI COLOMBIA",                                                                                     # Nombre del DASHBOARD o del panel navegador
             theme = shinytheme("simplex"),                                                                       # Tema del navbarpage
             
             #### * 3 Mapa ###############################################################
             
             tabPanel("Mapa",
                      icon = icon("map"),
                      div(class="outer",
                          
                          tags$head(includeCSS("styles.css")),
                          
                          leafletOutput("map", width = "100%", height = "100%"),
                          
                          absolutePanel(
                            id = "controls", class = "panel panel-default", fixed = TRUE, draggable = FALSE, top = "130", left = "12", right = "auto", bottom = "auto",
                            width = 330, height = "auto",
                            
                            pickerInput(inputId =  "mapa_indicadores",
                                        label = "Indicadores:",
                                        choices = c("Existencias", "Desaf??os de reabastecimiento"),
                                        selected = "Existencias",
                                        multiple = FALSE
                            ),
                            conditionalPanel(condition = "input.mapa_indicadores == 'Existencias'",
                                             pickerInput(inputId = "map_item_select",
                                                         label = HTML("Existencias:<br><i>(d??as de existencia de productos alimentarios)</i>"),
                                                         choices = sort(list_existencia$Group),
                                                         selected = "Dias de existencia Aceite",
                                                         options = list(`actions-box` = TRUE),
                                                         multiple = FALSE
                                             )
                                             
                            ),
                            conditionalPanel(condition = "input.mapa_indicadores == 'Desaf??os de reabastecimiento'",
                                             pickerInput(inputId =  "map_addindicator_select",
                                                         label = HTML("Desaf??os de reabastecimiento:<br><i>(% de comerciantes que reportaron)</i>"),
                                                         choices = sort(indicator_list),
                                                         selected = "% de comerciantes que reportaron enfrentar desaf??os en los ??ltimos 30 d??as",
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
                          
                
             )
  )
)

)



#### 7 SERVER ##################################################################

server <- function(input, output, session) {
  # Agregar las funciones del server
  
  
  #### 3 Mapa ######################################################################
  mapa_indicadores <- reactive({
    if (input$mapa_indicadores == "Existencias") {input$map_item_select} else {input$map_addindicator_select}
  })
  output$map_text <- renderText({""})
  
  output$map <- renderLeaflet({
    if (input$mapa_indicadores == "Existencias"){  
      stock_mapa <- stock %>% filter(Fecha == input$mapa_fecha_seleccionada)
      departamento <- left_join(departamento, stock_mapa, by =c("admin1Name" = "Departamento"))
      
      labels <- sprintf("<strong>%s</strong><br/>%s Stock (%s)", departamento$admin1Name, format(departamento$`D??as de existencia: Aceite`, big.mark=","), format(departamento$Fecha, "%b %Y")) %>% lapply(htmltools::HTML)
      pal <- colorNumeric(palette = c("#FFF98C", "#E0C45C", "#CB3B3B", "#85203B"),
                          domain = departamento$`D??as de existencia: Aceite`, na.color = "transparent"
      )
    } else {
      stock_mapa <- indicators2 %>% filter(Fecha == input$mapa_fecha_seleccionada)
      departamento <- left_join(departamento, indicators2, by = c("admin1Name" = "Departamento"))
      labels <- sprintf("<strong>%s</strong><br/>%s Stock (%s)", departamento$admin1Name, format(departamento$`% de comerciantes que reportaron enfrentar desaf??os en los ??ltimos 30 d??as`, big.mark=","), format(departamento$Fecha, "%b %Y")) %>% lapply(htmltools::HTML)
      pal <- colorNumeric(palette = c("#FFF98C", "#E0C45C", "#CB3B3B", "#85203B"),
                          domain = departamento$`% de comerciantes que reportaron enfrentar desaf??os en los ??ltimos 30 d??as`, na.color = "transparent"
      )
      
    }
    
    # Coordenadas
    bounds <- departamento %>% 
      st_bbox() %>% 
      as.character()
    
    map <- leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
      fitBounds((as.numeric(bounds[1])-15), bounds[2], bounds[3], bounds[4]) %>%
      addMapPane(name = "base", zIndex = 410) %>%
      addMapPane(name = "polygons", zIndex = 420) %>%
      addMapPane(name = "label", zIndex = 430) %>%
      addPolygons(data = departamento, group = "Departamento", fill = TRUE, fillOpacity = 0.7, fillColor = ~pal(departamento$`D??as de existencia: Aceite`),
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
      )%>%
      addPolygons(data = country, group = "Country", fill = FALSE, stroke = TRUE, color = "#58585A", weight = 1.2, opacity = 1) %>%
      addLegend("bottomright", pal = pal, values = departamento$`D??as de existencia: Aceite`,
                title = "Stock:",
                labFormat = labelFormat(prefix = "D??as:"),
                opacity = 1
      )%>%
      setMapWidgetStyle(style = list(background = "transparent")) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "Base map",
                       options = c(providerTileOptions(opacity = 0.6),
                                   leafletOptions(pane = "base"))) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "Labels",
                       options = c(providerTileOptions(opacity = 1),
                                   leafletOptions(pane = "label"))) %>%
      addLayersControl(overlayGroups = c("Labels", "Country", "Departamento", "Base map"))
    
  })
  
  observe({
    
    
    output$map_text <- renderText({
      if (all(is.na(departamento[["Departamento"]])) == TRUE) {
        "There is no data for this selection. Select another month or indicator."} else {NULL}
    })
    
    if (all(is.na(departamento[["Departamento"]])) == TRUE) {
      return(NULL)
    } 
    
    if (input$mapa_indicadores == "Existencias") {
      labels <- sprintf("<strong>%s</strong><br/>%s Stock (%s)", departamento$admin1name, format(departamento[[mapa_indicadores()]], big.mark=","), format(departamento$Fecha, "%b %Y")) %>% lapply(htmltools::HTML)
    } else {
      labels <- sprintf("<strong>%s</strong><br/>%s&#37; de comerciantes que reportaron (%s)", departamento$admin1name, departamento[[mapa_indicadores()]], format(departamento$Fecha, "%b %Y")) %>% lapply(htmltools::HTML)
    }
    
    pal <- colorNumeric(palette = c("#FFF98C", "#E0C45C", "#CB3B3B", "#85203B"),
                        domain = departamento[[mapa_indicadores()]], na.color = "transparent")
    pal2 <- colorNumeric(palette = c("#FFF98C", "#E0C45C", "#CB3B3B", "#85203B"),
                         domain = departamento[[mapa_indicadores()]], na.color = "transparent")
    
    
    leafletProxy("map") %>%
      clearControls %>%
      clearGroup("Departamento") %>%
      execute_if(input$mapa_indicadores == "Existencias",
                 addPolygons(data = departamento, group = "Departamento", fill = TRUE, fillOpacity = 0.7, fillColor = ~pal(departamento[[mapa_indicadores()]]),
                             stroke = TRUE, color = "#58585A", weight = 0.4, opacity = 1,
                             highlight = highlightOptions(
                               weight = 5,
                               color = "#666666",
                               fillOpacity = o.75,
                               bringToFront = TRUE
                             ),
                             label = labels,
                             labelOptions = labelOptions(
                               style = list("font-weight" = "normal", padding = "3px 8px"),
                               textsize = "15px",
                               direction = "auto"),
                             options = leafletOptions(pane = "polygons")
                 )
      )%>%
      execute_if(input$mapa_indicadores == "Desaf??os de reabastecimiento",
                 addPolygons(data = departamento, group = "Departamento", fill = TRUE, fillOpacity = 0.7, fillColor = ~pal2(departamento[[mapa_indicadores()]]),
                             stroke = TRUE, color = "#58585A", weight = 0.4, opacity = 1,
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
                               direction = "auto",
                               options = leafletOptions(pane = "polygons")
                             )
                 )
      )%>%
      execute_if(input$mapa_indicadores == "Existencias",
                 addLegend("bottomright", pal = pal, values = departamento [[mapa_indicadores()]],
                           title = "Existencias:",
                           labFormat = labelFormat(prefix = "Stock"),
                           opacity = 1)
      )%>%
      execute_if(input$mapa_indicadores == "Desaf??os de reabastecimiento",
                 addLegend("bottomright", pal = pal2, values = range(0,100),
                           title = "% de comerciantes que reportaron:",
                           labFormat = labelFormat(suffix = "%"),
                           opacity = 1
                 )
      )
    
    
    
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)











