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

#source("source/execute_if.R", encoding = "UTF-8")
#source("source/execute_in_pipeline.R", encoding = "UTF-8")
#
#### 2 LOAD DATA ###############################################################

data        <- read.csv("data/longterm_data.csv", na.strings = c("NA", ""))       # load JPMI dataset

item_list   <- read.csv("data/item_list.csv")                                     # load item list

country     <- st_read("gis/irq_admbnda_adm0_cso_itos_20190603.shp")              # load shapefile with country border
governorate <- st_read("gis/irq_admbnda_adm1_cso_20190603.shp")                   # load shapefile with governorate borders
district    <- st_read("gis/irq_admbnda_adm2_cso_20190603.shp")                   # load shapefile with district borders


if (grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", data$mes[1])) {                           # format date column in JPMI dataset as date
  data$mes <- as.Date(data$mes, format = "%Y-%m-%d")
} else if (grepl("[0-9]{2}/[0-9]{2}/[0-9]{4}", data$mes[1])) {
  data$mes <- as.Date(data$mes, format = "%d/%m/%Y")
}


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
         '% de comerciantes que reportaron desaf??os por: Los proveedores detuvieron sus produccioness' = desafios_6,
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


#### 4 REFERENCES ##############################################################

max(unique(lubridate::dmy(prices_long$Fecha)))
  


dates <- sort(unique(prices_long$Fecha))                                          # define list with date range in data
dates_min  <- min(lubridate::dmy(prices_long$Fecha))                              # set minimum date to be displayed
dates_max  <- max(lubridate::dmy(prices_long$Fecha))                              # maximum date in data
dates_max2 <- sort(unique(lubridate::dmy(prices_long$Fecha)), decreasing=T)[2]    # second-latest date

dates_max_1y <- as.POSIXlt(dates_max)                                             # most recent month minus 1 year
dates_max_1y$year <- dates_max_1y$year-1
dates_max_1y <- as.Date(format(dates_max_1y, "%Y-%m-01"), format = "%Y-%m-%d")


plot_location_list <- prices_long %>%                                           # define location list (which is later used as choice filter options)
  ungroup() %>%
  select(Departamento, Municipio) %>%                                             # extract governorate and district columns
  arrange(Departamento, Municipio) %>%                                            # list alphabetically
  filter(!duplicated(Departamento))                                               # remove duplicates

indicator_list <- names(indicators) %>%
  str_subset(c("Fecha", "Departamento", "Municipio"), negate = TRUE)              # extract additional indicator list

cols      <- c("rgb(238,88,89)",   "rgb(88,88,90)",    "rgb(165,201,161)",        # define color palette for plot lines
               "rgb(86,179,205)",  "rgb(246,158,97)",  "rgb(255,246,122)",        # est??n los colores reach + una paleta de amarillos y naranjas
               "rgb(210,203,184)", "rgb(247,172,172)", "rgb(172,172,173)",        # azul claro y distintos tonos de beige
               "rgb(210,228,208)", "rgb(171,217,230)", "rgb(251,207,176)",
               "rgb(255,251,189)", "rgb(233,229,220)")

full_list <- data.frame(Item = indicator_list,                                    # create full indicator list
                        Group = "Indicadores") %>% arrange(Item)
full_list <- rbind(item_list, full_list)                                          # combine prices and additional indicators


#### * 5.1 Define dataframe ####################################################

prices_country <- prices %>%                                                      # aggregate price data at country level
  select(-Departamento, -Municipio) %>%
  group_by(Fecha) %>% 
  summarise_all(median, na.rm = TRUE)
prices_country_long <- gather(prices_country, Item, Price, 2:ncol(prices_country))# transform country-level price data to long format

prices_country_home <- prices_country %>%                                         # filter out SMEB data from country level price data
  filter(lubridate::dmy(Fecha) >= dates_min) %>%
  select(Fecha, `Lista de productos alimentarios`, `Lista de productos no alimentarios`, 
         `Lista de productos compilados`) %>%
  gather(Item, Price, 'Lista de productos alimentarios':'Lista de productos compilados')                                    # transform SMEB data to long format so highcharter can read dataframe

one_year_from_current_date <- lubridate::dmy(prices_country_home$Fecha) %>% max() - 366

prices_country_home <- prices_country_home %>% filter(Fecha > one_year_from_current_date)

#prices_changes <- prices_country_long %>%                                         # calculate bi-monthly/yearly changes of item prices
#  filter(Fecha == dates_max | Fecha == dates_max2 | Fecha == "2020-11-1") %>%        # use "dates_max_1y" for yearly change
#  group_by(Item) %>%
#  mutate(change  = percent(Price/lag(Price, order_by=Fecha)-1, accuracy = 1),
#         change2 = percent(Price/lag(Price, n = 2, order_by=Fecha)-1, accuracy = 1)) %>%
#  mutate(change  = ifelse(!grepl('^\\-', change) & change != "0%" & !is.na(change), paste0("+", change, HTML(" &#9650;")), change),
#         change  = ifelse(grepl('^\\-', change), paste0(change, HTML(" &#9660;")), change),
#         change  = ifelse(change == "0%", paste0(change, HTML(" &#9654;")), change),
#         change2 = ifelse(!grepl('^\\-', change2) & change2 != "0%" & !is.na(change2), paste0("+", change2, HTML(" &#9650;")), change2),
#         change2 = ifelse(grepl('^\\-', change2), paste0(change2, HTML(" &#9660;")), change2),
#         change2 = ifelse(change2 == "0%", paste0(change2, HTML(" &#9654;")), change2)) %>%
#  filter(Fecha == dates_max,
#         !is.na(Price)) %>%
#  select(-Fecha) %>%
#  mutate(Price   = format(Price, big.mark=","),
#         change2 = replace_na(change2, "NA")) %>%
#  rename("Precio"= Price,
#         "Cambio mensual" = change,
#         "Cambio anual"       = change2)
#
#prices_changes_items <- prices_changes %>%
#  filter(!(str_detect(Item, "^SMEB") | Item == "US dollars (1 USD)"))
#
#prices_changes_meb <- prices_changes %>%
#  filter(str_detect(Item, "^SMEB") | str_detect(Item, "^US dollars")) %>%
#  arrange(Item)
#
#data_latest <- data %>%                                                                                   # latest dataset for download on dashboard page
#  filter(date == dates_max) %>%
#  select(-(106:ncol(data)), -starts_with("dias_exisnc"))


#### 6 UI ######################################################################

ui <- bootstrapPage(
  navbarPage("JMMI COLOMBIA",
             theme = shinytheme("simplex")
             )
  
  
  )


#### 7 SERVER ##################################################################

server <- function(input, output, session) {
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
