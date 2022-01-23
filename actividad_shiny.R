pacman::p_load(htmltools,shiny,shinydashboard,shinythemes,httr,tidyverse,jsonlite,janitor, purrr, readr, sp, dplyr, readxl, leaflet, leaflet.extras, devtools )

## CREAMOS DATASET ####
url <- 'https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/'
#cargamos la url
GET(url)
#guardamos la información en un df
df_gas <- url %>% fromJSON() %>% .$ListaEESSPrecio %>% tibble()
#limpiamos el df 
ds_gas <- df_gas %>% clean_names() %>% type_convert(locale = locale(decimal_mark = ',')) %>% rename(longitud=longitud_wgs84)
#introducimos en el df para que enseñe el nombre de la comunidad 
ccaa_nombre=c('ANDALUCIA','ARAGON','PRINCIPADO DE ASTURIAS', 'ILLES BALEARS', 'CANARIAS', 'CANTABRIA', 'CASTILLA Y LEON',
              'CASTILLA-LA MANCHA', 'CATALUï¿½A', 'COMUNITAT VALENCIANA', 'EXTREMADURA','GALICIA', 'COMUNIDAD DE MADRID', 
              'REGION DE MURCIA', 'COMUNIDAD FORAL DE NAVARRA', 'PAIS VASCO', 'LA RIOJA', 'CEUTA', 'MELILLA')

ds_gas <-ds_gas%>%
  mutate(ccaa=case_when(idccaa=='01'~ccaa_nombre[1], idccaa=='02'~ccaa_nombre[2], idccaa=='03'~ccaa_nombre[3], idccaa=='04'~ccaa_nombre[4], 
                        idccaa=='05'~ccaa_nombre[5], idccaa=='06'~ccaa_nombre[6], idccaa=='07'~ccaa_nombre[7], idccaa=='08'~ccaa_nombre[8], 
                        idccaa=='09'~ccaa_nombre[9], idccaa=='10'~ccaa_nombre[10], idccaa=='11'~ccaa_nombre[11], idccaa=='12'~ccaa_nombre[12], 
                        idccaa=='13'~ccaa_nombre[13], idccaa=='14'~ccaa_nombre[14], idccaa=='15'~ccaa_nombre[15], idccaa=='16'~ccaa_nombre[16], 
                        idccaa=='17'~ccaa_nombre[17], idccaa=='18'~ccaa_nombre[18], idccaa=='19'~ccaa_nombre[19]))  
#creamos una variable para contener precio y idcca
precio_combustible <- ds_gas[9:22] %>% mutate(ds_gas$idccaa) %>% rename(idccaa=`ds_gas$idccaa`)
#calculamos la media de los precios por idcca
precio_combustible <- precio_combustible %>% group_by(idccaa) %>% summarise_all(mean,na.rm=TRUE)
#creamos una variable con los nuevos nombre para la media de los precios de los combustibles
nueva_variable <- c('media_precio_biodiesel','media_precio_bioetanol', 'media_precio_gas_natural_comprimido',
                   'media_precio_gas_natural_licuado','media_precio_gases_licuados_del_petroleo', 'media_precio_gasoleo_a',
                   'media_precio_gasoleo_b','media_precio_gasoleo_premium','media_precio_gasolina_95_e10', 'media_precio_gasolina_95_e5', 
                   'media_precio_gasolina_95_e5_premium', 'media_precio_gasolina_98_e10', 'media_precio_gasolina_98_e5', 'media_precio_hidrogeno' )
#realizamos los cambios de los nombres
for(i in 2:16){
  precio_combustible <- precio_combustible %>% rename_at(i, function(x) nueva_variable[i-1])
}
#juntamnos los dos df
ds_gas <- merge(x = ds_gas, y = precio_combustible, by = c("idccaa", "idccaa")) %>% as_tibble()
#creamos nuevos nombres para determinar la valoración del tipo de combustible 
nueva_variable <- c('valoracion_precio_biodiesel','valoracion_precio_bioetanol', 'valoracion_precio_gas_natural_comprimido',
                   'valoracion_precio_gas_natural_licuado','valoracion_precio_gases_licuados_del_petroleo', 'valoracion_precio_gasoleo_a',
                   'valoracion_precio_gasoleo_b','valoracion_precio_gasoleo_premium','valoracion_precio_gasolina_95_e10', 'valoracion_precio_gasolina_95_e5', 
                   'valoracion_precio_gasolina_95_e5_premium', 'valoracion_precio_gasolina_98_e10', 'valoracion_precio_gasolina_98_e5', 'valoracion_precio_hidrogeno' )
#para determinar si es más o menos caro que la media con un true o false
# #true o false si es mas caro o no que la media 
ds_gas<-ds_gas %>% mutate(valoracion_precio_biodiesel= (ds_gas$precio_biodiesel>df_gas$media_precio_biodiesel))
ds_gas<-ds_gas %>% mutate(valoracion_precio_bioetanol= (ds_gas$precio_bioetanol>df_gas$media_precio_bioetanol))
ds_gas<-ds_gas %>% mutate(valoracion_precio_gas_natural_comprimido= (ds_gas$precio_gas_natural_comprimido>=df_gas$media_precio_gas_natural_comprimido))
ds_gas<-ds_gas %>% mutate(valoracion_precio_gas_natural_licuado= (ds_gas$precio_gasoleo_b>df_gas$media_precio_gasoleo_b))
ds_gas<-ds_gas %>% mutate(valoracion_precio_gases_licuados_del_petroleo= (ds_gas$precio_gases_licuados_del_petroleo>=df_gas$media_precio_gases_licuados_del_petroleo))
ds_gas<-ds_gas %>% mutate(valoracion_precio_gasoleo_a= (ds_gas$precio_gasoleo_a>df_gas$media_precio_gasoleo_a))
ds_gas<-ds_gas %>% mutate(valoracion_precio_gasoleo_b= (ds_gas$precio_gasoleo_b>df_gas$media_precio_gasoleo_b))
ds_gas<-ds_gas %>% mutate(valoracion_precio_gasoleo_premium= (ds_gas$precio_gasoleo_premium>df_gas$media_precio_gasoleo_premium))
ds_gas<-ds_gas %>% mutate(valoracion_precio_gasolina_95_e10= (ds_gas$precio_gasolina_95_e10>df_gas$media_precio_gasolina_95_e10))
ds_gas<-ds_gas %>% mutate(valoracion_precio_gasolina_95_e5= (ds_gas$precio_gasolina_95_e5>df_gas$media_precio_gasolina_95_e5))
ds_gas<-ds_gas %>% mutate(valoracion_precio_gasolina_95_e5_premium= (ds_gas$precio_gasolina_95_e5_premium>=df_gas$media_precio_gasolina_95_e5_premium))
ds_gas<-ds_gas %>% mutate(valoracion_precio_gasolina_98_e10= (ds_gas$precio_gasolina_98_e10>df_gas$media_precio_gasolina_98_e10))
ds_gas<-ds_gas %>% mutate(valoracion_precio_gasolina_98_e5= (ds_gas$precio_gasolina_98_e5>df_gas$media_precio_gasolina_98_e5))
ds_gas<-ds_gas %>% mutate(valoracion_precio_hidrogeno= (ds_gas$precio_hidrogeno>df_gas$media_precio_hidrogeno))
#hacemos que en el df aparezca este apartado 
for(i in 0:13){
  ds_gas[47+i] = ifelse(ds_gas[47+i] == 'TRUE','no_low_cost','low_cost')
}

## SERVER ####
server <- function(input, output, ShinySession) {
  ##COMBUSTIBLE ####
  combustible_seleccionado <- reactive({
    input$combustible
  })
  
  ##PRECIO COMBUSTIBLE ####
  
  precio_seleccionado <- reactive({
    input$precio
  })
  #para que nos devuelva el precio de cada fila del ds
  precio_bueno <- function(x){
    if(combustible_seleccionado()=='precio_gasoleo_a'){
      return(15) 
    }
    else if(combustible_seleccionado()=='precio_gasoleo_b'){
      return(16)
    }
    else if(combustible_seleccionado()=='precio_gasoleo_premium'){
      return(17)
    }
    else if(combustible_seleccionado()=='precio_gasolina_95_e10'){
      return(18)
    }
    else if(combustible_seleccionado()=='precio_gasolina_95_e5'){
      return(19)
    }
    else if(combustible_seleccionado()=='precio_gasolina_98_e5'){
      return(22)
    }
  }
  #para que devuelva el precio del combustible seleccionado de un municipio
  observeEvent(input$municipio1, {
    if(!''==(municipio_seleccionado())){
      observeEvent(input$combustible, {
        ds_gas <- ds_gas %>%
          filter(ds_gas[,ds_gas_loc(loc_seleccionado())]==municipio_seleccionado())
        updateSliderInput(
          inputId = "precio",
          min=min(ds_gas[,precio_bueno(combustible_seleccionado())],
                  na.rm = TRUE),
          max=max(ds_gas[,precio_bueno(combustible_seleccionado())],
                  na.rm = TRUE),
          value=max(ds_gas[,precio_bueno(combustible_seleccionado())],
                    na.rm = TRUE)
        )
      })
      
    }
    
  })
  ## CLIENTE ####
  output$df_gas_map_cliente <- renderLeaflet({
    #realizamos un filter para cuando selecciona un municipio seleccionado le salgan el precio, media y valoración del combustible que desea consultar y la localización del mismo
    if(!''==(municipio_seleccionado())){
      ds_gas <- ds_gas %>%
        filter(ds_gas[,ds_gas_loc(loc_seleccionado())]==municipio_seleccionado()) %>% 
        select(!starts_with('precio'),contains(combustible_seleccionado())) %>%
        select(!starts_with('media'),contains(combustible_seleccionado())) %>%
        select(!starts_with('valoracion'),contains(combustible_seleccionado()))  
  # filtramos por el precio seleccionado 
      ds_gas %>%filter(ds_gas[,20]<=precio_seleccionado()) %>%glimpse() %>%
        leaflet() %>%
        addProviderTiles('Esri') %>%
        addMarkers(~longitud,~latitud)
      
    }
    else{
      ds_gas %>%
        leaflet() %>%
        addProviderTiles('Esri') %>%
        fitBounds(~min(longitud), ~min(latitud), ~max(longitud), ~max(latitud))}
    
  })
  ##LOC ####
  #hacemos que nos devuelva los diferentes filtros en funcion de lo que seleccione el usuario proveniente del ds
  ds_gas_loc <- function(x){
    if(x=='municipio'){
      return(9) 
    }
    else if(x=='provincia'){
      return(24)
    }
    else if(x=='ccaa'){
      return(61)
    }
    else if(x=='c_p'){
      return(2)
    }
    else if(x=='direccion'){
      return(3)
    }
  }
  
  loc_seleccionado <- reactive({
    input$loc
  })
  #para que pueda introducir la ubicación deseada
  observeEvent(input$loc, {
    updateTextInput(inputId = "municipio1",
                    label = paste('Introduce', loc_seleccionado(), sep=" ")
    )
  })
  
  ##LOC ESPECIFICO ####
  
  simpleCap <- function(x) {
    x <- tolower(x)
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  mayus <- function(x){
    x <- toupper(x)
  }
  
  correct_word <- function(x){
    if(loc_seleccionado()=='municipio'){
      return(simpleCap(x)) 
    }
    else if(loc_seleccionado()=='provincia'){
      return(mayus(x))
    }
    else if(loc_seleccionado()=='ccaa'){
      return(mayus(x))
    }
    else if(loc_seleccionado()=='c_p'){
      return(x)
    }
    else if(loc_seleccionado()=='direccion'){
      return(mayus(x))
    }
  }
  
  municipio_seleccionado <- reactive({
    correct_word(input$municipio1)
    
    
  })
  
  ##EMPRESARIO ####
  output$ds_gas_map_empresario <- renderLeaflet({
    #realizamos lo mismo que para el cliente
    if(!''==(municipio_seleccionado2())){
      ds_gas <- ds_gas %>%
        filter(ds_gas[,ds_gas_loc2(loc_seleccionado2())]==municipio_seleccionado2()) %>% 
        select(!starts_with('precio'),contains(combustible_seleccionado2())) %>%
        select(!starts_with('media'),contains(combustible_seleccionado2())) %>%
        select(!starts_with('valoracion'),contains(combustible_seleccionado2())) 
      
      ds_gas %>%
        filter(ds_gas[,20]<=precio_seleccionado2()) %>%glimpse() %>%
        leaflet() %>%
        addProviderTiles('Esri') %>%
        addMarkers(~longitud,~latitud)

    }
    else{
      ds_gas %>%
        leaflet() %>%
        addProviderTiles('Esri') %>%
        fitBounds(~min(longitud), ~min(latitud), ~max(longitud), ~max(latitud))}
    
  })

  ###LOC EMPRESARIO####
  
  ds_gas_loc2 <- function(x){
    if(x=='municipio'){
      return(9) 
    }
    else if(x=='provincia'){
      return(24)
    }
    else if(x=='ccaa'){
      return(61)
    }
    else if(x=='c_p'){
      return(2)
    }
    else if(x=='direccion'){
      return(3)
    }
  }
  loc_seleccionado2 <- reactive({
    input$loc2
  })
  
  observeEvent(input$loc2, {
    updateTextInput(inputId = "municipio2",
                    label = paste('Introduce', loc_seleccionado2(), sep=" ")
    )
  })
  
  ###LOC ESPECIFICO ####
  simpleCape <- function(x) {
    x <- tolower(x)
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  mayuse <- function(x){
    x <- toupper(x)
  }
  correct_worde <- function(x){
    if(loc_seleccionado2()=='municipio'){
      return(simpleCap2(x)) 
    }
    else if(geo_seleccionado2()=='provincia'){
      return(mayus2(x))
    }
    else if(loc_seleccionado2()=='ccaa'){
      return(mayus2(x))
    }
    else if(loc_seleccionado2()=='c_p'){
      return(x)
    }
    else if(loc_seleccionado2()=='direccion'){
      return(mayus2(x))
    }
  }
  
  municipio_seleccionado2 <- reactive({
    correct_worde(input$municipio2)
    
    
  })
}
}
## SHINY APP #####
#deesarrollo de la parte gráfica de la aplicación
ap <- dashboardPage(
  dashboardHeader(title = 'Gasolineras'),
  dashboardSidebar(
    sidebarMenu(id='sidebarMenuID',
                menuItem('CLIENTE',
                         icon = icon('glyphicon glyphicon-user'),
                         tabName = 'tab_cliente'),
                menuItem('EMPRESARIO',
                         icon = icon('glyphicon glyphicon-euro'),
                         tabName = 'tab_empresario')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'tab_cliente',
              fluidPage(
                titlePanel("Cliente"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput('loc',
                                'Selecciona filtro',
                                choices = list('Municipio' = 'municipio',
                                               'Provincia' = 'provincia',
                                               'Comunidad' = 'ccaa',
                                               'Cod.Postal'='c_p',
                                               'Direccion'='direccion')
                    ),
                    textInput('municipio', 
                              'Introduce',
                              value = ''),
                    selectInput('combustible',
                                'Selecciona combustible',
                                choices = list('Gasoleo A' = 'precio_gasoleo_a',
                                               'Gasoleo B' = 'precio_gasoleo_b',
                                               'Gasoleo Premium' = 'precio_gasoleo_premium',
                                               'Gasolina 95 e10'='precio_gasolina_95_e10',
                                               'Gasolina 95 e5'='precio_gasolina_95_e5',
                                               'Gasolina 98 e5'='precio_gasolina_98_e5'),
                                selected ='precio_gasoleo_a' ),
                    
                    sliderInput("precio",
                                "Seleccion precio",
                                min =0,
                                max =1,
                                value =1
                    )
                    
                  ),
                  
                  mainPanel(
                    leafletOutput("ds_gas_map_cliente")
                  )
                )
              )
      ),
      
      tabItem(tabName = 'tab_empresario',
              fluidPage(
                titlePanel("Empresario"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput('loc2',
                                'Selecciona filtro',
                                choices = list('Municipio' = 'municipio',
                                               'Provincia' = 'provincia',
                                               'Comunidad' = 'ccaa',
                                               'Cod.Postal'='c_p',
                                               'Direccion'='direccion')
                    ),
                    textInput('municipio2', 
                              'Introduce',
                              value = ''),
                    
                    
                  )
                )
              )
      )
      
    )
  )
)

shinyApp(ui,server)

