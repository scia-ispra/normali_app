#scelta del parametro: temperatura o precipitazione
switchParametroUI<-function(id){
  
  ns<-NS(id)
  
  div(
    switchInput(
      inputId=ns("scegli"),
      label = "",
      value=TRUE,
      onLabel="Temperatura",
      offLabel="Precipitazione"
    ) %>% shinyInput_label_embed(icon("info-circle") %>% bs_embed_tooltip(title="Seleziona la grandezza di interesse e il relativo indicatore"))
  )
  #fine swithcParametroUI
}



switchParametroServer<-function(id){
 
  moduleServer(id,function(input,output,session){
    
    eventReactive(input$scegli,{
      
      if(input$scegli){ #Temperatura
        
        choices<-list("Tmax (°C)","Giorni con Tmax > 25 °C","Giorni con Tmax >= 30 °C","Giorni con Tmax >= 35 °C","Giorni con Tmax >= 40 °C","Tmean (°C)","Cooling Degree Days (°C)","Diurnal Temperature Range (°C)","Tmin (°C)","Giorni con Tmin < 0 °C","Giorni con Tmin > 20 °C")
        
      }else{
        
        choices<-list("Prec (mm)","Giorni con Prec >= 1 mm","Giorni con Prec >= 5 mm","Giorni con Prec >= 10 mm","Giorni con Prec >= 20 mm","Giorni con Prec >= 100 mm")
        
      }
      
      choices
      
    })


  }) 
  
  
}#fine switchParametroServer
#####################à

######################
# Scelta dell'indicatore: precipitazione cumulata, indicatori di estremi, temperatura media etc
######################
pickerIndicatoreUI<-function(id,choices=c("Tmax","Tmean","Tmin")){
  
  div(
    pickerInput(
      inputId = id,
      label = "Indicatore", 
      choices = choices,
      selected = "Tmax",
      multiple = FALSE)
  )
  
}
#########################



######## Pulsanti per scegliere il trentennio climatologico
radioButtonsGroupUI<-function(id){
  
  NS(id)->ns
  
  div(
    radioGroupButtons(
      inputId =ns("bottoni")
      ,label = "Trentennio climatologico"
      ,choices = list("1961-1990","1971-2000","1981-2010","1991-2020")
      ,selected = "1991-2020"
      ,status = "info"
      ,size = "normal"
      ,direction = "horizontal"
      ,justified = TRUE
      ,individual = FALSE
    ) %>% shinyInput_label_embed(icon("info-circle") %>% bs_embed_tooltip(title = "Seleziona il trentennio climatologico di interesse"))
  )
  
  
}#fine radioButtonsGroupUI


radioButtonsGroupServer<-function(id){
  
  moduleServer(id,function(input,output,session){
    
    reactive({input$bottoni})
    
  })
  
}#fine radioButtonsGroupServer
######################################



switchUI<-function(id){
  
  ns<-NS(id)
  
  materialSwitch(
    inputId = ns("annuale_completo"),
    label = "Solo serie complete", 
    value = TRUE,
    status = "primary"
  )
  
}#switchUI

switchServer<-function(id){
  
  moduleServer(id,function(input,output,session){
    
    reactive({input$annuale_completo})
    
  })
  
}#fine switchServer









tabellaUI<-function(id){
  
  NS(id)->ns
  
  div(shinycssloaders::withSpinner(reactableOutput(outputId = ns("tabella"))))
  
}


boxplotUI<-function(id){
  
  NS(id)->ns
  
  div(plotlyOutput(outputId=ns("grafico")))
  
}





mappaUI<-function(id){
  
  NS(id)->ns
  
  leaflet::leafletOutput(outputId = ns("mappa"))
  
  
}#fine mappaUI





