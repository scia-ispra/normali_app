#La pagina principale è divisa in panels
pannello<-function(titolo="",moduloUI,width=12,icon=NULL,...){
  
  tabPanel(
    title=titolo,
    fluidRow(column(width=width,moduloUI)),
    icon=icon,
    ...
  )
  
}#fine pannello


#La pagina principale è divisa in panels
pannelloGrafici<-function(titolo="",moduloUI,modulo2UI,modulo3UI,icon=NULL){
  
  tabPanel(
    title=titolo,
    icon=icon,
    fluidRow(column(width=12,moduloUI,style="margin-top: 50px;")),
    fluidRow(
      column(width=3,modulo2UI),
      column(width=8,modulo3UI,offset = 1),
      style="margin-top: 50px;"
    )
  )
  
}#fine pannello



#Fluid page
fluidPage(
  
  use_font("fira-mono","www/css/fira-mono.css"),
  theme = bslib::bs_theme(bootswatch = "minty"),
  includeCSS(path="./css/mappa.css"),
  includeCSS(path="./css/mytheme.css"),

  div(
    class="main_container",
    
    #header
    fluidRow(
      class="intestazione",
      column(
        width=12,
        div("Valori climatici normali",fa("cloud-sun",fill="#F4EFC4"),class="titolo"),
        div("Istituto Superiore per la Protezione e la Ricerca Ambientale",class="sottotitolo")
        )       
    ),
    
    #pagina principale
    fluidRow(
      class="app_content",
      column(
        width=10,
        offset = 1,
        
        fluidRow(
          class="pulsante_selezione_parametro",
          column(width=5,
                 switchParametroUI("seleziona_parametro")
          )
        ),
        
        #riga pulsanti
        fluidRow(
          class="pulsanti_selezione",
          column(selectInput(inputId = "indicatore",choices = list("Tmax","Tmax > 25 °C","Tmax >= 30 °C","Tmax >= 35 °C","Tmax >= 40 °C","Tmean","Tmin","Tmin < 0 °C","Tmin > 20 °C"),label = "Indicatore",selected = "Tmax",multiple = FALSE),width=3),
          column(radioButtonsGroupUI("bottoni_climatologici"),width=6)
        ),
        
        fluidRow(class="mostra_complete",
                 column(switchUI("dati_completi") %>% shinyInput_label_embed(icon("info-circle") %>% bs_embed_tooltip(title="Visualizza solo le serie per le quali e' stato possibile calcolare tutti i valori mensili, stagionali e annuali")),width=3)
        ),
        
        fluidRow(
          class="riga-estrai",
          column(actionButton(inputId = "elabora",label="Visualizza dati"),width=3)          
        ),
        
        #tabella e contenuto app
        fluidRow(
          class="riga_pannelli",
          column(
            width=12,
            tabsetPanel(
              id = "pannelli",
              #pannello dati con bottone  per scaricare i dati in formato .csv
              tabPanel(
                title="Introduzione",
                icon=icon("bars"),
                div(style="margin-top: 20px;",
                  includeHTML("./docs/normali.html")
                )
              ),
              
              
              pannello(
                titolo="Dati",
                icon=icon("table"),
                tagList(
                  div(
                    span("Scarica i dati visualizzati in formato testo"),
                    tags$button(
                      id="csv",
                      tagList(fontawesome::fa("download"), ".CSV"),
                      onclick = "Reactable.downloadDataCSV('tabella-dati-climatologici', 'climatologici.csv')"
                    )
                  ),
                  div(htmlOutput(outputId = "legenda",inline = FALSE)),
                  tabellaUI(id="tabella_climatologici")),
              ),

              #pannello mappa stazioni
              pannello(
                titolo = "Stazioni",
                icon=icon("location-arrow"),
                moduloUI=mappaUI("mappa_climatologici")
              ),
              
              #pannello con i grafici
              pannelloGrafici(
                titolo = "Grafici",
                icon=icon("chart-line"),
                moduloUI=boxplotUI("boxplot_climatologici"),
                modulo2UI=boxplotUI("boxplot_climatologici_annuali"),
                modulo3UI=boxplotUI("boxplot_climatologici_stagionali")
              ),
              
              tabPanel(
                title="Guida",
                icon=icon("life-ring"),
                includeHTML("./docs/help.html")
              ),
              
              tabPanel(
                title="Fonte dati",
                icon=icon("database"),
                includeHTML("./docs/fonte-dati.html")
              ),
              
              use_bs_tooltip()
              
            )#fine tabsetPanel
          )#fine column
        )#fine fluiRow
      )#fine column
    )#fine fluidRow
  ),#fine div main_container
  
  fluidRow(
    
    column(width=12,
    tags$footer(style="height: 50px;background: white;")       
    )
    
  )
  
)#fine fluidPage
  

  
  
  
  
  
  
