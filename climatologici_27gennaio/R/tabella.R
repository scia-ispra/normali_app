# Paletta dei colori
crea_paletta<-function(numero_bins=13,valori_dominio,param){
  
  DIRECTION<- -1
  
  ifelse(grepl("^pr",param,ignore.case =TRUE),"davos","romaO")->nomePaletta
  if(grepl("Tmin < 0",param,ignore.case =TRUE )){nomePaletta<-"romaO"; DIRECTION<-1}
  if(grepl("Tm.+ >.+",param,ignore.case =TRUE )){nomePaletta<-"romaO";DIRECTION<- -1}
  
  scico::scico(palette=nomePaletta,n=length(valori_dominio),direction=DIRECTION,alpha = 0.8)->colori
  scales::col_bin(colori,domain =valori_dominio,bins = valori_dominio,na.color = "#FFFFFF",alpha = TRUE,right=TRUE)
  
}#crea_paletta


# Colore del testo e dello sfondo di ciascuna cella
coloriCelle<-function(paletta,posizioneZero,valore,valori_dominio,numero_bins){
  
  paletta(valore)->coloreSfondo
  findInterval(valore,valori_dominio)->posizioneColoreSfondo
  
  ifelse(abs(posizioneColoreSfondo-posizioneZero)<=(numero_bins/3),"#000000","#FFFFFF")->coloreTesto
  
  #check                  
  if(!coloreTesto %in% c("#000000","#FFFFFF")) browser()
  
  list("coloreTesto"=coloreTesto,"coloreSfondo"=coloreSfondo)
  
}#coloriCelle



# Definizione generica delle colonne
stileGenerico<-function(.paletta,.dominio){
  
  function(value){
    

    if(is.na(value) || is.null(value)) return(list(backgroundColor="white",color="#000000"))
    
    #colore centrale della paletta
    .paletta(0)->coloreCentrale
    #paletta dei colori
    .paletta(.dominio)->tutti_colori
    #posizione del colore centrale all'interno della paletta
    mean(grep(coloreCentrale,tutti_colori))->posizioneZero
    
    coloriCelle(paletta=.paletta,posizioneZero=posizioneZero,valore=round(value,1),valori_dominio=.dominio,numero_bins = length(.dominio))->listaColori
    
    list(backgroundColor=listaColori$coloreSfondo,color=listaColori$coloreTesto,fontSize="12px",transition="background 1s ease")    
    
  }
  
}#stileGenerico


#tabellaServer
tabellaServer<-function(id,.x,.stile,.unita_misura,.trentennio,.nomeIndicatore){
  
  moduleServer(id,function(input,output,session){
    
    output$tabella<-renderReactable({
      
      
      #intestazioneTabella<-isolate(paste0("`",str_to_title(.nomeIndicatore()),"` - Trentennio climatologico: ",.trentennio()))
      intestazioneTabella<-isolate(paste0("Trentennio climatologico: ",.trentennio()))
      str_replace(intestazioneTabella,"Mm","mm")->titoloGrafico
 
      #tabella
      reactable(.x(),
                elementId = "tabella-dati-climatologici",
                selection="multiple",
                onClick="select",
                searchable=TRUE,
                highlight = TRUE,
                compact = TRUE,
                style =list(fontSize="12px",fontFamily="Fira Mono"),
                language=reactableLang(searchPlaceholder = "Cerca ...",noData = "Nessun risultato"),
                defaultPageSize = 25,
                defaultSorted=c("regione2"),
                defaultColDef =colDef(align="center",format = colFormat(digits=1),maxWidth = 100,width=71,na="-",style=.stile(),headerStyle = list(fontSize="10px",background="#FFFFFF")), #
                rowStyle = list(cursor="pointer"),
                columnGroups = list(colGroup(name=intestazioneTabella,align = "left",headerStyle = list(fontSize=12),columns = c(month.name,season.name))),
                columns = list(
                   Longitude=colDef(show=FALSE,style=function(value){list()}),
                   Latitude=colDef(show=FALSE,style=function(value){list()}),
                   regione2=colDef(show=TRUE,name = "Regione",width=120,style=function(value){list(fontWeight=500)}),
                   Elevation=colDef(show=TRUE,name="Quota",style=function(value){fontSize="10px"}),
                   SiteName=colDef(show=TRUE,name = "Stazione",width=120,style=function(value){fontSize="10px"}),
                   Annuale=colDef(show=TRUE,style=function(value){list(fontWeight=900,background="rgb(240, 233, 233, 0.34)",borderLeft = "1px solid #333")}),
                   Inverno=colDef(show=TRUE,style=function(value){list(fontWeight=900,background="rgb(240, 233, 233, 0.34)",borderLeft = "1px solid #333")}),
                   Primavera=colDef(show=TRUE,style=function(value){list(fontWeight=900,background="rgb(240, 233, 233, 0.34)",borderLeft = "1px solid #333")}),
                   Estate=colDef(show=TRUE,style=function(value){list(fontWeight=900,background="rgb(240, 233, 233, 0.34)",borderLeft = "1px solid #333")}),
                   Autunno=colDef(show=TRUE,style=function(value){list(fontWeight=900,background="rgb(240, 233, 233, 0.34)",borderLeft = "1px solid #333")})
                   
                   
                )) 

      
      
      
    })#fine renderReactable
  
    
  })
  
}