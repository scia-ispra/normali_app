mappaServer<-function(id,.x,.unita_misura,.nomeIndicatore){
  
  
  moduleServer(id,function(input,output,session){
    
    output$mappa<-renderLeaflet({
      
      .x()$data()->tabella      
      grep("Annuale",names(tabella))->colonna

      # min(tabella$Annuale,na.rm=TRUE)->minimo
      # max(tabella$Annuale,na.rm=TRUE)->massimo
      # max(abs(minimo),abs(massimo))->estremo
      # round(estremo)+1->estremo
      # dominio<-c(-estremo,estremo)
      # seq(-estremo,estremo,by=2)->valori_dominio
      
      DIRECTION<- -1
      
      ifelse(grepl("^pr",.nomeIndicatore(),ignore.case =TRUE),"davos","romaO")->nomePaletta
      if(grepl("Tmin < 0",.nomeIndicatore(),ignore.case =TRUE )){nomePaletta<-"romaO"; DIRECTION<-1}
      if(grepl("Tm.+ >.+",.nomeIndicatore(),ignore.case =TRUE )){nomePaletta<-"romaO";DIRECTION<- -1}
      
      
      if(grepl("Tmax.+°C",.nomeIndicatore(),ignore.case = TRUE)){dominio<-c(0,32); seq(0,32,by=4)->valori_dominio;etichette<-cut(seq(0.01,32,by=4),breaks =seq(0,32,by=4),dig.lab = 0)}
      if(grepl("Tmean.+°C",.nomeIndicatore(),ignore.case = TRUE)){dominio<-c(0,20); seq(0,20,by=2.5)->valori_dominio;etichette<-cut(seq(0.01,20,by=2.5),breaks =seq(0,20,by=2.5),dig.lab = 0)}
      if(grepl("Tmin.+°C",.nomeIndicatore(),ignore.case = TRUE)){dominio<-c(-4,20); seq(-4,20,by=4)->valori_dominio;etichette<-cut(seq(-3.9,20,by=4),breaks =seq(-4,20,by=4),dig.lab = 0)}
      if(grepl("pr.+mm",.nomeIndicatore(),ignore.case = TRUE)){dominio<-c(400,2000); seq(400,2000,by=200)->valori_dominio;etichette<-c("(400 - 600]","(600 - 800]","(800 - 1000]","(1000 - 1200]","(1200 - 1400]","(1400 - 1600]","(1600 - 1800]","(1800 - 2000]",">2000")}
      if(grepl("giorni",.nomeIndicatore(),ignore.case = TRUE)){dominio<-c(0,180); seq(0,180,by=30)->valori_dominio;etichette<-cut(seq(0.01,180,by=30),breaks =seq(0,180,by=30),dig.lab = 4)}
      if(grepl("diurnal",.nomeIndicatore(),ignore.case = TRUE)){dominio<-c(0,14); seq(0,14,by=2)->valori_dominio;etichette<-cut(seq(0.01,14,by=2),breaks =seq(0,14,by=2),dig.lab = 4)}
      if(grepl("cooling",.nomeIndicatore(),ignore.case = TRUE)){dominio<-c(0,400); seq(0,400,by=50)->valori_dominio;etichette<-as.character(cut(seq(0.01,400,by=50),breaks =seq(0,400,by=50),dig.lab = 4));etichette[length(etichette)]<-">350"}
      
      
      scico::scico(palette=nomePaletta,n=length(etichette),direction=DIRECTION,alpha = 1,begin = 0,end=0.9)->colori
      scales::col_bin(colori,domain =valori_dominio,bins = valori_dominio,na.color = "#000",alpha = TRUE,right=TRUE)->paletta

      titoloLegenda<-.nomeIndicatore()
      if(grepl("giorni",titoloLegenda,ignore.case = TRUE)) {
        str_replace(titoloLegenda,"[gG]iorni con","")->titoloLegenda  
        str_replace(titoloLegenda,"$"," (giorni)")->titoloLegenda  
      } 
      str_replace(titoloLegenda,"°c","°C")->titoloLegenda
      titoloLegenda<-str_to_sentence(str_to_lower(titoloLegenda))
      
      
      leaflet(data=.x()) %>%
        leaflet::setView(lng=12.5,lat=40,zoom=6) %>%
        leaflet::addTiles(group = "OpenStreetMap") %>%
        addProviderTiles(provider = "Stamen.Toner",group = "Bianco & nero") %>%
        addCircleMarkers(data = .x(),
                         lng=~Longitude,
                         lat=~Latitude,
                         radius = 3.5,
                         popup = ~paste0("<strong>Stazione</strong>: ",.x()$data()$SiteName,"<br>","<strong>Climatologico annuale</strong>: ",round(.x()$data()$Annuale,1),paste0(" ",.unita_misura()),"<br>","<strong>Quota</strong>: ",.x()$data()$Elevation, " metri","<br>"),
                         fillColor = ~paletta(Annuale2),
                         weight = 0,
                         fillOpacity = 1) %>%
        addLayersControl(baseGroups=c("OpenStreetMap"),overlayGroups=c("Bianco & nero"),position="topright",options = layersControlOptions(collapsed=FALSE)) %>%
        addLegend(values = valori_dominio,title = titoloLegenda,opacity = 1,labels =etichette,colors = colori,na.label = "NA") %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="Zoom to Level 1",
          onClick=JS("function(btn, map){ map.setView([40,12.5],6); }")))
      
    })
    
  })#fine moduleServer
  
  
  
  
}#fine mappaServer