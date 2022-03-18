leggi_ana<-function(nomeIndicatore){
  

            if(grepl("prec",nomeIndicatore,ignore.case = TRUE)){
              PARAM<-"prec"
            }else if(grepl("tmax",nomeIndicatore,ignore.case = TRUE)){
              PARAM<-"tmax"
            }else if(grepl("tmin",nomeIndicatore,ignore.case = TRUE)){
              PARAM<-"tmin"
            }else if(grepl("tmean",nomeIndicatore,ignore.case = TRUE)){
              PARAM<-"tmean"
            }else if(grepl("^Diurnal",nomeIndicatore,ignore.case = TRUE)){
              PARAM<-"tmean"
            }else if(grepl("^Cooling",nomeIndicatore,ignore.case = TRUE)){
              PARAM<-"tmean"
            }
            
            vroom(glue::glue("./data/fixed_anagrafica.{PARAM}.csv"),delim=";",col_names = TRUE) %>% 
                     setDT()
          

}#leggi_ana



leggi_dati<-function(nomeIndicatore){
    
          PARAM<-nomeIndicatore
          PARAM_OUT<-str_trim(str_remove(PARAM,"\\(..\\)"),side="both")

          if(grepl("Prec >= 1 mm",PARAM,ignore.case = TRUE)){PARAM_OUT<-"prcp_gt1"}
          if(grepl("Prec >= 5 mm",PARAM,ignore.case = TRUE)){PARAM_OUT<-"prcp_gt5"}
          if(grepl("Prec >= 10 mm",PARAM,ignore.case = TRUE)){PARAM_OUT<-"prcp_gt10"}
          if(grepl("Prec >= 20 mm",PARAM,ignore.case = TRUE)){PARAM_OUT<-"prcp_gt20"}
          if(grepl("Prec >= 100 mm",PARAM,ignore.case = TRUE)){PARAM_OUT<-"prcp_gt100"}
          
          
          if(grepl("Tmin > 20 °C",PARAM,ignore.case = TRUE)){PARAM_OUT<-"tmin_gt20"}
          if(grepl("Tmin < 0 °C",PARAM,ignore.case = TRUE)){PARAM_OUT<-"tmin_lt0"}
          
          if(grepl("Tmax > 25 °C",PARAM,ignore.case = TRUE)){PARAM_OUT<-"tmax_gt25"}
          if(grepl("Tmax >= 30 °C",PARAM,ignore.case = TRUE)){PARAM_OUT<-"tmax_gt30"}
          if(grepl("Tmax >= 35 °C",PARAM,ignore.case = TRUE)){PARAM_OUT<-"tmax_gt35"}
          if(grepl("Tmax >= 40 °C",PARAM,ignore.case = TRUE)){PARAM_OUT<-"tmax_gt40"}
          
          if(grepl("Diurnal",PARAM,ignore.case = TRUE)){PARAM_OUT<-"tmean_dtr"}
          if(grepl("Cooling",PARAM,ignore.case = TRUE)){PARAM_OUT<-"tmean_cdd"}
          
          cols(.default = col_number(),id=col_character(),yy=col_character())->colonne

          glue::glue("./data/climatologici_mensili_{PARAM_OUT}_homog.csv")->nomeFile

          vroom(nomeFile,delim = ";",col_names = TRUE,col_types =colonne ) %>%  
            rename(Gennaio=January,Febbraio=February,Marzo=March,Aprile=April,Maggio=May,Giugno=June,Luglio=July,Agosto=August,Settembre=September,Ottobre=October,Novembre=November,Dicembre=December) %>%
            setDT()->datiMensili

          glue::glue("./data/climatologici_annuali_{PARAM_OUT}_homog.csv")->nomeFileAnnuale
          vroom(nomeFileAnnuale,delim = ";",col_names = TRUE,col_types =colonne ) %>%  setDT()->datiAnnuali

          glue::glue("./data/climatologici_stagionali_{PARAM_OUT}_homog.csv")->nomeFileStagionale
          vroom(nomeFileStagionale,delim = ";",col_names = TRUE,col_types =colonne ) %>%  
            rename(Inverno=win,Primavera=spr,Estate=sum,Autunno=aut) %>%
            setDT()->datiStagionali
          
          melt(datiAnnuali,id.vars = "id",variable.name = "yy",value.name = "Annuale")->mdatiAnnuali
          merge(datiMensili,mdatiAnnuali,by = c("id","yy"),all.x = TRUE)->datiSenzaStagione
          merge(datiSenzaStagione,datiStagionali,by=c("id","yy"),all.x=TRUE)->dati

          
          dati[yy==1975,yy:=.("1961-1990")]
          dati[yy==1985,yy:=.("1971-2000")]
          dati[yy==1995,yy:=.("1981-2010")]  
          dati[yy==2005,yy:=.("1991-2020")]  
          
          #codici delle stazioni dell'aeronautica in Sardegna passate da Michele Fiori..vanno eliminate per non avere doppioni
          dati[!id %in% c("CA035A003","CA050A398","CA059A005","CA082A008","CA089A009","CA090A006","NU048A002","NU068A007","NU097A004","SS002A001","SS014A395","SS059A001","SS059A392"),]


}

merge_ana_dati<-function(dati,ana){
  
  merge(dati,ana[,c("id","regione2","SiteName","Elevation","Longitude","Latitude")],by=c("id"))
  
}



server<-function(input,output,session){
  
  switchParametroServer("seleziona_parametro")->choices
  
  observe({
    
    updateSelectInput(inputId = "indicatore",choices =choices())
    
  })
  
  reactive({tolower(input$indicatore)})->nomeIndicatore
  radioButtonsGroupServer("bottoni_climatologici")->trentennio
  switchServer("dati_completi")->annuale_completo

  eventReactive(input$elabora,{
    
    leggi_ana(isolate(nomeIndicatore()))->ana
    leggi_dati(isolate(nomeIndicatore()))->dati
    merge_ana_dati(dati,ana)->mdati
    mdati[yy==trentennio() ]->out
   
    out$Annuale2<-out$Annuale
 
    #questo serve per colorare i pallini nella mappa, riducendo il range di valori
    if(grepl("(mm)",nomeIndicatore(),ignore.case = TRUE)){
      out[Annuale2>2000,]$Annuale2<-2000
    }else if(grepl("giorni",nomeIndicatore(),ignore.case = TRUE)){
      out[Annuale2>180,]$Annuale2<-180
    }else if(grepl("cooling",nomeIndicatore(),ignore.case = TRUE)){
      out[Annuale2>400,]$Annuale2<-400
    }
    
    if(annuale_completo()){
      return(out[!is.na(Annuale)])
    }else{
      return(out)
    }
    
  })->selectedData
  
  
  
  
  reactive({
    
    crosstalk::SharedData$new(selectedData()[,.SD,.SDcols=c("regione2","SiteName","Elevation","Annuale",month.name,season.name,"Longitude","Latitude")],group = "dati")
    
  })->cross_selectedData  
  
  reactive({
    
    crosstalk::SharedData$new(selectedData()[,.SD,.SDcols=c("regione2","SiteName","Elevation","Annuale","Annuale2","Longitude","Latitude")],group="dati")
    
  })->cross_selectedData2
  

  eventReactive(input$elabora,{

    
    apply(isolate(cross_selectedData()$data()[,purrr::map(.SD,.f=min,na.rm=TRUE),.SDcols=month.name]),MARGIN = 1,min)->minimo
    apply(isolate(cross_selectedData()$data()[,purrr::map(.SD,.f=max,na.rm=TRUE),.SDcols=month.name]),MARGIN = 1,max)->massimo
    
    max(abs(massimo),abs(minimo))->estremo

    RoundTo(estremo,multiple = 5,FUN=ceiling)->estremo

    

    if(grepl("mm",unita_misura())){
      MULTIPLO<-50
    }else{
      MULTIPLO<-5
    }
    
    if(estremo==5) {estremo<-20}
    
    if(grepl("diurnal",nomeIndicatore(),ignore.case = TRUE)){
      RoundTo(estremo,multiple = 2,FUN=ceiling)->estremo
      MULTIPLO<-2
    }
    
    seq(-estremo,estremo,by=MULTIPLO)
    
  })->dominio
  
  eventReactive(input$elabora,{
    
    crea_paletta(numero_bins = length(dominio()),valori_dominio =dominio(),param=isolate(nomeIndicatore()))

  })->paletta
  
  eventReactive(input$elabora,{
    
    stileGenerico(paletta(),dominio())

  })->stile
  
  
  reactive({
    
    cross_selectedData()$data(withSelection=TRUE)->xx
    
    if(nrow(xx[selected_==TRUE,])!=0 ){
      melt(xx[selected_==TRUE,.SD,.SDcols=c(month.name,"SiteName")],measure.vars=month.name,variable.name="mese",value.name = "climatologico")[,climatologico:=.(round(climatologico,1))]->valori_mensili
      xx[selected_==TRUE,.SD,.SDcols=c("Annuale","SiteName")][,anno:="anno"]->valori_annuali
      melt(xx[selected_==TRUE,.SD,.SDcols=c(season.name,"SiteName")],measure.vars=season.name,variable.name="stagione",value.name = "climatologico")[,climatologico:=.(round(climatologico,1))]->valori_stagionali
    }else{
      melt(xx[,.SD,.SDcols=c(month.name,"SiteName")],measure.vars=month.name,variable.name="mese",value.name = "climatologico")[,climatologico:=.(round(climatologico,1))]->valori_mensili
      xx[,.SD,.SDcols=c("Annuale","SiteName")][,anno:="anno"]->valori_annuali
      melt(xx[,.SD,.SDcols=c(season.name,"SiteName")],measure.vars=season.name,variable.name="stagione",value.name = "climatologico")[,climatologico:=.(round(climatologico,1))]->valori_stagionali
    }

    list(mensili=valori_mensili,annuali=valori_annuali,stagionali=valori_stagionali)
    
  })->cross_selectedMeltedData
  
  eventReactive(input$elabora,{
    
    trovaUnitaMisura(isolate(nomeIndicatore()))
    
  })->unita_misura
  
  
  output$legenda<-renderUI({
    
    dominio()->valori
    paletta()(dominio())->colori
    
    if(!grepl("°C",unita_misura(),ignore.case = TRUE)){
      
      which(valori<0)->quali
      if(length(quali)){
        valori[-quali]->valori
        colori[-quali]->colori
      }
    }
    

    which(valori<=-15)->quali
    if(length(quali)){
      valori[-quali]->valori
      colori[-quali]->colori
    }
    
    if(grepl("diurnal",nomeIndicatore(),ignore.case = TRUE)){
      
      which(valori<=-2)->quali
      if(length(quali)){
        valori[-quali]->valori
        colori[-quali]->colori
      }
      
      
    }
    
    ifelse(length(valori)==2,"150px","300px")->width
    
    isolate(nomeIndicatore())->titoloColorbar

    str_remove(titoloColorbar,"[Gg]iorni con")->titoloColorbar
    str_to_sentence(titoloColorbar)->titoloColorbar
    str_replace(titoloColorbar,"°c","°C")->titoloColorbar
    if(grepl("[><]",titoloColorbar)){paste0(titoloColorbar," (giorni)")->titoloColorbar}
    str_trim(titoloColorbar,side = "both")->titoloColorbar

    tagList(
      div(titoloColorbar,style="font-weight: 900 !important; margin-bottom: 8px; color: #333"),
      div(
        style=glue::glue("display: flex; flex-direction: row; min-width: {width};max-width: 500px;align-items: baseline;"),
        tagList(purrr::map(1:length(valori),.f=function(indice){
  
          valori[indice]->valore
          colori[indice]->colore
          
          if(valore<0 && indice==1){paste0("<= ",valore)->valore}
          if(valore>0 && indice==length(valori)){paste0("<= ",valore)->valore}
  
          div(valore,style=paste0("color: white;font-weight: bold; font-size: 16px; text-align: center; ;flex-grow: 1;background:",colore))
          
        }))
      )

    )
    
    
  })


  mappaServer(id="mappa_climatologici",.x=cross_selectedData2,.unita_misura=unita_misura,.nomeIndicatore=nomeIndicatore)
  tabellaServer(id="tabella_climatologici",.x=cross_selectedData,.stile=stile,.unita_misura=unita_misura,.trentennio=trentennio,.nomeIndicatore=nomeIndicatore)
  boxplotServer("boxplot_climatologici",.x=cross_selectedMeltedData,.unita_misura=unita_misura,.trentennio=trentennio,.nomeIndicatore=nomeIndicatore)
  boxplotAnnualeServer("boxplot_climatologici_annuali",.x=cross_selectedMeltedData,.unita_misura=unita_misura,.trentennio=trentennio,.nomeIndicatore=nomeIndicatore)
  boxplotStagionaliServer("boxplot_climatologici_stagionali",.x=cross_selectedMeltedData,.unita_misura=unita_misura,.trentennio=trentennio,.nomeIndicatore=nomeIndicatore)
  
}#fine server function