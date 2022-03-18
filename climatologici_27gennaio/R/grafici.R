trovaUnitaMisura<-function(.indicatore){
  
  if(grepl("^p.+",.indicatore,ignore.case = TRUE)){
    unita_misura<-"mm"
  }else{
    unita_misura<-"°C"
  }
  
  if(grepl("^.+[><].+",.indicatore,ignore.case = TRUE)) unita_misura<-"Giorni"
  
  return(unita_misura)
  
}

boxplotServer<-function(id,.x,.unita_misura,.trentennio,.nomeIndicatore){
  
  moduleServer(id,function(input,output,session){
    
    output$grafico<-renderPlotly({
      
      titoloGrafico<-isolate(paste0("Distribuzione mensile `",str_to_title(.nomeIndicatore()),"`\nTrentennio:",.trentennio()))
      str_replace(titoloGrafico,"Mm","mm")->titoloGrafico
      
      plot_ly(data=.x()[["mensili"]],y=~climatologico,x = ~mese,type="box",fillcolor="#c5d2d6",line=list(color="#7295c0"),marker=list(color="#7295c0")) %>%
        layout(title=list(text=titoloGrafico,
                          x=0,
                          y=0.95,
                          font=list(size=12)),
               plot_bgcolor="#FFFFFF",
               xaxis=list(title=""),
               yaxis=list(title=paste0("",.unita_misura(),"")))
      
    })
    
    
  })
  
  
}#fine graficoServer


boxplotAnnualeServer<-function(id,.x,.unita_misura,.trentennio,.nomeIndicatore){
  
  moduleServer(id,function(input,output,session){
    
    output$grafico<-renderPlotly({
      
      titoloGrafico<-isolate(paste0("Distribuzione annuale `",str_to_title(.nomeIndicatore()),"`\nTrentennio:",.trentennio()))
      str_replace(titoloGrafico,"Mm","mm")->titoloGrafico
      
      
      plot_ly(data=.x()[["annuali"]],y=~Annuale,type="box",name=" ",fillcolor="#c5d2d6",line=list(color="#7295c0"),marker=list(color="#7295c0")) %>%
        layout(title=list(text=titoloGrafico,
                          x=0,
                          y=0.95,
                          font=list(size=12)),
               plot_bgcolor="#FFFFFF",
               xaxis=list(title=""),
               yaxis=list(title=paste0("",.unita_misura(),"")))
      
    })
    
    
  })
  
  
}#fine graficoServer

boxplotStagionaliServer<-function(id,.x,.unita_misura,.trentennio,.nomeIndicatore){
  
  moduleServer(id,function(input,output,session){
    
    output$grafico<-renderPlotly({
      
      titoloGrafico<-isolate(paste0("Distribuzione stagionale `",str_to_title(.nomeIndicatore()),"`\nTrentennio:",.trentennio()))
      str_replace(titoloGrafico,"Mm","mm")->titoloGrafico
      
      
      plot_ly(data=.x()[["stagionali"]],y=~climatologico,x=~stagione,type="box",fillcolor="#c5d2d6",line=list(color="#7295c0"),marker=list(color="#7295c0")) %>%
        layout(title=list(text=titoloGrafico,
                          x=0,
                          y=0.95,
                          font=list(size=12)),
               plot_bgcolor="#FFFFFF",
               xaxis=list(title=""),
               yaxis=list(title=paste0("",.unita_misura(),"")))
      
    })
    
    
  })
  
  
}#fine graficoServer