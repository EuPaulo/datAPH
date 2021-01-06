library(shiny)
library(leaflet)
library(leaflet.extras)#addHeatmap
library(plotly)
library(shinyjs)
#library(auth0)

#auth0_server(
function(input, output, session) {
  
  
  #Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -47.3, lat = -15.7, zoom = 10)
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  onevent("click", "nav", observe({
    #observe({ #Linha de cima é para o tabPanel do mapa ir para depois do panorama
    pal <- colorFactor(
      palette = c(corAtivo, corMorto, corRecuperado, corSuspeito),
      domain = dadosMapa$situacao
    )
    
    #Verifica se é mapa de Pontos
    if(!is.null(input$radio))
      if (input$radio == '1'){
        leafletProxy("map", data = dadosMapa[dadosMapa$Data ==input$slider,]) %>%
          clearShapes() %>%
          clearMarkerClusters() %>%
          #clearHeatmap() %>%
          clearWebGLHeatmap() %>%
          clearControls() %>%
          addCircleMarkers(~longitude, ~latitude, radius=1,
            fillOpacity=0.4, color = ~pal(situacao),  clusterOptions = markerClusterOptions())#%>%
        # addCircles(~longitude, ~latitude, radius= ~casos,
        #            fillOpacity=0.4, color = ~pal(situacao), layerId = ~id)%>%
        #addLegend("topleft", pal=pal, values=levels(vitimas$situacao))
        
        
        #Verifica se é mapa de calor
      }else if (input$radio == '2'){
        
        # ## Note, bandwidth choice is based on MASS::bandwidth.nrd()
        # library(rgdal)
        # library(KernSmooth)
        # kde <- bkde2D(dadosMapa[ , c('longitude', 'latitude')],
        #   bandwidth=c(.0145, .0168), gridsize = c(1000,1000))
        # CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)
        # 
        # ## EXTRACT CONTOUR LINE LEVELS
        # LEVS <- as.factor(sapply(CL, `[[`, "level"))
        # NLEV <- length(levels(LEVS))
        # 
        # ## CONVERT CONTOUR LINES TO POLYGONS
        # pgons <- lapply(1:length(CL), function(i)
        #   Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
        # spgons = SpatialPolygons(pgons)
        # 
        # ## Leaflet map with polygons
        # leaflet(spgons) %>% addTiles() %>% 
        #   addPolygons(color = heat.colors(NLEV, NULL)[LEVS])
        
        leafletProxy("map", data =  dadosMapa[dadosMapa$Data ==input$slider,]) %>%
          clearShapes() %>%
          #clearHeatmap() %>%
          clearWebGLHeatmap() %>%
          clearControls() %>%
          clearMarkerClusters() %>%
          addWebGLHeatmap(lng = ~longitude, lat =  ~latitude, size = 50,units = 'px', opacity = 0.5, alphaRange = 0.1)#, blur =  30, radius = 15, max= 0.8)
        #addHeatmap(lng = ~longitude, lat =  ~latitude, blur =  30, radius = 15, max= 0.8)
      }
  })
  )
  
 
  
  output$boxAtivos <- renderValueBox({
    valueBox(
      dadosDF$`Casos confirmados`[nrow(dadosDF)], "Casos confirmados", icon = icon("fas fa-user-md"),
      color = corAtivo
    )
  })
  
  output$boxSuspeitos <- renderValueBox({
    valueBox(
      dadosDF$`Casos suspeitos`[nrow(dadosDF)], "Casos suspeitos", icon = icon("fas fa-ambulance"),
      color = corSuspeito
    )
  })
  output$boxRecuperados <- renderValueBox({
    valueBox(
      '?', "Recuperados", icon = icon("thumbs-up", lib = "glyphicon"),
      color = corRecuperado
    )
  })
  output$boxMortos <- renderValueBox({
    valueBox(
      dadosDF$Óbitos[nrow(dadosDF)], "Óbitos",
      color = corMorto
    )
  })
  
  output$graficoBarrasRA <- renderPlotly(
    plot_ly(data = dadosRA,
      x = reorder(dadosRA$RA, -dadosRA$casos),
      y = ~ casos,
      type = "bar"#,
      #marker = list(color = corAtivo)
    )%>%
      
      plotly::layout(
        yaxis = list(title = "Número de casos")
      )
    
  )
  
  b = data.frame("Qtd" = (as.data.frame(t(dadosDF[30,11:20]))$V1), "Idades" = colnames(dadosDF[,11:20]))
  b$Idades <- factor(b$Idades, levels = c(rev(as.character(b$Idades))))
  output$graficoFEDF <- renderPlotly(
    plot_ly(data = b,
      x = ~ Qtd,
      y = ~ Idades,
      text = ~Qtd, 
      textposition = "auto", 
      insidetextfont = list(size=15, color = '#FFFFFF'),
      name = "Confirmados",
      type = 'bar'
    ) %>%
      plotly::layout(
        yaxis = list(title = "Faixa Etária"),
        xaxis = list(title = "Casos")
      )
  )
  
  output$boxUTI <- renderValueBox({
    valueBox(
      dadosDF$UTI[nrow(dadosDF)], "UTI", icon = icon("fas fa-user-md"),
      color = "red"
    )
  })
  output$boxEnf <- renderValueBox({
    valueBox(
      dadosDF$Enfermaria[nrow(dadosDF)], "Enfermaria", icon = icon("fas fa-user-md"),
      color = "orange"
    )
  })
  
  output$graficoAcumuladoDF <- renderPlotly(
    plot_ly(data = dadosDF,
        x = ~ Data,
        y = ~ porDia,
        type = "bar",
        name = 'Casos por dia',
        marker = list(color = corAtivo, opacity = 0.3)
    )%>%
      
      add_trace(
      x = ~ Data,
      y = dadosDF$`Casos confirmados`,
      type = "scatter",
      mode = 'lines+markers',
      name = "Acumulado",
        marker = list(color = '#428bca', opacity = 1),
        line = list(color='#428bca')
      )%>%
      
      plotly::layout(
        yaxis = list(title = "Casos confirmados"),
        xaxis = list(title = "Data"),
        #title = 'Casos confirmados',
        legend = list(x=0.1, y =0.9 )
      )
  )
  
  dadosBrOrdenados <- dadosBrasil[order(dadosBrasil$Casos, na.last = TRUE, decreasing = TRUE), ]
  dadosBrOrdenados$Estado <- factor(dadosBrOrdenados$Estado, levels = c(as.character(dadosBrOrdenados$Estado)))
  output$graficoBarrasBR <- renderPlotly(
    plot_ly(data = dadosBrOrdenados,
      x = ~ Estado,
      y = ~ Casos,
      name = "Casos confirmados",
      type = 'bar'#,
      #marker = list(color = corAtivo)
    ) %>%
      
      plotly::add_trace(y = ~ Obitos,
        name = "Óbitos",
        marker = list(color = corMorto)) %>%
      
      plotly::layout(barmode = 'stack',
        yaxis = list(title = "Total de casos"),
        xaxis = list(title = "Estado"),
        hovermode = "compare",
        legend = list(x=0.1, y =0.9 )
      )
  )
  
  
  a = data.frame(dadosDF[,21:51])
  a[a<minTemporal]=NA
  value = integer(0)
  teste <- NULL
  myList <- list()
  for (i in names(a)) {
    substituir <- max(which(is.na(a[,i])))
    linhaTransicao <-c(a[,i], rep(NA, substituir))
    a[,i] <- linhaTransicao[-c(1:substituir)]
    if(!identical(value, which(a[,i]>minTemporal))) {
      myList[[length(myList)+1]] <- list(x = max(which(a[,i]>minTemporal))+0.1, y = a[max(which(a[,i]>minTemporal)),i], text = i, showarrow = FALSE, xanchor = "left", yanchor = "middle")
    }
  }
  
  output$graficoTemporal <- renderPlotly(
    p <- plotly::plot_ly(
      x = row.names(a),
      y = a[,1],
      type = 'scatter',
      name = colnames(a[1]),
      mode = 'lines+markers'
    ) %>%
      plotly::add_trace(y = a[,2],
        name = colnames(a[2]))
    %>%
      plotly::add_trace(y = a[,3],
        name = colnames(a[3]))
    %>%
      plotly::add_trace(y = a[,4],
        name = colnames(a[4]))
    %>%
      plotly::add_trace(y = a[,5],
        name = colnames(a[5]))
    %>%
      plotly::add_trace(y = a[,6],
        name = colnames(a[6]))
    %>%
      plotly::add_trace(y = a[,7],
        name = colnames(a[7]))
    %>%
      plotly::add_trace(y = a[,8],
        name = colnames(a[8]))
    %>%
      plotly::add_trace(y = a[,9],
        name = colnames(a[9]))
    %>%
      plotly::add_trace(y = a[,10],
        name = colnames(a[10]))
    %>%
      plotly::add_trace(y = a[,11],
        name = colnames(a[11]))
    %>%
      plotly::add_trace(y = a[,12],
        name = colnames(a[12]))
    %>%
      plotly::add_trace(y = a[,13],
        name = colnames(a[13]))
    %>%
      plotly::add_trace(y = a[,14],
        name = colnames(a[14]))
    %>%
      plotly::add_trace(y = a[,15],
        name = colnames(a[15]))
    %>%
      plotly::add_trace(y = a[,16],
        name = colnames(a[16]))
    %>%
      plotly::add_trace(y = a[,17],
        name = colnames(a[17]))
    %>%
      plotly::add_trace(y = a[,18],
        name = colnames(a[18]))
    %>%
      plotly::add_trace(y = a[,19],
        name = colnames(a[19]))
    %>%
      plotly::add_trace(y = a[,20],
        name = colnames(a[20]))
    %>%
      plotly::add_trace(y = a[,21],
        name = colnames(a[21]))
    %>%
      plotly::add_trace(y = a[,22],
        name = colnames(a[22]))
    %>%
      plotly::add_trace(y = a[,23],
        name = colnames(a[23]))
    %>%
      plotly::add_trace(y = a[,24],
        name = colnames(a[24]))
    %>%
      plotly::add_trace(y = a[,25],
        name = colnames(a[25]))
    %>%
      plotly::add_trace(y = a[,26],
        name = colnames(a[26]))
    %>%
      plotly::add_trace(y = a[,27],
        name = colnames(a[27]))
    %>%
      plotly::add_trace(y = a[,28],
        name = colnames(a[28]))
    %>%
      plotly::add_trace(y = a[,29],
        name = colnames(a[29]))
    %>%
      plotly::add_trace(y = a[,30],
        name = colnames(a[30]))
    %>%
      plotly::add_trace(y = a[,31],
        name = colnames(a[31]))
    %>%
      plotly::layout(#annotations = myList, 
        showlegend = FALSE,  
        #font = list(family = 'sans serif', size = 16),
        xaxis = list(title = paste0('Dias a partir da confirmação do ', minTemporal, 'º caso.')),
        yaxis = list(title = 'Casos confirmados'))
  )
  
  
}
#  )