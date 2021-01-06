library(leaflet)
library(shinyWidgets)
library(shinydashboard)
library(plotly)
library(shiny)
library(shinyjs)
library(auth0)
    
#------------------ Variáveis dropdown mapa ------------------
varContagio <- c(
  "Todas",
  "Externo",
  "Local",
  "Comunitario"
)

varInternado <- c(
  "Todos",
  "Sim",
  "Nao"
)


tituloPagina <- 'Covid-19'
#auth0_ui(
navbarPage(windowTitle = tituloPagina, id="nav", 
  title =
    div(
      img(
        src = 'brasaoCbmdf100.png',
        height = 30,
        width = 25,
        style = 'padding-bottom:5px;'
      ),
      tituloPagina,
    ),
  position = "static-top", 
  header = tagList(
    useShinydashboard(),
    useShinyjs() #Essencial para a tab do mapa não precisar ficar em primeiro - função onevent() no server
  ),
  tabPanel("Panorama",
    # tags$hr(style="border-color: red;
    #      height: 5px;"),
    fluidRow(
      box(width = 12,status = 'primary', collapsible = TRUE,
        valueBoxOutput('boxAtivos', width = 3),
        valueBoxOutput('boxEnf', width = 3),
        valueBoxOutput('boxUTI', width = 3),
        valueBoxOutput('boxMortos', width = 3)
      )
    ),        
    
    box(title = "Casos no DF por Região Administrativa", width = 12, status = 'primary', collapsible = TRUE,
      plotlyOutput('graficoBarrasRA')
    ),
    
    fluidRow(
      box(title = 'Casos confirmados no DF', width = 12, status = 'primary', collapsible = TRUE,
        plotlyOutput('graficoAcumuladoDF')
      )
      
      # box(
      #   title = 'Casos hospitalizados',width = 6,status = 'primary', collapsible = TRUE,
      #   valueBoxOutput('boxUTI', width = 12),
      #   valueBoxOutput('boxEnf', width = 12)  
      #   
      # )
    ),
    
    fluidRow(
      box(title = 'Casos por faixa etária',width = 6,status = 'primary', collapsible = TRUE,
        plotlyOutput('graficoFEDF')),
      box(title = 'Brasil', width = 6, status = 'primary', collapsible = TRUE, #solidHeader = TRUE,
        plotlyOutput('graficoBarrasBR')
      )
    ),
    # fluidRow(
    #   column(width = 6, plotlyOutput('graficoAcumuladoDF')),
    #   column(width = 6, plotlyOutput('graficoBarrasBR'))
    # ),
    box(title = "Evolução temporal por Região Administrativa", width = 12, status = 'primary', collapsible = TRUE,
      plotlyOutput('graficoTemporal')
    ),
    
    tags$hr(style="border-color: white;
             height: 20px;")
  ), 
  
  tabPanel("Mapa",
    div(class="outer",
      
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),
      
      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = F,
        draggable = TRUE, top = 20, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        
        div(img(src="./brasaoCbmdf100.png"), align = 'center'),
        h2("Covid-19 CBMDF"),
        
        tags$hr(style="border-color: black;
                    height: 1px;"),
        
        div(
          radioButtons("radio", label = h3("Tipo de mapa"),
            choices = list("Pontos" = 1, "Calor" = 2)),
          align = 'center'
        ),
        
        tags$hr(style="border-color: black;
             height: 1px;"),
        
        sliderInput("slider",
          h3("Datas", align = 'center'),
          min = min(dadosMapa$Data, na.rm = TRUE), max = max(dadosMapa$Data, na.rm = TRUE),
          value = max(dadosMapa$Data, na.rm = TRUE)
        )
      ),
      
      tags$div(id="cite",
        'Dados dos boletins epidemiológicos publicados pela SESDF.'
      )
    )
  )#,
  

)
#)