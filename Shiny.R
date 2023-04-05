######LIBRERIE################
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(httr)
library(jsonlite)
library(readr)

######set today###########
today_date <- Sys.Date()
onemonthback <- today_date - 31

######scaricare dati ###################
url <- paste0("https://earthquake.usgs.gov/fdsnws/event/1/query?format=csv&starttime=", onemonthback, "&endtime=", today_date)
suk <- read_csv(url)
suk$time <- substr(suk$time, 1, 10)


###############UI###########################
ui <- dashboardPage( skin = "green", #colore della dahboard page 
                     dashboardHeader(title = "Earthquick",titleWidth = 250), #titolo e grandezza carattere
                     dashboardSidebar( #Creazione di menu a scomparsa 
                       sidebarMenu(#Gruppo di input per filtrazione 
                         menuItem("Magnitude", tabName = "mag", icon = icon("globe"),
                                  sliderInput("range1", "Magnitude lower", min = min(suk$mag), max=max(suk$mag), value = 2.5,step = 0.1), #Slider Input per magnitudo - grande
                                  sliderInput("range2", "Magnitude higher", min = min(suk$mag), max=max(suk$mag), value = 5,step = 0.1)), #Slider Input per magnitudo + grande
                         menuItem("Date", tabName = "date", icon = icon("calendar"),
                                  dateRangeInput('dateRange', label = 'Range of Date', start = tail(suk$time, n = 1), end = suk$time[[1]])), #Scelta data 
                         " "
                         
                       )
                     ),#Sidebar
                     dashboardBody(
                       fluidRow(
                         column(
                           12, 
                           verbatimTextOutput("update_date"), 
                           style = "text-align:left; color: #fff; font-size: 15px; padding: 10px;"
                         )
                       ), # Bunner per determinare ultimo giorno caricato 
                       tabItem(
                         "mappa",
                         fluidRow(
                           box(
                             status = "primary",
                             width = 8.6,
                             leafletOutput("mappa", height = "800") #Visualizzazione Mappa
                           )
                         )
                       ), # tabitem
                       # il tuo contenuto qui
                       tags$footer(
                         class = "main-footer",
                         "A cura di CEF"
                       )
                     ) # dashboard
) # ui


##################SERVER#################
server <- function(input, output, session) {
  output$mappa <- renderLeaflet({
    # Filtraggio del dataset in base al range di magnitudo selezionato dall'utente
    filtered_eq <- suk %>%
      filter(time >= input$dateRange[1] & time <= input$dateRange[2], 
             mag >= input$range1 & mag <= input$range2)
    
    
    # Graficazione mappa 
    pal <- colorNumeric(palette = "RdYlBu", domain = filtered_eq$mag, reverse = TRUE)
    #Creazione della base dell'etichetta con realiti dati filtrari da mostrare:
    mytext <- paste(
      "Date: ", filtered_eq$time, "<br/>",
      "Place: ", filtered_eq$place, "<br/>", 
      "Depth: ", filtered_eq$depth, "<br/>", 
      "Magnitude: ", filtered_eq$mag, "<br/>",
      "MagnitudeType: ", filtered_eq$magType, sep="" ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    leaflet(filtered_eq) %>% 
      addTiles()  %>% 
      addProviderTiles("Esri.WorldImagery") %>% #tipologia di mappa 
      addCircleMarkers(~longitude, ~latitude, fillOpacity = 0.7, color=~pal(filtered_eq$mag), #aggiunta markers per ogni terremoto
                       radius = 6, stroke=FALSE, 
                       label = mytext,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto") #impostazioni etichette
      ) %>%
      addLegend( pal=pal, values=~mag, opacity=0.9, title = "Magnitude", position = "bottomright" ) #impostazioni legenda 
  })
  output$update_date <- renderPrint({
    today_date <- Sys.Date()
    paste("I dati scaricati sono dal:",onemonthback, "al", today_date) #bunner per informare il range di periodo 
  })
}

shinyApp(ui, server)

