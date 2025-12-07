library(shiny)
library(leaflet)
library(terra)
library(geosphere)
library(suncalc)
library(data.table)
library(ggplot2)

# setwd("~/sun/sun_app")
source("horizon.R")
source("sun_position.R")
source("inc_angle.R")
source("am_factor.R")
source("energy_needed.R")
source("deg_to_rad.R")
source("get_exposition.R")

# DEM laden, Slope und Exposition rechnen
dem <- rast("data/dem_switzerland_50m_wgs84.tif")
slope <- terrain(dem, v = "slope", unit = "degrees")
aspect <- terrain(dem, v = "aspect", unit = "degrees")

# Parameters
solarconstant <- 1000 # W/m2
emission <- 80 # W/m2
interval <- 10 # Minuten

# ---- Shiny App ----
ui <- fluidPage(
  titlePanel("Horizontprofil, Sonnenverlauf, Strahlung & Energie"),
  
  sidebarLayout(
    sidebarPanel(
      tags$style(
        HTML("
            /* Keine Pfeile bei Eingabefeldern */
            input[type='number']::-webkit-inner-spin-button,
            input[type='number']::-webkit-outer-spin-button {
              -webkit-appearance: none;
              margin: 0;
            }
            input[type='number'] {
              -moz-appearance: textfield;
            }
            /* Primär-Button: Berechnen */
            #go {
              background-color: #007BFF;  /* kräftiges Blau */
              color: white;               /* Textfarbe weiß */
              font-weight: bold;          /* fetter Text */
              border-radius: 8px;         /* abgerundete Ecken */
              padding: 8px 16px;          /* Innenabstand für schönere Größe */
              border: none;               /* Rahmen entfernen */
            }
            
            /* Hover-Effekt */
            #go:hover {
              background-color: #0056b3;  /* dunkleres Blau beim Hover */
              cursor: pointer;             /* Mauszeiger ändert sich */
            }
            
            /* Fokus-Effekt */
            #go:focus {
              outline: none;
              box-shadow: 0 0 0 2px rgba(0,123,255,0.5);
            }
            ")
      ),
      
      numericInput("lon", "Längengrad:", value = 9.25, min = 5.902, max = 10.563, step = 0.0001),
      numericInput("lat", "Breitengrad:", value = 46.79, min = 45.688, max = 47.833, step = 0.0001),
      
      h4("Punkt auf der Karte wählen"),
      leafletOutput("map_sidebar", height = 300),
      
      br(),
      dateInput("date", "Datum für Sonnenverlauf:", value = Sys.Date()),
      
      br(),
      actionButton("go", "Berechnen"),
      
      br(), br(),
      h4("Gelände am gewählten Punkt"),
      tableOutput("terrain_table"),
      
      br(),
      h4("Hang kritisch ab"),
      textOutput("uhrzeit")
    ),
    
    mainPanel(
      plotOutput("plot1", height = "500px"),
      br(),
      plotOutput("plot2", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  
  # Leaflet-Karte rendern
  output$map_sidebar <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenTopoMap) %>%
      setView(lng = 9.25, lat = 46.79, zoom = 10)
  })
  
  # Klick auf Karte abfangen
  observeEvent(input$map_sidebar_click, {
    click <- input$map_sidebar_click
    
    # NumericInputs automatisch aktualisieren
    updateNumericInput(session, "lon", value = round(click$lng,4))
    updateNumericInput(session, "lat", value = round(click$lat,4))
    
    # Marker auf Karte setzen
    leafletProxy("map_sidebar") %>%
      clearMarkers() %>%
      addMarkers(lng = click$lng, lat = click$lat,
                 popup = paste0("Lat: ", round(click$lat, 4), 
                                "<br>Lng: ", round(click$lng, 4)))
  })
  
  # Marker auch setzen, wenn man die Inputs manuell ändert
  observe({
    lng <- input$lon
    lat <- input$lat
    
    leafletProxy("map_sidebar") %>%
      clearMarkers() %>%
      addMarkers(lng = lng, lat = lat,
                 popup = paste0("Lat: ", round(lat, 4), 
                                "<br>Lng: ", round(lng, 4)))
  })
  
  # -------------------------------------------------
  # Gemeinsame Datenberechnung (um Doppelberechnung zu vermeiden)
  # -------------------------------------------------
  data_reactive <- eventReactive(input$go, {
    
    lon <- input$lon
    lat <- input$lat
    date <- input$date
    
    validate(
      need(!is.na(terra::extract(dem, cbind(lon, lat))[, 1]), "Punkt muss in der Schweiz liegen!")
    )
    
    s <- sun_position(lat, lon, date, paste(interval, "min"))
    s$sun_azi_deg <- s$azimuth / pi * 180 + 180
    s$sun_alt_deg <- s$altitude / pi * 180
    
    s$hill_azi_deg <- terra::extract(aspect, cbind(lon, lat))[, 1]
    s$hill_slo_deg <- terra::extract(slope, cbind(lon, lat))[, 1]
    
    s$horizont <- horizon(dem, lat, lon, azimuths = s$sun_azi_deg)$angle
    
    setDT(s)
    s[sun_alt_deg < horizont, sun_alt_deg := NA_real_]
    s[sun_alt_deg > horizont,
      radiation := inc_angle(deg_to_rad(sun_alt_deg), deg_to_rad(sun_azi_deg),
                             deg_to_rad(hill_slo_deg), deg_to_rad(hill_azi_deg)) *
        solarconstant * am_factor(deg_to_rad(sun_alt_deg))]
    
    s[, radiation := radiation - emission] # Korrektur für Abstrahlung
    s[radiation < 0 | is.na(radiation), radiation := 0]
    s[, cum_radiation_day_MJ := cumsum(radiation * interval * 60 / 1e6)]
    
    return(s)
  })
  
  # ---- Plot 1: Horizontprofil & Sonnenverlauf ----
  output$plot1 <- renderPlot({
    
    s <- data_reactive()
    
    ggplot() +
      geom_ribbon(data = s,
                  aes(x = time, ymin = 0, ymax = horizont),
                  fill = "grey20", alpha = 0.5) +
      geom_line(data = s, aes(x = time, y = horizont),
                color = "grey20", linewidth = 1.2) +
      geom_line(data = s, aes(x = time, y = sun_alt_deg),
                color = "orange", linewidth = 1.2) +
      scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M",
                       expand = c(0,0),
                       limits = c(
                         as.POSIXct(paste(input$date, "05:00:00"), tz = "Europe/Berlin"),
                         as.POSIXct(paste(input$date, "22:00:00"), tz = "Europe/Berlin")
                       )) +
      scale_y_continuous(
        name = "Winkel (°)",
        limits = c(0, 90),
        breaks = seq(0, 90, by = 15),
        sec.axis = sec_axis(~ ., name = "Winkel (°)", breaks = seq(0, 90, by = 15))
      ) +
      labs(
        x = "Uhrzeit",
        title = "Horizontprofil & Sonnenverlauf"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major.x = element_line(linewidth = 0.6, color = "grey30"))
  }, res = 96)
  
  # ---- Plot 2: Kumulative Tagesstrahlung ----
  output$plot2 <- renderPlot({
    
    s <- data_reactive()
    
    
    ggplot(s, aes(x = time, y = cum_radiation_day_MJ)) +
      geom_line(color = "darkred", linewidth = 1) +
      geom_hline(yintercept = energy_needed_MJ(), linetype = "dashed", linewidth = 1) +
      geom_text(
        aes(x = as.POSIXct(paste(input$date, "21:30:00"), tz = "Europe/Berlin"),  # Position rechts
            y = energy_needed_MJ(),
            label = paste0("Energiebedarf: ", round(energy_needed_MJ(), 1), " MJ/m²")),
        color = "black",
        vjust = -0.5,  # leicht oberhalb der Linie
        hjust = 1
      ) +
      # Skalierung: auf kumulative Achse normieren
      geom_line(aes(y = radiation / 30), # Faktor anpassen!
                color = "steelblue", linewidth = 1) +
      
      scale_y_continuous(
        name = "Energie (MJ/m²)",
        limits = c(0, 30),
        sec.axis = sec_axis(~ . * 30, # Rückskalieren!
                            name = "Nettostrahlung (W/m²)",
                            breaks = seq(0, 900, by = 300))
      ) +
      scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M",
                       expand = c(0,0),
                       limits = c(
                         as.POSIXct(paste(input$date, "05:00:00"), tz = "Europe/Berlin"),
                         as.POSIXct(paste(input$date, "22:00:00"), tz = "Europe/Berlin")
                       )) +
      
      labs(
        x = "Uhrzeit",
        y = "Energie (MJ/m²)",
        title = "Strahlung & Energie"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major.x = element_line(linewidth = 0.6, color = "grey30"))
  }, res = 96)
  
  # ---- Terrain-Tabelle ----
  output$terrain_table <- renderTable({
    s <- data_reactive()
    lon <- unique(s$lon)
    lat <- unique(s$lat)
    
    # Werte aus DEM, Slope und Aspect extrahieren
    height <- terra::extract(dem, cbind(lon, lat))[, 1]
    slp <- terra::extract(slope, cbind(lon, lat))[, 1]
    asp <- terra::extract(aspect, cbind(lon, lat))[, 1]
    
    # Tabelle erstellen
    data.frame(
      "Höhe" = paste(as.character(as.integer(height)), "m"),
      "Neigung" = paste0(as.character(as.integer(slp)), "°"),
      "Exposition" = get_exposition(asp)
    )
  })
  
  output$uhrzeit <- renderText({
    
    s <- data_reactive()
    first_time <- s[cum_radiation_day_MJ > 5][1, time]
    if (is.na(first_time)) {
      "Zu wenig Energie am gewählten Datum"
    } else {
      paste(format(first_time, "%H:%M"), "Uhr")
    }
  })
}

shinyApp(ui, server)
