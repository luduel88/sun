library(shiny)
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
      # CSS zum Entfernen der Pfeile
      tags$style(
        HTML("
            input[type='number']::-webkit-inner-spin-button,
            input[type='number']::-webkit-outer-spin-button {
              -webkit-appearance: none;
              margin: 0;
            }
            input[type='number'] {
              -moz-appearance: textfield;
            }
            ")
      ),
      
      numericInput("lon", "Längengrad:", value = 9.25, min = 7, max = 10, step = 0.001),
      numericInput("lat", "Breitengrad:", value = 46.79, min = 46, max = 47, step = 0.001),
      
      dateInput("date", "Datum für Sonnenverlauf:", value = Sys.Date()),
      
      br(),
      actionButton("go", "Berechnen"),
      
      br(), br(),
      h4("Geländeinformationen"),
      tableOutput("terrain_table")
    ),
    
    mainPanel(
      plotOutput("plot1", height = "500px"),
      br(),
      plotOutput("plot2", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  
  # -------------------------------------------------
  # Gemeinsame Datenberechnung (um Doppelberechnung zu vermeiden)
  # -------------------------------------------------
  data_reactive <- eventReactive(input$go, {
    
    validate(
      need(input$lon >= 7 && input$lon <= 10, "Längengrad muss zwischen 7 und 10 Ost liegen"),
      need(input$lat >= 46 && input$lat <= 47, "Breitengrad muss zwischen 46 und 47 Nord liegen")
    )
    
    lon <- input$lon
    lat <- input$lat
    date <- input$date
    
    s <- sun_position(lat, lon, date, paste(interval, "min"))
    s$sun_azi_deg <- s$azimuth / pi * 180 + 180
    s$sun_alt_deg <- s$altitude / pi * 180
    
    s$hill_azi_deg <- terra::extract(aspect, cbind(lon, lat))[,1]
    s$hill_slo_deg <- terra::extract(slope, cbind(lon, lat))[,1]
    
    s$horizont <- horizon(dem, lat, lon, azimuths = s$sun_azi_deg)$angle
    
    setDT(s)
    s[sun_alt_deg < horizont, sun_alt_deg := NA_real_]
    s[sun_alt_deg > horizont,
      radiation := inc_angle(deg_to_rad(sun_alt_deg), deg_to_rad(sun_azi_deg),
                             deg_to_rad(hill_slo_deg), deg_to_rad(hill_azi_deg)) *
        solarconstant * am_factor(deg_to_rad(sun_alt_deg))]
    
    s[, radiation := radiation - emission]
    s[radiation < 0 | is.na(radiation), radiation := 0]
    s[, cum_radiation_day_MJ := cumsum(radiation * interval * 60 / 1e6)]
    
    return(s)
  })
  
  # ---- Plot 1: Horizontprofil & Sonnenverlauf ----
  output$plot1 <- renderPlot({
    s <- data_reactive()
    lon <- input$lon
    lat <- input$lat
    
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
  })
  
  # ---- Plot 2: Kumulative Tagesstrahlung ----
  output$plot2 <- renderPlot({
    s <- data_reactive()
    
    ggplot(s, aes(x = time, y = cum_radiation_day_MJ)) +
      geom_line(color = "darkred", linewidth = 1) +
      geom_hline(yintercept = energy_needed_MJ(), linetype = "dashed", linewidth = 1) +
      # Skalierung: auf kumulative Achse normieren
      geom_line(aes(y = radiation / 30),       # Faktor anpassen!
                color = "steelblue", linewidth = 1) +
      
      scale_y_continuous(
        name = "Kumulative Strahlung (MJ/m²)",
        limits = c(0, 30),
        sec.axis = sec_axis(~ . * 30, # Rückskalieren!
                            name = "Momentane Strahlung (W/m²)",
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
        y = "Kumulative Strahlung (MJ/m²)",
        title = "Strahlung & Energie"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major.x = element_line(linewidth = 0.6, color = "grey30"))
  })
  
  # ---- Terrain-Tabelle ----
  output$terrain_table <- renderTable({
    lon <- input$lon
    lat <- input$lat
    
    # Werte aus DEM, Slope und Aspect extrahieren
    height <- terra::extract(dem, cbind(lon, lat))[,1]
    slp <- terra::extract(slope, cbind(lon, lat))[,1]
    asp <- terra::extract(aspect, cbind(lon, lat))[,1]
    
    # Tabelle erstellen
    data.frame(
      "Höhe" = paste(as.character(as.integer(height)), "m"),
      "Neigung" = paste0(as.character(as.integer(slp)), "°"),
      "Exposition" = get_exposition(asp)
    )
  })
}

shinyApp(ui, server)
