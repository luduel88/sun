library(rStrava)
library(tidyverse)

# Deine Strava App-Daten
client_id     <- 193977
client_secret <- "8df9529c7129e294205db71f9646b16707b1ff44"

# Token anfordern (öffnet Browser)
stoken <- strava_oauth(
  app_name       = "Map Test",
  app_client_id  = client_id,
  app_secret     = client_secret
)

# Token speichern (damit du das nicht jedes Mal machen musst)
saveRDS(stoken, "strava_token.rds")
