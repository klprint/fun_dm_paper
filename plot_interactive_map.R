library(tidyverse)
library(leaflet)

de_stores <- read_csv("data/2020-10-23_15_27_03_deStoresStatus.csv")


get_colors <- function(x){
  ifelse(
    x == 0, "darkred",
    ifelse(
      x > 0 & x <= 10,
      "red",
      ifelse(
        x > 10 & x <= 50,
        "orange",
        ifelse(
          x > 50 & x <= 100,
          "lighblue",
          "blue"
        )
      )
    )
  )
}

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(
    lng = de_stores$lon,
    lat = de_stores$lat,
    color = get_colors(de_stores$nPaper), label = de_stores$nPaper,
    opacity = .5,
    radius = 2
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("darkred", "red", "orange", "lightblue", "blue"),
    labels = c("0 - shit happens ¯\\_(ツ)_/¯",
               "1-10 - jetzt aber los!",
               "10-50 - langsam wird's knapp",
               "50-100 - noch kein Problem",
               ">100 - Alles gut"),
    title = glue::glue(
      "Verfügbares Klopapier bei DM"
    )
  )
m

htmlwidgets::saveWidget(m, "index.html")
