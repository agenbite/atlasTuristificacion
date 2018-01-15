require(ggplot2)
require(maptools)
require(rgdal)
require(spatialEco)
require(spatial)
require(sp)


# # # # # # # # # # # # # # # # # # # # # # # # # 
# Leemos los datos

P4S.latlon <- CRS("+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
distris.shp <- readShapePoly("SHP_ED50/DISTRITOS.shp",
                             verbose=TRUE, proj4string=P4S.latlon)
distris <- spTransform(distris.shp, CRS("+proj=longlat +datum=WGS84"))

distris.ord <- distris
distris.ord@data <- distris@data[order(distris@data$NOMBRE),]
#distris.ord@data$id <- distris.ord@data$CODDISTRIT
distris.ord@data$id <- 1:21

alquileres <- read.csv("alquileres.csv")
alquileres$id <- distris.ord@data$CODDISTRIT
#alquileres$id <- 1:21

distris.ord@data$subida <- alquileres$subida

distris.f <- fortify(distris.ord, region = "id")
datos <- merge(distris.f, alquileres, by="id", all.x=TRUE)

# # # # # # # # # # # # # # # # # # # # # # # # # 
# A dibujar...

titleGraph <- "Incremento del precio del alquiler en 2017"

ggplot(datos) +
    theme_minimal()+  # no backgroundcolor
    geom_polygon(aes(x=long, y=lat, group=group,fill=subida),
                 color="black", alpha=1, size=0.1)+
    scale_fill_gradient(name="Incremento (%)",
                         high = "#132B43", low = "#56B1F7") +
    labs(title=titleGraph) +
    coord_map()

ggsave("incremento_precio_alquiler_2017.png")



# # # # # # # # # # # # # # # # # # # # # # # # # # #
# Coropleto interactivo

library(leaflet)

distris@data$subida <- alquileres$subida[order(as.numeric(as.character(alquileres$id)))]

m <- leaflet(distris) %>%
  setView(-3.7, 40.5, 11) %>%
##  fitBounds(-3.9, -3.5, 40.3, 40.7) %>%
    addProviderTiles("OpenStreetMap.Mapnik")

# Create a continuous palette function
pal <- colorNumeric(
  palette = "Blues",
  domain = alquileres$subida)

labels <- sprintf(
  "<strong>%s</strong><br/>%g &#37;",
  distris$NOMBRE, distris$subida
) %>% lapply(htmltools::HTML)


library(htmlwidgets)
m %>% addPolygons(smoothFactor = 0,
                  fillOpacity = 0.9,
                  color = ~pal(subida),
                  weight=1,
                  highlight = highlightOptions(
                      weight = 1,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 1.0,
                      bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                      style = list("font-weight" = "normal",
                                   padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
    addLegend(pal = pal, values = distris.ord$subida,
              opacity = 1, title = "Incremento (&#37;)",
              position = "topleft") %>%
    saveWidget(file="incremento_precio_alquiler_2017.html",
                 selfcontained=TRUE)

