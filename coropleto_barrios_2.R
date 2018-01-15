require(ggplot2)
require(maptools)
require(rgdal)
require(spatialEco)
require(spatial)
require(sp)


# # # # # # # # # # # # # # # # # # # # # # # # # 
# Leemos los datos

P4S.latlon <- CRS("+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
barrios.shp <- readShapePoly("SHP_ED50/Barrios.shp", verbose=TRUE, proj4string=P4S.latlon)
barrios <- spTransform(barrios.shp, CRS("+proj=longlat +datum=WGS84"))
barrios.f <- fortify(barrios, region = "CODBARRIO")

atlas <- read.csv("../atlasTuristificacion/atlas.csv")

# # # # # # # # # # # # # # # # # # # # # # # # # 
# Calculamos el índice que queramos

# A. 

indice <- as.data.frame(cbind(lat=atlas$lat, lon=atlas$lon,
                              i2 = (atlas$rn_fc_med_price /
                                   atlas$rn_med_tramo)))

rescale <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

indice$i2r <- rescale(indice$i2, na.rm=TRUE)

#Sustituimos los NA por la media de la ciudad
indice$i2r[is.na(indice$i2r)] <- mean(indice$i2r, na.rm=TRUE)

## indice$i2[is.na(indice$i2)] <- 0
## indice <- indice[!is.na(indice$i2),]

coordinates(indice) <- ~lat+lon
proj4string(indice) <- CRS("+proj=longlat +datum=WGS84")

# Asignamos cada punto a su barrio correspondiente
pts.poly <- point.in.poly(indice, barrios)

index <- as.data.frame(tapply(pts.poly@data$i2r,
        pts.poly@data$CODBARRIO, FUN=mean, na.rm=TRUE))
index$names <- rownames(index)
colnames(index) <- c("index", "id")
index$index <- index$index*100


# # # # # # # # # # # # # # # # # # # # # # # # # 
# Lo calculamos por barrios

datos <- merge(barrios.f, index, by="id", all.x=TRUE)
final.plot<-datos[order(datos$order), ]
final.plot$indexF <- as.double(final.plot$index)

barrios@data$index <- index$index

# # # # # # # # # # # # # # # # # # # # # # # # # 
# A dibujar...

titleGraph <- "Porcentaje de renta dedicado al alquiler"

ggplot(final.plot) +
    theme_minimal()+  # no backgroundcolor
    geom_polygon(aes(x = long, y = lat, group = group,
                     fill=indexF))+
    geom_polygon(data=barrios.f,aes(x=long, y=lat, group=group),
                 color="black", alpha=0, size=0.1)+
    scale_fill_gradient(name="ratio",
                        high = "#132B43", low = "#56B1F7") +
    labs(title=titleGraph) +
    coord_map()

ggsave("rentaMediaPrecioAlquiler.png")

ggplot(final.plot) +
    theme_minimal()+  # no backgroundcolor
    geom_polygon(aes(x = long, y = lat, group = group,
                     fill=cut(indexF, c(0,0.12,1))))+
    geom_polygon(data=barrios.f,aes(x=long, y=lat, group=group),
                 color="black", alpha=0, size=0.1)+
    scale_fill_brewer(type="seq",
             labels=c("<0.12",">0.12"))+
    guides(fill=guide_legend(title="Ratio"))+
    labs(title=titleGraph) +
    coord_map()

ggsave("rentaMediaPrecioAlquiler_2c.png")



# # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # #
# Coropleto interactivo

library(leaflet)
library(htmlwidgets)

m <- leaflet(barrios) %>%
  setView(-3.7, 40.5, 11) %>%
##  fitBounds(-3.9, -3.5, 40.3, 40.7) %>%
    addProviderTiles("OpenStreetMap.Mapnik")

# Create a continuous palette function
pal <- colorNumeric(
  palette = "Blues",
  domain = c(0.07,0.3))

labels <- sprintf(
  "<strong>%s</strong><br/>ratio: %g",
  barrios$NOMBRE, barrios$index
) %>% lapply(htmltools::HTML)


m %>% addPolygons(smoothFactor = 0,
                  fillOpacity = 0.9,
                  color = ~pal(index),
                  weight=1,
                  highlight = highlightOptions(
                      weight = 1,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 1.0,
                      bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
    addLegend(pal = pal, values = barrios$index,
              opacity = 1, title = "Ratio entre<br/>salario medio y<br/>precio del alquiler",
              position = "topleft") %>%
    saveWidget(file="rentaMediaPrecioAlquiler.html",
                 selfcontained=TRUE)

