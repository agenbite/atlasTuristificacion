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

atlas <- read.csv("../atlasTuristificacion/atlas.csv")
barrios.data.corregido <- read.csv("barrios.data.csv")
barrios.data.corregido
levels <- barrios$names[order(tabla$i1, decreasing=FALSE)]
## tabla$names <- factor(tabla$names, levels = levels)

barrios@data <- barrios.data.corregido

barrios.f <- fortify(barrios, region = "CODBARRIO")

# # # # # # # # # # # # # # # # # # # # # # # # # 
# Calculamos el índice que queramos

# A. 

indice <- as.data.frame(cbind(lat=atlas$lat, lon=atlas$lon,
                              i = atlas$rn_med_tramo,
                              i2 = (atlas$rn_fc_med_price /
                                   atlas$rn_med_tramo)))

rescale <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

indice$i2r <- rescale(indice$i2, na.rm=TRUE)

#Sustituimos los NA por la media de la ciudad
indice$i2r[is.na(indice$i2r)] <- mean(indice$i2r, na.rm=TRUE)
indice$i[is.na(indice$i)] <- mean(indice$i, na.rm=TRUE)

## indice$i2[is.na(indice$i2)] <- 0
## indice <- indice[!is.na(indice$i2),]

coordinates(indice) <- ~lat+lon
proj4string(indice) <- CRS("+proj=longlat +datum=WGS84")

# Asignamos cada punto a su barrio correspondiente
## pts.poly <- point.in.poly(indice, barrios)

## index <- as.data.frame(tapply(pts.poly@data$i2r,
##         pts.poly@data$CODBARRIO, FUN=mean, na.rm=TRUE))
## index$names <- rownames(index)
## colnames(index) <- c("index", "id")
## index$index <- index$index*100

# Asignamos cada punto a su barrio correspondiente (otra forma)
index <- sp::over(barrios, indice, fn = mean, na.rm=TRUE)
index$names <- barrios@data$NOMBRE
index$id <- barrios@data$CODBARRIO

index$i1 <- barrios@data$precio  / index$i 

# # # # # # # # # # # # # # # # # # # # # # # # # 
# Lo calculamos por barrios

datos <- merge(barrios.f, index, by="id", all.x=TRUE)

##barrios@data$index <- index$i1

# # # # # # # # # # # # # # # # # # # # # # # # # 
# A dibujar...

titleGraph <- "Precio medio de alquiler dividido entre renta media"

p1 <- ggplot(datos) +
    geom_polygon(aes(x = long, y = lat, group = group,
                     fill=i1))+
    geom_polygon(data=barrios.f, aes(x=long, y=lat, group=group),
                 color="black", alpha=0, size=0.1)+
    scale_fill_distiller(name="ratio",
                         palette="BrBG", direction=-1)+
    labs(title="",
         caption="Fuente: idealista.com, turistificacion.300000kms.net",
         y="", x="") +
    theme_minimal()+  # no backgroundcolor
    ggthemes::theme_map() +
    theme(legend.position=c(0.8,0.7),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    coord_map()
p1

# Tabla para diagrama de barras
tabla <- unique(datos[,c("id","names","i1", "group")])
tabla$lab <- format(round(tabla$i1,2), nsmall=2)

tabla.ord <- tabla[order(tabla$i1),]
## levels <- tabla$names[order(tabla$i1, decreasing=FALSE)]
## tabla$names <- factor(tabla$names, levels = levels)
tabla.red <- tabla.ord[c(1:11,118:129),]
## levels(tabla.red$names) <- sub("^Berruguete$", "(...)",
##                                levels(tabla.red$names))
levels(tabla.red$names) <- sub("^Numancia$", "Madrid (media)",
                               levels(tabla.red$names))
## levels(tabla.red$names) <- sub("^Valdemarin$", "(...)",
##                                levels(tabla.red$names))

## tabla.red[11, "i1"] <- 100
tabla.red[12, "i1"] <- mean(tabla$i1)
## tabla.red[13, "i1"] <- 100


p2 <- ggplot(tabla.red) +
    geom_col(aes(x=reorder(names, i1), y=i1, fill=i1)) +
    scale_fill_distiller(palette="BrBG", direction=-1)+
    ## geom_text(size = 4, hjust = -0.1) +
    labs(title=titleGraph, y="", x="") +
    coord_flip() +
#    scale_x_discrete(limits=c(1,20)) +
    scale_y_continuous(limits = c(0, 10)) +
    theme_minimal()+  # no backgroundcolor
    theme(axis.text.y=element_text(size=14),
          plot.title=element_text(size=14, face="bold"),
          plot.caption=element_text(size=8),
          legend.position="none",
          panel.grid.major.y = element_blank(),
#          panel.grid.major = element_blank(),
#          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size=10)) 
p2

p3 <- grid.arrange(p2, p1, ncol=2)

ggsave(grid.arrange(p2, p1, ncol = 2),
       filename="alquiler_entre_renta_media.png", device="png")


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

