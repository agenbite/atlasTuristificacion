require(ggplot2)
require(maptools)
require(rgdal)
require(spatialEco)
require(spatial)
require(sp)
require(gpclib)
require(mapproj)
library(grid)
library(gridExtra)
library(ggthemes)

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

distris.ord@data$subida <- alquileres$delta1317

distris.f <- fortify(distris.ord, region = "id")
datos <- merge(distris.f, alquileres, by="id", all.x=TRUE)

# # # # # # # # # # # # # # # # # # # # # # # # # 
# A dibujar...

titleGraph <- "Incremento del precio del alquiler 2013-2017"

p1 <- ggplot(datos) +
    theme_minimal()+  # no backgroundcolor
    geom_polygon(aes(x=long, y=lat, group=group,fill=delta1317),
                 color="black", alpha=1, size=0.1)+
    scale_fill_distiller(name="Incremento (%)",
                         palette="BrBG", direction=-1)+
    labs(title=titleGraph, subtitle="Fuente: idealista", x="", y="") +
    theme(axis.text=element_text(size=14),
          title=element_text(size=14), legend.position="right")
    coord_map()
p1

ggsave(p1, "incremento_precio_alquiler_2017.png")


# Tabla para diagrama de barras
tabla <- unique(datos[,c("distrito","delta1317")])
tabla$lab <- paste(format(round(tabla$delta1317,2), nsmall=2),"%",sep=" ")

levels <- tabla$distrito[order(tabla$delta1317, decreasing=FALSE)]
tabla$distrito <- factor(tabla$distrito, levels = levels)

p2 <- ggplot(tabla, aes(x=distrito, y=delta1317,
                        fill=delta1317, label=lab)) +
    geom_col() +
    theme_minimal()+  # no backgroundcolor
    scale_fill_distiller(name="Incremento (%)",
                         palette="BrBG", direction=-1)+
    geom_text(size = 3, hjust = -0.1) +
    labs(title=titleGraph, subtitle="Fuente: idealista", x="", y="") +
    coord_flip() +
    scale_y_continuous(limits = c(0, 37)) +
    theme(axis.text=element_text(size=14),
          title=element_text(size=14), legend.position="none")
p2

ggsave(p2, "incremento_precio_alquiler_2017_tabla.png")

# # # # # # # # # # # # # # # # # # # # # # # # # # #
# Combinar ambos gráficos

p1 <- ggplot(datos) +
    theme_minimal()+  # no backgroundcolor
    geom_polygon(aes(x=long, y=lat, group=group,fill=delta1317),
                 color="black", alpha=1, size=0.1)+
    scale_fill_distiller(name="Incremento (%)",
                         palette="BrBG", direction=-1)+
    labs(x="", y="", caption="Fuente: idealista.com") +
    ggthemes::theme_map() +
    theme(legend.position=c(0.8,0.7),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    coord_map()

# Tabla para diagrama de barras
tabla <- unique(datos[,c("distrito","delta1317")])
tabla$lab <- paste(format(round(tabla$delta1317,2), nsmall=2),"%",sep=" ")

levels <- tabla$distrito[order(tabla$delta1317, decreasing=FALSE)]
tabla$distrito <- factor(tabla$distrito, levels = levels)

p2 <- ggplot(tabla, aes(x=distrito, y=delta1317,
                        fill=delta1317, label=lab)) +
    geom_col() +
    scale_fill_distiller(name="Incremento (%)",
                         palette="BrBG", direction=-1)+
    geom_text(size = 3., hjust = -0.1) +
    labs(title=titleGraph, x="", y="") +
    coord_flip() +
    scale_y_continuous(limits = c(0, 40)) +
    theme_minimal()+  # no backgroundcolor
    theme(axis.text.y=element_text(size=14),
          plot.title=element_text(size=16),
          plot.caption=element_text(size=8),
          legend.position="none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank())

p3 <- grid.arrange(p2, p1, ncol = 2)

ggsave(grid.arrange(p2, p1, ncol = 2), filename="incremento_precio_alquiler_2013-2017.png", device="png")


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

