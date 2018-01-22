require(ggplot2)
require(maptools)
require(rgdal)
library(spatialEco)
require(spatial)
require(sp)
library(grid)
library(gridExtra)
library(ggthemes)


# # # # # # # # # # # # # # # # # # # # # # # # # 
# Leemos los datos

P4S.latlon <- CRS("+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
barrios.shp <- readShapePoly("SHP_ED50/Barrios.shp", verbose=TRUE, proj4string=P4S.latlon)
barrios <- spTransform(barrios.shp, CRS("+proj=longlat +datum=WGS84"))
barrios.f <- fortify(barrios, region = "CODBARRIO")

atlas <- read.csv("../atlasTuristificacion/atlas.csv")
nombres <- read.csv("precio alquiler barrios.csv")$Nombre.de.barrio

# # # # # # # # # # # # # # # # # # # # # # # # # 
# Calculamos el índice que queramos

# A. 

indice <- as.data.frame(cbind(lat=atlas$lat, lon=atlas$lon,
                             i1 = (atlas$abnb_tot_beds +
                                   0.03329 * atlas$cad_tot_srf_hotels)/
                                 0.026,
                             i2 = atlas$abnb_tot_beds))
                        

indice$i1[is.na(indice$i1)] <- 0
indice$i2[is.na(indice$i2)] <- 0

coordinates(indice) <- ~lat+lon
proj4string(indice) <- CRS("+proj=longlat +datum=WGS84")

# Asignamos cada punto a su barrio correspondiente
## pts.poly <- point.in.poly(indice, barrios)

## pt.in.poly$i <- indice$i1

## index <- as.data.frame(tapply(pt.in.poly$i,
##         pt.in.poly$CODBARRIO, FUN=mean, na.rm=TRUE))
## index$names <- rownames(index)
## colnames(index) <- c("index", "id")

## index$lim <- cut(index$index, c(0,600,20000),
##                  labels = c("low", "high"))

# Asignamos cada punto a su barrio correspondiente (otra forma)
index <- sp::over(barrios, indice, fn = mean)
index$names <- barrios@data$NOMBRE
index$id <- barrios@data$CODBARRIO


# Los valores altos son demasiado grandes. Saturamos
#sat <- 200
#index$index[index$index>sat] <- sat

#index$index <- log(1+index$index)


# # # # # # # # # # # # # # # # # # # # # # # # # 
# Lo calculamos por barrios

datos <- merge(barrios.f, index, by="id", all.x=TRUE)
final.plot<-datos[order(datos$order), ]
final.plot$indexF <- as.double(final.plot$i1)
## final.plot$indexF <- as.double(final.plot$i2)

barrios@data$index <- index$i1


# # # # # # # # # # # # # # # # # # # # # # # # # 
# A dibujar...

titleGraph <- expression("Camas para turistas por"~km^2)

ggplot(final.plot) +
    geom_polygon(aes(x = long, y = lat, group = group,
                     fill=indexF)) +
    geom_polygon(data=barrios.f,aes(x=long, y=lat, group=group),
                color="black", alpha=0, size=0.1)+
    scale_fill_distiller(name="Incremento (%)",
                         palette="BrBG", direction=-1)+
    labs(title=titleGraph) +
    theme_minimal()+  # no backgroundcolor
    ggthemes::theme_map() +
    theme(legend.position="none", axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) +
    coord_map()

ggsave("camas_por_km2.png")

ggplot(final.plot) +
    theme_minimal()+  # no backgroundcolor
    geom_polygon(aes(x = long, y = lat, group = group,
                     ## fill=indexF))+
                     fill=cut(indexF, c(0,600,20000),
                              include.lowest=TRUE)))+
    geom_polygon(data=barrios.f,aes(x=long, y=lat, group=group),
                color="black", alpha=0, size=0.1)+
    scale_fill_brewer(type="seq",
             labels=c("<600",">600"))+
    guides(fill=guide_legend(title=expression(camas/km^2)))+
    ## scale_fill_gradient(name=text("Camas"),
    ##                     high = "#132B43", low = "#56B1F7") +
    labs(title=titleGraph) +
    coord_map()

ggsave("camas_por_km2_2c.png")


# # # # # # # # # # # # # # # # # # # # # # # # # # #
# Combinar ambos gráficos
titleGraph <- expression("Camas para turistas por"~km^2)

p1 <- ggplot(final.plot) +
    geom_polygon(aes(x = long, y = lat, group = group,
                     fill=indexF)) +
    geom_polygon(data=barrios.f,aes(x=long, y=lat, group=group),
                color="black", alpha=0, size=0.1)+
    scale_fill_distiller(name="Camas",
                         palette="BrBG", direction=-1)+
    labs(title="",
         caption="Fuente: estimacion a partir de datos de turistificacion.300000kms.net",
         y="", x="") +
    theme_minimal()+  # no backgroundcolor
    ggthemes::theme_map() +
    theme(legend.position=c(0.8,0.7),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    coord_map()

# Tabla para diagrama de barras
tabla <- unique(final.plot[,c("names","i1")])
tabla$lab <- format(round(tabla$i,0), nsmall=0)

levels <- tabla$names[order(tabla$i1, decreasing=FALSE)]
tabla$names <- factor(tabla$names, levels = levels)

p2 <- ggplot(tabla[1:15,], aes(x=names, y=i1,
                        fill=i1, label=lab)) +
    geom_col() +
    scale_fill_distiller(palette="BrBG", direction=-1)+
    ## geom_text(size = 4, hjust = -0.1) +
    labs(title=titleGraph, y="", x="") +
    coord_flip() +
    ## scale_x_discrete(breaks=NULL) +
    scale_y_continuous(limits = c(0, 20000)) +
    theme_minimal()+  # no backgroundcolor
    theme(axis.text.y=element_text(size=14),
          plot.title=element_text(size=14, face="bold"),
          plot.caption=element_text(size=8),
          legend.position="none",
          panel.grid.major.y = element_blank() ,
#          panel.grid.major = element_blank(),
#          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size=10))
p2

p3 <- grid.arrange(p2, p1, ncol=2)

ggsave(grid.arrange(p2, p1, ncol = 2),
       filename="camas_por_km2.png", device="png")

# # # # # # # # # # # # # # # # # # # # # # # # # # #
# Combinar ambos gráficos
titleGraph <- expression("Oferta AirBnB por"~km^2)

p1 <- ggplot(final.plot) +
    geom_polygon(aes(x = long, y = lat, group = group,
                     fill=indexF)) +
    geom_polygon(data=barrios.f,aes(x=long, y=lat, group=group),
                color="black", alpha=0, size=0.1)+
    scale_fill_distiller(name="Camas",
                         palette="BrBG", direction=-1)+
    labs(title="",
         caption="Fuente: estimacion a partir de datos de turistificacion.300000kms.net",
         y="", x="") +
    theme_minimal()+  # no backgroundcolor
    ggthemes::theme_map() +
    theme(legend.position=c(0.8,0.7),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()) +
    coord_map()

# Tabla para diagrama de barras
tabla <- unique(final.plot[,c("names","i2")])
tabla$lab <- format(round(tabla$i2,0), nsmall=0)

levels <- tabla$names[order(tabla$i2, decreasing=FALSE)]
tabla$names <- factor(tabla$names, levels = levels)

p2 <- ggplot(tabla[1:15,], aes(x=names, y=i2,
                        fill=i2, label=lab)) +
    geom_col() +
    scale_fill_distiller(palette="BrBG", direction=-1)+
    ## geom_text(size = 4, hjust = -0.1) +
    labs(title=titleGraph, y="", x="") +
    coord_flip() +
    ## scale_x_discrete(breaks=NULL) +
    scale_y_continuous(limits = c(0, 125)) +
    theme_minimal()+  # no backgroundcolor
    theme(axis.text.y=element_text(size=14),
          plot.title=element_text(size=14, face="bold"),
          plot.caption=element_text(size=8),
          legend.position="none",
          panel.grid.major.y = element_blank() ,
#          panel.grid.major = element_blank(),
#          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size=10))
p2

p3 <- grid.arrange(p2, p1, ncol=2)

ggsave(grid.arrange(p2, p1, ncol = 2),
       filename="airbnb_por_km2.png", device="png")


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
  domain = indice$i1)

labels <- sprintf(
  "<strong>%s</strong><br/>%g",
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
                      style = list("font-weight" = "normal",
                                   padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
    addLegend(pal = pal, values = barrios$index,
              opacity = 1,
              title = "Camas para<br/> turistas<br/> por km&sup2;",
              position = "topleft") %>%
    saveWidget(file="camas_por_km2.html",
                 selfcontained=TRUE)
