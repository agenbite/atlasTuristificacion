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
                             i1 = (atlas$abnb_tot_beds +
                                   0.03329 * atlas$cad_tot_srf_hotels)/
                             0.026))

indice$i1[is.na(indice$i1)] <- 0

coordinates(indice) <- ~lat+lon
proj4string(indice) <- CRS("+proj=longlat +datum=WGS84")

# Asignamos cada punto a su barrio correspondiente
pts.poly <- point.in.poly(indice, barrios)

index <- as.data.frame(tapply(pts.poly@data$i1,
        pts.poly@data$CODBARRIO, FUN=mean, na.rm=TRUE))
index$names <- rownames(index)
colnames(index) <- c("index", "id")

index$lim <- cut(index$index, c(0,600,20000), labels = c("low", "high"))


# Los valores altos son demasiado grandes. Saturamos
#sat <- 200
#index$index[index$index>sat] <- sat

#index$index <- log(1+index$index)


# # # # # # # # # # # # # # # # # # # # # # # # # 
# Lo calculamos por barrios

datos <- merge(barrios.f, index, by="id", all.x=TRUE)
final.plot<-datos[order(datos$order), ]
final.plot$indexF <- as.double(final.plot$index)

barrios@data$index <- index$index


# # # # # # # # # # # # # # # # # # # # # # # # # 
# A dibujar...

titleGraph <- expression("Camas para turistas por"~km^2)

ggplot(final.plot) +
    theme_minimal()+  # no backgroundcolor
    geom_polygon(aes(x = long, y = lat, group = group,
                     fill=indexF)) +
    geom_polygon(data=barrios.f,aes(x=long, y=lat, group=group),
                color="black", alpha=0, size=0.1)+
    ## geom_polygon(data=barrios.f,aes(x=long, y=lat, group=group),
    ##              color=final.plot$lim, alpha=0, size=0.2)+
    ## scale_color_manual(labels=c("","Zonas amenazadas"),
    ##            values=c(NA,"red")) +
    scale_fill_gradient(name=expression(camas/km^2),
                        high = "#132B43", low = "#56B1F7") +
    labs(title=titleGraph) +
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
