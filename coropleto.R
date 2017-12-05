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

coordinates(atlas) <- ~lat+lon
proj4string(atlas) <- CRS("+proj=longlat +datum=WGS84")


# # # # # # # # # # # # # # # # # # # # # # # # # 
# Asignamos cada punto a su barrio correspondiente
pts.poly <- point.in.poly(atlas, barrios)

# # # # # # # # # # # # # # # # # # # # # # # # # 
# Calculamos el índice que queramos

# A. 

titleGraph <- "Relaci\xE1n entre el precio medio del alquiler de uso turistico y residencial"

tmp1 <- tapply(pts.poly@data$rn_fc_avg_price + pts.poly@data$rn_ab_offer,
       pts.poly@data$CODBARRIO, FUN=sum, na.rm=TRUE)

tmp2 <- tapply(pts.poly@data$rn_ab_offer,
       pts.poly@data$CODBARRIO, FUN=sum, na.rm=TRUE)

index <- tmp1/tmp2


# # # # # # # # # # # # # # # # # # # # # # # # # 
# Lo calculamos por barrios

df <- data.frame(cbind(index=as.vector(index),
                       id=as.character(names(tmp1))))

barrios.f <- fortify(barrios, region = "CODBARRIO")

datos <- merge(barrios.f, df, by="id", all.x=TRUE)
final.plot<-datos[order(datos$order), ]
final.plot$indexF <- as.double(final.plot$index)


# # # # # # # # # # # # # # # # # # # # # # # # # 
# A dibujar...
library(ggplot2)

ggplot(final.plot) +
    theme_minimal()+  # no backgroundcolor
    geom_polygon(aes(x = long, y = lat, group = group,
                     fill=indexF))+
    scale_fill_gradient(name="Indice", limits=c(1,110))+
    labs(title=titleGraph)+
    coord_map()

