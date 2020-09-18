data("Gulk", package = "riverdist")
library(raster)
x <- sf::st_as_sf(sp::disaggregate(Gulk$sp)[1, ])

## better to segmentize this one
x <- sf::st_segmentize(x, 5)
crs <- sf::st_crs(x)
x <- sf::st_set_crs(x, NA)
library(basf)

buf <- 100
buffer <- sf::st_buffer(x, buf)
## use divisor to give rough num triangles
mesh <- anglr::DEL0(buffer, max_area = unclass(sf::st_area(buffer))/1e5)

## track distance
d <- silicate::sc_coord(x)
d$distance <- c(0, trip::trackDistance(as.matrix(d[c("x_", "y_")]),
                                       longlat = FALSE))

## find nearest
?nabor::WKNND
fun <- nabor::WKNNF(as.matrix(d[c("x_", "y_")]))
idx <- fun$query(as.matrix(silicate::sc_vertex(mesh)), k = 1, eps = 0, radius = 0)

mesh$vertex$distance <- cumsum(d$distance)[idx$nn.idx]
mesh$vertex$z_ <- idx$nn.dists[,1]
library(anglr)
am <- as.mesh3d(mesh)
am$material$color <- colourvalues::color_values(mesh$vertex$distance)
am$meshColor <- "vertices"

plot3d(am)
