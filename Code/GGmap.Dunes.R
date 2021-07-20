library(ggmap)
library(ggplot2)
data = read.table(pipe("pbpaste"), header = T)

mapbox <- make_bbox(lon = data$lon, lat = data$lat, f = .1)
mapbox

#Map 1
sq_map <- get_map(location = mapbox, maptype = "satellite", source ="google", zoom = 10)

Stake.Position <- data$color.group

ggmap(sq_map) + geom_point(data = data, aes(x = data$lon, y = data$lat, color="Species"), size = 2)  +geom_line(data=data, aes(x = data$lon, y = data$lat, group=data$group))

ggmap(sq_map) +geom_point(data = site, aes(x = site$lon, y = site$lat, color = "Species"))+ guides(colour = FALSE)

#Map 2



#Map 3
sq_map <- get_map(location = mapbox, maptype = "satellite", source ="google", zoom = 16)

Stake.Position <- data$color.group

ggmap(sq_map) + geom_point(data = data, aes(x = data$lon, y = data$lat, color="Stake.Position"), size = 2)  +geom_line(data=data, aes(x = data$lon, y = data$lat, group=data$group))
