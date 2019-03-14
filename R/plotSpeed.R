#' Plot Speed from Satellite Track and Vessels
#'
#' 
#' @param out output object from searchVessel
#' @export 
plotSpeed = function(out){
	dat = out$drfdat
	dat2 = getSpeed(dat)
	locs = getSpeed(out$locs, track = "AIS")
	#pal <- colorNumeric(palette = "YlOrRd",domain = range(locs$speed[locs$speed!=Inf], na.rm=TRUE)
	pal <- colorNumeric(palette = "YlOrRd",domain = c(0,max(locs$speed[locs$speed!=Inf], na.rm=TRUE)))

	m <-leaflet::leaflet() %>% leaflet::addTiles() 

	m = addPolylines(m, lng= ~lon, # lines for tag locations
					 lat= ~lat,
					 data = dat2, 
					 color= "red",
					 weight = 2)

	m = addCircleMarkers(m, lng=~lon, # for tag locations
						lat=~lat, 
						data = dat2, 
						popup=paste(as.character(round(dat2$speed,2))," km/h",sep=""), 
						radius = 2, 
						color = ~pal(dat2$speed))
								  
	# m = addCircleMarkers(lng=~lon, # for tag locations
	# 					lat=~lat, 
	# 					data = dat2, 
	# 					popup=as.character(dat2$datetime), 
	# 					radius = 2, 
	# 					color = ~pal(dat2$speed)) %>%  
	m = addPolylines(m, lng= ~lon, # lines for vessel location
					 lat= ~lat,
					 data = locs, 
					 color= "blue",
					 weight = 2)
	m = addCircleMarkers(m,lng=~lon, # for tag locations
						lat=~lat, 
						data = locs, 
						popup=paste(as.character(round(locs$speed,2))," km/h",sep=""), 
						radius = 2, 
						color = ~pal(locs$speed))
					
	#  	m = addLegend(m, "bottomright", pal = pal, values = na.omit(dat2$speed),
	#     				title = "Tag Speed",
	#     				opacity = 1)

	m = addLegend(m, "bottomright", pal = pal, values = na.omit(c(dat2$speed,locs$speed[locs$speed!=Inf])), # all speeds are represented
						title = "Speed km/h",
						#labFormat = labelFormat(prefix = "$"),
						opacity = 1)	# adds the legend taking the domain of the vessel			         
	m
}