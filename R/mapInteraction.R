#' Map Possible Tag - Vessel Interaction
#'
#' This function uses the output of searchVessel and map the locations of te tags and vessel tracks on a leaflet map. 
#' @param out output of \code{searchVessel()}
#' @param CosestDis indicate closest point in time and space
#' @export 

mapInteraction = function(out, ClosestDist = TRUE){
	#require(dplyr)
	fpath <- system.file("ferry-18.png", package="OIB")
	oceanIcons <- leaflet::iconList(
  		ship = leaflet::makeIcon(fpath, "ferry-18@2x.png", 24, 24),
  		#ship = leaflet::makeIcon(fpath, 24, 24),
  		#ship = leaflet::makeIcon("../images/ferry-18.png","../images/ferry-18@2x.png", 24, 24),
  		pirate = leaflet::makeIcon("danger-24.png", "danger-24@2x.png", 24, 24)
	)
	# the above option will not work for a remote shiny app
	
	
	
	# extract data from out
	locs = out$locs # locations of boats
	popdat = out$popdat # popup location
	ships = out$ships # ship info
	drfdat = out$drfdat # drifting locations of PATs
	# remove inaccurate locations
	drfdat = drfdat[!is.na(drfdat$Location.error),] # it means excluding classes A, B and Z. http://www.argos-system.org/manual/3-location/34_location_classes.htm 
	drfdat = drfdat[order(drfdat$datetime),]
	
	m <-leaflet::leaflet() %>%
  			leaflet::addTiles()  # Add default OpenStreetMap map tiles
	
	if (nrow(locs)!=0){
	
		locs$file <- paste('http://baseline3.stanford.edu/pbImageRec/code/',locs$mmsi,'.jpg',sep = "") # gets images of the boat

		binpal = colorBin(palette="Blues", domain = locs$mmsi, bins = length(unique(locs$mmsi))+1) # creates a palette for different boats
							# +1 is necessary to avoid problems when there is only a boat
		
  	
		for(group in levels(as.factor(locs$mmsi))){
					m = addPolylines(m, 
						  lng= ~lon,
						  lat= ~lat,
						  data = locs[locs$mmsi == group,], 
						  color= ~ binpal(mmsi),
						  weight = 3)
		} # adds a track for each boat
	
	  	  
		locs2 = locs %>% 
    	dplyr::group_by(mmsi) %>% 
    	slice(which.max(timestamp))
    	locs2 = as.data.frame(locs2)
    	locs2 = unique(merge(locs2,ships[,c("mmsi","shipname","inferred_label","known_geartype")], by = "mmsi", all.x = T)) # not sure why I need to unique

		m = addMarkers(m, lng=~lon, lat=~lat, data = locs2, popup=paste(
		"MMSI:", as.character(locs2$mmsi),"<br>",
		"Timestamp:",as.character(locs2$timestamp),"<br>",
		"Ship name:", as.character(locs2$shipname),"<br>", 
		"Inferred label:", as.character(locs2$inferred_label),"<br>",
		"Known gear type:", as.character(locs2$known_geartype),"<br>",
		paste0("<img src = ", locs2$file, " width=150>"), sep = " "), 
		icon = ~oceanIcons["ship"])
	}
  	m = addAwesomeMarkers(m, lng = ~lon, 
  						lat = ~lat, 
  						data = popdat, 
  						icon = leaflet::awesomeIcons(icon = 'ion-ionic', library = 'ion', markerColor = 'red'), 
  						label = as.character(popdat$datetime))
  						
  	m = addCircles(m, lng=~lon, # for popup marker
  				lat=~lat, 
  				data = popdat, 
  				radius = ~Location.error, 
  				color = "red")
  	if (nrow(locs)!=0){			
		m = addCircleMarkers(m, lng=~lon, # for boat locations
						lat=~lat, 
						data = locs, 
						popup=as.character(locs$timestamp), 
						radius = 3, 
						color = ~ binpal(mmsi))
  	}				
  	m = addCircleMarkers(m, lng=~lon, # for tag locations
  					lat=~lat, 
  					data = drfdat, 
  					popup=as.character(drfdat$datetime), 
  					radius = 2, 
  					color = "red")
  					
  	m = addPolylines(m, lng= ~lon, # connect tag locations
                 lat= ~lat,
                 data = drfdat, 
                 color= "red",
                 weight = 2) 
     
    # get distance for the closest distance   
    if (nrow(locs)!=0 & ClosestDist){          
		segment = try(getClosestPosition(out)) # closest points between tag and ship trajectories - these also include the tag dimension
		if (class(segment)!="try-error"){
		m = addPolylines(m, lng= ~lon, # for tag locations
					 lat= ~lat,
					 data = segment, 
					 color= "yellow",
					 weight = 4,
					 dashArray = "4",
					 popup = paste(
							"Distance:", as.character(round(segment$distance[2],1)),"km <br>",
							"Timelag:",as.character(round(segment$timelag[2],2)),"hours",
							 sep = " ")) } # for dashed patterns           
	}                 
                 #%>%
    #addRectangles(lng1=min(c(popdat$lon,drfdat$lon)), lat1=min(c(popdat$lat,drfdat$lat)),
    #lng2=max(c(popdat$lon,drfdat$lon)), lat2=max(c(popdat$lat,drfdat$lat)))             
  	m

}  