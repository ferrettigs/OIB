#' Search for Vessels Close to Popups
#'
#' This function searches for vessels nearby popup events. It uses the Global Fishing Watch (GFW) AIS database and to Tag of Pelagic Predator (TOPP, `http://gtopp.org/`) database and searches within a time window specified by the user, centered on the popup date, with a spatial box specified by the user in latitude and longitude degrees.
#' @param con a connection to the OIB - TOPP database.
#' @param project Google Cloud Platform project name. This is needed to query the GFW database in bigQuery.
#' @param ptt is the argos ID. If the tag was reused that it is possible to have multiple tags for the sam ptt. In that case use latest date.
#' @param spatWin spatial window in degrees
#' @param tempWin temporal window in hours
#' @param gears fishing gears to search
#' @export 
searchVessel = function(con, project, ptt = "153499", spatWin = 1, tempWin = 24, gears = c("drifting_longlines","trawlers","purse_seines","unknown_fishing","squid_jigger", "pole_and_line","drifting_gillnets","no_idea_what_it_is", "research other_fishing", "set_longlines", "troller", "set_gillnets", "pots_and_traps")){

dat = fetch(dbSendQuery(con, statement = paste("select poplat, poplon, popdate, popup_lc from oibtable where ptt = ",ptt,sep = "")), n = -1) 

names(dat) = c("lat","lon","date","Location.class")


dat$date = strptime(dat$date, format = "%m/%d/%Y %H:%M", tz = "GMT")

locErr = read.csv("../data/locationErr.csv") # location error ranges
dat = merge(dat, locErr, by = "Location.class", all.x =TRUE)

mindate = dat$date-as.difftime(paste(tempWin/2,":00:00",sep=""))
maxdate = dat$date+as.difftime(paste(tempWin/2,":00:00",sep=""))	
# create a spatial window of 1 degree centered on the ping location in both lat and lon
minlat = dat$lat-spatWin/2 # these are too large
maxlat = dat$lat+spatWin/2

minlon = dat$lon-spatWin/2
maxlon = dat$lon+spatWin/2

# now connect with bigquery

require(AIS) # for cleanSql

sql = paste("SELECT
    mmsi, 
    timestamp, 
    lat,
    lon, 
    shiptype
  FROM TABLE_DATE_RANGE([",project,":pipeline_p_p550_daily.classify_],TIMESTAMP('",mindate,"'),TIMESTAMP('",maxdate,"'))
 WHERE 	lat >",minlat,"
    AND lon <",maxlon,"
    AND lat <",maxlat,"
    AND lon >",minlon, sep = "")
sql = cleanSql(sql)


res <- bigrquery::query_exec(sql, project)

res$distance = geosphere::distm(x = res[,c("lon","lat")], y = dat[,c("lon","lat")])/1000 # distance in kilometers
res$timediff = difftime(res$timestamp,dat$date[1], units = "mins")

# now I need to check how many of these have been identified as fishing ships
mmsis = unique(res$mmsi)

sql2 = paste("SELECT 
	shiptype_text,
	inferred_label,
	mmsi,
	on_fishing_list 
	FROM [gfw_research.vessel_info_20180418] 
	WHERE mmsi IN (",paste(mmsis, collapse = ","),")", sep = "")
sql2 = cleanSql(sql2)	

ships <- bigrquery::query_exec(sql2, project)
fs = subset(ships, shiptype_text=="Fishing" | inferred_label %in% gears)
if (nrow(fs)==0) print("No fishing boat identified")

# now extract locations in res
locs = subset(res, mmsi %in% unique(fs$mmsi))
locs <- with(locs, locs[order(mmsi, timestamp),]) 

out = list(locs = locs, ships = ships, dat = dat)
out
}

#' Map Possible Tag - Vessel Interaction
#'
#' This function uses the output of searchVessel and map the locations of te tags and vessel tracks on a leaflet map. 
#' @param out output of \code{searchVessel()}
#' @export 

mapInteraction = function(out){
	require(dplyr)
	# now make a map
	locs = out$locs
	dat = out$dat
	
	m <- leaflet::leaflet() %>%
  		leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
  		leaflet::addMarkers(lng=locs$lon, lat=locs$lat, popup=paste(as.character(locs$mmsi), as.character(locs$timestamp), sep = " ")) %>%
  		leaflet::addAwesomeMarkers(lng = dat$lon, lat = dat$lat, icon = leaflet::awesomeIcons(icon = 'ion-ionic', library = 'ion', markerColor = 'red'), label = as.character(dat$date)) %>%
  		leaflet::addCircles(lng=dat$lon, lat=dat$lat, radius = dat$Location.error, color = "red") 
	m 
}  
  
  
#' Search for Vessels in bbox
#'
#' This function searches for vessels nearby popup events. It uses the Global Fishing Watch (GFW) AIS database and to Tag of Pelagic Predator (TOPP, `http://gtopp.org/`) database and searches within a time window specified by the user, centered on the popup date, with a spatial box specified by the user in latitude and longitude degrees.
#' @param con a connection to the OIB - TOPP database.
#' @param project Google Cloud Platform project name. This is needed to query the GFW database in bigQuery.
#' @param ptt is the argos ID. If the tag was reused that it is possible to have multiple tags for the sam ptt. In that case use latest date.
#' @param spatWin spatial window in degrees
#' @param tempWin temporal window in hours
#' @param gears fishing gears to search
#' @export 
searchVesselInBbox = function(con, project, bbox = c(0,70,-180,-95), year = "2014", month1="01", day1 = "01", year2 = year, month2 = "12", day2 = "31", gears = c("drifting_longlines","trawlers","purse_seines","unknown_fishing","squid_jigger", "pole_and_line","drifting_gillnets","no_idea_what_it_is", "research other_fishing", "set_longlines", "troller", "set_gillnets", "pots_and_traps")){

	time1 = paste(year,"-",month1,"-",day1," 00:00", sep = "")
	time2 = paste(year2,"-",month2,"-",day2," 23:59", sep = "")

	minlat = bbox[1]
	maxlat = bbox[2]
	minlon = bbox[3]
	maxlon = bbox[4]



# now connect with bigquery

require(AIS) # for cleanSql

sql = paste("SELECT
    mmsi, 
    timestamp, 
    lat,
    lon, 
    shiptype
  FROM TABLE_DATE_RANGE([",project,":pipeline_p_p550_daily.classify_],TIMESTAMP('",time1,"'),TIMESTAMP('",time2,"'))
 WHERE 	lat >",minlat,"
    AND lon <",maxlon,"
    AND lat <",maxlat,"
    AND lon >",minlon, sep = "")
sql = cleanSql(sql)


res <- bigrquery::query_exec(sql, project)
mmsis = unique(res$mmsi)

# now I need to check how many of these have been identified as fishing ships


sql2 = paste("SELECT 
	shiptype_text,
	inferred_label,
	mmsi,
	on_fishing_list 
	FROM [gfw_research.vessel_info_20180418] 
	WHERE mmsi IN (",paste(mmsis, collapse = ","),")", sep = "")
sql2 = cleanSql(sql2)	

ships <- bigrquery::query_exec(sql2, project)
fs = subset(ships, shiptype_text=="Fishing" | inferred_label %in% gears)
if (nrow(fs)==0) print("No fishing boat identified")
fs = unique(fs[,c("inferred_label","mmsi","on_fishing_list")])

# some vessels are not being seen
#----- work in progress

# 	sql = paste("SELECT
# 		mmsi, 
# 		timestamp, 
# 		lat,
# 		lon, 
# 		shiptype
# 	  FROM TABLE_DATE_RANGE([",project,":pipeline_p_p550_daily.classify_],TIMESTAMP('",mindate,"'),TIMESTAMP('",maxdate,"'))
# 	 WHERE 	lat >",minlat,"
# 		AND lon <",maxlon,"
# 		AND lat <",maxlat,"
# 		AND lon >",minlon,"
# 		AND mmsi IN (",paste(mmsis, collapse = ","),")" sep = "")
# 	sql = cleanSql(sql)

#-----------------------

out = list(ships = ships, fs = fs)
out
}  
  
  

 