#' Search for Vessels Close to Popups
#'
#' search for nearby vessels closby popup events. It searches within a time window of three days centered on popupdate and spatial box of one degree in lat and lon.
#' @param con a connection to the TOPP database
#' @param rtagnumber tag number code in the TOPP database. This is the serial number from the wildlife computers
#' @param ptt is the argos ID. If the tag was reused that it is possible to have multiple tags for the sam ptt. In that case use latest date.
#' @param spatWin spatial window in degrees
#' @param tempWin temporal window in hours
#' @param gears fishing gears to search
#' @export 
searchVessel = function(con, ptt = "153499", spatWin = 1, tempWin = 24, Plot=FALSE, gears = c("drifting_longlines","trawlers")){

dat = fetch(dbSendQuery(con, statement = paste("select poplat, poplon, popdate, popup_lc from tm_sat_with_eventid where ptt = ",ptt,sep = "")), n = -1) 

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
  FROM 	TABLE_DATE_RANGE([world-fishing-827:pipeline_classify_p_p516_daily.],TIMESTAMP('",mindate,"'),TIMESTAMP('",maxdate,"'))
 WHERE 	lat >",minlat,"
    AND lon <",maxlon,"
    AND lat <",maxlat,"
    AND lon >",minlon, sep = "")
sql = cleanSql(sql)


project = "world-fishing-827"

res <- bigrquery::query_exec(sql, project)

# now I need to check how many of these have been identified as fishing ships
mmsis = unique(res$mmsi)

sql2 = paste("SELECT 
	shiptype_text,
	inferred_label,
	mmsi,
	on_fishing_list 
	FROM [gfw_research.vessel_info_20170717] 
	WHERE mmsi IN (",paste(mmsis, collapse = ","),")", sep = "")
sql2 = cleanSql(sql2)	

ships <- bigrquery::query_exec(sql2, project)
fs = subset(ships, shiptype_text=="Fishing" & inferred_label %in% gears)

# now extract locations in res
locs = subset(res, mmsi %in% unique(fs$mmsi))
locs <- with(locs, locs[order(mmsi, timestamp),]) 
# now make a map


m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=locs$lon, lat=locs$lat, popup=paste(as.character(locs$mmsi), as.character(locs$timestamp), sep = " ")) %>%
  addAwesomeMarkers(lng = dat$lon, lat = dat$lat, icon = awesomeIcons(icon = 'ion-ionic', library = 'ion', markerColor = 'red'), label = as.character(dat$date)) %>%
  addCircles(lng=dat$lon, lat=dat$lat, radius = dat$Location.error, color = "red") 
if (Plot) m else ships # if plot is true it generates the map else a table 

}  
  
  

 